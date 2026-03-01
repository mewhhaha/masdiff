# masdiff

`masdiff` is a Haskell library and CLI for generating MTSDF glyph textures from TrueType/OpenType fonts, validating parity, and preview rendering.

Runtime model:

- library/runtime generation is Haskell (`native`).
- consumers can plug in their own raster backend via `MSDF.Native.generateGlyphNativeWithIO` / `generateGlyphBatchNativeWithIO`.
- external oracle comparison is development-only (documented below).

## Build

```bash
cabal build
cabal test
cabal bench masdiff-bench
```

## Library-First Workflow (Recommended)

This is the intended integration path: use `masdiff` as a library to generate MTSDF textures and render them with your own WGSL shader.

### 1) Generate glyph MTSDF from Haskell

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Char (ord)
import MSDF.Encode (writePngRGBA8File)
import MSDF.Generate (defaultRuntimeCfg, generateGlyphIO)
import MSDF.Types
  ( FontSrc (..), GenCfg (..), Mode (..)
  , mkDim, mkGlyphCode, mkPxRange
  )

main :: IO ()
main = do
  dim <- either fail pure (mkDim 64)
  pxr <- either fail pure (mkPxRange 8.0)
  glyph <- either fail pure (mkGlyphCode (ord 'A'))
  let cfg =
        GenCfg
          { mode = Mtsdf
          , dim = dim
          , pxr = pxr
          , seed = 1
          , autoframe = True
          , ovlp = False
          }
      src = FontFile {path = "assets/Inter/static/Inter_24pt-Regular.ttf"}
  result <- generateGlyphIO defaultRuntimeCfg cfg src glyph
  case result of
    Left err -> fail (show err)
    Right out -> writePngRGBA8File "A.mtsdf.png" out.img
```

### 1b) Generate a glyph batch with bounded parallelism

```haskell
import Data.Char (ord)
import MSDF.Generate (defaultRuntimeCfg, generateGlyphBatchIO)
import MSDF.Types
  ( FontSrc (..),
    GenCfg (..),
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkPxRange
  )

main :: IO ()
main = do
  dim <- either fail pure (mkDim 64)
  pxr <- either fail pure (mkPxRange 8.0)
  let cfg =
        GenCfg
          { mode = Mtsdf
          , dim = dim
          , pxr = pxr
          , seed = 1
          , autoframe = True
          , ovlp = False
          }
      src = FontFile {path = "assets/Inter/static/Inter_24pt-Regular.ttf"}
  -- jobs=1 is sequential; jobs>1 enables bounded worker concurrency.
  glyphs <- traverse (either fail pure . mkGlyphCode . ord) "MASDIFF"
  results <- generateGlyphBatchIO defaultRuntimeCfg 8 cfg src glyphs
  case sequence results of
    Left err -> fail (show err)
    Right outs -> putStrLn ("Generated " <> show (length outs) <> " glyphs")
```

Notes:
- In `BackendNative`, batch generation parses OpenType/TrueType tables once per batch (not once per glyph).
- For multicore throughput, run with `+RTS -N -RTS` and choose `jobs` around `numCapabilities`.

### 1c) Prepare once, raster later (advanced API)

Use this path when you want to cache font parsing/outline prep separately from raster strategy.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Char (ord)
import MSDF.Native
  ( prepareGlyphNativeIO
  , rasterPreparedCpu
  )
import MSDF.Types (FontSrc (..), GenCfg (..), Mode (..), mkDim, mkGlyphCode, mkPxRange)

main :: IO ()
main = do
  dim <- either fail pure (mkDim 64)
  pxr <- either fail pure (mkPxRange 8.0)
  g <- either fail pure (mkGlyphCode (ord 'A'))
  let cfg = GenCfg {mode = Mtsdf, dim = dim, pxr = pxr, seed = 1, autoframe = True, ovlp = False}
      src = FontFile {path = "assets/Inter/static/Inter_24pt-Regular.ttf"}
  prepared <- prepareGlyphNativeIO src g >>= either (fail . show) pure
  outCpu <- either fail pure (rasterPreparedCpu cfg prepared)
  print outCpu.metrics.adv
```

### 1d) Consumer-managed GPU raster callback

If you have your own Vulkan/WGPU rasterizer, keep `masdiff` for font parsing + prep and provide your own raster callback.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

import MSDF.Native
  ( RasterPreparedIO
  , generateGlyphBatchNativeWithIO
  , rasterPreparedCpu
  )
import MSDF.Atlas (generateAtlasWithRasterIO)

-- Replace this with your own GPU implementation.
gpuRaster :: RasterPreparedIO
gpuRaster cfg prepared =
  pure (rasterPreparedCpu cfg prepared)

-- Then call:
-- results <- generateGlyphBatchNativeWithIO 8 gpuRaster cfg src glyphs
-- atlas  <- generateAtlasWithRasterIO 8 gpuRaster atlasCfg cfg src glyphs
```

### 1e) Build atlas pages from Haskell

```haskell
import Data.Char (ord)
import MSDF.Atlas (defaultAtlasCfg, generateAtlasIO, renderAtlasTsv)
import MSDF.Encode (writePngRGBA8File)
import MSDF.Generate (defaultRuntimeCfg)
import MSDF.Types (mkGlyphCode)

main :: IO ()
main = do
  glyphs <- traverse (either fail pure . mkGlyphCode . ord) "MASDIFF ATLAS"
  atlasResult <- generateAtlasIO defaultRuntimeCfg 8 defaultAtlasCfg cfg src glyphs
  atlas <- either fail pure atlasResult
  traverse_ (\page -> writePngRGBA8File ("atlas.page-" <> show page.idx <> ".png") page.img) atlas.pages
  writeFile "atlas.tsv" (renderAtlasTsv atlas)
```

Atlas notes:
- input glyph list may contain duplicates; atlas generation deduplicates by glyph code.
- output includes page images and TSV metadata (`page`, `x`, `y`, `w`, `h`, metrics).

Texture contract:

- `rgb` = MSDF channels.
- `a` = SDF fallback channel.
- Texture format should be sampled as normalized floats in shader (`texture_2d<f32>` in WGSL).

### 2) WGSL shader implementation

The shader logic below matches the library-side `MSDF.TextRender.sampleCoverage` semantics:

- signed distance from `median(r,g,b) - 0.5`
- optional alpha fallback when `abs(msdfSd - sdfSd) > fallbackThreshold`
- coverage = `clamp(screenPxRange * sd + 0.5, 0, 1)`

```wgsl
struct MsdfParams {
  px_range: f32,                 // generation pxrange used in GenCfg
  fallback_threshold: f32,       // e.g. 0.0
  alpha_fallback: u32,           // 0 or 1
  use_fixed_screen_px_range: u32,// 0 or 1
  fixed_screen_px_range: f32,    // used when flag above is 1
};

@group(0) @binding(0) var msdf_tex: texture_2d<f32>;
@group(0) @binding(1) var msdf_smp: sampler;
@group(0) @binding(2) var<uniform> msdf: MsdfParams;

fn median3(a: f32, b: f32, c: f32) -> f32 {
  return max(min(a, b), min(max(a, b), c));
}

fn auto_screen_px_range(uv: vec2<f32>) -> f32 {
  let tex_size = vec2<f32>(textureDimensions(msdf_tex));
  let unit_range = vec2<f32>(msdf.px_range) / tex_size;
  let screen_tex_size = 1.0 / fwidth(uv);
  return max(1.0, 0.5 * dot(unit_range, screen_tex_size));
}

fn mtsdf_coverage(sample_rgba: vec4<f32>, screen_px_range: f32) -> f32 {
  let msdf_sd = median3(sample_rgba.r, sample_rgba.g, sample_rgba.b) - 0.5;
  let sdf_sd = sample_rgba.a - 0.5;
  let msdf_cov = clamp(screen_px_range * msdf_sd + 0.5, 0.0, 1.0);
  let sdf_cov = clamp(screen_px_range * sdf_sd + 0.5, 0.0, 1.0);
  let use_alpha =
    (msdf.alpha_fallback != 0u) &&
    abs(msdf_sd - sdf_sd) > msdf.fallback_threshold;
  return select(msdf_cov, max(msdf_cov, sdf_cov), use_alpha);
}

@fragment
fn fs_main(@location(0) uv: vec2<f32>) -> @location(0) vec4<f32> {
  let s = textureSample(msdf_tex, msdf_smp, uv);
  let spr = select(
    auto_screen_px_range(uv),
    msdf.fixed_screen_px_range,
    msdf.use_fixed_screen_px_range != 0u
  );
  let coverage = mtsdf_coverage(s, spr);

  // Black text on white background:
  let gray = 1.0 - coverage;
  return vec4<f32>(gray, gray, gray, 1.0);
}
```

Practical defaults:

- `px_range`: use the same value you generated with (`GenCfg.pxr`).
- `alpha_fallback`: `1`
- `fallback_threshold`: `0.0`
- `use_fixed_screen_px_range`: `0` (auto mode)

### 3) Use the CPU shader path as a reference

For debugging and regression tests, `MSDF.TextRender` provides a CPU implementation of the same shader logic.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

import MSDF.TextRender
  ( ScreenPxRange (..)
  , mkShaderCfg
  , shadeMtsdfImgTo
  )

-- Given `out.img` from generation:
shader <- either fail pure (mkShaderCfg (AutoPxRange 6.0) True 0.0)
preview <- either fail pure (shadeMtsdfImgTo shader 64 64 out.img)
```

## File Generation and Validation (CLI)

Use CLI when you want fixtures/artifacts on disk or validation jobs.

### Generate Inter fixture corpus

```bash
cabal run generate-inter-mtsdf-fixtures
```

Default output:

- `out/reference/inter-mtsdf/manifest.tsv`
- PNG fixtures under `out/reference/inter-mtsdf/<font-case>/`

Environment:

- `MASDIFF_BACKEND=native` (default)
- `MASDIFF_BACKEND=process` (optional oracle backend for comparison/fixtures)
- `MTSDF_OUT=out/reference/inter-mtsdf`
- `MTSDF_DIM=64`
- `MTSDF_PXRANGE=8.0`
- `MTSDF_CLEAN=true|false`

### Validate manifest

```bash
cabal run masdiff-validate -- --manifest out/reference/inter-mtsdf/manifest.tsv --verbose
```

### Native vs process parity

```bash
cabal run masdiff-parity -- --verbose
```

## SDL3 Spirdo text example

For SDL3 users, see the bundled example at `examples/sdl3-spirdo-text`:
- It demonstrates **manual SDL3 bindings** with no `slop` dependency.
- It generates/shades text with `masdiff` and renders it in an SDL3 window.
- It compiles WGSL with **Spirdo** as a shader validation step.

Run the example:

```bash
cd examples/sdl3-spirdo-text
MASDIFF_SDL_GEN_BACKEND=gpu MASDIFF_SDL_GEN_STRICT=1 cabal run sdl3-spirdo-text
```

See [`examples/sdl3-spirdo-text/README.md`](examples/sdl3-spirdo-text/README.md) for exact launch steps.

### Render preview line

```bash
cabal run masdiff-text-render -- \
  --text "MASDIFF PARITY 2026" \
  -font assets/Inter/static/Inter_24pt-Regular.ttf \
  -o final.png \
  --gen-dim 96 \
  --dim 32 \
  --pxrange 6.0
```

### Build atlas pages (CLI)

```bash
cabal run masdiff-atlas -- \
  --text "PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS" \
  -font assets/Inter/static/Inter_24pt-Regular.ttf \
  --out-prefix out/atlas/inter24 \
  --gen-dim 64 \
  --pxrange 8.0 \
  --atlas-w 1024 \
  --atlas-h 1024 \
  --padding 1 \
  --jobs 8 \
  --verbose
```

Outputs:
- `out/atlas/inter24.page-000.png`, `out/atlas/inter24.page-001.png`, ...
- `out/atlas/inter24.tsv`

### Single glyph generation (CLI subset)

```bash
cabal run masdiff -- \
  mtsdf \
  -font assets/Inter/static/Inter_24pt-Regular.ttf A \
  -dimensions 64 64 \
  -pxrange 8.0 \
  -o out/A.png \
  -format png \
  -printmetrics
```

## Development Process (Profiled Parity)

This is the workflow used during development to tune and verify native output.

`msdfgen` (`process`) is still supported as an oracle, and optional `msdfgl`
artifacts can be compared through a manifest path.

`masdiff-parity` runs coverage-first and expands scope by profile.

### 1) Validate PR profile (stable corpus, exact)

```bash
cabal run masdiff-parity -- \
  --profile pr \
  --manifest out/reference/inter-mtsdf-oracle/manifest.tsv \
  --require-oracle \
  --verbose
```

### 2) Extend coverage with nightly profile

```bash
cabal run masdiff-parity -- \
  --profile nightly \
  --oracle both \
  --manifest out/reference/inter-mtsdf-oracle/manifest.tsv \
  --json-out out/parity-nightly.json \
  --allow-missing-oracle
```

### 3) Optional full-coverage pass

```bash
cabal run masdiff-parity -- \
  --profile full \
  --max-cases 1000 \
  --oracle msdfgl \
  --manifest out/reference/inter-mtsdf-oracle/manifest.tsv \
  --json-out out/parity-full.json
```

Notes:

- `--profile pr` remains strict exact on the stable corpus.
- `--profile nightly` and `--profile full` expand coverage and use shape/metrics thresholds for broader cases.
- production/runtime usage should use `native` generation and the WGSL shader path above.
- `--oracle process` needs `MSDFGEN_BIN`/`PATH` for `msdfgen` access; `--oracle msdfgl` reads from the manifest artifacts.

## Docs

- CLI reference: `docs/CLI.md`
- Exposed API: `docs/API.md`
- GPU backend handoff: `docs/GPU_BACKEND_HANDOFF.md`
- Docs index: `docs/README.md`
