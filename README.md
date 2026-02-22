# masdiff

`masdiff` is a Haskell library and CLI for generating MTSDF glyph textures from TrueType/OpenType fonts, validating parity, and preview rendering.

Runtime model:

- library/runtime generation is Haskell (`native`).
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

## Development Process (Using `msdfgen` as Oracle)

This is the workflow used during development to tune and verify native output.
`msdfgen` is treated as an external oracle only and is not a runtime dependency
of the shipped native library path.

Reference project: [Chlumsky/msdfgen](https://github.com/Chlumsky/msdfgen)

### 1) Generate oracle fixtures with external `msdfgen`

```bash
MSDFGEN_BIN=msdfgen \
MASDIFF_BACKEND=process \
MTSDF_OUT=out/reference/inter-mtsdf-oracle \
cabal run generate-inter-mtsdf-fixtures
```

### 2) Generate native fixtures with the same corpus settings

```bash
MASDIFF_BACKEND=native \
MTSDF_OUT=out/reference/inter-mtsdf-native \
cabal run generate-inter-mtsdf-fixtures
```

### 3) Validate native generation against oracle manifest

This re-generates each manifest row with native code and diffs against oracle PNGs.

```bash
MASDIFF_BACKEND=native \
cabal run masdiff-validate -- \
  --manifest out/reference/inter-mtsdf-oracle/manifest.tsv \
  --verbose
```

### 4) Run parity smoke checks

```bash
cabal run masdiff-parity -- --verbose
```

Notes:

- `masdiff-parity` requires an external `msdfgen` on `PATH` (or configured via `MSDFGEN_BIN`).
- oracle comparison is a development gate only.
- production/runtime usage should use `native` generation and the WGSL shader path above.

## Docs

- CLI reference: `docs/CLI.md`
- Exposed API: `docs/API.md`
- Docs index: `docs/README.md`
