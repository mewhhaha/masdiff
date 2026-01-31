# masdiff

Pure Haskell MSDF generator for TrueType fonts. It parses `glyf/loca/cmap` outlines and produces per‑glyph MSDF bitmaps plus metrics and kerning (legacy `kern` and GPOS pair adjustments).

## Features

- Pure Haskell pipeline (no external CLI/tools).
- TrueType outline parsing (simple + composite glyphs).
- MSDF raster generation per glyph (packed RGB bytes).
- Kerning support via `kern` and GPOS pair adjustment (lookup type 2).
- Vertical metrics via `vhea/vmtx` when present.
- Variable font support (fvar/avar/gvar + HVAR/VVAR/MVAR for metrics).
- Optional atlas packing with per-glyph UV placement.

## Usage

```haskell
import MSDF.Generated (generateMSDF, generateMSDFOrThrow)
import MSDF.TTF.Parser (ParseError(..))

main :: IO ()
main = do
  result <- generateMSDF "path/to/font.ttf"
  case result of
    Left err -> putStrLn (err.context ++ ": " ++ err.message)
    Right atlas -> print (atlas.fontName)

-- Or throw on parse errors:
main = do
  atlas <- generateMSDFOrThrow "path/to/font.ttf"
  print (atlas.fontName)
```

### Custom config

```haskell
import MSDF.Generated (generateMSDFWithConfig)
import MSDF.MSDF (defaultMSDFConfig, GlyphSet(..))
import qualified MSDF.MSDF as MSDF
import MSDF.TTF.Parser (ParseError(..))

main :: IO ()
main = do
  let cfg = defaultMSDFConfig
        { MSDF.pixelSize = 24
        , MSDF.glyphSet = GlyphSetCodepoints [65,66,67]
        , MSDF.parallelism = 64 -- optional: chunk size for parallel rendering
        , MSDF.variations = [("wght", 700)] -- optional: variable font axes
        , MSDF.packAtlas = True
        , MSDF.atlasPadding = 1
        }
  result <- generateMSDFWithConfig cfg "path/to/font.ttf"
  case result of
    Left err -> putStrLn (err.context ++ ": " ++ err.message)
    Right atlas -> print (length (atlas.glyphs))
```

Variable font example (Inter Variable, weight 400 vs 700):

```haskell
let cfgRegular = defaultMSDFConfig { MSDF.variations = [("wght", 400)] }
let cfgBold = defaultMSDFConfig { MSDF.variations = [("wght", 700)] }
atlasRegular <- generateMSDFWithConfig cfgRegular "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
atlasBold <- generateMSDFWithConfig cfgBold "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
```

## Tests

```sh
cabal test msdf-tests
```

## API overview

See `docs/api.md` for a concise overview of the public modules, data ordering
invariants, and determinism guarantees.
See `docs/render_guide.md` for a concrete MSDF rendering reference (GLSL/WGSL).
See `docs/reference_renderer.md` for a minimal rendering sample.
See `docs/versioning.md` for versioning and changelog policy.

## Pseudocode: rendering MSDF glyphs

This is a high-level sketch of how to use the generated MSDF data to render text.
It assumes you have a shader that decodes MSDF and outputs alpha.
Helper utilities are available in `MSDF.Render` (`glyphQuad`, `glyphUV`, `pixelRange`).
If you render the same glyphs at multiple sizes, use `MSDF.MSDF.prepareGlyphCache`
and `renderGlyphMSDFCached` to reuse parsed outlines.

```
atlas = generateMSDFOrThrow("path/to/font.ttf")

-- Build a glyph texture for each glyph (or use packed atlas).
if atlas.atlas != None:
  uploadTexture("atlas", atlas.atlas.width, atlas.atlas.height, atlas.atlas.pixelsRGB)
else:
  for glyph in atlas.glyphs:
    if glyph.bitmap.width == 0:
      continue
    uploadTexture(glyph.index, glyph.bitmap.width, glyph.bitmap.height, glyph.bitmap.pixelsRGB)

-- Layout text.
penX = 0
penY = baseline
for codepoint in text:
  glyphIndex = lookupCodepoint(atlas, codepoint)
  if glyphIndex == None:
    continue
  glyph = atlas.glyphs[glyphIndex]

  -- Apply kerning (if you have a previous glyph).
  if prevGlyphIndex != None:
    penX += lookupKerning(atlas, prevGlyphIndex, glyphIndex)

  -- Compute quad in screen space.
  x0 = penX + glyph.bbox.xMin - glyph.bitmap.offsetX
  y0 = penY - glyph.bbox.yMax - glyph.bitmap.offsetY
  x1 = x0 + glyph.bitmap.width
  y1 = y0 + glyph.bitmap.height

  -- Draw quad textured with the glyph's MSDF bitmap.
  if glyph.placement != None:
    uv = (glyph.placement.u0, glyph.placement.v0, glyph.placement.u1, glyph.placement.v1)
    texture = "atlas"
  else:
    uv = (0,0,1,1)
    texture = glyph.index
  drawQuad(texture = texture, position = (x0, y0, x1, y1), uv = uv)

  -- Advance pen.
  penX += glyph.advance
  prevGlyphIndex = glyphIndex
```

MSDF shader idea (pseudocode):

```
sample = texture(msdfTex, uv).rgb
sd = median(sample.r, sample.g, sample.b)
alpha = clamp((sd - 0.5) * pxRange + 0.5, 0, 1)
output = vec4(textColor.rgb, textColor.a * alpha)
```

### Shader expectations (example)

When rendering MSDF glyphs, the shader is expected to:

- Sample the MSDF texture in **linear color space** (disable sRGB sampling for the MSDF).
- Use **bilinear filtering** (nearest gives jagged edges).
- Compute a pixel-range factor based on glyph scale and the MSDF `range` used at generation time.
- Use the **median** of RGB channels to get the signed distance.
- Convert distance to alpha with a smooth range to avoid hard edges.

Example GLSL-like fragment shader:

```
uniform sampler2D msdfTex;
uniform vec4 textColor;
uniform float pxRange;  // screen-space pixel range, e.g. (atlasRange * scale)

float median(float r, float g, float b) {
  return max(min(r, g), min(max(r, g), b));
}

void main() {
  vec3 sample = texture(msdfTex, vUV).rgb;
  float sd = median(sample.r, sample.g, sample.b) - 0.5;
  float alpha = clamp(sd * pxRange + 0.5, 0.0, 1.0);
  outColor = vec4(textColor.rgb, textColor.a * alpha);
}
```

Notes:

- `pxRange` depends on how the glyph is scaled on screen. If MSDF was generated with `range = 4` and the glyph is scaled by `S`, a common approximation is `pxRange = 4 * S`.
- Use **premultiplied alpha** if your pipeline expects it.

Example WESL fragment shader:

```
@group(0) @binding(0) var msdfTex : texture_2d<f32>;
@group(0) @binding(1) var msdfSampler : sampler;
@group(0) @binding(2) var<uniform> uTextColor : vec4<f32>;
@group(0) @binding(3) var<uniform> uPxRange : f32;

fn median(r: f32, g: f32, b: f32) -> f32 {
  return max(min(r, g), min(max(r, g), b));
}

@fragment
fn fs_main(@location(0) vUV: vec2<f32>) -> @location(0) vec4<f32> {
  let sample = textureSample(msdfTex, msdfSampler, vUV).rgb;
  let sd = median(sample.r, sample.g, sample.b) - 0.5;
  let alpha = clamp(sd * uPxRange + 0.5, 0.0, 1.0);
  return vec4<f32>(uTextColor.rgb, uTextColor.a * alpha);
}
```

## Limitations

- TrueType outlines only (no CFF).
- Variable fonts support glyf variations and metrics via HVAR/VVAR/MVAR.
  Most other MVAR tags are not implemented.
- GPOS support includes Pair Adjustment (format 1/2), MarkToBase (type 4), and MarkToMark (type 6).
  Other lookups (mark-to-ligature, contextual, device/variation anchors) are not implemented.
- Edge-coloring correction is heuristic and not a full msdfgen parity pass.

## Benchmarks

```
cabal run msdf-bench -- --pixel-size 32 --glyphs 256
```

Glyph selection:

- `GlyphSetAll` renders all glyphs.
- `GlyphSetNone` renders only metrics (no bitmaps).
- `GlyphSetCodepoints [...]` renders a subset.

## Assets and licensing

The repository includes the Inter font for test coverage under its own license (OFL). See `assets/Inter/OFL.txt`.

## License

MIT. See `LICENSE`.
