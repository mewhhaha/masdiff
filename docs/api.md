# API overview

This document summarizes the public API and the key determinism/data-ordering
invariants for masdiff.

## Modules

- `MSDF.Generated`
  - `generateMSDF :: FilePath -> IO (Either ParseError MSDFAtlas)`
  - `generateMSDFWithConfig :: MSDFConfig -> FilePath -> IO (Either ParseError MSDFAtlas)`
  - `generateMSDFFromTTF :: MSDFConfig -> TTF -> MSDFAtlas`
  - `generateMSDFOrThrow :: FilePath -> IO MSDFAtlas`
- `MSDF.MSDF`
  - `MSDFConfig` (render configuration)
  - `GlyphSet` (glyph selection)
  - `GlyphCache`, `prepareGlyphCache`, `renderGlyphMSDFCached`
  - `GlyphCacheLazy`, `prepareGlyphCacheLazy`, `renderGlyphMSDFCachedLazy` (on-demand outline caching)
  - `defaultMSDFConfig`
  - `renderGlyphMSDF` / `glyphMetricsOnly`
- `MSDF.Render`
  - `Origin`, `YAxis` (coordinate conventions)
  - `SamplerHints` (recommended sampling settings)
  - `atlasOrigin`, `uvOrigin`, `glyphQuadSpace`, `msdfSamplerHints`
  - `pixelRange` (screen-space range factor)
  - `scaleForPixelSize` / `pixelRangeForAtlas` (size/normalization helpers)
  - `glyphQuad` / `glyphQuadYDown` (quad bounds from pen position)
  - `glyphUV` / `glyphUVTopLeft` (atlas UVs or default)
- `MSDF.Types`
  - `MSDFAtlas`, `GlyphMSDF`, `VerticalMetrics`, `MSDFBitmap`, `BBox`, `KerningPair`
  - `Anchor`, `MarkGlyph`, `BaseGlyph`, `MarkToBase`, `MarkToMark`
  - `AtlasImage`, `GlyphPlacement` (packed atlas support)
  - `lookupCodepoint`, `lookupKerning`
- `MSDF.TTF.Parser`
  - `parseTTF`, `parseTTFUnsafe`
  - `glyphOutline`, `glyphBBoxRaw`
  - `VariationLocation`, `normalizeLocation`

## MSDFConfig fields

- `pixelSize`: output pixel size (used for scaling from font units).
- `range`: MSDF pixel range (see shader expectations in README).
- `cornerThreshold`: edge coloring threshold in radians.
- `glyphSet`: which glyphs to rasterize (others get metrics only).
- `parallelism`: `0` disables parallel rendering, otherwise chunk size for `parListChunk`.
- `variations`: axis settings like `[("wght", 700)]` (user-space values).
- `packAtlas`: when `True`, pack glyphs into a single atlas and populate `atlas`.
- `atlasPadding`: pixel padding around glyphs in the atlas.
- `atlasMinSize`/`atlasMaxSize`: atlas size bounds (power‑of‑two when enabled).
- `atlasPowerOfTwo`: round atlas dimensions to next power‑of‑two.
- `msdfCorrectionThreshold`: correction threshold for MSDF error smoothing.

## Determinism and ordering

masdiff aims to be deterministic for a given font/config:

- `glyphs` is an `Array` indexed by glyph index. This index is stable for a given font.
- `codepointIndex` is sorted by codepoint ascending and contains unique codepoints.
  If a font provides multiple mappings for the same codepoint, masdiff picks the
  smallest glyph index for predictability.
- `atlas` (when present) is deterministically packed; `GlyphMSDF.placement` holds
  pixel and UV coordinates into the atlas.
- `lookupCodepoint` returns `Nothing` if the mapped glyph index is outside the
  atlas bounds (malformed cmap/metrics tables).
- `codepoints` in each `GlyphMSDF` are unique and sorted ascending.
- `kerning` is sorted by `(left, right)` ascending; `lookupKerning` is binary search.
- `markToBase` / `markToMark` preserve lookup order; each entry maps glyph indices to
  anchors in pixel units (`Anchor`).
- `parallelism` changes evaluation strategy only; output data is identical to
  single-threaded rendering.

## Variable fonts

- Supported tables: `fvar`, `avar`, `gvar`, `HVAR`, `VVAR`, `MVAR` (hhea/vhea tags).
- `MSDFConfig.variations` selects axes by tag in user-space values; coordinates
  are normalized and mapped by `avar` if present.
- Vertical metrics are exposed via `GlyphMSDF.vertical` and `MSDFAtlas.vAscent/vDescent/vLineGap`.
- Unsupported: most other `MVAR` tags and CFF outlines.
