# API overview

This document summarizes the public API and the key determinism/data-ordering
invariants for masdiff.

## Modules

- `MSDF.Generated`
  - `generateMSDF :: FilePath -> IO (Either ParseError MSDFAtlas)`
  - `generateMSDFWithConfig :: MSDFConfig -> FilePath -> IO (Either ParseError MSDFAtlas)`
  - `generateMSDFOrThrow :: FilePath -> IO MSDFAtlas`
- `MSDF.MSDF`
  - `MSDFConfig` (render configuration)
  - `GlyphSet` (glyph selection)
  - `defaultMSDFConfig`
  - `renderGlyphMSDF` / `glyphMetricsOnly`
- `MSDF.Types`
  - `MSDFAtlas`, `GlyphMSDF`, `MSDFBitmap`, `BBox`, `KerningPair`
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

## Determinism and ordering

masdiff aims to be deterministic for a given font/config:

- `glyphs` is an `Array` indexed by glyph index. This index is stable for a given font.
- `codepointIndex` is sorted by codepoint ascending and contains unique codepoints.
  If a font provides multiple mappings for the same codepoint, masdiff picks the
  smallest glyph index for predictability.
- `lookupCodepoint` returns `Nothing` if the mapped glyph index is outside the
  atlas bounds (malformed cmap/metrics tables).
- `codepoints` in each `GlyphMSDF` are unique and sorted ascending.
- `kerning` is sorted by `(left, right)` ascending; `lookupKerning` is binary search.
- `parallelism` changes evaluation strategy only; output data is identical to
  single-threaded rendering.

## Variable fonts

- Supported tables: `fvar`, `avar`, `gvar`, `HVAR`, `MVAR` (horizontal metrics).
- `MSDFConfig.variations` selects axes by tag in user-space values; coordinates
  are normalized and mapped by `avar` if present.
- Unsupported: VVAR, most MVAR tags (only `hasc`, `hdes`, `hlgp` are applied),
  and CFF outlines.
