# GPU Consumer Handoff

This documents how GPU rasterization is integrated after removing the built-in `native-vulkan` runtime mode.

## Current state

- Runtime backend selection is now:
  - `native` (pure Haskell generation)
  - `process` (msdfgen oracle path)
- GPU integration is consumer-supplied through callback APIs:
  - `type RasterPreparedIO = GenCfg -> PreparedGlyph -> IO (Either GenErr GenOut)`
  - `generateGlyphNativeWithIO`
  - `generateGlyphBatchNativeWithIO`
  - `generateAtlasWithRasterIO`
- Prep remains in masdiff:
  - `prepareGlyphNativeIO` / `prepareGlyphBatchNativeIO`
  - `PreparedGlyph` carries parsed/normalized outline data for raster callbacks.

## Integration plan for Vulkan consumers

1. Implement a Vulkan compute raster callback with type `RasterPreparedIO`.
2. Feed the callback to:
   - `generateGlyphNativeWithIO` / `generateGlyphBatchNativeWithIO`
   - `generateAtlasWithRasterIO`
3. Keep a CPU fallback inside your application for unsupported GPUs/drivers.
4. Keep `MASDIFF_BACKEND=native` in runtime config; GPU choice happens in your callback, not in `BackendMode`.

## Verification gates

1. `cabal test masdiff-test`
2. `cabal run masdiff-parity -- --require-exact`
3. Add consumer-side diff checks (CPU callback vs Vulkan callback) for:
   - representative glyph set (`A`, `M`, `R`, `Y`, `a`, `g`, `q`, `0`, `9`, `/`, `:`, `?`, `!`)
   - static + variable Inter cases (`wght=300, opsz=14` and `wght=900, opsz=32`)
4. In SDL3 demo/app, save debug snapshots for both callback paths and compare.
