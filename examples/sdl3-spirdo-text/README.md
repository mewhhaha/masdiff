# SDL3 Spirdo Text Example

This demo is for SDL3 users who want a live rendering path:
- MTSDF glyph generation and atlas packing via `masdiff`,
- WGSL -> SPIR-V compilation via [`spirdo`](https://github.com/mewhhaha/spirdo),
- direct SDL3 GPU rendering through manual FFI bindings (no external SDL wrapper).

## Run

```bash
cd examples/sdl3-spirdo-text
MASDIFF_SDL_GEN_BACKEND=gpu MASDIFF_SDL_GEN_STRICT=1 cabal run sdl3-spirdo-text
```

The first run may take longer because Cabal fetches the pinned `spirdo` dependency from GitHub.

## What it does

1. Generates line atlases of **raw MTSDF glyphs** from Inter fonts (regular + variable axes) through a consumer-style raster callback.
2. Compiles vertex/fragment WGSL shaders with Spirdo.
3. Creates an SDL3 GPU pipeline from Spirdo SPIR-V.
4. Renders text by drawing glyph quads sampled from the MTSDF atlas in the fragment shader.

With `MASDIFF_SDL_GEN_BACKEND=gpu`, atlas generation runs through the demo's SDL GPU raster callback.
With `MASDIFF_SDL_GEN_STRICT=1`, the demo fails fast instead of silently falling back to CPU generation.

## Pipeline probe mode (fast crash triage)

Use probe mode to test generation shader pipeline creation without building atlases or entering the render loop:

```bash
cd examples/sdl3-spirdo-text
MASDIFF_SDL_PIPELINE_PROBE=1 MASDIFF_SDL_GEN_SHADER=flat cabal run sdl3-spirdo-text
```

Useful shader modes:
- `MASDIFF_SDL_GEN_SHADER=flat` (default): flattened uniform layout, currently stable.
- `MASDIFF_SDL_GEN_SHADER=struct`: original array-of-struct uniform layout; can crash on some Vulkan drivers.
- `MASDIFF_SDL_GEN_SHADER=sanity`: minimal fragment shader for smoke testing pipeline creation.

Generation budget knobs:
- `MASDIFF_SDL_GEN_MAX_SEGS` (default `120`): max line segments sent to the GPU generation pass.
- `MASDIFF_SDL_GEN_MAX_PUSH_BYTES` (default `4096`): max bytes allowed for fragment-uniform push payload.

If GPU generation falls back often due segment budgets, increase these values gradually and re-check stability.

Recommended probe matrix:

```bash
MASDIFF_SDL_PIPELINE_PROBE=1 MASDIFF_SDL_GEN_SHADER=flat cabal run sdl3-spirdo-text
MASDIFF_SDL_PIPELINE_PROBE=1 MASDIFF_SDL_GEN_SHADER=struct cabal run sdl3-spirdo-text
MASDIFF_SDL_PIPELINE_PROBE=1 MASDIFF_SDL_GEN_SHADER=sanity cabal run sdl3-spirdo-text
```

## Troubleshooting

- `SDL_CreateGPUDevice failed: No supported SDL_GPU backend found!`
  - Your environment has no usable SDL GPU backend (common in headless/dummy sessions).
  - Run on a machine/session with Vulkan/Metal/D3D support.
- `SDL_Init failed: No available video device`
  - No display/video driver is available in the current shell session.
  - Run from a normal desktop session or provide a valid SDL video driver.

Close the window to exit.
