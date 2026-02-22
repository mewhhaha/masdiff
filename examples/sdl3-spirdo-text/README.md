# SDL3 Spirdo Text Example

This demo is for SDL3 users who want a live rendering path:
- MTSDF glyph generation and atlas packing via `masdiff`,
- WGSL -> SPIR-V compilation via [`spirdo`](https://github.com/mewhhaha/spirdo),
- direct SDL3 GPU rendering through manual FFI bindings (no external SDL wrapper).

## Run

```bash
cd examples/sdl3-spirdo-text
cabal run sdl3-spirdo-text
```

The first run may take longer because Cabal fetches the pinned `spirdo` dependency from GitHub.

## What it does

1. Generates line atlases of **raw MTSDF glyphs** from Inter fonts (regular + variable axes).
2. Compiles vertex/fragment WGSL shaders with Spirdo.
3. Creates an SDL3 GPU pipeline from Spirdo SPIR-V.
4. Renders text by drawing glyph quads sampled from the MTSDF atlas in the fragment shader.

The shader currently applies median-channel MSDF reconstruction directly in GPU space.
This is the path you want for real MTSDF behavior, rather than CPU pre-shaded glyph images.

Close the window to exit.
