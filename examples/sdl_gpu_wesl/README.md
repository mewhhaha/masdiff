# SDL_gpu + masdiff + spirdo

This example generates an MSDF (or MTSDF) atlas + SPIR-V shaders in Haskell (masdiff + spirdo), then renders them with SDL 3.4 GPU.

## 1) Generate assets (atlas + SPIR-V)

From the repo root:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen
```

To use a different font:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen -- --font /path/to/font.ttf
```

If you see specks, try a more conservative generation setup:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen -- --range 16 --padding 24 --correction 0.08 --edge-threshold 1.0
```

You can also toggle sign mode and overlap handling to match msdfgen behavior:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen -- --sign-mode scanline --fill-rule nonzero --overlap
```

And test different edge coloring strategies:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen -- --coloring distance
```

To check whether artifacts are atlas-related, disable packing:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen -- --no-pack
```

If you have `just` installed:

```sh
just demo-gen
```

This writes (under `examples/sdl_gpu_wesl/out`):

- `out/atlas_msdf.rgba`, `out/vertices_msdf.bin`, `out/meta_msdf.txt`
- `out/atlas_mtsdf.rgba`, `out/vertices_mtsdf.bin`, `out/meta_mtsdf.txt`
- (compat) `out/atlas.rgba`, `out/vertices.bin`, `out/meta.txt` (MSDF only)
- `out/msdf.vert.spv`
- `out/msdf.frag.spv`

`msdf-sdl-gen` compiles WESL via **spirdo** and builds the MSDF/MTSDF atlas via **masdiff**.

## 2) Build the SDL renderer

```sh
cc sdl_gpu_msdf.c -o sdl_gpu_msdf $(pkg-config --cflags --libs sdl3)
```

If your system uses `sdl3-config`, you can swap the pkg-config call for:

```sh
cc sdl_gpu_msdf.c -o sdl_gpu_msdf $(sdl3-config --cflags --libs)
```

## 3) Run (side-by-side MSDF + MTSDF)

```sh
./sdl_gpu_msdf
```

Or from repo root (screenshot + exit, brief delay to see the window):

```sh
just demo
```

To keep the window open:

```sh
just demo-live
```

Force Vulkan backend:

```sh
just demo-vulkan
```

Headless offscreen render (no window):

```sh
just demo-headless
```

## Notes

- The generator bakes positions into clip space for a **1280x720** window.
  If you change the window size, update `screenW/screenH` in `Main.hs`.
- The fragment shader expects a **linear** texture, uses `pxRange` from `meta.txt`,
  and derives `screenPxRange` from UV derivatives and atlas size.
- `just demo` writes `examples/sdl_gpu_wesl/out/screenshot.tga` and exits.
- Customize the delay with `SDL_MSDF_SCREENSHOT_DELAY_MS=1000 just demo`.
- If you see `No supported SDL_GPU backend found`, ensure SDL_gpu has a backend
  available (Vulkan/Metal/D3D12) and that your system drivers are installed.
- To force a backend: `SDL_GPU_BACKEND=vulkan just demo` (the demo defaults to
  Vulkan when headless).
- If you want different text, edit `sampleText` in `Main.hs` and re-run the generator.
- Debug views: set `SDL_MSDF_DEBUG_VIEW=alpha|r|g|b|median|split|fill` to inspect channels
  (use `split` for a 2x2 grid of R/G/B/A, `fill` for a binary 0.5 threshold).
- `SDL_MSDF_DEBUG_GRID=1` renders a 4x2 grid of modes (alpha/fill/split/median/r/g/b/normal).
