# SDL_gpu + masdiff + spirdo

This example generates an MSDF atlas + SPIR-V shaders in Haskell (masdiff + spirdo), then renders them with SDL 3.4 GPU.

## 1) Generate assets (atlas + SPIR-V)

From the repo root:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen
```

To use a different font:

```sh
cabal run --project-dir=examples/sdl_gpu_wesl msdf-sdl-gen -- --font /path/to/font.ttf
```

If you have `just` installed:

```sh
just demo-gen
```

This writes (under `examples/sdl_gpu_wesl/out`):

- `out/atlas.rgba`
- `out/vertices.bin`
- `out/meta.txt`
- `out/msdf.vert.spv`
- `out/msdf.frag.spv`

`msdf-sdl-gen` compiles WESL via **spirdo** and builds the MSDF atlas via **masdiff**.

## 2) Build the SDL renderer

```sh
cc sdl_gpu_msdf.c -o sdl_gpu_msdf $(pkg-config --cflags --libs sdl3)
```

If your system uses `sdl3-config`, you can swap the pkg-config call for:

```sh
cc sdl_gpu_msdf.c -o sdl_gpu_msdf $(sdl3-config --cflags --libs)
```

## 3) Run

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
- The fragment shader expects a **linear** texture and uses `pxRange` from `meta.txt`.
- `just demo` writes `examples/sdl_gpu_wesl/out/screenshot.tga` and exits.
- Customize the delay with `SDL_MSDF_SCREENSHOT_DELAY_MS=1000 just demo`.
- If you see `No supported SDL_GPU backend found`, ensure SDL_gpu has a backend
  available (Vulkan/Metal/D3D12) and that your system drivers are installed.
- To force a backend: `SDL_GPU_BACKEND=vulkan just demo` (the demo defaults to
  Vulkan when headless).
- If you want different text, edit `sampleText` in `Main.hs` and re-run the generator.
