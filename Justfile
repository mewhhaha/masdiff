set shell := ["bash", "-lc"]

root := justfile_directory()

# Build MSDF atlas + SPIR-V shaders (writes to examples/sdl_gpu_wesl/out).
demo-gen:
    CABAL_DIR="{{root}}/.cabal" cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen

# Build SDL_gpu demo binary.
demo-build:
    cc "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf.c" -o "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" $(pkg-config --cflags --libs sdl3 2>/dev/null || sdl3-config --cflags --libs)

# Run the full demo from repo root (in-memory pipeline).
demo: demo-build
    SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS=500 \
      CABAL_DIR="{{root}}/.cabal" cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen -- --emit-blob \
      | "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Force Vulkan backend (uses SDL_GPU_DRIVER).
demo-vulkan: demo-build
    SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_GPU_DRIVER=vulkan SDL_MSDF_SCREENSHOT=1 \
      CABAL_DIR="{{root}}/.cabal" cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen -- --emit-blob \
      | "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Run without auto-screenshot/exit.
demo-live: demo-build
    SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_DEBUG_GRID=1 \
      CABAL_DIR="{{root}}/.cabal" cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen -- --emit-blob \
      | "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin

# Run live with the final render only (no debug grid).
demo-live-text: demo-build
    SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      CABAL_DIR="{{root}}/.cabal" cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen -- --emit-blob \
      | "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin

# Headless offscreen screenshot (requires a GPU backend that works without a window).
demo-headless: demo-build
    SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_HEADLESS=1 SDL_MSDF_SCREENSHOT=1 \
      CABAL_DIR="{{root}}/.cabal" cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen -- --emit-blob \
      | "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Optional: generate with a specific font path.
demo-gen-font font:
    CABAL_DIR="{{root}}/.cabal" cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen -- --font {{font}}
