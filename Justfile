set shell := ["bash", "-lc"]

root := justfile_directory()

# Build MSDF atlas + SPIR-V shaders (writes to examples/sdl_gpu_wesl/out).
demo-gen:
    cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen

# Build SDL_gpu demo binary.
demo-build:
    cc "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf.c" -o "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" $(pkg-config --cflags --libs sdl3 2>/dev/null || sdl3-config --cflags --libs)

# Run the full demo from repo root.
demo: demo-gen demo-build
    SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS=500 "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf"
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Force Vulkan backend (uses SDL_GPU_DRIVER).
demo-vulkan: demo-gen demo-build
    SDL_GPU_DRIVER=vulkan SDL_MSDF_SCREENSHOT=1 "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf"
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Run without auto-screenshot/exit.
demo-live: demo-gen demo-build
    "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf"

# Headless offscreen screenshot (requires a GPU backend that works without a window).
demo-headless: demo-gen demo-build
    SDL_MSDF_HEADLESS=1 SDL_MSDF_SCREENSHOT=1 "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf"
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Optional: generate with a specific font path.
demo-gen-font font:
    cabal run --project-dir="{{root}}/examples/sdl_gpu_wesl" msdf-sdl-gen -- --font {{font}}
