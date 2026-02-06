set shell := ["bash", "-lc"]

root := justfile_directory()
# Quality-first default for demo/debug captures.
default_gen_args := "--split-intersections"

# Build MSDF atlas + SPIR-V shaders (writes to examples/sdl_gpu_wesl/out).
demo-gen:
    (cd "{{root}}/examples/sdl_gpu_wesl" && cabal run msdf-sdl-gen -- ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}})

# Build SDL_gpu demo binary.
demo-build:
    cc "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf.c" -o "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" $(pkg-config --cflags --libs sdl3 2>/dev/null || sdl3-config --cflags --libs)

# Run the full demo from repo root (in-memory pipeline).
demo: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS=500 "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Force Vulkan backend (uses SDL_GPU_DRIVER).
demo-vulkan: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_GPU_DRIVER=vulkan SDL_MSDF_SCREENSHOT=1 "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Run without auto-screenshot/exit.
demo-live: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_DEBUG_GRID=1 "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin

# Run live with the final render only (no debug grid).
demo-live-text: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin

# Run text-only view, capture screenshot, then auto-exit.
demo-live-text-shot: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Run text-only view with explicit split-intersections enabled.
demo-live-text-shot-split: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob --split-intersections) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Run text-only view with split-intersections + preprocess enabled.
demo-live-text-shot-split-preprocess: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 MSDF_PREPROCESS=1 \
      cabal run msdf-sdl-gen -- --emit-blob --split-intersections) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Capture a reproducible A/B bundle for split/preprocess/filter debugging.
# Outputs to out/debug_shots:
#   repro_baseline.png, repro_nearest.png, repro_nosplit.png,
#   repro_split.png, repro_preprocess_on.png, repro_split_preprocess_on.png
demo-repro-set: demo-build
    mkdir -p "{{root}}/out/debug_shots"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/repro_baseline.png"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_NEAREST=1 SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/repro_nearest.png"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob --no-split-intersections) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/repro_nosplit.png"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob --split-intersections) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/repro_split.png"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 MSDF_PREPROCESS=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/repro_preprocess_on.png"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 MSDF_PREPROCESS=1 \
      cabal run msdf-sdl-gen -- --emit-blob --split-intersections) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/repro_split_preprocess_on.png"

# Debug-friendly live render with a smaller default text size.
demo-live-text-small: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin

# Small text version: capture screenshot, then auto-exit.
demo-live-text-small-shot: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Headless offscreen screenshot (requires a GPU backend that works without a window).
demo-headless: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_HEADLESS=1 SDL_MSDF_SCREENSHOT=1 "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi

# Capture a single debug-view screenshot to out/debug_shots/<mode>.png.
# Valid modes: alpha, r, g, b, median, split, fill.
demo-shot mode: demo-build
    mkdir -p "{{root}}/out/debug_shots"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" SDL_MSDF_DEBUG_VIEW="{{mode}}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/{{mode}}.png"

# Capture normal (non-debug-view) screenshot to out/debug_shots/normal.png.
demo-shot-normal: demo-build
    mkdir -p "{{root}}/out/debug_shots"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/normal.png"

# Capture a debug bundle: normal + alpha + median + split + fill + r + g + b.
demo-shot-set: demo-build
    mkdir -p "{{root}}/out/debug_shots"
    modes=(normal alpha median split fill r g b); \
    for mode in "${modes[@]}"; do \
      if [ "$mode" = "normal" ]; then \
        (cd "{{root}}/examples/sdl_gpu_wesl" && \
          SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" \
          cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
          | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin; \
      else \
        (cd "{{root}}/examples/sdl_gpu_wesl" && \
          SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" \
          cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
          | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" SDL_MSDF_DEBUG_VIEW="$mode" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin; \
      fi; \
      if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi; \
      cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/$mode.png"; \
    done

# Optional: generate with a specific font path.
demo-gen-font font:
    (cd "{{root}}/examples/sdl_gpu_wesl" && cabal run msdf-sdl-gen -- --font {{font}} ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}})
