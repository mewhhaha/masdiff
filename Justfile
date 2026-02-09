set shell := ["bash", "-lc"]

root := justfile_directory()
# Stability-first default for demo/debug captures.
# Intersection splitting is still available via explicit split targets.
default_gen_args := "--no-split-intersections"

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
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 MSDF_CORRECTION="${MSDF_CORRECTION:-0}" MSDF_CORRECTION_BADMAP="${MSDF_CORRECTION_BADMAP:-0}" \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin

# Run text-only view, capture screenshot, then auto-exit.
demo-live-text-shot: demo-build
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 MSDF_CORRECTION="${MSDF_CORRECTION:-0}" MSDF_CORRECTION_BADMAP="${MSDF_CORRECTION_BADMAP:-0}" \
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
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" MSDF_CORRECTION="${MSDF_CORRECTION:-0}" MSDF_CORRECTION_BADMAP="${MSDF_CORRECTION_BADMAP:-0}" \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" SDL_MSDF_DEBUG_VIEW="{{mode}}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/{{mode}}.png"

# Capture normal (non-debug-view) screenshot to out/debug_shots/normal.png.
demo-shot-normal: demo-build
    mkdir -p "{{root}}/out/debug_shots"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" MSDF_CORRECTION="${MSDF_CORRECTION:-0}" MSDF_CORRECTION_BADMAP="${MSDF_CORRECTION_BADMAP:-0}" \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/normal.png"

# Capture normal screenshot with bilinear bad-map correction disabled.
demo-shot-normal-nobadmap: demo-build
    mkdir -p "{{root}}/out/debug_shots"
    (cd "{{root}}/examples/sdl_gpu_wesl" && \
      SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" MSDF_CORRECTION="${MSDF_CORRECTION:-0}" MSDF_CORRECTION_BADMAP=0 \
      cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
      | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin
    if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/normal_nobadmap.png"

# Capture a debug bundle: normal + alpha + median + split + fill + r + g + b.
demo-shot-set: demo-build
    mkdir -p "{{root}}/out/debug_shots"
    modes=(normal alpha median split fill r g b); \
    for mode in "${modes[@]}"; do \
      if [ "$mode" = "normal" ]; then \
        (cd "{{root}}/examples/sdl_gpu_wesl" && \
          SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" MSDF_CORRECTION="${MSDF_CORRECTION:-0}" MSDF_CORRECTION_BADMAP="${MSDF_CORRECTION_BADMAP:-0}" \
          cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
          | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin; \
      else \
        (cd "{{root}}/examples/sdl_gpu_wesl" && \
          SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_IN_MEMORY=1 SDL_MSDF_EMIT_BLOB=1 SDL_MSDF_PIXEL_SIZE="${SDL_MSDF_PIXEL_SIZE:-128}" MSDF_CORRECTION="${MSDF_CORRECTION:-0}" MSDF_CORRECTION_BADMAP="${MSDF_CORRECTION_BADMAP:-0}" \
          cabal run msdf-sdl-gen -- --emit-blob ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}}) \
          | SDL_MSDF_OUT_DIR="{{root}}/examples/sdl_gpu_wesl/out" SDL_MSDF_SCREENSHOT=1 SDL_MSDF_SCREENSHOT_DELAY_MS="${SDL_MSDF_SCREENSHOT_DELAY_MS:-100}" SDL_MSDF_DEBUG_VIEW="$mode" "{{root}}/examples/sdl_gpu_wesl/sdl_gpu_msdf" --stdin; \
      fi; \
      if [ -f "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga" ]; then python "{{root}}/examples/sdl_gpu_wesl/tools/tga_to_png.py" "{{root}}/examples/sdl_gpu_wesl/out/screenshot.tga"; fi; \
      cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/$mode.png"; \
    done

# Optional: generate with a specific font path.
demo-gen-font font:
    (cd "{{root}}/examples/sdl_gpu_wesl" && cabal run msdf-sdl-gen -- --font {{font}} ${SDL_MSDF_GEN_ARGS:-{{default_gen_args}}})

# Debug helpers --------------------------------------------------------------

# Run all debug helpers in one go.
debug: debug-msdfgen-h debug-msdfgen-h-diff debug-msdfgen-h-trace debug-sdl-shot-set debug-sdl-alpha debug-sdl-median

# Generate masdiff msdf-dump output for a single glyph (default: 'h').
debug-msdfgen-h:
    cabal run msdf-dump -- \
      --font "{{root}}/assets/Inter/Inter-VariableFont_opsz,wght.ttf" \
      --text "h" \
      --pixel-size 256 \
      --range 16 \
      --padding 16 \
      --format mtsdf \
      --no-overlap \
      --out-dir "{{root}}/out/msdfgen_compare/masdiff"

# Generate msdfgen reference + diff images for glyph 'h'.
debug-msdfgen-h-diff:
    python "{{root}}/tools/msdfgen_compare.py" \
      --format mtsdf \
      --codepoints 0068 \
      --our-dir "{{root}}/out/msdfgen_compare/masdiff" \
      --ref-dir "{{root}}/out/msdfgen_compare/msdfgen" \
      --generate --match-masdiff \
      --font "{{root}}/assets/Inter/Inter-VariableFont_opsz,wght.ttf" \
      --range 16 \
      --diff-dir "{{root}}/out/debug_shots/msdfgen_h"

# Trace the msdf-dump pipeline for glyph 'h'.
debug-msdfgen-h-trace:
    MSDF_TRACE=1 cabal run msdf-dump -- \
      --font "{{root}}/assets/Inter/Inter-VariableFont_opsz,wght.ttf" \
      --text "h" \
      --pixel-size 256 \
      --range 16 \
      --padding 16 \
      --format mtsdf \
      --no-overlap \
      --out-dir "{{root}}/out/debug_shots/dump_h" \
      > "{{root}}/out/debug_shots/dump_h/trace.log" 2>&1

# SDL debug: write normal + alpha + median + split + fill + r/g/b to out/debug_shots.
debug-sdl-shot-set:
    SDL_MSDF_TEXT_DEBUG=1 SDL_MSDF_SAMPLE_TEXT="the" SDL_MSDF_PIXEL_SIZE=256 SDL_MSDF_RANGE=16 just demo-shot-set

# SDL debug: capture alpha render and median render side by side.
debug-sdl-alpha:
    SDL_MSDF_RENDER_ALPHA=1 SDL_MSDF_SAMPLE_TEXT="the" just demo-shot-normal
    cp "{{root}}/out/debug_shots/normal.png" "{{root}}/out/debug_shots/normal_alpha.png"

debug-sdl-median:
    SDL_MSDF_RENDER_ALPHA=0 SDL_MSDF_SAMPLE_TEXT="the" just demo-shot-normal
    cp "{{root}}/out/debug_shots/normal.png" "{{root}}/out/debug_shots/normal_median.png"

debug-sdl-nobadmap:
    SDL_MSDF_SAMPLE_TEXT="the" MSDF_CORRECTION_BADMAP=0 just demo-shot-normal-nobadmap

debug-sdl-correction-on:
    SDL_MSDF_SAMPLE_TEXT="the" MSDF_CORRECTION=1 MSDF_CORRECTION_BADMAP=0 just demo-shot-normal
    cp "{{root}}/out/debug_shots/normal.png" "{{root}}/out/debug_shots/normal_corr_on.png"

debug-sdl-correction-off:
    SDL_MSDF_SAMPLE_TEXT="the" MSDF_CORRECTION=0 MSDF_CORRECTION_BADMAP=0 just demo-shot-normal
    cp "{{root}}/out/debug_shots/normal.png" "{{root}}/out/debug_shots/normal_corr_off.png"

debug-sdl-alpha-vs-median:
    SDL_MSDF_SAMPLE_TEXT="the" SDL_MSDF_RENDER_ALPHA=0 just demo-live-text-shot
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/live_default_median.png"
    SDL_MSDF_SAMPLE_TEXT="the" SDL_MSDF_RENDER_ALPHA=1 just demo-live-text-shot
    cp "{{root}}/examples/sdl_gpu_wesl/out/screenshot.png" "{{root}}/out/debug_shots/live_alpha_forced.png"
