#!/usr/bin/env bash
set -euo pipefail

MODE=${1:-compare}
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

SHOT="$ROOT/examples/sdl_gpu_wesl/out/screenshot.tga"
REF="$ROOT/tests/snapshots/render/demo_live_text.tga"
OUT_DIR="$ROOT/out/snapshots/render"
OUT_TGA="$OUT_DIR/demo_live_text.tga"
DIFF_PNG="$OUT_DIR/demo_live_text.diff.png"
SEAM_PNG="$OUT_DIR/demo_live_text.seam.png"

run_seam_guard() {
  if python3 "$ROOT/tools/sdl_seam_guard.py" --image "$SHOT" --debug-out "$SEAM_PNG"; then
    return 0
  fi
  if [ "${MSDF_ALLOW_SEAM:-}" != "" ]; then
    echo "WARNING: seam guard failed but MSDF_ALLOW_SEAM is set; continuing" >&2
    return 0
  fi
  return 1
}

run_demo() {
  if [ "${MSDF_SDL_HEADLESS:-}" != "" ]; then
    SDL_MSDF_HEADLESS=1 \
      just -f "$ROOT/Justfile" demo-live-text-shot
  else
    just -f "$ROOT/Justfile" demo-live-text-shot
  fi
}

run_demo_with_capture() {
  local log_file="$1"
  if run_demo >"$log_file" 2>&1; then
    return 0
  fi
  if grep -q "No supported SDL_GPU backend found" "$log_file" || \
     grep -q "SDL_CreateGPUDevice" "$log_file"; then
    echo "SKIP: SDL_GPU backend unavailable" >&2
    return 2
  fi
  return 1
}

mkdir -p "$OUT_DIR"

case "$MODE" in
  update)
    tmp_log="$(mktemp -t masdiff_sdl_snapshot.XXXXXX)"
    if run_demo_with_capture "$tmp_log"; then
      :
    else
      code=$?
      cat "$tmp_log" >&2
      rm -f "$tmp_log"
      exit "$code"
    fi
    rm -f "$tmp_log"
    if [ ! -f "$SHOT" ]; then
      echo "missing screenshot: $SHOT" >&2
      exit 1
    fi
    run_seam_guard
    mkdir -p "$(dirname "$REF")"
    cp "$SHOT" "$REF"
    echo "updated snapshot: $REF"
    ;;
  compare)
    if [ ! -f "$REF" ]; then
      echo "SKIP: missing snapshot: $REF (run update on a machine with SDL_GPU support)" >&2
      exit 2
    fi
    tmp_log="$(mktemp -t masdiff_sdl_snapshot.XXXXXX)"
    if run_demo_with_capture "$tmp_log"; then
      :
    else
      code=$?
      cat "$tmp_log" >&2
      rm -f "$tmp_log"
      exit "$code"
    fi
    rm -f "$tmp_log"
    if [ ! -f "$SHOT" ]; then
      echo "missing screenshot: $SHOT" >&2
      exit 1
    fi
    run_seam_guard
    cp "$SHOT" "$OUT_TGA"
    if python3 "$ROOT/tools/sdl_snapshot_diff.py" --ref "$REF" --test "$SHOT"; then
      :
    else
      python3 "$ROOT/tools/sdl_snapshot_diff.py" --ref "$REF" --test "$SHOT" --diff-out "$DIFF_PNG" || true
      exit 1
    fi
    ;;
  *)
    echo "usage: $0 [compare|update]" >&2
    exit 1
    ;;
esac
