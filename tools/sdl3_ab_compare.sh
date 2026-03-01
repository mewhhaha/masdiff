#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EX="$ROOT/examples/sdl3-spirdo-text"
OUT="$EX/out/ab"
mkdir -p "$OUT"

if [ "${MASDIFF_SDL_REQUIRE_ORACLE:-1}" = "1" ]; then
  echo "[ab] running msdfgen oracle gate"
  MASDIFF_ORACLE_ENFORCE="${MASDIFF_SDL_ORACLE_ENFORCE:-0}" "$ROOT/tools/run_msdfgen_oracle_gate.sh"
fi

if [ ! -f "$EX/.cabal/packages/hackage.haskell.org/01-index.cache" ] && [ ! -f "$EX/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then
  (cd "$EX" && CABAL_DIR="$EX/.cabal" CABAL_LOGDIR="$EX/.cabal-logs" cabal update)
fi

run_case() {
  local name="$1"
  local backend="$2"
  local batch="$3"
  local heal="$4"
  local scene="$5"
  local em="${6:-}"

  local cap="$OUT/$name.png"
  local meta="$OUT/$name.tsv"
  local log="$OUT/$name.log"
  local report="$OUT/$name.report.json"

  (
    cd "$EX"
    CABAL_DIR="$EX/.cabal" \
      CABAL_LOGDIR="$EX/.cabal-logs" \
      SDL_AUDIODRIVER=dummy \
      SDL_VIDEODRIVER="${SDL_VIDEODRIVER:-x11}" \
      MASDIFF_SDL_GEN_BACKEND="$backend" \
      MASDIFF_SDL_GPU_BATCH="$batch" \
      MASDIFF_SDL_GEN_STRICT=1 \
      MASDIFF_SDL_GEN_SHADER=flat \
      MASDIFF_SDL_PRESENT_HEAL="$heal" \
      MASDIFF_SDL_PRESENT_HEAL_MODE="${MASDIFF_SDL_PRESENT_HEAL_MODE:-1}" \
      MASDIFF_SDL_PXRANGE="${MASDIFF_SDL_PXRANGE:-7}" \
      MASDIFF_SDL_SCENE="$scene" \
      MASDIFF_SDL_SINGLE_EM="${em:-640}" \
      MASDIFF_SDL_CAPTURE="$cap" \
      MASDIFF_SDL_META="$meta" \
      cabal run sdl3-spirdo-text >"$log" 2>&1
  )

  if [ "$scene" = "default" ]; then
    python3 "$ROOT/tools/sdl3_artifact_harness.py" --scene default --image "$cap" --meta "$meta" >"$report" || true
  elif [ "$scene" = "single-var-bold-a" ]; then
    python3 "$ROOT/tools/sdl3_artifact_harness.py" --scene single-var-bold-a --image "$cap" --meta "$meta" >"$report" || true
  fi

  echo "$name done"
}

run_case "cpu-heal1-default" cpu 0 1 default
run_case "gpu-b0-heal1-default" gpu 0 1 default
run_case "gpu-b1-heal1-default" gpu 1 1 default
run_case "gpu-b0-heal0-default" gpu 0 0 default
run_case "gpu-b1-heal0-default" gpu 1 0 default

run_case "gpu-b0-heal1-single-var-bold-a" gpu 0 1 single-var-bold-a 640
run_case "gpu-b1-heal1-single-var-bold-a" gpu 1 1 single-var-bold-a 640
run_case "gpu-b0-heal0-single-var-bold-a" gpu 0 0 single-var-bold-a 640
run_case "gpu-b1-heal0-single-var-bold-a" gpu 1 0 single-var-bold-a 640

magick montage \
  "$OUT/cpu-heal1-default.png" \
  "$OUT/gpu-b0-heal1-default.png" \
  "$OUT/gpu-b1-heal1-default.png" \
  "$OUT/gpu-b0-heal0-default.png" \
  "$OUT/gpu-b1-heal0-default.png" \
  -tile 3x2 -geometry +8+8 "$OUT/default-matrix.png"

magick montage \
  "$OUT/gpu-b0-heal1-single-var-bold-a.png" \
  "$OUT/gpu-b1-heal1-single-var-bold-a.png" \
  "$OUT/gpu-b0-heal0-single-var-bold-a.png" \
  "$OUT/gpu-b1-heal0-single-var-bold-a.png" \
  -tile 2x2 -geometry +8+8 "$OUT/single-var-bold-a-matrix.png"

echo "Wrote A/B outputs to $OUT"
