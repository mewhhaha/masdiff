#!/usr/bin/env bash
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXAMPLE_DIR="$ROOT/examples/sdl3-spirdo-text"
OUT_DIR="$EXAMPLE_DIR/out/harness"
PY="$ROOT/tools/sdl3_artifact_harness.py"

mkdir -p "$OUT_DIR"

run_case() {
  local scene="$1"
  local tag="$2"
  local nofit="$3"
  local em="$4"
  local image="$OUT_DIR/${tag}.png"
  local meta="$OUT_DIR/${tag}.tsv"
  local report="$OUT_DIR/${tag}.json"
  local status=0

  echo "[harness] running ${tag}"
  (
    cd "$EXAMPLE_DIR"
    local -a envs
    envs=(
      "CABAL_DIR=$PWD/.cabal"
      "CABAL_LOGDIR=$PWD/.cabal-logs"
      "SDL_AUDIODRIVER=${SDL_AUDIODRIVER:-dummy}"
      "MASDIFF_SDL_GEN_BACKEND=${MASDIFF_SDL_GEN_BACKEND:-gpu}"
      "MASDIFF_SDL_GPU_BATCH=0"
      "MASDIFF_SDL_GEN_STRICT=${MASDIFF_SDL_GEN_STRICT:-1}"
      "MASDIFF_SDL_SCENE=$scene"
      "MASDIFF_SDL_CAPTURE=$image"
      "MASDIFF_SDL_META=$meta"
    )
    if [ "$nofit" = "1" ]; then
      envs+=("MASDIFF_SDL_NO_FIT=1")
    fi
    if [ -n "$em" ]; then
      envs+=("MASDIFF_SDL_SINGLE_EM=$em")
    fi
    env "${envs[@]}" cabal run sdl3-spirdo-text >/dev/null
  )
  status=$?
  if [ $status -ne 0 ]; then
    echo "[harness] ERROR: render failed for ${tag} (status=$status)"
    return $status
  fi

  python3 "$PY" --scene "$scene" --image "$image" --meta "$meta" --json-out "$report"
  if [ $? -ne 0 ]; then
    echo "[harness] ERROR: checks failed for ${tag}"
    return 1
  fi
  echo "[harness] ${tag} PASSED"
  return 0
}

overall_status=0

run_case "default" "default-fit" "0" "" || overall_status=1

single_ems_raw="${MASDIFF_SDL_HARNESS_EMS:-24 32}"
read -r -a single_ems <<<"$single_ems_raw"
single_scenes=(
  single-a
  single-m
  single-p
  single-r
  single-y
  single-var-light-a
  single-var-light-m
  single-var-light-p
  single-var-light-r
  single-var-light-y
  single-var-bold-a
  single-var-bold-m
  single-var-bold-p
  single-var-bold-r
  single-var-bold-y
)
for scene in "${single_scenes[@]}"; do
  for em in "${single_ems[@]}"; do
    run_case "$scene" "${scene}-em${em}" "1" "$em" || overall_status=1
  done
done

echo "[harness] reports in $OUT_DIR"
if [ $overall_status -ne 0 ]; then
  exit 1
fi
