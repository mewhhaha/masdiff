#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EX="$ROOT/examples/sdl3-spirdo-text"
OUT="$EX/out/overlap-order-gate"
HARNESS="$ROOT/tools/sdl3_artifact_harness.py"

mkdir -p "$OUT"

SCENE="${MASDIFF_SDL_OVERLAP_SCENE:-single-var-bold-a}"
EM="${MASDIFF_SDL_OVERLAP_EM:-32}"
PXRANGE="${MASDIFF_SDL_OVERLAP_PXRANGE:-7}"
PRESENT_HEAL="${MASDIFF_SDL_OVERLAP_PRESENT_HEAL:-1}"
GPU_BATCH="${MASDIFF_SDL_OVERLAP_GPU_BATCH:-0}"
OVLP="${MASDIFF_SDL_OVERLAP_OVLP:-1}"

CPU_JSON="$OUT/cpu.report.json"
GPU_JSON="$OUT/gpu.report.json"

render_case() {
  local backend="$1"
  local image="$2"
  local meta="$3"
  local report="$4"
  (
    cd "$EX"
    env \
      CABAL_DIR="$PWD/.cabal" \
      CABAL_LOGDIR="$PWD/.cabal-logs" \
      SDL_AUDIODRIVER="${SDL_AUDIODRIVER:-dummy}" \
      SDL_VIDEODRIVER="${SDL_VIDEODRIVER:-x11}" \
      MASDIFF_SDL_GEN_BACKEND="$backend" \
      MASDIFF_SDL_GPU_BATCH="$GPU_BATCH" \
      MASDIFF_SDL_GEN_STRICT=1 \
      MASDIFF_SDL_GEN_SHADER=flat \
      MASDIFF_SDL_PRESENT_HEAL="$PRESENT_HEAL" \
      MASDIFF_SDL_PRESENT_HEAL_MODE="${MASDIFF_SDL_PRESENT_HEAL_MODE:-1}" \
      MASDIFF_SDL_PXRANGE="$PXRANGE" \
      MASDIFF_SDL_OVLP="$OVLP" \
      MASDIFF_SDL_SCENE="$SCENE" \
      MASDIFF_SDL_SINGLE_EM="$EM" \
      MASDIFF_SDL_CAPTURE="$image" \
      MASDIFF_SDL_META="$meta" \
      cabal run sdl3-spirdo-text >/dev/null
  )
  python3 "$HARNESS" --scene "$SCENE" --image "$image" --meta "$meta" --json-out "$report" >/dev/null || true
}

cpu_img="$OUT/cpu.png"
cpu_meta="$OUT/cpu.tsv"
gpu_img="$OUT/gpu.png"
gpu_meta="$OUT/gpu.tsv"

echo "[overlap-order] rendering cpu baseline"
render_case cpu "$cpu_img" "$cpu_meta" "$CPU_JSON"
echo "[overlap-order] rendering gpu candidate"
render_case gpu "$gpu_img" "$gpu_meta" "$GPU_JSON"

extract_metric() {
  local report="$1"
  local jq_expr="$2"
  jq -r "$jq_expr" "$report"
}

cpu_issue="$(extract_metric "$CPU_JSON" '.aggregate.issue_score // 0')"
gpu_issue="$(extract_metric "$GPU_JSON" '.aggregate.issue_score // 0')"
cpu_pass="$(extract_metric "$CPU_JSON" '.pass // false')"
gpu_pass="$(extract_metric "$GPU_JSON" '.pass // false')"

cpu_core="$(extract_metric "$CPU_JSON" '[.glyphs[] | select(.ch=="A") | .counter_integrity.core_bad_pixels // 0] | if length==0 then 0 else .[0] end')"
gpu_core="$(extract_metric "$GPU_JSON" '[.glyphs[] | select(.ch=="A") | .counter_integrity.core_bad_pixels // 0] | if length==0 then 0 else .[0] end')"
cpu_apex="$(extract_metric "$CPU_JSON" '[.glyphs[] | select(.ch=="A") | .counter_integrity.apex_core_bad_pixels // 0] | if length==0 then 0 else .[0] end')"
gpu_apex="$(extract_metric "$GPU_JSON" '[.glyphs[] | select(.ch=="A") | .counter_integrity.apex_core_bad_pixels // 0] | if length==0 then 0 else .[0] end')"

target_ch=""
case "$SCENE" in
  single-var-bold-?|single-var-light-?|single-?)
    target_ch="${SCENE##*-}"
    target_ch="${target_ch^^}"
    ;;
esac
if [ -z "$target_ch" ]; then
  target_ch="A"
fi

cpu_seam="$(jq -r --arg ch "$target_ch" '[.glyphs[] | select(.ch==$ch) | ((.seam_lines.horiz_pixels // 0) + (.seam_lines.vert_pixels // 0) + (.seam_components.max_component_area // 0))] | if length==0 then 0 else .[0] end' "$CPU_JSON")"
gpu_seam="$(jq -r --arg ch "$target_ch" '[.glyphs[] | select(.ch==$ch) | ((.seam_lines.horiz_pixels // 0) + (.seam_lines.vert_pixels // 0) + (.seam_components.max_component_area // 0))] | if length==0 then 0 else .[0] end' "$GPU_JSON")"

echo "[overlap-order] scene=$SCENE em=$EM pxrange=$PXRANGE heal=$PRESENT_HEAL gpu_batch=$GPU_BATCH ovlp=$OVLP"
echo "[overlap-order] cpu pass=$cpu_pass issue_score=$cpu_issue core_bad=$cpu_core apex_core_bad=$cpu_apex"
echo "[overlap-order] gpu pass=$gpu_pass issue_score=$gpu_issue core_bad=$gpu_core apex_core_bad=$gpu_apex"
echo "[overlap-order] seam target_ch=$target_ch cpu_seam_issue=$cpu_seam gpu_seam_issue=$gpu_seam"
echo "[overlap-order] cpu report: $CPU_JSON"
echo "[overlap-order] gpu report: $GPU_JSON"

if [ "$gpu_core" -gt "$cpu_core" ] || [ "$gpu_apex" -gt "$cpu_apex" ]; then
  echo "[overlap-order] FAIL: gpu generation introduces extra internal counter artifacts before presentation alpha."
  exit 1
fi

if [ "$gpu_seam" -gt "$cpu_seam" ]; then
  echo "[overlap-order] FAIL: gpu generation introduces extra seam artifacts for glyph $target_ch."
  exit 1
fi

echo "[overlap-order] PASS: gpu internal counter integrity is not worse than cpu."
