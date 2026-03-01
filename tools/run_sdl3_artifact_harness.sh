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
  local ovlp="$5"
  local allow_check_fail="${6:-0}"
  local image="$OUT_DIR/${tag}.png"
  local meta="$OUT_DIR/${tag}.tsv"
  local report="$OUT_DIR/${tag}.json"
  local atlas_base="$OUT_DIR/${tag}-atlas.png"
  local -a atlas_args
  local status=0

  echo "[harness] running ${tag}"
  rm -f "$image" "$meta" "$report" "$OUT_DIR/${tag}-atlas-line"*.png "$OUT_DIR/${tag}-atlas-line"*.tsv
  (
    cd "$EXAMPLE_DIR"
    local -a envs
    envs=(
      "CABAL_DIR=$PWD/.cabal"
      "CABAL_LOGDIR=$PWD/.cabal-logs"
      "SDL_AUDIODRIVER=${SDL_AUDIODRIVER:-dummy}"
      "SDL_VIDEODRIVER=${SDL_VIDEODRIVER:-x11}"
      "MASDIFF_SDL_GEN_BACKEND=${MASDIFF_SDL_GEN_BACKEND:-gpu}"
      "MASDIFF_SDL_GPU_BATCH=${MASDIFF_SDL_GPU_BATCH:-0}"
      "MASDIFF_SDL_GEN_STRICT=${MASDIFF_SDL_GEN_STRICT:-1}"
      "MASDIFF_SDL_PRESENT_HEAL=${MASDIFF_SDL_PRESENT_HEAL:-1}"
      "MASDIFF_SDL_PRESENT_HEAL_MODE=${MASDIFF_SDL_PRESENT_HEAL_MODE:-1}"
      "MASDIFF_SDL_PXRANGE=${MASDIFF_SDL_PXRANGE:-7}"
      "MASDIFF_SDL_OVLP=$ovlp"
      "MASDIFF_SDL_SCENE=$scene"
      "MASDIFF_SDL_CAPTURE=$image"
      "MASDIFF_SDL_META=$meta"
    )
    if [ "${MASDIFF_SDL_HARNESS_ATLAS_CONSISTENCY:-1}" = "1" ]; then
      envs+=("MASDIFF_SDL_DUMP_LINE_ATLAS=$atlas_base")
    fi
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

  atlas_args=()
  if [ "${MASDIFF_SDL_HARNESS_ATLAS_CONSISTENCY:-1}" = "1" ]; then
    shopt -s nullglob
    local atlas_img
    for atlas_img in "$OUT_DIR/${tag}-atlas-line"*.png; do
      local atlas_meta="${atlas_img%.png}.tsv"
      if [ -f "$atlas_meta" ]; then
        atlas_args+=(--atlas-image "$atlas_img" --atlas-meta "$atlas_meta")
      fi
    done
    shopt -u nullglob
  fi

  python3 "$PY" --scene "$scene" --image "$image" --meta "$meta" --json-out "$report" "${atlas_args[@]}"
  if [ $? -ne 0 ]; then
    if [ "$allow_check_fail" = "1" ]; then
      echo "[harness] WARN: checks failed for ${tag} (allowed for score-only A/B)"
      return 0
    fi
    echo "[harness] ERROR: checks failed for ${tag}"
    return 1
  fi
  echo "[harness] ${tag} PASSED"
  return 0
}

compare_overlap_reports() {
  local baseline_json="$1"
  local overlap_json="$2"
  local label="$3"

  python3 - "$baseline_json" "$overlap_json" "$label" <<'PY'
import json
import sys

base_path, ovlp_path, label = sys.argv[1], sys.argv[2], sys.argv[3]
base = json.load(open(base_path, "r", encoding="utf-8"))
ovlp = json.load(open(ovlp_path, "r", encoding="utf-8"))
b = int(base.get("aggregate", {}).get("issue_score", 0))
o = int(ovlp.get("aggregate", {}).get("issue_score", 0))
print(f"[harness] overlap A/B {label}: baseline={b} overlap={o}")
if o > b:
    print(f"[harness] ERROR: overlap increased issue score for {label}")
    sys.exit(1)
PY
}

compare_pxrange_reports() {
  local baseline_json="$1"
  local candidate_json="$2"
  local label="$3"
  local base_pxr="$4"
  local cand_pxr="$5"

  python3 - "$baseline_json" "$candidate_json" "$label" "$base_pxr" "$cand_pxr" <<'PY'
import json
import sys

base_path, cand_path, label, base_pxr, cand_pxr = sys.argv[1:6]
base = json.load(open(base_path, "r", encoding="utf-8"))
cand = json.load(open(cand_path, "r", encoding="utf-8"))
b = int(base.get("aggregate", {}).get("issue_score", 0))
c = int(cand.get("aggregate", {}).get("issue_score", 0))
print(f"[harness] pxrange A/B {label}: pxr={base_pxr}:{b} pxr={cand_pxr}:{c}")
if c > b:
    print(f"[harness] ERROR: pxrange candidate regressed issue score for {label}")
    sys.exit(1)
PY
}

compare_inset_reports() {
  local baseline_json="$1"
  local candidate_json="$2"
  local label="$3"
  local base_inset="$4"
  local cand_inset="$5"

  python3 - "$baseline_json" "$candidate_json" "$label" "$base_inset" "$cand_inset" <<'PY'
import json
import sys

base_path, cand_path, label, base_inset, cand_inset = sys.argv[1:6]
base = json.load(open(base_path, "r", encoding="utf-8"))
cand = json.load(open(cand_path, "r", encoding="utf-8"))
b = int(base.get("aggregate", {}).get("issue_score", 0))
c = int(cand.get("aggregate", {}).get("issue_score", 0))
print(f"[harness] inset A/B {label}: inset={base_inset}:{b} inset={cand_inset}:{c}")
if c > b:
    print(f"[harness] ERROR: inset candidate regressed issue score for {label}")
    sys.exit(1)
PY
}

compare_present_heal_reports() {
  local baseline_json="$1"
  local candidate_json="$2"
  local label="$3"
  local base_heal="$4"
  local cand_heal="$5"

  python3 - "$baseline_json" "$candidate_json" "$label" "$base_heal" "$cand_heal" <<'PY'
import json
import sys

base_path, cand_path, label, base_heal, cand_heal = sys.argv[1:6]
base = json.load(open(base_path, "r", encoding="utf-8"))
cand = json.load(open(cand_path, "r", encoding="utf-8"))
b = int(base.get("aggregate", {}).get("issue_score", 0))
c = int(cand.get("aggregate", {}).get("issue_score", 0))
print(f"[harness] present-heal A/B {label}: heal={base_heal}:{b} heal={cand_heal}:{c}")
if c > b:
    print(f"[harness] ERROR: present-heal candidate regressed issue score for {label}")
    sys.exit(1)
PY
}

compare_present_heal_mode_reports() {
  local baseline_json="$1"
  local candidate_json="$2"
  local label="$3"
  local base_mode="$4"
  local cand_mode="$5"

  python3 - "$baseline_json" "$candidate_json" "$label" "$base_mode" "$cand_mode" <<'PY'
import json
import sys

base_path, cand_path, label, base_mode, cand_mode = sys.argv[1:6]
base = json.load(open(base_path, "r", encoding="utf-8"))
cand = json.load(open(cand_path, "r", encoding="utf-8"))
b = int(base.get("aggregate", {}).get("issue_score", 0))
c = int(cand.get("aggregate", {}).get("issue_score", 0))
print(f"[harness] present-heal-mode A/B {label}: mode={base_mode}:{b} mode={cand_mode}:{c}")
if c > b:
    print(f"[harness] ERROR: present-heal-mode candidate regressed issue score for {label}")
    sys.exit(1)
PY
}

compare_dim_reports() {
  local baseline_json="$1"
  local candidate_json="$2"
  local label="$3"
  local base_dim="$4"
  local cand_dim="$5"

  python3 - "$baseline_json" "$candidate_json" "$label" "$base_dim" "$cand_dim" <<'PY'
import json
import sys

base_path, cand_path, label, base_dim, cand_dim = sys.argv[1:6]
base = json.load(open(base_path, "r", encoding="utf-8"))
cand = json.load(open(cand_path, "r", encoding="utf-8"))
b = int(base.get("aggregate", {}).get("issue_score", 0))
c = int(cand.get("aggregate", {}).get("issue_score", 0))
print(f"[harness] dim A/B {label}: dim={base_dim}:{b} dim={cand_dim}:{c}")
if c > b:
    print(f"[harness] ERROR: dim candidate regressed issue score for {label}")
    sys.exit(1)
PY
}

run_overlap_ab_case() {
  local scene="$1"
  local tag_prefix="$2"
  local nofit="$3"
  local em="$4"
  local allow_fail="${5:-0}"
  local base_ok=1
  local cand_ok=1
  run_case "$scene" "${tag_prefix}-ovlp0" "$nofit" "$em" "0" "$allow_fail" && base_ok=0 || overall_status=1
  run_case "$scene" "${tag_prefix}-ovlp1" "$nofit" "$em" "1" "$allow_fail" && cand_ok=0 || overall_status=1
  if [ $base_ok -eq 0 ] && [ $cand_ok -eq 0 ]; then
    compare_overlap_reports "$OUT_DIR/${tag_prefix}-ovlp0.json" "$OUT_DIR/${tag_prefix}-ovlp1.json" "$scene" || overall_status=1
  else
    echo "[harness] SKIP overlap A/B ${scene}: missing baseline or candidate report"
  fi
}

run_pxrange_ab_case() {
  local scene="$1"
  local tag_prefix="$2"
  local nofit="$3"
  local em="$4"
  local allow_fail="${5:-0}"
  local base_pxr="${MASDIFF_SDL_HARNESS_PXRANGE_BASE:-6}"
  local cand_pxr="${MASDIFF_SDL_HARNESS_PXRANGE_CANDIDATE:-8}"
  local base_ok=1
  local cand_ok=1
  MASDIFF_SDL_PXRANGE="$base_pxr" run_case "$scene" "${tag_prefix}-pxr${base_pxr}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && base_ok=0 || overall_status=1
  MASDIFF_SDL_PXRANGE="$cand_pxr" run_case "$scene" "${tag_prefix}-pxr${cand_pxr}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && cand_ok=0 || overall_status=1
  if [ $base_ok -eq 0 ] && [ $cand_ok -eq 0 ]; then
    compare_pxrange_reports "$OUT_DIR/${tag_prefix}-pxr${base_pxr}.json" "$OUT_DIR/${tag_prefix}-pxr${cand_pxr}.json" "$scene" "$base_pxr" "$cand_pxr" || overall_status=1
  else
    echo "[harness] SKIP pxrange A/B ${scene}: missing baseline or candidate report"
  fi
}

run_inset_ab_case() {
  local scene="$1"
  local tag_prefix="$2"
  local nofit="$3"
  local em="$4"
  local allow_fail="${5:-0}"
  local base_inset="${MASDIFF_SDL_HARNESS_INSET_BASE:-0.0}"
  local cand_inset="${MASDIFF_SDL_HARNESS_INSET_CANDIDATE:-0.5}"
  local base_ok=1
  local cand_ok=1
  MASDIFF_SDL_UV_INSET="$base_inset" run_case "$scene" "${tag_prefix}-inset${base_inset}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && base_ok=0 || overall_status=1
  MASDIFF_SDL_UV_INSET="$cand_inset" run_case "$scene" "${tag_prefix}-inset${cand_inset}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && cand_ok=0 || overall_status=1
  if [ $base_ok -eq 0 ] && [ $cand_ok -eq 0 ]; then
    compare_inset_reports "$OUT_DIR/${tag_prefix}-inset${base_inset}.json" "$OUT_DIR/${tag_prefix}-inset${cand_inset}.json" "$scene" "$base_inset" "$cand_inset" || overall_status=1
  else
    echo "[harness] SKIP inset A/B ${scene}: missing baseline or candidate report"
  fi
}

run_dim_ab_case() {
  local scene="$1"
  local tag_prefix="$2"
  local nofit="$3"
  local em="$4"
  local allow_fail="${5:-0}"
  local base_dim="${MASDIFF_SDL_HARNESS_DIM_BASE:-192}"
  local cand_dim="${MASDIFF_SDL_HARNESS_DIM_CANDIDATE:-256}"
  local base_ok=1
  local cand_ok=1
  MASDIFF_SDL_DIM="$base_dim" run_case "$scene" "${tag_prefix}-dim${base_dim}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && base_ok=0 || overall_status=1
  MASDIFF_SDL_DIM="$cand_dim" run_case "$scene" "${tag_prefix}-dim${cand_dim}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && cand_ok=0 || overall_status=1
  if [ $base_ok -eq 0 ] && [ $cand_ok -eq 0 ]; then
    compare_dim_reports "$OUT_DIR/${tag_prefix}-dim${base_dim}.json" "$OUT_DIR/${tag_prefix}-dim${cand_dim}.json" "$scene" "$base_dim" "$cand_dim" || overall_status=1
  else
    echo "[harness] SKIP dim A/B ${scene}: missing baseline or candidate report"
  fi
}

run_present_heal_ab_case() {
  local scene="$1"
  local tag_prefix="$2"
  local nofit="$3"
  local em="$4"
  local allow_fail="${5:-0}"
  local base_heal="${MASDIFF_SDL_HARNESS_PRESENT_HEAL_BASE:-0}"
  local cand_heal="${MASDIFF_SDL_HARNESS_PRESENT_HEAL_CANDIDATE:-1}"
  local base_ok=1
  local cand_ok=1
  MASDIFF_SDL_PRESENT_HEAL="$base_heal" run_case "$scene" "${tag_prefix}-heal${base_heal}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && base_ok=0 || overall_status=1
  MASDIFF_SDL_PRESENT_HEAL="$cand_heal" run_case "$scene" "${tag_prefix}-heal${cand_heal}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && cand_ok=0 || overall_status=1
  if [ $base_ok -eq 0 ] && [ $cand_ok -eq 0 ]; then
    compare_present_heal_reports "$OUT_DIR/${tag_prefix}-heal${base_heal}.json" "$OUT_DIR/${tag_prefix}-heal${cand_heal}.json" "$scene" "$base_heal" "$cand_heal" || overall_status=1
  else
    echo "[harness] SKIP present-heal A/B ${scene}: missing baseline or candidate report"
  fi
}

run_present_heal_mode_ab_case() {
  local scene="$1"
  local tag_prefix="$2"
  local nofit="$3"
  local em="$4"
  local allow_fail="${5:-0}"
  local base_mode="${MASDIFF_SDL_HARNESS_PRESENT_HEAL_MODE_BASE:-1}"
  local cand_mode="${MASDIFF_SDL_HARNESS_PRESENT_HEAL_MODE_CANDIDATE:-2}"
  local base_ok=1
  local cand_ok=1
  MASDIFF_SDL_PRESENT_HEAL=1 MASDIFF_SDL_PRESENT_HEAL_MODE="$base_mode" run_case "$scene" "${tag_prefix}-healmode${base_mode}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && base_ok=0 || overall_status=1
  MASDIFF_SDL_PRESENT_HEAL=1 MASDIFF_SDL_PRESENT_HEAL_MODE="$cand_mode" run_case "$scene" "${tag_prefix}-healmode${cand_mode}" "$nofit" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" "$allow_fail" && cand_ok=0 || overall_status=1
  if [ $base_ok -eq 0 ] && [ $cand_ok -eq 0 ]; then
    compare_present_heal_mode_reports "$OUT_DIR/${tag_prefix}-healmode${base_mode}.json" "$OUT_DIR/${tag_prefix}-healmode${cand_mode}.json" "$scene" "$base_mode" "$cand_mode" || overall_status=1
  else
    echo "[harness] SKIP present-heal-mode A/B ${scene}: missing baseline or candidate report"
  fi
}

overall_status=0

if [ "${MASDIFF_SDL_REQUIRE_ORACLE:-1}" = "1" ]; then
  echo "[harness] running msdfgen oracle gate"
  if ! MASDIFF_ORACLE_ENFORCE="${MASDIFF_SDL_ORACLE_ENFORCE:-0}" "$ROOT/tools/run_msdfgen_oracle_gate.sh"; then
    echo "[harness] ERROR: msdfgen oracle gate failed"
    overall_status=1
  fi
fi

run_case "default" "default-fit" "0" "" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" || overall_status=1

single_ems_raw="${MASDIFF_SDL_HARNESS_EMS:-24 32}"
read -r -a single_ems <<<"$single_ems_raw"
single_scenes=(
  single-a
  single-4
  single-m
  single-p
  single-r
  single-y
  single-var-light-a
  single-var-light-4
  single-var-light-m
  single-var-light-p
  single-var-light-r
  single-var-light-y
  single-var-bold-a
  single-var-bold-4
  single-var-bold-m
  single-var-bold-p
  single-var-bold-r
  single-var-bold-y
)
if [ "${MASDIFF_SDL_HARNESS_SKIP_SINGLES:-0}" != "1" ]; then
  for scene in "${single_scenes[@]}"; do
    for em in "${single_ems[@]}"; do
      run_case "$scene" "${scene}-em${em}" "1" "$em" "${MASDIFF_SDL_OVLP_DEFAULT:-0}" || overall_status=1
    done
  done
fi

if [ "${MASDIFF_SDL_HARNESS_OVLP_AB:-1}" = "1" ]; then
  run_overlap_ab_case "default" "default-fit" "0" ""
  run_overlap_ab_case "single-var-bold-a" "single-var-bold-a-em24" "1" "24" "1"
  run_overlap_ab_case "single-var-bold-a" "single-var-bold-a-em32" "1" "32" "1"
  run_overlap_ab_case "single-var-bold-4" "single-var-bold-4-em24" "1" "24" "1"
  run_overlap_ab_case "single-var-bold-4" "single-var-bold-4-em32" "1" "32" "1"
  run_overlap_ab_case "single-var-bold-r" "single-var-bold-r-em24" "1" "24" "1"
  run_overlap_ab_case "single-var-bold-r" "single-var-bold-r-em32" "1" "32" "1"
fi

if [ "${MASDIFF_SDL_HARNESS_PXRANGE_AB:-1}" = "1" ]; then
  run_pxrange_ab_case "default" "default-fit" "0" ""
  run_pxrange_ab_case "single-var-bold-a" "single-var-bold-a-em24" "1" "24" "1"
  run_pxrange_ab_case "single-var-bold-a" "single-var-bold-a-em32" "1" "32" "1"
  run_pxrange_ab_case "single-var-bold-4" "single-var-bold-4-em24" "1" "24" "1"
  run_pxrange_ab_case "single-var-bold-4" "single-var-bold-4-em32" "1" "32" "1"
  run_pxrange_ab_case "single-var-bold-r" "single-var-bold-r-em24" "1" "24" "1"
  run_pxrange_ab_case "single-var-bold-r" "single-var-bold-r-em32" "1" "32" "1"
fi

if [ "${MASDIFF_SDL_HARNESS_DIM_AB:-1}" = "1" ]; then
  run_dim_ab_case "default" "default-fit" "0" ""
  run_dim_ab_case "single-var-bold-a" "single-var-bold-a-em24" "1" "24" "1"
  run_dim_ab_case "single-var-bold-a" "single-var-bold-a-em32" "1" "32" "1"
  run_dim_ab_case "single-var-bold-4" "single-var-bold-4-em24" "1" "24" "1"
  run_dim_ab_case "single-var-bold-4" "single-var-bold-4-em32" "1" "32" "1"
  run_dim_ab_case "single-var-bold-r" "single-var-bold-r-em24" "1" "24" "1"
  run_dim_ab_case "single-var-bold-r" "single-var-bold-r-em32" "1" "32" "1"
fi

if [ "${MASDIFF_SDL_HARNESS_INSET_AB:-1}" = "1" ]; then
  run_inset_ab_case "default" "default-fit" "0" ""
  run_inset_ab_case "single-var-bold-a" "single-var-bold-a-em24" "1" "24" "1"
  run_inset_ab_case "single-var-bold-a" "single-var-bold-a-em32" "1" "32" "1"
  run_inset_ab_case "single-var-bold-4" "single-var-bold-4-em24" "1" "24" "1"
  run_inset_ab_case "single-var-bold-4" "single-var-bold-4-em32" "1" "32" "1"
  run_inset_ab_case "single-var-bold-r" "single-var-bold-r-em24" "1" "24" "1"
  run_inset_ab_case "single-var-bold-r" "single-var-bold-r-em32" "1" "32" "1"
fi

if [ "${MASDIFF_SDL_HARNESS_PRESENT_HEAL_AB:-1}" = "1" ]; then
  run_present_heal_ab_case "default" "default-fit" "0" ""
  run_present_heal_ab_case "single-var-bold-a" "single-var-bold-a-em24" "1" "24" "1"
  run_present_heal_ab_case "single-var-bold-a" "single-var-bold-a-em32" "1" "32" "1"
  run_present_heal_ab_case "single-var-bold-4" "single-var-bold-4-em24" "1" "24" "1"
  run_present_heal_ab_case "single-var-bold-4" "single-var-bold-4-em32" "1" "32" "1"
  run_present_heal_ab_case "single-var-bold-r" "single-var-bold-r-em24" "1" "24" "1"
  run_present_heal_ab_case "single-var-bold-r" "single-var-bold-r-em32" "1" "32" "1"
fi

if [ "${MASDIFF_SDL_HARNESS_PRESENT_HEAL_MODE_AB:-0}" = "1" ]; then
  run_present_heal_mode_ab_case "default" "default-fit" "0" ""
  run_present_heal_mode_ab_case "single-var-bold-a" "single-var-bold-a-em24" "1" "24" "1"
  run_present_heal_mode_ab_case "single-var-bold-a" "single-var-bold-a-em32" "1" "32" "1"
  run_present_heal_mode_ab_case "single-var-bold-4" "single-var-bold-4-em24" "1" "24" "1"
  run_present_heal_mode_ab_case "single-var-bold-4" "single-var-bold-4-em32" "1" "32" "1"
  run_present_heal_mode_ab_case "single-var-bold-r" "single-var-bold-r-em24" "1" "24" "1"
  run_present_heal_mode_ab_case "single-var-bold-r" "single-var-bold-r-em32" "1" "32" "1"
fi

echo "[harness] reports in $OUT_DIR"
if [ $overall_status -ne 0 ]; then
  exit 1
fi
