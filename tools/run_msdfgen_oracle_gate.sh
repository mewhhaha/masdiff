#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT="$ROOT/out/oracle"
mkdir -p "$OUT"

CABAL_DIR_PATH="$ROOT/.cabal"
CABAL_LOGDIR_PATH="$ROOT/.cabal-logs"
ENFORCE="${MASDIFF_ORACLE_ENFORCE:-0}"
INSTANCE_VAR="${MASDIFF_ORACLE_INSTANCE_VAR:-1}"
STATUS=0
REPORT_JSONL="$OUT/report.jsonl"

: >"$REPORT_JSONL"

instantiate_varfont() {
  local spec="$1"
  local out_ttf="$2"
  python3 - "$spec" "$out_ttf" <<'PY'
import sys
from fontTools.ttLib import TTFont
from fontTools.varLib.instancer import instantiateVariableFont

spec, out_ttf = sys.argv[1], sys.argv[2]
if "?" not in spec:
    raise SystemExit("varfont spec is missing axis query")
path, query = spec.split("?", 1)
axes = {}
for pair in [p for p in query.split("&") if p]:
    if "=" not in pair:
        raise SystemExit(f"invalid axis assignment: {pair}")
    k, v = pair.split("=", 1)
    axes[k] = float(v)
tt = TTFont(path)
inst = instantiateVariableFont(tt, axes, inplace=False)
inst.save(out_ttf)
PY
}

run_case() {
  local case_id="$1"
  local mode="$2"
  local src="$3"
  local glyph="$4"
  local dim="$5"
  local pxr="$6"

  local native_mode="$mode"
  local native_src="$src"
  local process_mode="$mode"
  local process_src="$src"
  local compare_mode="strict"
  local inst_ttf="$OUT/${case_id}-instance.ttf"

  if [ "$mode" = "varfont" ] && [ "$INSTANCE_VAR" = "1" ] && [[ "$src" == *"?"* ]]; then
    if instantiate_varfont "$src" "$inst_ttf"; then
      process_mode="font"
      process_src="$inst_ttf"
      compare_mode="varfont"
      echo "[oracle] case ${case_id}: process uses instantiated varfont -> ${inst_ttf}"
    else
      echo "[oracle] WARN ${case_id}: varfont instancing failed, falling back to process -varfont"
    fi
  fi

  local native_png="$OUT/${case_id}-native.png"
  local process_png="$OUT/${case_id}-process.png"
  local native_metrics="$OUT/${case_id}-native.metrics"
  local process_metrics="$OUT/${case_id}-process.metrics"
  local native_log="$OUT/${case_id}-native.log"
  local process_log="$OUT/${case_id}-process.log"

  echo "[oracle] case ${case_id}: rendering native"
  (
    cd "$ROOT"
    MASDIFF_BACKEND=native CABAL_DIR="$CABAL_DIR_PATH" CABAL_LOGDIR="$CABAL_LOGDIR_PATH" \
      cabal run masdiff -- mtsdf "-${native_mode}" "$native_src" "$glyph" \
      -dimensions "$dim" "$dim" -pxrange "$pxr" -autoframe \
      -o "$native_png" -printmetrics >"$native_metrics" 2>"$native_log"
  ) || {
    echo "[oracle] FAIL ${case_id}: native render failed"
    STATUS=1
    return
  }

  echo "[oracle] case ${case_id}: rendering process(msdfgen)"
  (
    cd "$ROOT"
    MASDIFF_BACKEND=process CABAL_DIR="$CABAL_DIR_PATH" CABAL_LOGDIR="$CABAL_LOGDIR_PATH" \
      cabal run masdiff -- mtsdf "-${process_mode}" "$process_src" "$glyph" \
      -dimensions "$dim" "$dim" -pxrange "$pxr" -autoframe \
      -o "$process_png" -printmetrics >"$process_metrics" 2>"$process_log"
  ) || {
    echo "[oracle] FAIL ${case_id}: process render failed"
    STATUS=1
    return
  }

  local summary
  summary="$(python3 - "$case_id" "$native_png" "$process_png" "$native_metrics" "$process_metrics" "$compare_mode" <<'PY'
import json
import re
import sys

from PIL import Image

case_id, native_png, process_png, native_metrics, process_metrics, compare_mode = sys.argv[1:7]


def parse_metrics(path: str):
    out = {}
    with open(path, "r", encoding="utf-8") as f:
        for raw in f:
            line = raw.strip()
            if not line:
                continue
            if "=" not in line:
                continue
            key, value = [x.strip() for x in line.split("=", 1)]
            if key == "bounds":
                out[key] = [float(x.strip()) for x in value.split(",")]
            elif key == "translate":
                out[key] = [float(x.strip()) for x in value.split(",")]
            elif key == "range":
                m = re.match(r"([+-]?[0-9]*\.?[0-9]+(?:[eE][+-]?[0-9]+)?) to ([+-]?[0-9]*\.?[0-9]+(?:[eE][+-]?[0-9]+)?)", value)
                if m:
                    out[key] = [float(m.group(1)), float(m.group(2))]
            else:
                try:
                    out[key] = float(value)
                except ValueError:
                    out[key] = value
    return out


def max_metric_delta(a, b):
    keys = sorted(set(a.keys()) | set(b.keys()))
    max_delta = 0.0
    for key in keys:
        va = a.get(key)
        vb = b.get(key)
        if isinstance(va, list) and isinstance(vb, list) and len(va) == len(vb):
            for xa, xb in zip(va, vb):
                max_delta = max(max_delta, abs(float(xa) - float(xb)))
        elif isinstance(va, (int, float)) and isinstance(vb, (int, float)):
            max_delta = max(max_delta, abs(float(va) - float(vb)))
        elif va != vb:
            max_delta = max(max_delta, 1.0)
    return max_delta


native_rgba = Image.open(native_png).convert("RGBA")
process_rgba = Image.open(process_png).convert("RGBA")
if native_rgba.size != process_rgba.size:
    diff_pixels = native_rgba.size[0] * native_rgba.size[1]
    max_abs = 255
    shape_diff_pixels = diff_pixels
    shape_diff_ratio = 1.0
else:
    a = native_rgba.tobytes()
    b = process_rgba.tobytes()
    px_count = native_rgba.size[0] * native_rgba.size[1]
    max_abs = 0
    diff_pixels = 0
    shape_diff_pixels = 0
    for i in range(px_count):
        off = i * 4
        eq = True
        ra, ga, ba = a[off + 0] / 255.0, a[off + 1] / 255.0, a[off + 2] / 255.0
        rb, gb, bb = b[off + 0] / 255.0, b[off + 1] / 255.0, b[off + 2] / 255.0
        ma = max(min(ra, ga), min(max(ra, ga), ba))
        mb = max(min(rb, gb), min(max(rb, gb), bb))
        if (ma >= 0.5) != (mb >= 0.5):
            shape_diff_pixels += 1
        for ch in range(4):
            da = abs(a[off + ch] - b[off + ch])
            if da > max_abs:
                max_abs = da
            if da != 0:
                eq = False
        if not eq:
            diff_pixels += 1
    shape_diff_ratio = shape_diff_pixels / max(1, px_count)

native_metrics_obj = parse_metrics(native_metrics)
process_metrics_obj = parse_metrics(process_metrics)
metrics_delta = max_metric_delta(native_metrics_obj, process_metrics_obj)

if compare_mode == "strict":
    passed = (native_rgba.size == process_rgba.size) and diff_pixels == 0 and max_abs == 0 and metrics_delta <= 1.0e-6
else:
    passed = (native_rgba.size == process_rgba.size) and shape_diff_ratio <= 0.002 and metrics_delta <= 2.0e-2

summary = {
    "case": case_id,
    "compare_mode": compare_mode,
    "dim_equal": native_rgba.size == process_rgba.size,
    "image_diff_pixels": diff_pixels,
    "image_max_abs": max_abs,
    "shape_diff_pixels": shape_diff_pixels,
    "shape_diff_ratio": shape_diff_ratio,
    "metrics_max_delta": metrics_delta,
    "pass": passed,
}
print(json.dumps(summary, sort_keys=True))
PY
)"

  echo "$summary" >>"$REPORT_JSONL"

  local passed
  passed="$(python3 - "$summary" <<'PY'
import json
import sys
d = json.loads(sys.argv[1])
print("1" if d["pass"] else "0")
PY
)"

  local line
  line="$(python3 - "$summary" <<'PY'
import json
import sys
d = json.loads(sys.argv[1])
print(
    f"[oracle] {d['case']} [{d['compare_mode']}]: pass={d['pass']} "
    f"shape_diff={d['shape_diff_pixels']} ({d['shape_diff_ratio']:.6f}) "
    f"raw_diff={d['image_diff_pixels']} max_abs={d['image_max_abs']} "
    f"metrics_max_delta={d['metrics_max_delta']:.6f}"
)
PY
)"
  echo "$line"

  if [ "$passed" != "1" ]; then
    STATUS=1
  fi
}

echo "[oracle] strict corpus parity (native vs process over manifest cases)"
if ! (
  cd "$ROOT"
  CABAL_DIR="$CABAL_DIR_PATH" CABAL_LOGDIR="$CABAL_LOGDIR_PATH" \
    cabal run masdiff-parity -- --require-exact >"$OUT/parity.log" 2>&1
); then
  echo "[oracle] FAIL strict corpus parity (see $OUT/parity.log)"
  STATUS=1
else
  echo "[oracle] parity pass"
fi

# Control: known-stable static font case.
run_case "static-inter24-A" "font" "assets/Inter/static/Inter_24pt-Regular.ttf" "A" "256" "8"

# Stress cases aligned with current SDL3 artifact triage.
run_case "var-inter-old-bold-A" "varfont" "assets/Inter/Inter-VariableFont_opsz,wght.ttf?wght=900&opsz=32" "A" "256" "8"
run_case "var-inter-v41-bold-A" "varfont" "assets/inter-v4.1-source/InterVariable.ttf?wght=900&opsz=32" "A" "256" "8"
run_case "var-inter-v41-bold-R" "varfont" "assets/inter-v4.1-source/InterVariable.ttf?wght=900&opsz=32" "R" "256" "8"
run_case "var-roboto-flex-bold-A" "varfont" "assets/roboto-flex-source/RobotoFlex-VF.ttf?wght=900&opsz=32" "A" "256" "8"

if [ "$STATUS" -ne 0 ]; then
  echo "[oracle] summary: FAIL (see $REPORT_JSONL)"
  if [ "$ENFORCE" = "1" ]; then
    exit 1
  fi
else
  echo "[oracle] summary: PASS"
fi
