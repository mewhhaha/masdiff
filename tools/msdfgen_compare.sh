#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
FONT_PATH=${1:-"$ROOT_DIR/assets/Inter/Inter-VariableFont_opsz,wght.ttf"}
TEXT=${2:-"masdiff"}
PX_SIZE=${PX_SIZE:-128}
PX_RANGE=${PX_RANGE:-12}
PADDING=${PADDING:-16}
OUT_DIR=${OUT_DIR:-"$ROOT_DIR/out/msdfgen_compare"}

mkdir -p "$OUT_DIR/masdiff" "$OUT_DIR/msdfgen"

cabal run --project-dir="$ROOT_DIR" msdf-dump -- \
  --font "$FONT_PATH" \
  --text "$TEXT" \
  --pixel-size "$PX_SIZE" \
  --range "$PX_RANGE" \
  --padding "$PADDING" \
  --format mtsdf \
  --out-dir "$OUT_DIR/masdiff"

MSDFGEN_BIN=${MSDFGEN_BIN:-""}
if [ -z "$MSDFGEN_BIN" ]; then
  if command -v msdfgen >/dev/null 2>&1; then
    MSDFGEN_BIN="msdfgen"
  fi
fi

if [ -z "$MSDFGEN_BIN" ]; then
  echo "msdfgen not found. Set MSDFGEN_BIN to compare against msdfgen output." >&2
  exit 0
fi

text_len=${#TEXT}
for ((i=0; i<text_len; i++)); do
  ch=${TEXT:$i:1}
  code=$(printf "%04X" "'${ch}'")
  out_file="$OUT_DIR/msdfgen/U+$code.png"
  "$MSDFGEN_BIN" mtsdf \
    -font "$FONT_PATH" "$ch" \
    -o "$out_file" \
    -pxrange "$PX_RANGE" \
    -autoframe \
    -size "$PX_SIZE" "$PX_SIZE" >/dev/null 2>&1 || {
      echo "msdfgen failed for '$ch' (U+$code)" >&2
    }
  echo "msdfgen wrote $out_file"
done

printf "\nOutput:\n  masdiff: %s\n  msdfgen: %s\n" "$OUT_DIR/masdiff" "$OUT_DIR/msdfgen"
