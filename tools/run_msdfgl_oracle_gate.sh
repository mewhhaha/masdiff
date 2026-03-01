#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="$ROOT/out/oracle-msdfgl"
mkdir -p "$OUT_DIR"

MANIFEST="${MASDIFF_MSDFGL_MANIFEST:-$ROOT/out/reference/msdfgl-mtsdf/manifest.tsv}"
PROFILE="${MASDIFF_MSDFGL_PROFILE:-nightly}"
ENFORCE="${MASDIFF_ORACLE_ENFORCE:-0}"

if [ ! -f "$MANIFEST" ]; then
  echo "[msdfgl-oracle] SKIP: manifest not found: $MANIFEST"
  if [ "$ENFORCE" = "1" ]; then
    exit 1
  fi
  exit 0
fi

echo "[msdfgl-oracle] running parity against manifest: $MANIFEST"
set +e
if [ "$ENFORCE" = "1" ]; then
  (
    cd "$ROOT"
    CABAL_DIR="$ROOT/.cabal" CABAL_LOGDIR="$ROOT/.cabal-logs" \
      cabal run masdiff-parity -- \
      --profile "$PROFILE" \
      --oracle msdfgl \
      --manifest "$MANIFEST" \
      --json-out "$OUT_DIR/report.json" \
      --require-oracle
  )
  STATUS=$?
else
  (
    cd "$ROOT"
    CABAL_DIR="$ROOT/.cabal" CABAL_LOGDIR="$ROOT/.cabal-logs" \
      cabal run masdiff-parity -- \
      --profile "$PROFILE" \
      --oracle msdfgl \
      --manifest "$MANIFEST" \
      --json-out "$OUT_DIR/report.json" \
      --allow-missing-oracle
  )
  STATUS=$?
fi
set -e

if [ "$STATUS" -ne 0 ]; then
  echo "[msdfgl-oracle] FAIL: parity command failed (status=$STATUS)"
  if [ "$ENFORCE" = "1" ]; then
    exit "$STATUS"
  fi
  echo "[msdfgl-oracle] non-enforcing mode: continuing"
  exit 0
fi

echo "[msdfgl-oracle] PASS: report written to $OUT_DIR/report.json"
