#!/usr/bin/env bash
set -euo pipefail

MODE=${1:-compare}

case "$MODE" in
  compare)
    cabal test msdf-tests
    ;;
  update)
    MSDF_UPDATE_SNAPSHOTS=1 cabal test msdf-tests
    ;;
  *)
    echo "usage: $0 [compare|update]" >&2
    exit 1
    ;;
esac
