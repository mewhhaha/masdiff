# SDL3 Render Artifact Handoff

This note captures what is needed to continue the SDL3 artifact cleanup quickly.

## Current status

- SDL shader now includes:
  - MTSDF alpha fallback (`max(msdfCov, sdfCov)` when channels disagree).
  - Shader-side seam/pinhole healing logic (ported from CPU heuristics).
- `MASDIFF_SDL_OVLP` default is `False` in `examples/sdl3-spirdo-text/app/Main.hsc`.
- Visual quality improved significantly for default `just sdl3` and single large `A`.

## Repro commands

- Default capture:
```bash
cd examples/sdl3-spirdo-text
CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" \
SDL_AUDIODRIVER=dummy \
MASDIFF_SDL_CAPTURE=out/verify-current3.png \
MASDIFF_SDL_META=out/verify-current3.tsv \
cabal run sdl3-spirdo-text
```

- Single large var-bold `A` capture:
```bash
cd examples/sdl3-spirdo-text
CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" \
SDL_AUDIODRIVER=dummy \
MASDIFF_SDL_SCENE=single-var-bold-a \
MASDIFF_SDL_SINGLE_EM=640 \
MASDIFF_SDL_CAPTURE=out/harness/postfix3-single-var-bold-a-em640-fit.png \
MASDIFF_SDL_META=out/harness/postfix3-single-var-bold-a-em640-fit.tsv \
cabal run sdl3-spirdo-text
```

- Full deterministic harness:
```bash
./tools/run_sdl3_artifact_harness.sh
```

## What is still failing

From current harness run:
- `single-var-light-r-em32` (seam line threshold trip)
- `single-var-bold-a-em24`
- `single-var-bold-a-em32`
- `single-var-bold-m-em24`
- `single-var-bold-p-em24`
- `single-var-bold-p-em32`
- `single-var-bold-r-em24`
- `single-var-bold-r-em32`

Observed reason split:
- `A/P/R` failures are mostly probe-region failures (`counter-*`, `bowl-*`) because hardcoded normalized ROIs no longer match heavy variable shapes.
- `M/R` seam failures are threshold-based (`vert_pixels`/`horiz_pixels`) and need revalidation against visual output.

## What is needed to continue

1. Decide harness policy for `single-var-bold-*`:
   - Option A: keep strict fixed ROIs and tune them per variable weight.
   - Option B: switch to adaptive ROIs (derive hole/bowl components from coverage map).

2. Decide overlap-fix policy for SDL demo:
   - Option A: keep overlap mode exposed and debug separately.
   - Option B: keep overlap disabled for demo path until overlap artifacts are fully solved.

3. After policy decision:
   - update `tools/sdl3_artifact_harness.py`,
   - rerun `./tools/run_sdl3_artifact_harness.sh`,
   - keep only checks that correlate with visible artifacts.

## Files touched in current pass

- `examples/sdl3-spirdo-text/app/Main.hsc`
  - fragment decode and heal logic updated.

