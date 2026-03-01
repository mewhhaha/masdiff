# SDL3 Crisp Font Plan (Deterministic)

Goal: remove visible SDL3 text artifacts (pinholes/seams/box bleed) while keeping correctness and parity intact.

## Why this plan is predictable

Every change must pass **all** gates before it is kept:
1. correctness gate (`masdiff-test`),
2. parity gate (`masdiff-parity -- --require-exact`),
3. visual oracle gate (`just sdl3-ab` + harness JSON + matrix images).

No multi-change patches in one step.

## Upstream references driving decisions

- msdfgen shader guidance (median-based MSDF decode and screen-space range):
  - https://github.com/Chlumsky/msdfgen
- msdf-atlas-gen overlap/error-correction controls and atlas pipeline:
  - https://github.com/Chlumsky/msdf-atlas-gen
- awesome-msdf practical rendering notes:
  - https://github.com/Blatko1/awesome-msdf
- msdfgl GPU-generation architecture (CPU shape decomposition, GPU raster stages):
  - https://github.com/nyyManni/msdfgl

## Fixed baseline commands

Run in repo root:

```sh
just oracle
just sdl3-ab
CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal test masdiff-test -j1
CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal run masdiff-parity -- --require-exact
```

Notes:
- `just oracle` always runs `native` vs `process(msdfgen)` checks, including stress cases.
- Set `MASDIFF_ORACLE_ENFORCE=1` (or `MASDIFF_SDL_ORACLE_ENFORCE=1` in SDL scripts) to fail fast on any oracle mismatch.
- Variable-font oracle checks instantiate static fonts on the process side by default (`MASDIFF_ORACLE_INSTANCE_VAR=1`) so axis support gaps in `process -varfont` do not hide failures.

Visual artifacts must be judged from:
- `examples/sdl3-spirdo-text/out/ab/default-matrix.png`
- `examples/sdl3-spirdo-text/out/ab/single-var-bold-a-matrix.png`

## Current status

- Correctness: passing.
- Parity: strict exact passing.
- Visual baseline (`just sdl3`, now default CPU generation): clean enough for demo use.
- GPU generation path (`MASDIFF_SDL_GEN_BACKEND=gpu`): still fails default-scene artifact harness (`issue_score=270`, failing glyphs includes overlap-attributed `R` with visible `A` seam contamination in the same kerning pair).

## Iteration roadmap

### Step A (done): Canonical presentation shader

- Remove aggressive present-time heuristics (`heal` behavior + unconditional alpha-max fallback behavior).
- Keep decode path close to canonical median-based MSDF rendering.

Acceptance:
- build/test/parity pass,
- no regression in default matrix.

### Step B: Derivative and UV guard normalization

Scope:
- compute `screenPxRange` from interpolated UV, not post-clamped UV,
- ensure only one guard mechanism is active (either CPU inset or fragment clamp), not both.

Acceptance:
- `single-var-bold-a` harness stops reporting counter contamination,
- no new seams in default matrix.

### Step C: MTSDF alpha fallback as opt-in threshold (not default)

Scope:
- keep RGB-median as default decode,
- if alpha assistance is needed, gate by explicit threshold and env toggle,
- never use unconditional `max(msdf,sdf)` behavior.

Acceptance:
- no box/bleed artifacts in bold rows,
- equal or better edge smoothness metrics.

### Step D: Harness hardening (prevent false positives)

Scope:
- fix probe placement drift for variable-font bold scene,
- add per-scene probe calibration and keep default checks strict,
- fail only on artifact-correlated metrics.

Acceptance:
- harness failures correlate with visible defects,
- no clean image should fail probes.

### Step E: Final lock

- Keep only changes that pass all gates for 3 consecutive runs.
- Record final environment and commands in `docs/PERFORMANCE.md` and `docs/SDL3_RENDER_HANDBACK.md`.

## Keep/revert rules

- If parity or correctness fails: immediate revert of current step.
- If visuals improve but harness worsens due obvious probe drift: fix harness in next isolated step.
- If visual wins are not reproducible across 3 runs: do not keep.
