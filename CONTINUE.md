# CONTINUE (2026-02-26)

## Checkpoint summary
- Major GPU overlap failure (large interior cutout on `R`) is fixed.
- GPU generation now runs non-zero winding fill consistently (matching native fill semantics), with split selector/winding segment buffers.
- Residual issue remains: faint seam lines at overlap joins (e.g. `A` crossbar / `R` joint) in `just sdl3` output.

## What changed in this checkpoint
1. GPU generation winding mode:
   - In `examples/sdl3-spirdo-text/app/Main.hsc`, generation uniform now sets `useNonZeroWinding = True` for both single and atlas GPU paths.
2. Segment payload and shader metadata:
   - `PreparedLineSeg` carries `caps`, `cid` (contour id), and `cw` (contour winding sign) in `lib/MSDF/Native.hs`.
   - Segment upload in `Main.hsc` writes all fields (`x0,y0,x1,y1,col,caps,cid,cw`).
3. Generation shader robustness:
   - Added post-distance sign-consistency correction in generation shaders (`flat` and `struct`) to align channel sign with inside/outside fill.
   - Kept canonical msdf distance path (`dRgbEdge = max(pseudo, clamped)`).
4. Scene harness support:
   - `parseScenePreset` accepts `single-4`, `single-var-light-4`, `single-var-bold-4`.
5. Presentation shaders:
   - `fragmentShader` (heal path) now uses msdf/sdf divergence mix.
   - `fragmentShaderNoHeal` is now separate canonical median-only shader.

## Verification run in this checkpoint
- Build:
  - `cd examples/sdl3-spirdo-text && CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR="$PWD/.cabal-logs" cabal build sdl3-spirdo-text` -> PASS
- Native oracle parity:
  - `./tools/run_msdfgen_oracle_gate.sh` -> PASS
- Overlap-order gates:
  - `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-r ... ./tools/run_sdl3_overlap_order_gate.sh`
    - CPU `issue_score=0`, GPU `issue_score=16`, gate PASS
  - `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-4 ... ./tools/run_sdl3_overlap_order_gate.sh`
    - CPU `issue_score=4`, GPU `issue_score=2`, gate PASS
- Live capture (`just sdl3` equivalent env):
  - `/tmp/just_sdl3_final.png` confirms major cutout is gone; faint seams remain.

## Known remaining problem
- Full visual artifact criteria are not yet met due faint overlap seams in default text scenes.
- This appears to be GPU generation selector/edge-composition residue, not font-file corruption.

## Resume plan
1. Add deterministic CPU-vs-GPU pixel-diff gate for `single-var-bold-a` and `single-var-bold-r` captures at fixed env.
2. Port contour-aware selector composition from native raster (`accumulateSamples` semantics) into GPU generation path without per-glyph exceptions.
3. Keep `msdfgen` oracle and overlap-order gates green after each candidate.
4. Only keep changes that reduce seam metrics and visible seams in `just sdl3` default scene.
