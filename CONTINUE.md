# CONTINUE (2026-02-26 pause handoff)

## Current state
- Checkpoint commit exists: `4d4d950` (`sdl3 gpu: checkpoint non-zero winding overlap progress`).
- Non-batch GPU path (`MASDIFF_SDL_GPU_BATCH=0`) is aligned with CPU on the strict `R` overlap gate.
- Batch overlap scenes now fall back to per-glyph GPU raster path (`buildViaRaster`) to avoid known batch seam regressions.
- Remaining faint seams in default scene are not purely GPU-specific: CPU/native output still shows seam-line residue on bold `A`.

## What is already implemented
1. Non-batch GPU path post-processing:
   - `gpuRasterIO` applies `postCorrectPreparedImage`.
   - Then merges CPU alpha via `replaceImageAlpha correctedImg cpuOut.img`.
2. Present shader currently uses alpha SDF in heal path:
   - `fragmentShader`: `let sd = sdf`.
3. Atlas draw upload lifetime fix:
   - Added `GpuAtlasDrawUpload`, `uploadAtlasDrawBuffers`, `releaseAtlasDrawBuffers`.
   - Atlas path now submits and waits on fence before releasing upload buffers.
4. Overlap gate tightened:
   - `tools/run_sdl3_overlap_order_gate.sh` checks seam issue (`horiz + vert + max_component_area`) and fails when GPU > CPU.

## Last measured results
- `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-r MASDIFF_SDL_OVERLAP_GPU_BATCH=0 ./tools/run_sdl3_overlap_order_gate.sh`
  - CPU `issue_score=2`, GPU `issue_score=2`, seam CPU/GPU `2/2` -> PASS
- `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-r MASDIFF_SDL_OVERLAP_GPU_BATCH=1 ./tools/run_sdl3_overlap_order_gate.sh`
  - CPU `issue_score=2`, GPU `issue_score=2`, seam CPU/GPU `2/2` -> PASS
- `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-a MASDIFF_SDL_OVERLAP_GPU_BATCH=0 MASDIFF_SDL_OVERLAP_EM=640 ./tools/run_sdl3_overlap_order_gate.sh`
  - CPU `issue_score=18`, GPU `issue_score=18`, seam CPU/GPU `14/14` -> PASS (shared residue)
- `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-a MASDIFF_SDL_OVERLAP_GPU_BATCH=1 MASDIFF_SDL_OVERLAP_EM=640 ./tools/run_sdl3_overlap_order_gate.sh`
  - CPU `issue_score=18`, GPU `issue_score=18`, seam CPU/GPU `14/14` -> PASS
- Default scene captures:
  - CPU report: `examples/sdl3-spirdo-text/out/debug_cpu_default.report.json` -> FAIL, `issue_score=225`, failing glyph `A`
  - GPU (batch=0, heal=1): `.../debug_gpu_default.report.json` -> PASS, `issue_score=194`

## What is needed to continue
1. Keep focus split by path:
   - Path A: eliminate shared CPU/native seam residue (source of remaining faint lines in default scene).
   - Path B: fix batch-only seam regression (`gpu_batch=1`) without per-glyph logic.
2. Add deterministic tests before more shader/math changes:
   - A CPU seam regression gate for `single-var-bold-a` (expected to fail now; use as target).
   - Keep existing `R` gate to ensure no regression in both batch and non-batch modes.
3. Investigate overlap handling in native raster core:
   - `lib/MSDF/Native/Raster.hs` around `samplePixelAt`, `applyAmbiguousSignFix`, `applyModernErrorCorrection`.
   - Verify whether seam on `A` is introduced pre- or post-correction.
4. Re-verify every candidate against both:
   - `./tools/run_msdfgen_oracle_gate.sh`
   - overlap gates (`single-var-bold-a` and `single-var-bold-r`, batch 0 and 1).

## Resume commands (in order)
1. `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-a MASDIFF_SDL_OVERLAP_GPU_BATCH=0 MASDIFF_SDL_OVERLAP_EM=640 ./tools/run_sdl3_overlap_order_gate.sh`
2. `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-r MASDIFF_SDL_OVERLAP_GPU_BATCH=0 ./tools/run_sdl3_overlap_order_gate.sh`
3. `MASDIFF_SDL_OVERLAP_SCENE=single-var-bold-r MASDIFF_SDL_OVERLAP_GPU_BATCH=1 ./tools/run_sdl3_overlap_order_gate.sh` (expected PASS with overlap fallback)
4. `./tools/run_msdfgen_oracle_gate.sh`
5. `just sdl3` (visual confirmation after any fix)

## Guardrails
- No per-glyph special handling.
- Keep changes small and reversible.
- Treat visual proof + harness parity as required before declaring done.
