# MSDF/MTSDF Parity Roadmap (msdfgen‑class)

This file defines **goals** and **phases** to reach parity with msdfgen/msdf-atlas-gen quality and behavior.
It is the single source of truth for scope, sequencing, and exit criteria.

## Goals

- Match msdfgen/MSDF‑Atlas‑Gen output quality for MSDF and MTSDF (including joins/crossbars, sharp corners, and stable alpha).
- Keep generation deterministic for a given font/config/seed.
- Preserve an in‑memory workflow: TTF → MSDF/MTSDF bytes without disk round‑trips.
- Provide a clean public API with explicit knobs that map to msdfgen concepts.

## Agent Execution Rules

- Never use a local Cabal directory setup in this repo.
- Hard prohibition: never create or use repo-local `.cabal` or `.cabal-logs`.
- Do not set `CABAL_DIR` or `CABAL_LOGDIR` (including one-off command prefixes).
- If Cabal execution needs any local-dir/logdir override or sandbox escape, stop and hand off to the user with the exact command to run manually.

## Pause Handoff (2026-02-08)

What the agent needs to proceed after pause:

1) Priority target for optimization:
   - choose one: `no-pack`, `pack`, or `balanced` (both).
2) Performance target for this machine:
   - provide desired `msdf-bench --glyphs 1024` wall-time target(s).
3) Tradeoff policy:
   - confirm whether small render/output drift is allowed for speed (`strict parity` vs `perf-first within tolerance`).

Current checkpoint (for comparison on resume):

- `cabal run msdf-bench -- --glyphs 512 +RTS -s`
  - no-pack: ~`7.4-7.5s`
  - pack: ~`7.0-7.1s`
- `cabal run msdf-bench -- --glyphs 1024 +RTS -s`
  - no-pack: ~`13.1s` (last known; refresh needed)
  - pack: ~`13.1s` (last known; refresh needed)

No extra manual setup is required to continue; the repo is ready for the next optimization pass.

## Research anchors (source of truth)

- **msdfgen README**: modes (`sdf`, `psdf`, `msdf`, `mtsdf`) and that **MTSDF stores true SDF in alpha**.
  https://github.com/Chlumsky/msdfgen
- **msdf-atlas-gen README**: generator options for corner angle, coloring strategy, error correction,
  overlap/scanline handling, preprocessing toggle, and seed.
  https://github.com/Chlumsky/msdf-atlas-gen
- **msdfgen release notes**:
  - v1.8: Skia preprocessing, additional error correction (bilinear prediction), pseudo-distance algorithm adjustments,
    overlap/scanline behavior changes.
  - v1.9–1.9.1: major error-correction rework + edge coloring improvements.
  - v1.12: edge coloring changes and further preprocessing fixes.
  https://github.com/Chlumsky/msdfgen/releases
- **Valve SDF paper**: baseline signed‑distance‑field rendering model the MSDF pipeline builds upon.
  https://steamcdn-a.akamaihd.net/apps/valve/2007/SIGGRAPH2007_AlphaTestedMagnification.pdf

## Phases (parity track)

### Phase 0 — Reference Baseline & Fixtures
- Generate reference outputs with msdfgen/msdf-atlas-gen for a fixed glyph set and seed.
- Lock down “golden” images/metrics for MSDF and MTSDF.
- Add regression tests that compare output **visually** (hash + tolerance) against reference.
**Status:** In progress — `tools/msdfgen_compare.sh` exists and basic regression tests exist, but reference outputs for intersection cases (f/m/a/d) are not locked yet.

### Phase 1 — Shape Normalization & Preprocess (msdfgen‑style)
- Implement `Shape::normalize` equivalent: contour cleanup, consistent winding, and explicit close.
- Add **optional preprocessing** to resolve self‑intersections and overlapping contours.
  - Match msdfgen’s behavior where preprocessing can be disabled (`-nopreprocess`).
  - Provide overlap/scanline fallback modes (`-overlap`, `-scanline`).
- Exit when complex glyphs (f/m/a/d) are consistent across preprocessing on/off.
**Status:** Partially done — contour cleanup + explicit close + orientation normalization + scanline sign mode are in place. **Missing:** intersection splitting + contour recomposition (self‑intersection fix).

### Phase 2 — Edge Coloring (corner‑aware)
- Implement msdfgen edge coloring **strategies** (`simple`, `inktrap`, `distance`).
- Respect angle threshold (`-angle`) and coloring seed (`-seed`).
- Enforce msdfgen constraints: edges at sharp corners share only one channel; smooth contours can be white.
- Exit when edge coloring matches msdfgen outputs for reference glyphs.
**Status:** Mostly done — simple/inktrap/distance strategies, smooth‑contour white, conflict resolution, CLI toggles. **Pending:** recolor after intersection splitting (post‑preprocess coloring must be applied to split edges).

### Phase 3 — Distance & Sign Field Correctness
- Accurate curve distance for line/quad/cubic (subdivision + analytic roots).
- Implement **pseudo/perpendicular distance** handling for corner endpoints (PSDF/MSDF).
- Implement fill rule + sign resolution that matches msdfgen, including scanline correction.
- Exit when alpha (true SDF) is free of crossbar/join breaks on reference glyphs.
**Status:** Partially done — exact/pseudo distance paths, scanline sign + fill rule, overlap filtering are in place. **Blocking:** crossbar/join artifacts persist because edges are not split at intersections.

### Phase 3.1 — Variable Font Delta Correctness
- Validate gvar tuple header parsing (packed format) and shared-point offsets.
- Ensure decoded deltas are within sane bounds (no multi-million unit bboxes).
- Composite glyphs: detect point-based vs component-based deltas; apply correctly.
- Add regression on Inter variable: `wght=900` glyphs (`i`, `j`, `z`, `e`, `:`) match msdfgen output.
**Status:** In progress — packed header parsing enforced; remaining composite detection + regression comparison pending.

### Phase 4 — MSDF/MTSDF Generation (true SDF in alpha)
- MSDF: median of RGB from edge‑colored channels.
- MTSDF: RGB = MSDF, **alpha = true SDF** (msdfgen semantics).
- Verify MTSDF supports sharp corners and true‑distance effects per msdf-atlas-gen table.
- Exit when MTSDF alpha matches reference SDF distances within tolerance.
**Status:** Done — alpha uses true distance; debug/SDL shaders treat alpha as SDF.

### Phase 5 — Error Correction (msdfgen rework parity)
- Implement msdfgen’s **error correction** pipeline:
  - bilinear interpolation artifact prediction (v1.8+)
  - reworked correction logic (v1.9+)
- Provide correction modes aligned with msdf-atlas-gen `-errorcorrection` option.
- Exit when speckle/edge artifacts are eliminated on the reference suite.
**Status:** Done — bilinear prediction + clash detection + thresholded correction (configurable).

### Phase 6 — Atlas & Metadata Parity
- Deterministic atlas packing with padding and PoT options.
- Align metadata output (UVs, layout) and scaling conventions with msdf-atlas-gen.
- Exit when atlas layout is stable across runs and matches reference metrics.
**Status:** Done — deterministic skyline packer, stable ordering, PoT controls, metadata outputs.

### Phase 7 — Performance + Determinism (re-opened)
Goal: close the performance gap vs msdfgen while preserving output quality.

#### Phase 7.1 — Bench Harness + Baseline
- Maintain msdf-bench/msdf-batch wall-clock timings (single atlas + batch).
- Keep wall vs CPU time reporting to avoid misreads under parallelism.
- Track msdfgen baseline (e.g., Inter variable font, 24px/28px, full glyph set).
**Status:** Done — msdf-batch exists, wall-time reporting added.

#### Phase 7.2 — Parallel Scheduling + Overhead Reduction
- Prefer dynamic worker pools over static chunking for glyph rendering.
- Avoid nested parallelism (atlas-level and per-glyph row workers).
- Gate per-glyph row parallelism by pixel count threshold.
**Status:** In progress — dynamic glyph worker pool added; row parallel threshold added.

#### Phase 7.3 — Render Buffer Data Layout
- Convert per-pixel distance buffers from `Double` to `Float` (or fixed-point).
- Keep geometric math in `Double`, store buffers in `Float` to reduce bandwidth.
- Preserve output parity; add a toggle if needed for strict regression tests.
**Status:** Done — distance buffers are `Float` while geometry math remains `Double`.

#### Phase 7.4 — Edge Index Construction
- Replace list-to-array builds in `buildEdgeIndexInfo` with mutable array fills.
- Reduce intermediate list allocation in edge/segment preprocessing.
- Migrate edge/segment collections to unboxed/storable vectors for cache-friendly scans.
**Status:** In progress — edge params/bboxes unboxed; edge masks + overlap flags unboxed; edge segments packed into SoA arrays with offsets/counts.

#### Phase 7.5 — Compact Auxiliary Structures
- Replace inside-mask `Bool` arrays with packed bitsets.
- Consider packed storage for edge attributes (single contiguous buffer).
- Reduce codepoint mapping overhead (avoid `Array Int [Int]` cons chains).
**Status:** Done — inside mask is a packed bitset; codepoint mapping uses compact offsets/counts.

#### Phase 7.6 — Verification + Guardrails
- For each optimization, run reference renders and msdfgen compare.
- Ensure determinism (seeded edge coloring, stable ordering).
**Status:** Pending.

#### Phase 7.7 — Data Structure Review (Performance Track)
Focus on layout to reduce GC churn and pointer chasing in hot paths.

- `EdgeIndexInfo`:
  - Move remaining boxed fields to SoA form (edge masks + overlaps + segments).
  - Replace `Array Int [LineSeg]` with SoA segment arrays (offsets/counts + coords).
- Inside mask:
  - Packed bitset with bit-test helper (no per-pixel `Word8`).
- Cell index:
  - Replace `UArray Int Int` with `PrimArray Int` or `Vector Int` to enable tight loops.
- Glyph mapping:
  - Replace `Array Int [Int]`/list chains in codepoint→glyph mapping with compact offsets/counts arrays.
- Distance buffers:
  - Consider `PrimArray Float` or `Storable` buffers for cache-friendly contiguous writes.
**Status:** Identified — implement incrementally with correctness checks.

**Exit when:** full-glyph Inter atlas at 24px is within ~2x msdfgen wall time and no visual regressions.

### Phase 8 — Docs, Examples, & Acceptance
- Update render guide and SDL example to match msdfgen semantics (alpha is true SDF).
- Provide “known‑good” reference renders for QA.
- Exit when parity tests are green and examples match reference screenshots.
**Status:** In progress — SDL example updated + CLI toggles; reference comparison script; docs updated. **Missing:** acceptance screenshots once intersection splitting is complete.

## Exit Criteria (Parity Definition)

- MSDF/MTSDF output visually matches msdfgen/msdf-atlas-gen on the reference suite.
- No join/crossbar artifacts in alpha (true SDF), especially `f`, `m`, `a`, `d` (requires intersection splitting).
- Deterministic outputs for identical inputs/config/seed.

---

# Variable Font (TTF) Support (parallel track)

1) Discovery + scope
   - Confirm required tables: fvar/avar/gvar (glyf), and whether HVAR/MVAR are in-scope for metrics.
   - Define the public API for axis selection (user-space values vs. normalized coordinates).

2) Table parsing
   - Parse fvar (axes + instances), avar (axis maps), and gvar (shared tuples + glyph variations).
   - If in-scope, parse HVAR/MVAR for metrics deltas.

3) Variation model + normalization
   - Implement user-space → normalized axis mapping, then apply avar mapping.
   - Define a single VariationLocation structure used by glyf/metrics.

4) Glyph deltas (gvar)
   - Apply gvar deltas to glyf outlines (including phantom points).
   - Handle composite glyphs and component transforms with deltas.

5) Metrics variation
   - Apply HVAR/MVAR deltas to advances/side bearings where present.
   - Fall back to base metrics when tables are missing.

6) MSDF integration
   - Thread VariationLocation through parseTTF/renderGlyphMSDF.
   - Cache per-glyph variation results to avoid recomputation.

7) Tests + validation
   - Add variable font fixtures and compare outlines across axis settings.
   - Add metrics regression tests for weight/width changes.

8) Docs + examples
   - Document axis selection, defaults, and limitations.
   - Add usage examples (e.g., Inter Variable at weight 400 vs 700).

---

# Resume Notes (2026-02-08)

## Current Regression
- SDL demo render is still noisy/jagged (e.g., “the” / “lazy dog”) even after snapshot updates.
- `msdfgen_compare.py` shows consistent distance magnitude mismatch vs msdfgen:
  - Mean diff ~0.09–0.11 for many glyphs at `range=16`.
  - Alpha and median are both off by a similar amount, so issue is not just channel mixing.
  - Best-fit scale factor varies ~1.1–1.3x depending on glyph.

## Debug Outputs (Already Generated)
- `out/debug_shots/` contains:
  - `alpha.png`, `median.png`, `normal.png`, `split.png`, `fill.png`, `r.png`, `g.png`, `b.png`.
  - `msdfgen_h/` diff images (`U+0068_our/ref/diff.png`).
  - `dump_h/trace.log` (pipeline trace).
- `out/msdfgen_compare/masdiff/` + `out/msdfgen_compare/msdfgen/` contain per-glyph dumps.
- `out/debug_shots/normal.png` vs `out/debug_shots/normal_nobadmap.png` show bad-map correction impact.

## Commands to Reproduce (Do Not Use CABAL_DIR/LOGDIR)
- Full debug bundle:
  - `just debug`
- Single glyph compare (h):
  - `cabal run msdf-dump -- --font assets/Inter/Inter-VariableFont_opsz,wght.ttf --text "h" --pixel-size 256 --range 16 --padding 16 --format mtsdf --out-dir out/msdfgen_compare/masdiff`
  - `python tools/msdfgen_compare.py --format mtsdf --codepoints 0068 --our-dir out/msdfgen_compare/masdiff --ref-dir out/msdfgen_compare/msdfgen --generate --match-masdiff --font assets/Inter/Inter-VariableFont_opsz,wght.ttf --range 16 --diff-dir out/debug_shots/msdfgen_h`
- SDL correction A/B (same text, same snapshot path):
  - `SDL_MSDF_SAMPLE_TEXT="the" MSDF_CORRECTION=1 MSDF_CORRECTION_BADMAP=0 just debug-sdl-correction-on`
  - `SDL_MSDF_SAMPLE_TEXT="the" MSDF_CORRECTION=0 MSDF_CORRECTION_BADMAP=0 just debug-sdl-correction-off`

## Next Investigation Steps
- Confirm whether seams are introduced by correction pipeline:
  - Compare `debug-sdl-correction-on` vs `debug-sdl-correction-off` screenshots.
  - If seams only exist with correction on, focus on `renderBitmap` correction subpasses (`computeCorr`, clash/hard fallback), not distance generation.
- Isolate the distance magnitude mismatch:
  - Compare a single edge’s distance field vs msdfgen along a normal sample line.
  - Verify unit conversion flow: glyph units → scaled contours → distance in pixel units → `distanceToByteF`.
  - Check whether flattening / edge indexing / pseudo-distance are biasing distances (not just signs).
- Add a small “distance probe” output (per-glyph or per-edge) to log a few sample distances vs msdfgen.
- Re-run `just debug` after any change and inspect `out/debug_shots/*diff.png`.

## Latest Finding (2026-02-09)
- Overlap filtering is currently a major source of visible sliver artifacts on problematic glyphs (`u`, `t` family).
- A/B compare (`--no-overlap`) reduces seam/sliver artifacts without introducing sign mismatches in current compares.
- SDL generator (`examples/sdl_gpu_wesl/Main.hs`) now defaults to overlap disabled; `--overlap` remains available for explicit opt-in/debug.

## Latest Finding (2026-02-09, render path)
- `MSDF_CORRECTION=1` vs `MSDF_CORRECTION=0` produced identical SDL snapshots for the current repro (`the`), so correction pass is not the active source of the seam.
- A likely render-stage cause is the fragment shader AA formula:
  - old: `alpha = smoothstep(-fwidth(dist), fwidth(dist), dist)`
  - new: `alpha = clamp(screenPxDistance + 0.5, 0.0, 1.0)` (standard MSDF formula)
- File changed: `examples/sdl_gpu_wesl/shaders/msdf.frag.wesl`.
- SDL runtime default now uses MSDF median (RGB) for MTSDF text rendering:
  - `SDL_MSDF_RENDER_ALPHA` default switched from `true` to `false` in `examples/sdl_gpu_wesl/sdl_gpu_msdf.c`.
  - Alpha SDF rendering remains available via `SDL_MSDF_RENDER_ALPHA=1`.
- Next verification command:
  - `SDL_MSDF_SAMPLE_TEXT="the" just demo-live-text-shot`
  - compare `examples/sdl_gpu_wesl/out/screenshot.png` to previous run.

## Latest Finding (2026-02-09, split path)
- Split-intersections remains a likely seam source in demo output.
- Demo/debug defaults were switched to no-split for stability:
  - `Justfile` default generation args now use `--no-split-intersections`.
  - SDL generator baseline config (`examples/sdl_gpu_wesl/Main.hs`) now sets `splitIntersections = False` unless explicitly overridden by CLI.
- Explicit split A/B remains available:
  - `just demo-live-text-shot-split`
  - `just demo-live-text-shot`
- Added guard attempt in splitter:
  - `splitEdgeAt` now receives a practical param epsilon in `splitContoursIntersections`:
    `max contourEpsilon (windingFlatness * 0.25)`
  - For sampled glyphs (`t,h,e,f,n`) this guard did not materially change outputs, so root split artifact cause is still unresolved.

## Latest Finding (2026-02-09, SDL guardrail)
- Added SDL seam guard script: `tools/sdl_seam_guard.py`.
- `tools/sdl_render_snapshot.sh` now runs seam guard in both `update` and `compare` modes.
  - Fails if suspicious tiny enclosed holes are detected in the live screenshot.
  - Writes debug overlay to `out/snapshots/render/demo_live_text.seam.png`.
  - Temporary bypass (debug only): `MSDF_ALLOW_SEAM=1`.
