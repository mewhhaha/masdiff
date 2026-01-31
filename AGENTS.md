# Variable Font (TTF) Support Phases

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

# MSDF Parity Roadmap (Full Feature Track)

This roadmap outlines the work to reach feature parity with mature MSDF engines (e.g., msdfgen‑class quality). It is organized by phases, from foundational refactors through quality enhancements and packaging.

## Phase 0 — Baseline Stabilization
- Freeze current API behavior; document determinism guarantees.
- Add regression tests for gvar “no deltas” and malformed tables.
- Expand parsing guards so malformed fonts never crash (return empty contours/metrics).

## Phase 1 — Geometry and Contour Fidelity
- Implement full contour normalization:
  - Fix winding direction consistency (clockwise/counter‑clockwise).
  - Handle overlaps and self‑intersections robustly.
  - Ensure contours are closed and contiguous; remove duplicate points.
- Add robust curve flattening error bounds for MSDF sampling.

## Phase 2 — Edge Coloring and Conflict Resolution
- Implement advanced edge coloring (corner detection + edge‑color continuity).
- Add edge‑color conflict resolution:
  - Detect “nearby edges” and re‑color to avoid color ambiguity.
  - Integrate edge‑color correction pass (msdfgen‑style).
- Add tests with known corner cases (e.g., sharp joins, close parallel edges).

## Phase 3 — MSDF Quality Enhancements
- Implement error‑correction pass:
  - Fix artifacts from overlapping channels.
  - Median correction + per‑pixel reprojection.
- Support configurable pixel‑range normalization and distance smoothing.
- Add debug visualization outputs (optional feature flags).

## Phase 4 — Atlas Packing + Layout
- Add atlas packing (bin‑packing):
  - Shelf or skyline packer with padding and power‑of‑two options.
  - Output atlas‑level UVs for each glyph.
- Update `MSDFAtlas` schema to optionally include packed atlas metadata.
- Provide deterministic packing (stable glyph ordering).

## Phase 5 — Advanced Font Features
- Extend GPOS support beyond Pair Adjustment:
  - Class kerning / mark‑to‑base / mark‑to‑mark (as needed).
- Expand MVAR tags (not just hhea) and support VVAR.
- Add support for variable font composite deltas and component transforms.

## Phase 6 — Rendering and Integration
- Provide a “render guide” with shader expectations (GLSL + WESL + WGSL).
- Add optional MSDF normalization helpers (scale/range calculation utilities).
- Provide reference renderer sample (minimal).

## Phase 7 — Performance + Parallelism
- Optimize edge extraction and distance computation:
  - Spatial indexing for nearest‑edge queries.
  - SIMD‑friendly loops (where possible in Haskell).
- Add per‑glyph caching for repeated renders with different sizes.
- Make parallel rendering deterministic (stable chunking, no non‑deterministic folds).

## Phase 8 — Packaging & Release
- Formalize versioning policy and changelog discipline.
- Add public API stability notes.
- Add benchmarks for MSDF generation time and atlas packing.

## Exit Criteria (Parity Definition)
- Output visually comparable MSDF quality to msdfgen on a standard test suite.
- Handles common fonts (static + variable) without crashes or artifacts.
- Provides atlas packing + UVs suitable for real‑time rendering.
