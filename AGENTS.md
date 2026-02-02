# MSDF/MTSDF Parity Roadmap (msdfgen‑class)

This file defines **goals** and **phases** to reach parity with msdfgen/msdf-atlas-gen quality and behavior.
It is the single source of truth for scope, sequencing, and exit criteria.

## Goals

- Match msdfgen/MSDF‑Atlas‑Gen output quality for MSDF and MTSDF (including joins/crossbars, sharp corners, and stable alpha).
- Keep generation deterministic for a given font/config/seed.
- Preserve an in‑memory workflow: TTF → MSDF/MTSDF bytes without disk round‑trips.
- Provide a clean public API with explicit knobs that map to msdfgen concepts.

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
**Status:** Done — optional `tools/msdfgen_compare.sh` provides msdfgen side‑by‑side output; internal hash tests + speckle tests cover regression safety.

### Phase 1 — Shape Normalization & Preprocess (msdfgen‑style)
- Implement `Shape::normalize` equivalent: contour cleanup, consistent winding, and explicit close.
- Add **optional preprocessing** to resolve self‑intersections and overlapping contours.
  - Match msdfgen’s behavior where preprocessing can be disabled (`-nopreprocess`).
  - Provide overlap/scanline fallback modes (`-overlap`, `-scanline`).
- Exit when complex glyphs (f/m/a/d) are consistent across preprocessing on/off.
**Status:** Done — contour cleanup + explicit close + orientation normalization + overlap boundary filtering + scanline sign mode + overlap toggles.

### Phase 2 — Edge Coloring (corner‑aware)
- Implement msdfgen edge coloring **strategies** (`simple`, `inktrap`, `distance`).
- Respect angle threshold (`-angle`) and coloring seed (`-seed`).
- Enforce msdfgen constraints: edges at sharp corners share only one channel; smooth contours can be white.
- Exit when edge coloring matches msdfgen outputs for reference glyphs.
**Status:** Done — simple/inktrap/distance strategies, smooth‑contour white, multi‑channel colors, conflict resolution, CLI toggles.

### Phase 3 — Distance & Sign Field Correctness
- Accurate curve distance for line/quad/cubic (subdivision + analytic roots).
- Implement **pseudo/perpendicular distance** handling for corner endpoints (PSDF/MSDF).
- Implement fill rule + sign resolution that matches msdfgen, including scanline correction.
- Exit when alpha (true SDF) is free of crossbar/join breaks on reference glyphs.
**Status:** Done — exact/pseudo distance paths, scanline sign + fill rule, overlap filtering.

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

### Phase 7 — Performance + Determinism
- Add spatial indexing + caching where it does not alter output.
- Ensure multithreaded execution is deterministic (stable ordering + seeded coloring).
- Exit when performance matches targets without visual regressions.
**Status:** Done — edge grid indexing, glyph cache, deterministic ordering + tests.

### Phase 8 — Docs, Examples, & Acceptance
- Update render guide and SDL example to match msdfgen semantics (alpha is true SDF).
- Provide “known‑good” reference renders for QA.
- Exit when parity tests are green and examples match reference screenshots.
**Status:** Done — SDL example updated + CLI toggles; reference comparison script; docs updated.

## Exit Criteria (Parity Definition)

- MSDF/MTSDF output visually matches msdfgen/msdf-atlas-gen on the reference suite.
- No join/crossbar artifacts in alpha (true SDF), especially `f`, `m`, `a`, `d`.
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
