# Revision history for masdiff

## Unreleased

* (No changes yet.)

## 0.2.0.1 -- 2026-01-31

* Added GPOS mark-to-base/mark-to-mark parsing and exposed anchors in `MSDFAtlas`.
* Applied gvar component deltas for composite glyph offsets and phantom points.
* Added an edge spatial index for faster MSDF distance queries.
* Added render guide and versioning policy docs.
* Added per-glyph outline caching APIs for repeated renders.
* Added `msdf-bench` for basic generation/packing timing.
* Fixed glyf repeat-flag parsing offset to avoid misaligned coordinates.
* Added repeat-flag regression coverage for simple glyph parsing.

## 0.2.0.0 -- 2026-01-30

* Hardened variable font parsing (gvar tuple bounds + zero-delta handling).
* Deterministic cmap mapping (dedupe by codepoint, smallest glyph index).
* Added atlas packing with glyph placements and UVs.
* Added MSDF error correction pass and edge-color conflict mitigation.
* Added API overview docs and expanded regression tests (parallelism + gvar safety).
* Added vertical metrics (vhea/vmtx) and VVAR/MVAR vhea deltas.
* Fixed glyf flag-repeat parsing for zero-count repeats.
* Added MSDF.Render helpers for quad/UV and pixel range.

## 0.1.0.0 -- 2026-01-30

* First version. Released on an unsuspecting world.
