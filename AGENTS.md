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
