# Spec gaps

This document tracks known gaps vs. the TrueType/OpenType specs.

## TrueType tables

- CFF/OTF outlines are not supported (glyf only).
- Variable fonts (glyf) are supported via fvar/avar/gvar, plus HVAR/VVAR/MVAR for metrics
  (advance/LSB/RSB, advanceHeight/TSB/BSB, and hhea/vhea ascent/descent/lineGap). Other MVAR tags
  are not implemented.
- Hinting instructions are ignored.

## Composite glyphs

- `WE_HAVE_INSTRUCTIONS` data is ignored.
- Overlap/compound flags are ignored.

## GPOS

- Pair Adjustment (type 2) plus MarkToBase (type 4) and MarkToMark (type 6) are supported
  (including Extension lookups type 9). Mark-to-ligature and contextual lookups are not.
- ValueRecord fields other than xAdvance are ignored.
- Anchor device/variation tables are ignored (static x/y only).

## MSDF

- Atlas packing is implemented with a skyline packer; no optimal packing or rotation.
- Overlap handling uses boundary filtering; no msdfgen-style preprocess/overlap resolution.
- Edge-coloring correction is heuristic; no full msdfgen conflict/repair pipeline.
