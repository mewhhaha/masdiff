# Spec gaps

This document tracks known gaps vs. the TrueType/OpenType specs.

## TrueType tables

- CFF/OTF outlines are not supported (glyf only).
- Variable font axes are ignored (default instance only).
- Hinting instructions are ignored.

## Composite glyphs

- `WE_HAVE_INSTRUCTIONS` data is ignored.
- Overlap/compound flags are ignored.

## GPOS

- Only Pair Adjustment lookups (type 2) and Extension lookups (type 9) are supported.
- ValueRecord fields other than xAdvance are ignored.

## MSDF

- Output is per-glyph; no atlas packing.
- No edge-coloring correction/median fix beyond per-channel distances.
