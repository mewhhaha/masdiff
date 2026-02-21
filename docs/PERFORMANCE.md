# Performance Notes

## Baseline (2026-02-21)

Command (3 runs, identical settings):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'
```

Raw logs:

- `out/bench/baseline_n1_run1.log`
- `out/bench/baseline_n1_run2.log`
- `out/bench/baseline_n1_run3.log`

Metrics:

- `diffRGBA8 avg ms`: runs `[0.009027003750, 0.009832778750, 0.008680858750]`, median `0.009027003750`
- `generateGlyphIO/static avg ms`: runs `[15.876045000000, 19.817828083333, 18.034105041667]`, median `18.034105041667`
- `generateGlyphIO/variable avg ms`: runs `[17.094807916667, 19.762621416667, 18.172960375000]`, median `18.172960375000`

## Focused optimization pass (2026-02-21, same-session A/B)

Change under test (`lib/MSDF/Native/Raster.hs`):
- In `selectorAddEdge`, remove normalization from tangent-sum vectors used only for sign checks:
  - from `dotVec ... (normalizeAllowZero (addPt ...))`
  - to `dotVec ... (addPt ...)`

Rationale:
- The branch only checks `> 0.0`.
- `normalizeAllowZero` scales by a positive factor (or zero), so sign is preserved for this predicate.

Command (both control and optimized, 3 runs each):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'
```

Control logs (original expression):
- `out/bench/control_n1_run1.log`
- `out/bench/control_n1_run2.log`
- `out/bench/control_n1_run3.log`

Control medians:
- `diffRGBA8 avg ms`: median `0.008590975000`
- `generateGlyphIO/static avg ms`: median `19.865510416667`
- `generateGlyphIO/variable avg ms`: median `20.001561291667`

Optimized logs (sign-equivalent simplification):
- `out/bench/optimized3_n1_run1.log`
- `out/bench/optimized3_n1_run2.log`
- `out/bench/optimized3_n1_run3.log`

Optimized medians:
- `diffRGBA8 avg ms`: median `0.008593087500`
- `generateGlyphIO/static avg ms`: median `19.024293833333`
- `generateGlyphIO/variable avg ms`: median `19.768974208333`

Delta (optimized vs control):
- `diffRGBA8 avg ms`: `+0.000002112500` (`+0.025%`, effectively flat)
- `generateGlyphIO/static avg ms`: `-0.841216583334` (`-4.235%`)
- `generateGlyphIO/variable avg ms`: `-0.232587083334` (`-1.163%`)

Verification gates after change:
- `cabal test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Strict failures: 0`, `Exact mismatches: 0`

## Focused optimization pass #2 (2026-02-21, same-session A/B)

Change under test (`lib/MSDF/Native/Raster.hs`):
- In `lessSignedDist`, hoist repeated `abs` calls:
  - from repeated `abs a.distance` / `abs b.distance`
  - to precomputed `absA` / `absB`

Command (both variants, 3 runs each):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'
```

Control logs (without abs hoist):
- `out/bench/noles_n1_run1.log`
- `out/bench/noles_n1_run2.log`
- `out/bench/noles_n1_run3.log`

Control medians:
- `diffRGBA8 avg ms`: median `0.008598908750`
- `generateGlyphIO/static avg ms`: median `18.165380750000`
- `generateGlyphIO/variable avg ms`: median `18.463816458333`

Optimized logs (with abs hoist):
- `out/bench/optimized4_n1_run1.log`
- `out/bench/optimized4_n1_run2.log`
- `out/bench/optimized4_n1_run3.log`

Optimized medians:
- `diffRGBA8 avg ms`: median `0.008881815000`
- `generateGlyphIO/static avg ms`: median `17.336303541667`
- `generateGlyphIO/variable avg ms`: median `18.002378625000`

Delta (optimized vs control):
- `diffRGBA8 avg ms`: `+0.000282906250` (`+3.290%`)
- `generateGlyphIO/static avg ms`: `-0.829077208333` (`-4.564%`)
- `generateGlyphIO/variable avg ms`: `-0.461437833333` (`-2.499%`)

Verification gates after change:
- `cabal test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Strict failures: 0`, `Exact mismatches: 0`
