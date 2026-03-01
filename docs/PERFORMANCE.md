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

## Focused optimization pass #3 (2026-02-21, same-session A/B)

Changes under test (`lib/MSDF/Native/Raster.hs`):
- `applyAmbiguousSignFix`:
  - switched from dense `IntMap` lookups to dense array indexing,
  - replaced list-comprehension neighbor accumulation with direct cardinal checks.
- `applyModernErrorCorrection` first pass:
  - switched read-only pixel lookups used by `protectEdgesModern`, `findErrorsModern`, and `applyStencilCorrection` from `IntMap` lookups to dense array lookup.
  - stencil map logic remained unchanged.

Command (3 runs, `-N1`):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'
```

Pass #3a logs (after ambiguous-fix dense lookup, before modern-pass lookup switch):
- `out/bench/focused_dense_n1_run1.log`
- `out/bench/focused_dense_n1_run2.log`
- `out/bench/focused_dense_n1_run3.log`

Pass #3a medians:
- `diffRGBA8 avg ms`: median `0.009125612500`
- `generateGlyphIO/static avg ms`: median `15.990467083333`
- `generateGlyphIO/variable avg ms`: median `16.649449375000`

Pass #3b logs (final: modern-pass dense lookup included):
- `out/bench/focused_dense_array_n1_run1.log`
- `out/bench/focused_dense_array_n1_run2.log`
- `out/bench/focused_dense_array_n1_run3.log`

Pass #3b medians:
- `diffRGBA8 avg ms`: median `0.008641255000`
- `generateGlyphIO/static avg ms`: median `14.696306083333`
- `generateGlyphIO/variable avg ms`: median `14.836043208333`

Delta (pass #3b vs pass #3a):
- `diffRGBA8 avg ms`: `-0.000484357500` (`-5.308%`)
- `generateGlyphIO/static avg ms`: `-1.294161000000` (`-8.093%`)
- `generateGlyphIO/variable avg ms`: `-1.813406166667` (`-10.892%`)

Verification gates after pass #3:
- `cabal test masdiff-test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Checked cases: 288`, `Strict failures: 0`, `Exact mismatches: 0`

## Focused optimization pass #4 (2026-02-21, same-session A/B)

Changes under test (`lib/MSDF/Native/Raster.hs`):
- Added `ContourTriple` precomputation in `contourSelectorInput`:
  - precomputes channel mask booleans (`hasR`/`hasG`/`hasB`),
  - precomputes normalized tangent-derived vectors (`aBlend`, `bBlend`, `negADir`, `bDir`) once per contour edge triple.
- Updated `distanceForContour` hot loop:
  - reuses precomputed triple data per pixel,
  - computes perpendicular candidates (`negPd`, `posPd`) once per edge-distance sample and reuses them across selected channels.
- Simplified `selectorAddEdgePrepared` to consume precomputed perpendicular candidates.

Command (3 runs, `-N1`):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'
```

Baseline logs (pre-change):
- `out/bench/focused_pass4_base_n1_run1.log`
- `out/bench/focused_pass4_base_n1_run2.log`
- `out/bench/focused_pass4_base_n1_run3.log`

Baseline medians:
- `diffRGBA8 avg ms`: median `0.008939721250`
- `generateGlyphIO/static avg ms`: median `14.953525125000`
- `generateGlyphIO/variable avg ms`: median `15.145552333333`

Optimized logs (this pass):
- `out/bench/focused_pass5_n1_run1.log`
- `out/bench/focused_pass5_n1_run2.log`
- `out/bench/focused_pass5_n1_run3.log`

Optimized medians:
- `diffRGBA8 avg ms`: median `0.008993138750`
- `generateGlyphIO/static avg ms`: median `11.688292958333`
- `generateGlyphIO/variable avg ms`: median `11.860360833333`

Delta (optimized vs baseline):
- `diffRGBA8 avg ms`: `+0.000053417500` (`+0.598%`)
- `generateGlyphIO/static avg ms`: `-3.265232166667` (`-21.836%`)
- `generateGlyphIO/variable avg ms`: `-3.285191500000` (`-21.691%`)

Verification gates after pass #4:
- `cabal test masdiff-test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Checked cases: 288`, `Strict failures: 0`, `Exact mismatches: 0`, `Worst max abs diff: 0`

## Focused optimization pass #5 (2026-02-22, candidate rejected)

Candidate under test (`lib/MSDF/Native/Raster.hs`):
- replaced temporary `Pt` values (`ap`, `bp`) in `distanceForContour.step` with scalar `x/y` arithmetic and a scalar perpendicular helper.

Reason for rejection:
- two confirmation sweeps did not show a stable median win across all tracked metrics, so the change was reverted.

Sweep A logs:
- `out/bench/focused_pass6_n1_run1.log`
- `out/bench/focused_pass6_n1_run2.log`
- `out/bench/focused_pass6_n1_run3.log`

Sweep A medians:
- `diffRGBA8 avg ms`: median `0.009385178750`
- `generateGlyphIO/static avg ms`: median `12.096337958333`
- `generateGlyphIO/variable avg ms`: median `12.397015666667`

Sweep B logs:
- `out/bench/focused_pass6b_n1_run1.log`
- `out/bench/focused_pass6b_n1_run2.log`
- `out/bench/focused_pass6b_n1_run3.log`

Sweep B medians:
- `diffRGBA8 avg ms`: median `0.009166068750`
- `generateGlyphIO/static avg ms`: median `11.531585083333`
- `generateGlyphIO/variable avg ms`: median `11.803168458333`

Post-revert confirmation logs:
- `out/bench/focused_pass7_n1_run1.log`
- `out/bench/focused_pass7_n1_run2.log`
- `out/bench/focused_pass7_n1_run3.log`

Post-revert medians:
- `diffRGBA8 avg ms`: median `0.008688585000`
- `generateGlyphIO/static avg ms`: median `11.795343625000`
- `generateGlyphIO/variable avg ms`: median `12.051926666667`

Verification gates after rollback:
- `cabal test masdiff-test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Checked cases: 288`, `Strict failures: 0`, `Exact mismatches: 0`, `Worst max abs diff: 0`

## Focused optimization pass #7 (2026-02-22, kept)

Change under test (`lib/MSDF/Native/Raster.hs`):
- In `findErrorsModern`, hoisted `BaseArtifactClassifier` construction in each pixel once per contour family:
  - from repeated `BaseArtifactClassifier ...` expressions to `hClassifier`, `vClassifier`, `dClassifier`.
- In `findErrorsModernDistanceAware`, reused the same classifier strategy and preloaded diagonal samples (`leftUp`, `rightUp`, `leftDown`, `rightDown`) into locals before candidate checks.

Command (3 runs, `-N1`):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'
```

Logs:
- `out/bench/pass7_n1_run1.log`
- `out/bench/pass7_n1_run2.log`
- `out/bench/pass7_n1_run3.log`

Medians:
- `diffRGBA8 avg ms`: `0.008694218750`
- `generateGlyphIO/static avg ms`: `11.716919583333`
- `generateGlyphIO/variable avg ms`: `11.855357000000`
- `bytes allocated in the heap`: `3,898,246,952`

## Profiling snapshot (2026-02-22)

Commands:

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --enable-profiling --benchmark-options='+RTS -N1 -p -s -RTS'

CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --enable-profiling --ghc-options='-fprof-auto' --benchmark-options='+RTS -N1 -p -s -RTS'
```

Profile artifacts:
- `out/bench/pass7_profile.log`
- `out/bench/pass7_profile_auto.log`
- `masdiff-bench.prof`

Top cost centers from `masdiff-bench.prof` (`-fprof-auto`):
- `distanceForContour.step`: `16.7% time`, `20.7% alloc`
- `signedDistanceQuad`: `6.2% time`, `6.2% alloc`
- `signedDistanceLine`: `5.9% time`, `7.1% alloc`
- `dotVec`: `5.5% time`, `10.2% alloc`
- `diffPt`: `4.9% time`, `1.1% alloc`
- `lengthVec`: `3.2% time`, `2.7% alloc`
- `hasDiagonalArtifact`: `3.2% time`, `2.9% alloc`

## Focused optimization pass #8 (2026-02-22, rejected)

Candidate under test (`lib/MSDF/Native/Raster.hs`):
- rewired `distanceForContour.step`/`selectorAddEdgePrepared` to avoid `Maybe` plumbing for perpendicular updates.

Logs:
- `out/bench/pass8_n1_run1.log`
- `out/bench/pass8_n1_run2.log`
- `out/bench/pass8_n1_run3.log`

Candidate medians:
- `diffRGBA8 avg ms`: `0.009037146250`
- `generateGlyphIO/static avg ms`: `13.765392708333`
- `generateGlyphIO/variable avg ms`: `13.914951375000`
- `bytes allocated in the heap`: `4,818,033,128`

Delta vs pass #7 baseline:
- `diffRGBA8 avg ms`: `+0.000342927500` (`+3.944%`)
- `generateGlyphIO/static avg ms`: `+2.048473124999` (`+17.482%`)
- `generateGlyphIO/variable avg ms`: `+2.059594375000` (`+17.372%`)
- `bytes allocated in the heap`: `+919,786,176` (`+23.594%`)

Result:
- Reverted.

## Focused optimization pass #9 (2026-02-22, rejected)

Candidate under test (`lib/MSDF/Native/Raster.hs`):
- scalarized parts of `signedDistanceLine` and normalized-dot usage in `signedDistanceQuad`.

Logs:
- `out/bench/pass9_n1_run1.log`
- `out/bench/pass9_n1_run2.log`
- `out/bench/pass9_n1_run3.log`

Candidate medians:
- `diffRGBA8 avg ms`: `0.008922567500`
- `generateGlyphIO/static avg ms`: `11.949626583333`
- `generateGlyphIO/variable avg ms`: `12.187280166667`
- `bytes allocated in the heap`: `4,005,761,184`

Delta vs pass #7 baseline:
- `diffRGBA8 avg ms`: `+0.000228348750` (`+2.626%`)
- `generateGlyphIO/static avg ms`: `+0.232706999999` (`+1.986%`)
- `generateGlyphIO/variable avg ms`: `+0.331923166667` (`+2.800%`)
- `bytes allocated in the heap`: `+107,514,232` (`+2.757%`)

Result:
- Reverted.

## Focused optimization pass #10 (2026-02-22, rejected)

Candidate under test (`lib/MSDF/Native/Types.hs`, `lib/MSDF/Native/Raster.hs`):
- added `UNPACK` annotations to hot `Double` fields (`Pt`, `Frame`, `Pixel`, `SignedDist`, `Samples`, `DistanceValue`).

Logs:
- `out/bench/pass10_n1_run1.log`
- `out/bench/pass10_n1_run2.log`
- `out/bench/pass10_n1_run3.log`

Candidate medians:
- `diffRGBA8 avg ms`: `0.008479248750`
- `generateGlyphIO/static avg ms`: `11.830525500000`
- `generateGlyphIO/variable avg ms`: `11.944847833333`
- `bytes allocated in the heap`: `3,898,246,864`

Delta vs pass #7 baseline:
- `diffRGBA8 avg ms`: `-0.000214970000` (`-2.473%`)
- `generateGlyphIO/static avg ms`: `+0.113605916667` (`+0.970%`)
- `generateGlyphIO/variable avg ms`: `+0.089490833333` (`+0.755%`)
- `bytes allocated in the heap`: `-88` (`-0.000%`)

Result:
- Reverted (mixed signal with slower generation medians).

Verification gates on current state:
- `cabal test masdiff-test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Checked cases: 288`, `Strict failures: 0`, `Exact mismatches: 0`, `Worst max abs diff: 0`

## Focused optimization pass #11 (2026-02-22, parallelism exploration)

### Pass #11d (kept, conservative parallel path)

Change under test (`lib/MSDF/Native/Raster.hs`):
- Added non-invasive row-level parallel sampling helpers using base-only primitives:
  - `parMapChunked` (`GHC.Conc.par` / `GHC.Conc.pseq`)
- Split sampling into explicit paths:
  - `rawPixelsSequential` (source-of-truth sequential path)
  - `rawPixelsParallel` (chunked row path)
- Runtime gate:
  - `useParallelRows = numCapabilities > 1 && dim >= 192`

Rationale:
- Keep parallelism available for larger workloads while avoiding overhead on small/medium glyph sizes.
- Preserve a simple sequential fallback.

Control baseline (same-session, forced-off gate for A/B):
- `out/bench/pass11c_control_n1_run1.log`
- `out/bench/pass11c_control_n1_run2.log`
- `out/bench/pass11c_control_n1_run3.log`

Control medians (`-N1`):
- `diffRGBA8 avg ms`: `0.009479212500`
- `generateGlyphIO/static avg ms`: `12.164067291667`
- `generateGlyphIO/variable avg ms`: `12.165373000000`
- `bytes allocated in the heap`: `3,898,243,856`

Kept logs:
- `out/bench/pass11d_n1_run1.log`
- `out/bench/pass11d_n1_run2.log`
- `out/bench/pass11d_n1_run3.log`
- `out/bench/pass11d_n_run1.log`
- `out/bench/pass11d_n_run2.log`
- `out/bench/pass11d_n_run3.log`

Kept medians:
- `-N1`:
  - `diffRGBA8 avg ms`: `0.008702566250`
  - `generateGlyphIO/static avg ms`: `11.964340416667`
  - `generateGlyphIO/variable avg ms`: `12.003776583333`
  - `bytes allocated in the heap`: `3,958,779,928`
- `-N`:
  - `diffRGBA8 avg ms`: `0.009594706250`
  - `generateGlyphIO/static avg ms`: `13.904574041667`
  - `generateGlyphIO/variable avg ms`: `14.450213125000`
  - `bytes allocated in the heap`: `3,959,032,184`

Delta (`pass11d -N1` vs same-session control `-N1`):
- `diffRGBA8 avg ms`: `-0.000776646250` (`-8.193%`)
- `generateGlyphIO/static avg ms`: `-0.199726875000` (`-1.642%`)
- `generateGlyphIO/variable avg ms`: `-0.161596416667` (`-1.328%`)
- `bytes allocated in the heap`: `+60,536,072` (`+1.553%`)

Observation:
- Under current benchmark workload, `-N` runs show `SPARKS: 0`; the conservative gate does not activate the row-parallel path.

### Pass #11e (rejected, aggressive parallel gate)

Candidate under test (`lib/MSDF/Native/Raster.hs`):
- Lowered parallel gate to `dim >= 64` to activate row parallelism in current benchmark workload.

Logs:
- `out/bench/pass11e_n_run1.log`
- `out/bench/pass11e_n_run2.log`
- `out/bench/pass11e_n_run3.log`

Candidate medians (`-N`):
- `diffRGBA8 avg ms`: `0.009596045000`
- `generateGlyphIO/static avg ms`: `14.439048666667`
- `generateGlyphIO/variable avg ms`: `14.921243041667`
- `bytes allocated in the heap`: `4,015,324,304`

Delta vs kept `pass11d -N`:
- `diffRGBA8 avg ms`: `+0.000001338750` (`+0.014%`)
- `generateGlyphIO/static avg ms`: `+0.534474625000` (`+3.844%`)
- `generateGlyphIO/variable avg ms`: `+0.471029916667` (`+3.260%`)
- `bytes allocated in the heap`: `+56,292,120` (`+1.422%`)

Result:
- Rejected and reverted to conservative gate (`dim >= 192`).

Final verification gates after pass #11:
- `cabal test masdiff-test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Checked cases: 288`, `Strict failures: 0`, `Exact mismatches: 0`, `Worst max abs diff: 0`

## Focused optimization pass #12 (2026-02-22, glyph-batch API)

Change under test:
- Added library-level bounded batch API in `lib/MSDF/Generate.hs`:
  - `generateGlyphBatchIO :: RuntimeCfg -> Int -> GenCfg -> FontSrc -> [GlyphCode] -> IO [Either GenErr GenOut]`
  - implementation uses base-only bounded workers (`forkFinally` + `QSem`), preserving input order.
- Added batch benchmark cases in `bench/Main.hs`:
  - `generateGlyphBatchIO/static/jobs1`
  - `generateGlyphBatchIO/static/jobs16`
- Added batch behavior tests in `test/Main.hs`:
  - jobs=1 equals sequential,
  - jobs=4 equals sequential,
  - missing font returns `MissingInput`.

Benchmark command (`3` runs each for `-N1` and `-N`):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_BATCH_ITERS=6 \
BENCH_BATCH_JOBS=16 \
BENCH_WARMUP_ITERS=1 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'

CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_BATCH_ITERS=6 \
BENCH_BATCH_JOBS=16 \
BENCH_WARMUP_ITERS=1 \
cabal bench masdiff-bench --benchmark-options='+RTS -N -s -RTS'
```

Logs:
- `out/bench/pass12_n1_run1.log`
- `out/bench/pass12_n1_run2.log`
- `out/bench/pass12_n1_run3.log`
- `out/bench/pass12_n_run1.log`
- `out/bench/pass12_n_run2.log`
- `out/bench/pass12_n_run3.log`

Medians:
- `-N1`:
  - `diffRGBA8 avg ms`: `0.009329681250`
  - `generateGlyphIO/static avg ms`: `12.005909666667`
  - `generateGlyphIO/variable avg ms`: `12.093369375000`
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `480.948765500000`
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `484.365052500000`
- `-N`:
  - `diffRGBA8 avg ms`: `0.009802061250`
  - `generateGlyphIO/static avg ms`: `14.076012916667`
  - `generateGlyphIO/variable avg ms`: `14.391368458333`
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `610.236721333333`
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `601.294745166667`

Batch delta (`jobs16` vs `jobs1`):
- `-N1`: `+3.416287000000` (`+0.710%`) (expected overhead)
- `-N`: `-8.941976166667` (`-1.465%`) (small throughput win)

Notes:
- Heap-allocation totals are not directly comparable to pre-pass runs because benchmark workload now includes additional batch loops.
- Existing single-glyph metrics remain in the benchmark output for continuity.

Final verification gates after pass #12:
- `cabal test masdiff-test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Checked cases: 288`, `Strict failures: 0`, `Exact mismatches: 0`, `Worst max abs diff: 0`

## Focused optimization pass #13 (2026-02-22, baseline refresh)

Command (`3` runs each for `-N1` and `-N`):

```bash
CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_BATCH_ITERS=6 \
BENCH_BATCH_JOBS=16 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'

CABAL_DIR="$PWD/.cabal" \
CABAL_LOGDIR="$PWD/.cabal-logs" \
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_BATCH_ITERS=6 \
BENCH_BATCH_JOBS=16 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N -s -RTS'
```

Logs:
- `out/bench/pass13_n1_run1.log`
- `out/bench/pass13_n1_run2.log`
- `out/bench/pass13_n1_run3.log`
- `out/bench/pass13_n_run1.log`
- `out/bench/pass13_n_run2.log`
- `out/bench/pass13_n_run3.log`

Medians:
- `-N1`:
  - `diffRGBA8 avg ms`: `0.009193788750`
  - `generateGlyphIO/static avg ms`: `12.046861583333`
  - `generateGlyphIO/variable avg ms`: `12.074900208333`
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `475.057183666667`
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `478.865315999999`
- `-N`:
  - `diffRGBA8 avg ms`: `0.010562507500`
  - `generateGlyphIO/static avg ms`: `14.220391999999`
  - `generateGlyphIO/variable avg ms`: `14.503061583333`
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `609.976218500000`
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `599.617776500000`

## Focused optimization pass #14 (2026-02-22, kept)

Change under test:
- Added native batch outline loading in `lib/MSDF/Native/TTF.hs`:
  - `loadOutlinesIO` parses OpenType/TrueType tables once per batch and decodes each requested glyph outline from the parsed font.
- Added native batch generation in `lib/MSDF/Native.hs`:
  - `generateGlyphBatchNativeIO` renders outlines from `loadOutlinesIO`.
  - uses bounded worker concurrency for rendering when `jobs > 1` and runtime has multiple capabilities.
- Routed native `generateGlyphBatchIO` through this path in `lib/MSDF/Generate.hs`.
- Added variable-font batch regression check in `test/Main.hs`.

Primary logs (final kept implementation):
- `out/bench/pass14c_n1_run1.log`
- `out/bench/pass14c_n1_run2.log`
- `out/bench/pass14c_n1_run3.log`
- `out/bench/pass14c_n_run1.log`
- `out/bench/pass14c_n_run2.log`
- `out/bench/pass14c_n_run3.log`

Additional `-N1` confirmation sweep logs:
- `out/bench/pass14b_n1_run1.log`
- `out/bench/pass14b_n1_run2.log`
- `out/bench/pass14b_n1_run3.log`

Medians (kept pass `pass14c`):
- `-N1`:
  - `diffRGBA8 avg ms`: `0.008505453750`
  - `generateGlyphIO/static avg ms`: `11.888276208333`
  - `generateGlyphIO/variable avg ms`: `12.229657250000`
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `483.869740833333`
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `484.153752500000`
- `-N`:
  - `diffRGBA8 avg ms`: `0.009905670000`
  - `generateGlyphIO/static avg ms`: `14.139588083333`
  - `generateGlyphIO/variable avg ms`: `14.464256791667`
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `604.514015666667`
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `112.657375666667`

Delta (`pass14c` vs `pass13`):
- `-N1`:
  - `diffRGBA8 avg ms`: `-0.000688335000` (`-7.487%`)
  - `generateGlyphIO/static avg ms`: `-0.158585375000` (`-1.316%`)
  - `generateGlyphIO/variable avg ms`: `+0.154757041667` (`+1.282%`)
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `+8.812557166667` (`+1.855%`)
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `+5.288436500000` (`+1.104%`)
- `-N`:
  - `diffRGBA8 avg ms`: `-0.000656837500` (`-6.219%`)
  - `generateGlyphIO/static avg ms`: `-0.080803916667` (`-0.568%`)
  - `generateGlyphIO/variable avg ms`: `-0.038804791666` (`-0.268%`)
  - `generateGlyphBatchIO/static/jobs1 avg ms`: `-5.462202833333` (`-0.895%`)
  - `generateGlyphBatchIO/static/jobs16 avg ms`: `-486.960400833333` (`-81.212%`)

Notes:
- Multicore batch throughput (`jobs16` under `-N`) improves substantially due the native parse-once path plus bounded batch rendering workers.
- Single-capability (`-N1`) batch timings are noisy and do not show a stable throughput win; this is expected because the parallel path is targeted at multicore throughput.

Final verification gates after pass #14:
- `cabal test masdiff-test`: pass
- `cabal run masdiff-parity -- --require-exact`: `Checked cases: 288`, `Strict failures: 0`, `Exact mismatches: 0`, `Worst max abs diff: 0`

Post-pass safety hardening (same implementation family):
- Hardened bounded-worker helpers against async-exception semaphore leaks and ensured all worker results are drained before rethrowing.
- Smoke benchmark logs:
  - `out/bench/pass14d_n1_smoke.log`
  - `out/bench/pass14d_n_smoke.log`
- Smoke benchmark key points:
  - `-N1`: `generateGlyphBatchIO/static/jobs1 avg ms = 463.958428500000`, `jobs16 avg ms = 464.306697166667`
  - `-N`: `generateGlyphBatchIO/static/jobs1 avg ms = 580.250341999999`, `jobs16 avg ms = 107.031718500000`

## Focused optimization pass #15 (2026-03-01, rejected)

Baseline command (`-N1`):

```bash
BENCH_DIFF_ITERS=800 \
BENCH_GEN_ITERS=24 \
BENCH_BATCH_ITERS=12 \
BENCH_WARMUP_ITERS=2 \
cabal bench masdiff-bench --benchmark-options='+RTS -N1 -s -RTS'
```

Baseline snapshot:
- `generateGlyphIO/static avg ms`: `12.614296250000`
- `generateGlyphIO/variable avg ms`: `12.794172166667`
- `generateGlyphBatchIO/static/jobs1 avg ms`: `521.669034666667`
- `bytes allocated in the heap`: `51,083,393,800`
- `MUT time`: `6.683s`
- `GC time`: `1.242s`
- `Total time`: `7.926s`

Candidate A (`lib/MSDF/Native/Raster.hs`, rejected):
- replaced stencil flag updates with `IntMap.insertWith (.|.)` and reused loaded stencil values in error scans.

Candidate A snapshot:
- `generateGlyphIO/static avg ms`: `13.062024375000`
- `generateGlyphIO/variable avg ms`: `13.362410583333`
- `generateGlyphBatchIO/static/jobs1 avg ms`: `535.400028666667`
- `bytes allocated in the heap`: `51,117,116,744`
- `MUT time`: `6.867s`
- `GC time`: `1.238s`
- `Total time`: `8.106s`

Candidate B (`lib/MSDF/Native/Raster.hs`, rejected):
- rewrote `protectEdgesModern` tuple-list folds into strict nested loops.

Candidate B snapshot:
- `generateGlyphIO/static avg ms`: `13.431717791667`
- `generateGlyphIO/variable avg ms`: `13.259338125000`
- `generateGlyphBatchIO/static/jobs1 avg ms`: `527.010034416667`
- `bytes allocated in the heap`: `51,174,615,264`
- `MUT time`: `6.788s`
- `GC time`: `1.249s`
- `Total time`: `8.038s`

Result:
- Both candidates regressed against baseline and were reverted.
- No `Raster.hs` performance change was kept in this pass.
