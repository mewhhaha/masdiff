# Haskell Engineering Guide (Skill Style)

Use this guide for code in this repo. Optimize for:
- correctness by construction,
- pure core logic,
- minimal API surface,
- measurable performance,
- simple data and predictable runtime behavior.

## 1) Push IO To The Boundary

Keep parsing, shaping, and math pure. Do file/process/env work in thin boundary functions.

Good:
```haskell
readConfig :: IO Config
readConfig = do
  raw <- readFile "config.json"
  case decodeConfig raw of
    Left err -> fail err
    Right cfg -> pure cfg

decodeConfig :: String -> Either String Config
decodeConfig = ...
```

Bad:
```haskell
decodeConfig :: IO Config
decodeConfig = do
  raw <- readFile "config.json"
  ...
```

## 2) Make Illegal States Unrepresentable

Prefer domain types and smart constructors over loose tuples and `String`.

Good:
```haskell
newtype PxRange = PxRange Double

mkPxRange :: Double -> Either String PxRange
mkPxRange x
  | x > 0 = Right (PxRange x)
  | otherwise = Left "pxRange must be > 0"
```

Bad:
```haskell
type PxRange = Double
-- any value accepted, including negatives and NaN
```

## 3) GHC 9.12 Record Style

Use modern record ergonomics:
- `NoFieldSelectors`
- `DuplicateRecordFields`
- `OverloadedRecordDot`
- `OverloadedRecordUpdate` (when it helps readability)
- `ApplicativeDo` (prefer when effects are independent)

Prefer short field names scoped by type (`id`, `adv`, `pts`, `bbox`). Keep them clear but compact.

Good:
```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE ApplicativeDo #-}

data Glyph = Glyph { id :: !Int, adv :: !Double }
data Run   = Run   { id :: !Int, adv :: !Double }

scaleAdvance :: Double -> Glyph -> Glyph
scaleAdvance k g = g { adv = g.adv * k }
```

Bad:
```haskell
data Glyph = Glyph
  { glyphIdentifier :: Int
  , glyphAdvanceInFontUnitsAlongX :: Double
  }
```

## 4) Prefer Total Functions

No partial pattern matches, `head`, `tail`, `fromJust`, or unchecked indexing.

Good:
```haskell
firstGlyph :: [a] -> Maybe a
firstGlyph [] = Nothing
firstGlyph (x:_) = Just x
```

Bad:
```haskell
firstGlyph :: [a] -> a
firstGlyph xs = head xs
```

## 5) Use Explicit Errors

Return `Either` for recoverable failures. Reserve exceptions for truly exceptional cases.

Good:
```haskell
parseAxis :: String -> Either String Axis
parseAxis s = ...
```

Bad:
```haskell
parseAxis :: String -> Axis
parseAxis s = error ("bad axis: " <> s)
```

## 6) Keep Data Flow Simple

Break transformations into small pure functions with descriptive names.

Good:
```haskell
buildGlyphPlan :: Font -> [GlyphId] -> Plan
buildGlyphPlan font = finalize . colorize . flatten . outlines font
```

Bad:
```haskell
buildGlyphPlan font ids = -- 150 lines of mixed concerns
```

## 7) Be Deliberate With Strictness

Start clear, then add strictness where profiling proves need. Use strict fields and `foldl'` in hot paths.

Good:
```haskell
data Stats = Stats
  { glyphs :: !Int
  , segments :: !Int
  }

countSegments :: [Int] -> Int
countSegments = foldl' (+) 0
```

Bad:
```haskell
countSegments :: [Int] -> Int
countSegments = foldl (+) 0
```

## 8) Keep Module Surfaces Small

Export only what callers need. Hide constructors when invariants matter.

Good:
```haskell
module Font.Range (PxRange, mkPxRange, unPxRange) where
```

Bad:
```haskell
module Font.Range where
```

## 9) Use Typeclasses Sparingly

Use typeclasses for real ad-hoc polymorphism. Prefer concrete functions for local logic.

Good:
```haskell
encodeGlyphId :: GlyphId -> Text
```

Bad:
```haskell
class Encodable a where
  encode :: a -> Text
-- with only one instance in the whole repo
```

## 10) Test Pure Core First (Prefer TDD)

Put core algorithms behind pure APIs and test those directly. IO wrappers should stay thin.
When practical, do TDD:
1. write a failing test,
2. implement the smallest fix,
3. refactor while keeping tests green.

Property testing should be the default for deterministic core logic.
Use example/unit tests for edge cases and regression locks.

Good:
```haskell
-- property test + targeted regression examples for contour and distance invariants
```

Bad:
```haskell
-- tests only through end-to-end CLI calls, no core properties
```

## 11) Optimize With Evidence

Profile before changing hot code, then remeasure. Keep diffs small and reversible.

Good:
```haskell
-- capture baseline
-- apply one focused optimization
-- rerun benchmark and compare
```

Bad:
```haskell
-- broad refactor + perf assumptions + no benchmark delta
```

## 12) Naming And Comments

Use names that encode domain meaning. Add comments for invariants and non-obvious performance constraints.

Good:
```haskell
-- Invariant: contour winding is normalized before edge coloring.
normalizeContours :: [Contour] -> [Contour]
```

Bad:
```haskell
go2 :: [A] -> [A]
go2 = ...
```

## 13) Default Pragmas And Warnings

Prefer strong warnings and avoid broad suppression.
- Keep `-Wall` enabled.
- If disabling a warning, document why near the pragma.

Useful non-default pragmas for this repo (with `default-language: GHC2024`):
- `NoFieldSelectors`
- `DuplicateRecordFields`
- `OverloadedRecordDot`
- `OverloadedRecordUpdate`
- `ApplicativeDo`
- `DerivingVia`
- `DeriveAnyClass`
- `TypeFamilies`
- `TypeFamilyDependencies` (only when needed)
- `PatternSynonyms`
- `ViewPatterns`
- `StrictData` (hot data modules only)
- `UnboxedTuples`, `UnboxedSums`, `MagicHash` (hot-path leaf modules only)

Use sparingly and only with an inline rationale:
- `UndecidableInstances`
- `IncoherentInstances`
- `OverlappingInstances`
- `ImpredicativeTypes`

Rule of thumb:
- prefer enabling pragmas at module scope, not project-wide,
- keep specialized pragmas close to the small set of modules that need them.

## 14) Performance: Keep Data Simple, Keep Allocations Low

In hot paths, straightforward code with simple data usually beats clever tricks.
- Prefer compact records with strict fields.
- Prefer single-pass loops over chains of `map`/`filter`/`concat`.
- Avoid unnecessary temporary structures.
- Choose readability plus predictable allocation patterns over "clever" hacks.

Good:
```haskell
sumLen :: [Edge] -> Double
sumLen = foldl' step 0
  where
    step !acc e = acc + edgeLen e
```

Bad:
```haskell
sumLen :: [Edge] -> Double
sumLen es = sum (map edgeLen es)
```

Good:
```haskell
-- clear and direct representation used in hot code
data Edge = Edge { x0 :: !Double, y0 :: !Double, x1 :: !Double, y1 :: !Double }
```

Bad:
```haskell
-- opaque "clever" encoding that hurts maintainability
newtype Edge = Edge Word64
```

## 15) Parallelism: Use It Deliberately And Non-Invasively

Parallelism is a tool, not a default. Start from a clear sequential baseline, then parallelize only proven hot pure work.

### Heuristics
- Parallelize CPU-bound, pure, independent work items.
- Avoid parallelizing tiny tasks; chunk work to amortize scheduling overhead.
- Prefer bounded parallelism (typically `numCapabilities` or small multiples).
- Keep each parallel unit self-contained to avoid shared mutable state.
- If the algorithm is memory-bandwidth bound, parallelism may not help.

### Non-invasive rollout pattern
1. Keep a clear sequential function as the source of truth.
2. Add a parallel wrapper that preserves type and output.
3. Gate parallel execution behind config/flag so fallback is trivial.
4. Keep the diff small: do not redesign domain types just to add parallelism.

Good:
```haskell
buildAllSeq :: [GlyphReq] -> [GlyphOut]
buildAllSeq = map buildOne

buildAllPar :: Int -> [GlyphReq] -> [GlyphOut]
buildAllPar chunkN xs =
  concatMap (withStrategy (parList rdeepseq) . map buildOne) (chunksOf chunkN xs)
```

Bad:
```haskell
-- mixes algorithm rewrite, mutable state, and parallelism in one risky diff
buildAll :: [GlyphReq] -> IO [GlyphOut]
buildAll = ...
```

### Benchmarking parallel usefulness
- Measure sequential baseline first.
- Run with fixed workload and fixed RTS settings.
- Compare wall time, allocation, and GC stats.
- Require repeatable wins, not one fast outlier.
- Validate no output differences from sequential mode.

Suggested loop:
1. `cabal bench --benchmark-options='--match "<target>" +RTS -N1 -s -RTS'`
2. `cabal bench --benchmark-options='--match "<target>" +RTS -N -s -RTS'`
3. Repeat both several times and compare medians.
4. Keep parallel path only if speedup is stable and memory blow-up is acceptable.

Red flags:
- Parallel is faster once but slower on median.
- Allocation or GC time spikes enough to erase CPU gains.
- Throughput improves while latency worsens for real workload shape.

## 16) Collect And Enforce Invariants

Invariants are part of the design, not comments-only documentation.
- Collect them near domain types and constructors.
- Enforce them at boundaries (parse/load/decode), then keep core logic assumption-safe.
- Name invariants explicitly (`invBBoxFinite`, `invEdgesNonEmpty`, etc.).
- Add property tests for invariant-preserving transforms.

Good:
```haskell
newtype PxRange = PxRange Double

mkPxRange :: Double -> Either String PxRange
mkPxRange x
  | x > 0 && isFinite x = Right (PxRange x)
  | otherwise = Left "PxRange invariant failed: must be finite and > 0"
```

Bad:
```haskell
type PxRange = Double
-- invariant exists only in developer memory
```

Good:
```haskell
-- property: normalization does not change contour count
prop_normalizeContours_preservesCount :: [Contour] -> Bool
prop_normalizeContours_preservesCount cs =
  length (normalizeContours cs) == length cs
```

Bad:
```haskell
-- no property/regression tests for stated invariants
```

## 17) Prefer Standard Algebraic Abstractions

Use standard typeclasses before inventing custom ones.

### Core classes to reach for
- `Semigroup`: associative combine of partial results (`<>`).
- `Monoid`: `Semigroup` plus identity (`mempty`).
- `Functor`: map values without changing structure.
- `Applicative`: combine independent effects/validations.
- `Monad`: sequence dependent effects.
- `Foldable`: consume/reduce structures.
- `Traversable`: map with effects while preserving shape.
- `Alternative`: choice/fallback for parser-like flows.

### Heuristics
- If computations are independent, prefer `Applicative` over `Monad`.
- If order/dependency matters, use `Monad`.
- If you only need combination, use `Semigroup`/`Monoid`.
- If you only need traversal with effects, use `traverse` (not manual recursion).
- Avoid custom typeclasses unless there are multiple real instances.

Good:
```haskell
-- independent field checks: Applicative style
mkGlyphCfg :: Double -> Double -> Either String GlyphCfg
mkGlyphCfg px rng = GlyphCfg <$> mkScale px <*> mkPxRange rng
```

Bad:
```haskell
-- monadic style used with fake dependency and extra noise
mkGlyphCfg px rng = do
  s <- mkScale px
  r <- mkPxRange rng
  pure (GlyphCfg s r)
```

Good:
```haskell
-- semigroup/monoid for combining summaries
data Stats = Stats { glyphs :: !Int, edges :: !Int }

instance Semigroup Stats where
  a <> b = Stats { glyphs = a.glyphs + b.glyphs, edges = a.edges + b.edges }

instance Monoid Stats where
  mempty = Stats 0 0
```

Bad:
```haskell
-- ad-hoc combine function repeated in multiple call sites
mergeStats :: Stats -> Stats -> Stats
mergeStats = ...
```

Good:
```haskell
-- prefer traverse for effectful mapping with preserved shape
loadAll :: [FilePath] -> IO [Font]
loadAll = traverse loadFont
```

Bad:
```haskell
loadAll [] = pure []
loadAll (p:ps) = do
  f <- loadFont p
  fs <- loadAll ps
  pure (f:fs)
```

## 18) Use Subagents For Substantial Implementation Tasks

For substantial implementation tasks, use subagents in parallel when work can be partitioned safely.
- Do not parallelize edits that contend on the same file or hunk.
- Do not parallelize `apply_patch` flows that target the same file.
- Keep work sequential when later steps depend on earlier outputs or decisions.

## Quick Checklist

Before shipping:
- IO at edges, pure core in library.
- No partial functions in new code.
- Domain constraints encoded in types or constructors.
- Record style follows GHC 9.12 features (`NoFieldSelectors`, duplicate fields, overloaded record syntax).
- Field names are short, clear, and type-scoped.
- Hot paths use simple data and single-pass allocation-aware loops.
- Parallelism is applied only to proven hot pure independent work.
- Sequential path remains simple and available as fallback.
- Parallel speedup is verified with repeated `-N1` vs `-N` benchmark comparisons.
- Invariants are explicit, enforced at boundaries, and covered by property/regression tests.
- Standard typeclasses are used where they fit (`Semigroup`, `Monoid`, `Functor`, `Applicative`, `Monad`, `Foldable`, `Traversable`).
- Non-default pragmas are intentional, local, and justified.
- Add tests for new behavior; prefer property tests for pure core logic.
- Prefer TDD when practical (failing test first).
- Benchmarks/tests rerun for touched behavior.
- Public exports are intentionally minimal.
