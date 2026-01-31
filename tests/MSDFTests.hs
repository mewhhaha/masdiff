module Main (main) where

import Control.Exception (SomeException, try, evaluate)
import Data.Array (Array, elems, (!))
import Data.Array.IArray (bounds, rangeSize)
import qualified Data.Array.Unboxed as UA
import Data.Bits (testBit, (.&.))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stdout, stderr)

import MSDF.Generated (generateMSDFWithConfig)
import MSDF.MSDF
  ( MSDFConfig(..)
  , defaultMSDFConfig
  , renderGlyphMSDF
  , renderGlyphMSDFCached
  , prepareGlyphCache
  , glyphMetricsOnly
  , GlyphSet(..)
  )
import qualified MSDF.MSDF as MSDF
import MSDF.Render (glyphQuad, glyphUV)
import MSDF.Binary (ByteBuffer, readS16BE, readU16BE, slice)
import MSDF.Outline (Point(..))
import MSDF.TTF.GPOS (GPOSMarksRaw(..), MarkToBaseRaw(..), MarkToMarkRaw(..), MarkGlyphRaw(..), BaseGlyphRaw(..))
import MSDF.TTF.Parser
import MSDF.TTF.Variations (Fvar(..), FvarAxis(..), Gvar(..), applyGvarToContours, hvarDeltas, vvarDeltas, mvarHheaDeltas, mvarVheaDeltas)
import MSDF.Types
import Paths_masdiff (getDataFileName)

ttfPath :: IO FilePath
ttfPath = getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf"

main :: IO ()
main = do
  fontPath <- ttfPath
  ttf <- requireRight "parseTTF" =<< parseTTF fontPath
  let cfgSmall :: MSDFConfig
      cfgSmall = defaultMSDFConfig { MSDF.pixelSize = 16 }
      cfgSubset = cfgSmall { MSDF.glyphSet = GlyphSetCodepoints [65, 66] }
  subsetAtlas <- requireRight "generateMSDFWithConfig" =<< generateMSDFWithConfig cfgSubset fontPath
  parallelAtlas <- requireRight "generateMSDFWithConfig" =<< generateMSDFWithConfig (cfgSubset { MSDF.parallelism = 16 }) fontPath
  results <- sequence
    [ runTest "parseTTF basic" (testParseTTF ttf)
    , runTest "cmap includes" (testCmap ttf)
    , runTest "glyph outline A" (testOutlineA ttf)
    , runTest "composite glyph" (testCompositeGlyph ttf)
    , runTest "render glyph MSDF" (testRenderGlyph ttf cfgSmall)
    , runTest "variable font axes" (testVariableFont ttf)
    , runTest "gvar default safe" (testGvarDefaultSafe ttf)
    , runTest "gpos marks" (testGposMarks ttf)
    , runTest "subset atlas" (testGenerateSubset subsetAtlas)
    , runTest "atlas packing" (testAtlasPacking subsetAtlas)
    , runTest "render helpers" (testRenderHelpers subsetAtlas)
    , runTest "kerning sorted" (testKerningSorted subsetAtlas)
    , runTest "codepoint index sorted" (testCodepointIndexSorted subsetAtlas)
    , runTest "lookup codepoint" (testLookupCodepoint subsetAtlas ttf)
    , runTest "parallelism deterministic" (testParallelismDeterministic ttf subsetAtlas parallelAtlas)
    , runTest "glyph cache" (testGlyphCache ttf cfgSmall)
    , runTest "metrics only" (testMetricsOnly ttf cfgSmall)
    , runTest "vertical metrics" (testVerticalMetrics ttf cfgSmall)
    , runTest "composite point match" (testCompositePointMatch ttf)
    ]
  if and results
    then putStrLn "All tests passed."
    else exitFailure

runTest :: String -> IO () -> IO Bool
runTest name action = do
  putStrLn ("[test] " ++ name)
  hFlush stdout
  result <- try action
  case result of
    Left (e :: SomeException) -> do
      hPutStrLn stderr ("  FAIL: " ++ show e)
      pure False
    Right _ -> pure True

assert :: Bool -> String -> IO ()
assert cond msg = if cond then pure () else error msg

requireRight :: String -> Either ParseError a -> IO a
requireRight label result =
  case result of
    Left err -> error (label ++ ": " ++ err.context ++ ": " ++ err.message)
    Right val -> pure val

lookupCodepointList :: Int -> [(Int, Int)] -> Maybe Int
lookupCodepointList cp mappings = lookup cp mappings

-- Tests ---------------------------------------------------------------------

testParseTTF :: TTF -> IO ()
testParseTTF ttf = do
  assert (ttf.head.unitsPerEm > 0) "unitsPerEm should be > 0"
  assert (ttf.maxp.numGlyphs > 0) "numGlyphs should be > 0"
  assert (not (null ttf.cmap.mappings)) "cmap mappings should not be empty"


testCmap :: TTF -> IO ()
testCmap ttf = do
  let mappings = ttf.cmap.mappings
      numGlyphs = ttf.maxp.numGlyphs
  assert (lookupCodepointList 32 mappings /= Nothing) "cmap missing U+0020"
  assert (lookupCodepointList 65 mappings /= Nothing) "cmap missing U+0041"
  let gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0 && gA < numGlyphs) "glyph index for A out of range"


testOutlineA :: TTF -> IO ()
testOutlineA ttf = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let contours = glyphOutline ttf gA
  assert (not (null contours)) "glyph A has no contours"
  assert (any (not . null) contours) "glyph A contours empty"


testCompositeGlyph :: TTF -> IO ()
testCompositeGlyph ttf = do
  let mappings = ttf.cmap.mappings
      cp = 0x00C5 -- Å
      mGlyph = lookupCodepointList cp mappings
  assert (mGlyph /= Nothing) "cmap missing U+00C5 (Å)"
  let g = fromMaybe (-1) mGlyph
      contours = glyphOutline ttf g
  assert (not (null contours)) "glyph Å has no contours"

testCompositePointMatch :: TTF -> IO ()
testCompositePointMatch ttf = do
  case findCompositePointMatchGlyph ttf of
    Nothing -> pure () -- skip if font has no point-matching composites
    Just g -> do
      let contours = glyphOutline ttf g
      assert (not (null contours)) "point-match composite has no contours"

testGposMarks :: TTF -> IO ()
testGposMarks ttf = do
  let numGlyphs = ttf.maxp.numGlyphs
      marks = ttf.gposMarks
      GPOSMarksRaw markToBase' markToMark' = marks
  mapM_ (checkMarkToBase numGlyphs) markToBase'
  mapM_ (checkMarkToMark numGlyphs) markToMark'

checkMarkToBase :: Int -> MarkToBaseRaw -> IO ()
checkMarkToBase numGlyphs (MarkToBaseRaw classCount marks bases) = do
  assertArrayBounds numGlyphs marks "markToBase marks bounds"
  assertArrayBounds numGlyphs bases "markToBase bases bounds"
  mapM_ (checkMarkClass classCount) (elems marks)
  mapM_ (checkBaseAnchors classCount) (elems bases)

checkMarkToMark :: Int -> MarkToMarkRaw -> IO ()
checkMarkToMark numGlyphs (MarkToMarkRaw classCount marks1 marks2) = do
  assertArrayBounds numGlyphs marks1 "markToMark marks1 bounds"
  assertArrayBounds numGlyphs marks2 "markToMark marks2 bounds"
  mapM_ (checkMarkClass classCount) (elems marks1)
  mapM_ (checkBaseAnchors classCount) (elems marks2)

checkMarkClass :: Int -> Maybe MarkGlyphRaw -> IO ()
checkMarkClass classCount entry =
  case entry of
    Nothing -> pure ()
    Just (MarkGlyphRaw cls _) ->
      assert (cls >= 0 && cls < classCount) "mark class out of range"

checkBaseAnchors :: Int -> Maybe BaseGlyphRaw -> IO ()
checkBaseAnchors classCount entry =
  case entry of
    Nothing -> pure ()
    Just (BaseGlyphRaw anchors) ->
      let (lo, hi) = bounds anchors
      in if classCount <= 0
         then assert (lo > hi) "base anchors should be empty"
         else assert (lo == 0 && hi == classCount - 1) "base anchors bounds mismatch"

assertArrayBounds :: Int -> Array Int a -> String -> IO ()
assertArrayBounds numGlyphs arr msg =
  let (lo, hi) = bounds arr
  in if numGlyphs <= 0
     then assert (lo > hi) (msg ++ ": expected empty array")
     else assert (lo == 0 && hi == numGlyphs - 1) (msg ++ ": bounds mismatch")


testRenderGlyph :: TTF -> MSDFConfig -> IO ()
testRenderGlyph ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let glyph = renderGlyphMSDF cfg ttf gA
      bmp = glyph.bitmap
  assert (bmp.width > 0 && bmp.height > 0) "bitmap should have positive dimensions"
  let (lo, hi) = bounds bmp.pixels
      size = rangeSize (lo, hi)
      expected = bmp.width * bmp.height * 3
  assert (size == expected) "bitmap pixel buffer size mismatch"

testVariableFont :: TTF -> IO ()
testVariableFont ttf = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  case ttf.variations of
    Nothing -> assert False "font has no variation tables"
    Just vars -> do
      let Fvar axes _ = vars.fvar
      assert (not (null axes)) "fvar has no axes"
      case find (\a -> case a of FvarAxis t _ _ _ _ -> t == "wght") axes of
        Nothing -> assert False "wght axis missing"
        Just (FvarAxis _ minV defV maxV _) ->
          assert (minV < defV && defV < maxV) "wght axis values invalid"
      let loc@(VariationLocation locCoords) = normalizeLocation vars.fvar vars.avar [("wght", 900)]
      assert (not (null locCoords)) "normalized variation coords are empty"
      let tags = [ t | FvarAxis t _ _ _ _ <- axes ]
          wghtIndex = fromMaybe (-1) (lookup "wght" (zip tags [0..]))
      assert (wghtIndex >= 0) "wght axis index not found"
      case find (\a -> case a of FvarAxis t _ _ _ _ -> t == "wght") axes of
        Nothing -> assert False "wght axis missing"
        Just (FvarAxis _ minV defV maxV _) -> do
          let expected = if 900 < defV
                         then (900 - defV) / (defV - minV)
                         else (900 - defV) / (maxV - defV)
          assert (expected /= 0) "expected normalized value is zero"
          assert (locCoords !! wghtIndex /= 0) "normalized variation coords are all zero"
      case vars.gvar of
        Nothing -> assert False "gvar table missing"
        Just (Gvar _ _ offsets dataOffset _ buffer) ->
          let (lo, hi) = bounds offsets
          in if gA < lo || gA + 1 > hi
             then assert False "gvar offsets out of range"
             else do
               let off = offsets ! gA
                   off' = offsets ! (gA + 1)
               assert (off' > off) "gvar has no data for glyph A"
               let bb = slice buffer (dataOffset + off) (off' - off)
                   tupleCount = readU16BE bb 0 .&. 0x0FFF
               assert (tupleCount > 0) "gvar has zero tuple variations for glyph A"
      case vars.hvar of
        Nothing -> pure ()
        Just hv -> do
          let (advDelta, lsbDelta, rsbDelta) = hvarDeltas hv loc gA
          assert (all isFinite [advDelta, lsbDelta, rsbDelta]) "HVAR deltas must be finite"
      case vars.vvar of
        Nothing -> pure ()
        Just vv -> do
          let (advDelta, tsbDelta, bsbDelta) = vvarDeltas vv loc gA
          assert (all isFinite [advDelta, tsbDelta, bsbDelta]) "VVAR deltas must be finite"
      case vars.mvar of
        Nothing -> pure ()
        Just mv -> do
          let (dAsc, dDesc, dGap) = mvarHheaDeltas mv loc
          assert (all isFinite [dAsc, dDesc, dGap]) "MVAR deltas must be finite"
          let (vAsc, vDesc, vGap) = mvarVheaDeltas mv loc
          assert (all isFinite [vAsc, vDesc, vGap]) "MVAR vhea deltas must be finite"
      let maxGlyph = min (ttf.maxp.numGlyphs - 1) 200
      varies <- anyM (glyphVaries ttf loc) [0 .. maxGlyph]
      assert varies "variable font should alter at least one glyph outline"
  let cfgDefault = defaultMSDFConfig { MSDF.pixelSize = 24 }
      cfgVar = defaultMSDFConfig
        { MSDF.pixelSize = 24
        , MSDF.variations = [("wght", 900)]
        }
      _glyphDefault = renderGlyphMSDF cfgDefault ttf gA
      _glyphVar = renderGlyphMSDF cfgVar ttf gA
  pure ()

testGvarDefaultSafe :: TTF -> IO ()
testGvarDefaultSafe ttf = do
  case ttf.variations of
    Nothing -> pure ()
    Just vars ->
      case vars.gvar of
        Nothing -> pure ()
        Just gv -> do
          let loc = normalizeLocation vars.fvar vars.avar []
              offsets = gv.offsets
              (lo, hi) = bounds offsets
              maxIdx = min (ttf.maxp.numGlyphs - 1) (hi - 1)
              candidates =
                [ i
                | i <- [lo .. maxIdx]
                , let off = offsets ! i
                      off' = offsets ! (i + 1)
                , off' > off
                ]
          case candidates of
            [] -> pure ()
            (i:_) -> do
              let baseContours = glyphOutlineAt Nothing ttf i
                  (contours', (dL, dR, dT, dB)) = applyGvarToContours gv loc i baseContours
              _ <- evaluate (contourHash contours')
              assert (all isFinite [dL, dR, dT, dB]) "gvar deltas non-finite at default location"


testGenerateSubset :: MSDFAtlas -> IO ()
testGenerateSubset atlas = do
  let mA = lookupCodepoint atlas 65
      mB = lookupCodepoint atlas 66
  assert (mA /= Nothing) "atlas missing U+0041"
  assert (mB /= Nothing) "atlas missing U+0042"
  let gA = fromMaybe (-1) mA
      gB = fromMaybe (-1) mB
      glyphA = atlas.glyphs ! gA
      glyphB = atlas.glyphs ! gB
  assert (glyphA.bitmap.width > 0) "glyph A bitmap should be rendered"
  assert (glyphB.bitmap.width > 0) "glyph B bitmap should be rendered"
  let mC = lookupCodepoint atlas 67
  case mC of
    Nothing -> pure ()
    Just gC ->
      if gC == gA || gC == gB
      then pure ()
      else do
        let glyphC = atlas.glyphs ! gC
        assert (glyphC.bitmap.width == 0) "glyph C bitmap should be empty in subset"

testAtlasPacking :: MSDFAtlas -> IO ()
testAtlasPacking atlas = do
  case atlas.atlas of
    Nothing -> assert False "atlas packing missing"
    Just img -> do
      assert (img.width > 0 && img.height > 0) "atlas image dimensions should be positive"
      let (lo, hi) = bounds img.pixels
          expected = img.width * img.height * 3
      assert ((hi - lo + 1) == expected) "atlas pixel buffer size mismatch"
      case lookupCodepoint atlas 65 of
        Nothing -> assert False "atlas missing A placement"
        Just gA -> do
          let glyphA = atlas.glyphs ! gA
          case glyphA.placement of
            Nothing -> assert False "glyph A has no placement"
            Just plA -> do
              assert (plA.u0 >= 0 && plA.v0 >= 0 && plA.u1 <= 1 && plA.v1 <= 1) "glyph A UVs out of range"
      case lookupCodepoint atlas 66 of
        Nothing -> assert False "atlas missing B placement"
        Just gB -> do
          let glyphB = atlas.glyphs ! gB
          case glyphB.placement of
            Nothing -> assert False "glyph B has no placement"
            Just plB -> do
              assert (plB.u0 >= 0 && plB.v0 >= 0 && plB.u1 <= 1 && plB.v1 <= 1) "glyph B UVs out of range"
      case (lookupCodepoint atlas 65, lookupCodepoint atlas 66) of
        (Just gA, Just gB) ->
          case ((atlas.glyphs ! gA).placement, (atlas.glyphs ! gB).placement) of
            (Just plA, Just plB) ->
              assert (not (rectsOverlap plA plB)) "glyph A/B overlap in atlas"
            _ -> assert False "glyph A/B placement missing"
        _ -> pure ()

testRenderHelpers :: MSDFAtlas -> IO ()
testRenderHelpers atlas = do
  case lookupCodepoint atlas 65 of
    Nothing -> assert False "atlas missing A for render helper test"
    Just gA -> do
      let glyphA = atlas.glyphs ! gA
          bmp = glyphA.bitmap
          (x0, y0, x1, y1) = glyphQuad glyphA (0, 0)
      assert (x1 - x0 == fromIntegral bmp.width) "glyphQuad width mismatch"
      assert (y1 - y0 == fromIntegral bmp.height) "glyphQuad height mismatch"
      let (u0, v0, u1, v1) = glyphUV glyphA
      case glyphA.placement of
        Nothing -> do
          assert (u0 == 0 && v0 == 0 && u1 == 1 && v1 == 1) "glyphUV default mismatch"
        Just pl -> do
          assert (u0 == pl.u0 && v0 == pl.v0 && u1 == pl.u1 && v1 == pl.v1) "glyphUV placement mismatch"


testKerningSorted :: MSDFAtlas -> IO ()
testKerningSorted atlas = do
  let ks = elems (atlas.kerning)
  assert (isSortedBy kernKey ks) "kerning pairs not sorted"
  assert (all (not . isNaN . (\k -> k.xAdvance)) ks) "kerning contains NaN"
  where
    kernKey k = (k.left, k.right)

testCodepointIndexSorted :: MSDFAtlas -> IO ()
testCodepointIndexSorted atlas = do
  let entries = elems (atlas.codepointIndex)
  assert (isSortedBy (\e -> e.codepoint) entries) "codepoint index not sorted"
  assert (isStrictlyIncreasing (map (\e -> e.codepoint) entries)) "codepoint index has duplicates"

testLookupCodepoint :: MSDFAtlas -> TTF -> IO ()
testLookupCodepoint atlas ttf = do
  let mappings = ttf.cmap.mappings
      cpSpace = 32
      cpA = 65
      glyphsFor cp = [ g | (c, g) <- mappings, c == cp ]
  case glyphsFor cpSpace of
    [] -> pure ()
    gs ->
      assert (lookupCodepoint atlas cpSpace == Just (minimum gs)) "lookupCodepoint mismatch for space"
  case glyphsFor cpA of
    [] -> pure ()
    gs ->
      assert (lookupCodepoint atlas cpA == Just (minimum gs)) "lookupCodepoint mismatch for A"

testParallelismDeterministic :: TTF -> MSDFAtlas -> MSDFAtlas -> IO ()
testParallelismDeterministic ttf atlasSeq atlasPar = do
  let mappings = ttf.cmap.mappings
      mA = lookupCodepointList 65 mappings
      mB = lookupCodepointList 66 mappings
  assert (mA /= Nothing && mB /= Nothing) "missing A/B in cmap"
  let gA = fromMaybe (-1) mA
      gB = fromMaybe (-1) mB
      glyphsSeq = atlasSeq.glyphs
      glyphsPar = atlasPar.glyphs
  gASeq <- safeIndex "seq A" glyphsSeq gA
  gAPar <- safeIndex "par A" glyphsPar gA
  gBSeq <- safeIndex "seq B" glyphsSeq gB
  gBPar <- safeIndex "par B" glyphsPar gB
  sigASeq <- glyphSignatureSafe "seq A" gASeq
  sigAPar <- glyphSignatureSafe "par A" gAPar
  sigBSeq <- glyphSignatureSafe "seq B" gBSeq
  sigBPar <- glyphSignatureSafe "par B" gBPar
  assertEq "compare A" sigASeq sigAPar "parallelism changed glyph A"
  assertEq "compare B" sigBSeq sigBPar "parallelism changed glyph B"

testGlyphCache :: TTF -> MSDFConfig -> IO ()
testGlyphCache ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let cache = prepareGlyphCache cfg ttf
      glyphCached = renderGlyphMSDFCached cache cfg ttf gA
      glyphPlain = renderGlyphMSDF cfg ttf gA
  sigCached <- glyphSignatureSafe "cache A" glyphCached
  sigPlain <- glyphSignatureSafe "plain A" glyphPlain
  assertEq "cache compare A" sigPlain sigCached "glyph cache changed output"


testMetricsOnly :: TTF -> MSDFConfig -> IO ()
testMetricsOnly ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let glyph = glyphMetricsOnly cfg ttf gA
  assert (glyph.advance /= 0) "advance should be non-zero"
  assert (glyph.bitmap.width == 0) "metrics-only should have empty bitmap"

testVerticalMetrics :: TTF -> MSDFConfig -> IO ()
testVerticalMetrics ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let glyph = glyphMetricsOnly cfg ttf gA
  case ttf.vmtx of
    Nothing -> assert (glyph.vertical == Nothing) "vertical metrics should be absent when vmtx is missing"
    Just _ ->
      case glyph.vertical of
        Nothing -> assert False "vertical metrics missing with vmtx present"
        Just vm -> do
          assert (isFinite vm.advance) "vertical advance must be finite"
          assert (isFinite vm.topSideBearing) "vertical top side bearing must be finite"

-- Helpers -------------------------------------------------------------------

isSortedBy :: Ord b => (a -> b) -> [a] -> Bool
isSortedBy _ [] = True
isSortedBy key (x:xs) = go (key x) xs
  where
    go _ [] = True
    go prev (y:ys) =
      let k = key y
      in if prev <= k then go k ys else False

isStrictlyIncreasing :: Ord a => [a] -> Bool
isStrictlyIncreasing [] = True
isStrictlyIncreasing (x:xs) = go x xs
  where
    go _ [] = True
    go prev (y:ys) =
      if prev < y then go y ys else False

safeIndex :: String -> Array Int a -> Int -> IO a
safeIndex label arr idx = do
  arrResult <- tryAny (evaluate arr)
  case arrResult of
    Left e -> error (label ++ ": array thunk " ++ show e)
    Right arr' -> do
      let (lo, hi) = bounds arr'
      assert (idx >= lo && idx <= hi) (label ++ ": index out of bounds")
      result <- tryAny (evaluate (arr' ! idx))
      case result of
        Left e -> error (label ++ ": " ++ show e)
        Right v -> pure v

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

rectsOverlap :: GlyphPlacement -> GlyphPlacement -> Bool
rectsOverlap a b =
  let ax0 = a.x
      ay0 = a.y
      ax1 = a.x + a.width
      ay1 = a.y + a.height
      bx0 = b.x
      by0 = b.y
      bx1 = b.x + b.width
      by1 = b.y + b.height
  in ax0 < bx1 && ax1 > bx0 && ay0 < by1 && ay1 > by0

assertEq :: (Eq a, Show a) => String -> a -> a -> String -> IO ()
assertEq label a b msg = do
  result <- tryAny (evaluate (a == b))
  case result of
    Left e -> error (label ++ ": " ++ show e)
    Right ok -> assert ok msg

glyphSignatureSafe :: String -> GlyphMSDF -> IO (Double, Double, Double, BBox, Maybe VerticalMetrics, Int, Int, Int, Maybe GlyphPlacement)
glyphSignatureSafe label glyph = do
  result <- tryAny (evaluate (forceSignature (glyphSignature glyph)))
  case result of
    Left e -> error (label ++ ": " ++ show e)
    Right sig -> pure sig

forceSignature :: (Double, Double, Double, BBox, Maybe VerticalMetrics, Int, Int, Int, Maybe GlyphPlacement)
               -> (Double, Double, Double, BBox, Maybe VerticalMetrics, Int, Int, Int, Maybe GlyphPlacement)
forceSignature sig@(adv, bx, by, bb, vm, w, h, sumPx, pl) =
  adv `seq` bx `seq` by `seq` w `seq` h `seq` sumPx `seq`
  bb.xMin `seq` bb.yMin `seq` bb.xMax `seq` bb.yMax `seq` vm `seq` pl `seq` sig

glyphSignature :: GlyphMSDF -> (Double, Double, Double, BBox, Maybe VerticalMetrics, Int, Int, Int, Maybe GlyphPlacement)
glyphSignature glyph =
  let bmp = glyph.bitmap
      pixelsSum = foldl' (\acc w -> acc + fromIntegral w) 0 (UA.elems bmp.pixels)
  in (glyph.advance, glyph.bearingX, glyph.bearingY, glyph.bbox, glyph.vertical, bmp.width, bmp.height, pixelsSum, glyph.placement)

findCompositePointMatchGlyph :: TTF -> Maybe Int
findCompositePointMatchGlyph ttf = go 0
  where
    numGlyphs = ttf.maxp.numGlyphs
    glyf = ttf.glyf
    offsets = ttf.loca.offsets
    go i
      | i >= numGlyphs = Nothing
      | otherwise =
          let start = offsets ! i
              end = offsets ! (i + 1)
          in if end <= start
             then go (i + 1)
             else
               let bb = slice glyf start (end - start)
                   numContours = fromIntegral (readS16BE bb 0) :: Int
               in if numContours >= 0
                  then go (i + 1)
                  else if compositeHasPointMatch bb
                       then Just i
                       else go (i + 1)

compositeHasPointMatch :: ByteBuffer -> Bool
compositeHasPointMatch bb = go 10
  where
    go off =
      let flags = readU16BE bb off
          argsAreWords = testBit flags 0
          argsAreXY = testBit flags 1
          offArgs = off + 2 + if argsAreWords then 4 else 2
          offTrans = offArgs + transformSize flags
          hasPointMatch = not argsAreXY
      in if hasPointMatch
         then True
         else if testBit flags 5
              then go offTrans
              else False

transformSize :: Word16 -> Int
transformSize flags
  | testBit flags 3 = 2
  | testBit flags 6 = 4
  | testBit flags 7 = 8
  | otherwise = 0

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM f (x:xs) = do
  ok <- f x
  if ok then pure True else anyM f xs

glyphVaries :: TTF -> VariationLocation -> Int -> IO Bool
glyphVaries ttf loc glyphIndex = do
  base <- safeHash Nothing
  var <- safeHash (Just loc)
  pure (case (base, var) of
    (Just b, Just v) -> b /= v
    _ -> False)
  where
    safeHash mLoc = do
      let contours = glyphOutlineAt mLoc ttf glyphIndex
          h = contourHash contours
      result <- try (evaluate h) :: IO (Either SomeException Double)
      case result of
        Left _ -> pure Nothing
        Right v -> pure (Just v)

contourHash :: [[Point]] -> Double
contourHash contours =
  foldl' (\acc (Point px py _) -> acc + px * 0.7 + py * 0.3) 0 [ p | c <- contours, p <- c ]

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)
