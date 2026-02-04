module Main (main) where

import Control.Exception (SomeException, try, evaluate)
import Data.Array (Array, elems, (!))
import Data.Array.IArray (bounds, rangeSize)
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import Data.Bits (testBit, (.&.), xor, shiftR)
import Data.Char (ord)
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stdout, stderr, withBinaryFile, IOMode(WriteMode), hPutBuf)

import MSDF.Generated (generateMSDFWithConfig, generateMSDFFromTTF)
import MSDF.MSDF
  ( MSDFConfig(..)
  , defaultMSDFConfig
  , renderGlyphMSDF
  , renderGlyphMSDFCached
  , renderGlyphMSDFCachedLazy
  , prepareGlyphCache
  , prepareGlyphCacheLazy
  , glyphMetricsOnly
  , GlyphSet(..)
  )
import qualified MSDF.MSDF as MSDF
import MSDF.Render (glyphQuad, glyphUV, glyphUVTopLeft, scaleForPixelSize, pixelRangeForAtlas)
import MSDF.Binary (ByteBuffer(..), readByteBuffer, readS16BE, readU16BE, readU8, readTag, slice)
import MSDF.Outline (Point(..))
import MSDF.TTF.GPOS (AnchorRaw(..), GPOSMarksRaw(..), MarkToBaseRaw(..), MarkToMarkRaw(..), MarkGlyphRaw(..), BaseGlyphRaw(..))
import MSDF.TTF.Parser
import MSDF.TTF.Variations (Fvar(..), FvarAxis(..), Gvar(..), applyGvarToContours, componentDeltas, hvarDeltas, vvarDeltas, mvarHheaDeltas, mvarVheaDeltas)
import MSDF.Types
import Paths_masdiff (getDataFileName)

ttfPath :: IO FilePath
ttfPath = getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf"

staticRegularPath :: IO FilePath
staticRegularPath = getDataFileName "assets/Inter/static/Inter_24pt-Regular.ttf"

staticBoldPath :: IO FilePath
staticBoldPath = getDataFileName "assets/Inter/static/Inter_24pt-Bold.ttf"

italicVarPath :: IO FilePath
italicVarPath = getDataFileName "assets/Inter/Inter-Italic-VariableFont_opsz,wght.ttf"

main :: IO ()
main = do
  fontPath <- ttfPath
  ttf <- requireRight "parseTTF" =<< parseTTF fontPath
  regPath <- staticRegularPath
  boldPath <- staticBoldPath
  italicPath <- italicVarPath
  ttfRegular <- requireRight "parseTTF regular" =<< parseTTF regPath
  ttfBold <- requireRight "parseTTF bold" =<< parseTTF boldPath
  ttfItalic <- requireRight "parseTTF italic" =<< parseTTF italicPath
  let cfgSmall :: MSDFConfig
      cfgSmall = defaultMSDFConfig { MSDF.pixelSize = 16 }
      cfgSubset = cfgSmall { MSDF.glyphSet = GlyphSetCodepoints [65, 66] }
      cfgUnpacked = cfgSubset { MSDF.atlas = cfgSubset.atlas { MSDF.packAtlas = False } }
      cfgVarSubset = cfgSmall
        { MSDF.glyphSet = GlyphSetCodepoints [65, 66, 67, 68]
        , MSDF.variations = [("wght", 700), ("opsz", 32)]
        , MSDF.atlas = cfgSmall.atlas { MSDF.packAtlas = False }
        }
  subsetAtlas <- requireRight "generateMSDFWithConfig" =<< generateMSDFWithConfig cfgSubset fontPath
  unpackedAtlas <- requireRight "generateMSDFWithConfig" =<< generateMSDFWithConfig cfgUnpacked fontPath
  varAtlas <- requireRight "generateMSDFWithConfig" =<< generateMSDFWithConfig cfgVarSubset fontPath
  parallelAtlas <- requireRight "generateMSDFWithConfig" =<< generateMSDFWithConfig (cfgSubset { MSDF.parallelism = 16 }) fontPath
  results <- sequence
    [ runTest "parseTTF basic" (testParseTTF ttf)
    , runTest "parseTTF bytes" (testParseTTFBytes fontPath)
    , runTest "static font parse" (testStaticFontParse ttfRegular ttfBold)
    , runTest "static font differ" (testStaticFontDiffer ttfRegular ttfBold)
    , runTest "cmap includes" (testCmap ttf)
    , runTest "glyph outline A" (testOutlineA ttf)
    , runTest "repeat flags parsing" (testRepeatFlagsParsing ttf)
    , runTest "composite glyph" (testCompositeGlyph ttf)
    , runTest "render glyph MSDF" (testRenderGlyph ttf cfgSmall)
    , runTest "render glyph MTSDF" (testRenderGlyphMTSDF ttf cfgSmall)
    , runTest "variable font axes" (testVariableFont ttf)
    , runTest "variation normalization" (testVariationNormalization ttf)
    , runTest "italic variable font" (testItalicVariableFont ttfItalic)
    , runTest "variable glyph batch" (testVariableGlyphBatch ttf)
    , runTest "variable composite deltas" (testVariableCompositeDeltas ttf)
    , runTest "variable metrics batch" (testVariableMetricsBatch ttf)
    , runTest "variable atlas" (testVariableAtlas varAtlas)
    , runTest "gvar default safe" (testGvarDefaultSafe ttf)
    , runTest "gpos marks" (testGposMarks ttf)
    , runTest "subset atlas" (testGenerateSubset subsetAtlas)
    , runTest "atlas packing" (testAtlasPacking subsetAtlas)
    , runTest "atlas placement bounds" (testAtlasPlacementBounds subsetAtlas)
    , runTest "atlas power of two" (testAtlasPowerOfTwo subsetAtlas)
    , runTest "atlas non power of two" (testAtlasNonPowerOfTwo ttf)
    , runTest "render helpers" (testRenderHelpers subsetAtlas)
    , runTest "render scale helpers" (testRenderScaleHelpers subsetAtlas)
    , runTest "SDL atlas interop" (testSDLAtlasInterop subsetAtlas)
    , runTest "unpacked atlas" (testUnpackedAtlas unpackedAtlas)
    , runTest "kerning sorted" (testKerningSorted subsetAtlas)
    , runTest "codepoint index sorted" (testCodepointIndexSorted subsetAtlas)
    , runTest "lookup codepoint" (testLookupCodepoint subsetAtlas ttf)
    , runTest "glyph set dedupes codepoints" (testGlyphSetDedupesCodepoints ttf)
    , runTest "parallelism deterministic" (testParallelismDeterministic ttf subsetAtlas parallelAtlas)
    , runTest "glyph cache" (testGlyphCache ttf cfgSmall)
    , runTest "glyph cache lazy" (testGlyphCacheLazy ttf cfgSmall)
    , runTest "bitmap hash" (testBitmapHash ttf cfgSmall)
    , runTest "bitmap hash variable" (testBitmapHashVariable ttf)
    , runTest "bitmap hash italic" (testBitmapHashItalic ttfItalic)
    , runTest "atlas speckle msdf" (testAtlasSpeckle ttf BitmapMSDF)
    , runTest "atlas speckle mtsdf" (testAtlasSpeckle ttf BitmapMTSDF)
    , runTest "composite variation" (testCompositeVariation ttf)
    , runTest "metrics only" (testMetricsOnly ttf cfgSmall)
    , runTest "vertical metrics" (testVerticalMetrics ttf cfgSmall)
    , runTest "composite point match" (testCompositePointMatch ttf)
    , runTest "malformed tables" (testMalformedTables fontPath)
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

testParseTTFBytes :: FilePath -> IO ()
testParseTTFBytes path = do
  bytes <- BS.readFile path
  case parseTTFBytes bytes of
    Left err -> error ("parseTTFBytes failed: " ++ err.context ++ ": " ++ err.message)
    Right ttf -> do
      assert (ttf.head.unitsPerEm > 0) "parseTTFBytes unitsPerEm should be > 0"
      assert (ttf.maxp.numGlyphs > 0) "parseTTFBytes numGlyphs should be > 0"

testStaticFontParse :: TTF -> TTF -> IO ()
testStaticFontParse regular bold = do
  assert (isNothing regular.variations) "regular should not have variations"
  assert (isNothing bold.variations) "bold should not have variations"
  assert (regular.maxp.numGlyphs > 0) "regular font has no glyphs"
  assert (bold.maxp.numGlyphs > 0) "bold font has no glyphs"

testStaticFontDiffer :: TTF -> TTF -> IO ()
testStaticFontDiffer regular bold = do
  let cps = [65, 66, 67, 68, 69, 77, 97, 109]
      diffCp =
        [ cp
        | cp <- cps
        , let gR = fromMaybe (-1) (lookupCodepointList cp regular.cmap.mappings)
        , let gB = fromMaybe (-1) (lookupCodepointList cp bold.cmap.mappings)
        , gR >= 0 && gB >= 0
        , contourHash (glyphOutline regular gR) /= contourHash (glyphOutline bold gB)
        ]
  assert (not (null diffCp)) "regular and bold outlines identical for sampled glyphs"


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

testRepeatFlagsParsing :: TTF -> IO ()
testRepeatFlagsParsing ttf =
  case findRepeatFlagGlyph ttf of
    Nothing -> pure ()
    Just (glyphIndex, bb, endPts, flags, flagsOff, bytesConsumed) -> do
      let xOff = flagsOff + bytesConsumed
          (xs, yOff) = readCoordsTest bb xOff flags True
          (ys, _) = readCoordsTest bb yOff flags False
          points =
            [ Point (fromIntegral (xs !! i)) (fromIntegral (ys !! i)) (testBit (flags !! i) 0)
            | i <- [0 .. min (length flags - 1) (length xs - 1)]
            ]
          contoursExpected = splitContoursTest points endPts
          contoursActual = glyphOutline ttf glyphIndex
          expectedHash = contourHash contoursExpected
          actualHash = contourHash contoursActual
      assert (not (null contoursExpected)) "repeat-flag glyph has no contours"
      assert (bytesConsumed /= length flags) "repeat-flag glyph did not use compression"
      assert (expectedHash == actualHash) "repeat-flag glyph contour mismatch"


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
  let anchors = concatMap anchorsFromMarkToBase markToBase' ++ concatMap anchorsFromMarkToMark markToMark'
  if null markToBase' && null markToMark'
    then pure ()
    else assert (not (null anchors)) "GPOS mark tables present but no anchors"

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

anchorsFromMarkToBase :: MarkToBaseRaw -> [AnchorRaw]
anchorsFromMarkToBase (MarkToBaseRaw _ marks bases) =
  markAnchors marks ++ baseAnchors bases

anchorsFromMarkToMark :: MarkToMarkRaw -> [AnchorRaw]
anchorsFromMarkToMark (MarkToMarkRaw _ marks1 marks2) =
  markAnchors marks1 ++ baseAnchors marks2

markAnchors :: Array Int (Maybe MarkGlyphRaw) -> [AnchorRaw]
markAnchors arr = [ anchor | Just (MarkGlyphRaw _ anchor) <- elems arr ]

baseAnchors :: Array Int (Maybe BaseGlyphRaw) -> [AnchorRaw]
baseAnchors arr =
  concat
    [ [ anchor | Just anchor <- elems anchors ]
    | Just (BaseGlyphRaw anchors) <- elems arr
    ]

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
      expected = bmp.width * bmp.height * bitmapChannels bmp.format
  assert (size == expected) "bitmap pixel buffer size mismatch"

testRenderGlyphMTSDF :: TTF -> MSDFConfig -> IO ()
testRenderGlyphMTSDF ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let glyph = renderGlyphMSDF (cfg { MSDF.outputFormat = BitmapMTSDF }) ttf gA
      bmp = glyph.bitmap
  assert (bmp.format == BitmapMTSDF) "bitmap format should be MTSDF"
  let (lo, hi) = bounds bmp.pixels
      size = rangeSize (lo, hi)
      expected = bmp.width * bmp.height * 4
  assert (size == expected) "MTSDF bitmap pixel buffer size mismatch"

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
        Just (Gvar _ _ offsets dataOffset _ buffer _) ->
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

testVariationNormalization :: TTF -> IO ()
testVariationNormalization ttf =
  case ttf.variations of
    Nothing -> pure ()
    Just vars ->
      case vars.fvar.axes of
        [] -> pure ()
        (axis0:_) -> do
          let axisSpan = axis0.maxValue - axis0.minValue
              big = axis0.maxValue + axisSpan * 10
              VariationLocation coords =
                normalizeLocation vars.fvar vars.avar [(axis0.tag, big)]
          assert (length coords == length vars.fvar.axes) "normalized coords length mismatch"
          assert (all (\c -> c >= -1.01 && c <= 1.01) coords) "normalized coords out of range"

testItalicVariableFont :: TTF -> IO ()
testItalicVariableFont ttf =
  case ttf.variations of
    Nothing -> assert False "italic variable font has no variation tables"
    Just vars -> do
      assert (not (null vars.fvar.axes)) "italic fvar has no axes"
      case vars.gvar of
        Nothing -> assert False "italic gvar missing"
        Just gvar -> do
          let axisMaxes = [ (axis.tag, axis.maxValue) | axis <- vars.fvar.axes ]
              loc = normalizeLocation vars.fvar vars.avar axisMaxes
              allCandidates = gvarGlyphsWithData gvar (ttf.maxp.numGlyphs - 1)
              candidates = take 200 allCandidates ++ take 200 (reverse allCandidates)
          assert (not (null candidates)) "italic gvar has no glyph data"
          varies <- anyM (glyphVariesOrPhantom ttf gvar loc) candidates
          assert varies "italic variable font should alter outline or phantom deltas"

testVariableGlyphBatch :: TTF -> IO ()
testVariableGlyphBatch ttf =
  case ttf.variations of
    Nothing -> pure ()
    Just vars ->
      case vars.gvar of
        Nothing -> assert False "gvar missing"
        Just gvar -> do
          let loc = normalizeLocation vars.fvar vars.avar [("wght", 700), ("opsz", 32)]
              candidates = take 50 (gvarGlyphsWithData gvar (ttf.maxp.numGlyphs - 1))
          assert (not (null candidates)) "gvar has no glyph data"
          mapM_ (checkGlyph gvar loc) candidates
  where
    checkGlyph gvar loc i = do
      let baseContours = glyphOutlineAt Nothing ttf i
          (varContours, (dL, dR, dT, dB)) = applyGvarToContours gvar loc i baseContours
      assert (length baseContours == length varContours) "gvar changed contour count"
      assert (and (zipWith sameLen baseContours varContours)) "gvar changed contour point count"
      assert (all isFinite [dL, dR, dT, dB]) "gvar phantom deltas not finite"
    sameLen a b = length a == length b

testVariableCompositeDeltas :: TTF -> IO ()
testVariableCompositeDeltas ttf =
  case ttf.variations of
    Nothing -> pure ()
    Just vars ->
      case vars.gvar of
        Nothing -> pure ()
        Just gvar -> do
          let loc = normalizeLocation vars.fvar vars.avar [("wght", 700)]
              maxGlyph = min (ttf.maxp.numGlyphs - 1) (snd (bounds gvar.offsets) - 1)
              compGlyphs =
                [ i
                | i <- [0 .. maxGlyph]
                , let count = compositeComponentCount ttf i
                , count > 0
                , gvarHasData gvar i
                ]
              sample = take 20 compGlyphs
          if null sample
            then pure ()
            else mapM_ (checkComposite gvar loc) sample
  where
    checkComposite gvar loc i = do
      let count = compositeComponentCount ttf i
          (dxs, dys, (dL, dR, dT, dB)) = componentDeltas gvar loc i count
          (loX, hiX) = bounds dxs
          (loY, hiY) = bounds dys
      assert (loX == 0 && hiX == count - 1) "component delta X bounds mismatch"
      assert (loY == 0 && hiY == count - 1) "component delta Y bounds mismatch"
      let vals = elems dxs ++ elems dys ++ [dL, dR, dT, dB]
      assert (all isFinite vals) "component deltas not finite"

testVariableMetricsBatch :: TTF -> IO ()
testVariableMetricsBatch ttf =
  case ttf.variations of
    Nothing -> pure ()
    Just _ -> do
      let cfg = defaultMSDFConfig
            { MSDF.pixelSize = 16
            , MSDF.variations = [("wght", 700)]
            }
          maxGlyph = min (ttf.maxp.numGlyphs - 1) 120
      mapM_ (checkMetrics cfg ttf) [0 .. maxGlyph]
  where
    checkMetrics cfg ttf' i = do
      let glyph = glyphMetricsOnly cfg ttf' i
          bb = glyph.bbox
          vals = [ glyph.advance
                 , glyph.bearingX
                 , glyph.bearingY
                 , bb.xMin
                 , bb.yMin
                 , bb.xMax
                 , bb.yMax
                 ]
      assert (all isFinite vals) "metrics contain non-finite values"
      assert (bb.xMin <= bb.xMax) "bbox xMin > xMax"
      assert (bb.yMin <= bb.yMax) "bbox yMin > yMax"

testVariableAtlas :: MSDFAtlas -> IO ()
testVariableAtlas atlas = do
  assert (isNothing atlas.atlas) "variable atlas should be unpacked"
  case lookupCodepoint atlas 65 of
    Nothing -> assert False "variable atlas missing A"
    Just gA -> do
      let glyphA = atlas.glyphs ! gA
      assert (glyphA.bitmap.width > 0) "variable atlas glyph A bitmap empty"

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

testAtlasPlacementBounds :: MSDFAtlas -> IO ()
testAtlasPlacementBounds atlas =
  case atlas.atlas of
    Nothing -> assert False "atlas packing missing"
    Just img -> do
      let glyphs = elems atlas.glyphs
      mapM_ (checkPlacement img) glyphs
  where
    checkPlacement img glyph =
      case glyph.placement of
        Nothing -> pure ()
        Just pl -> do
          assert (pl.width == glyph.bitmap.width) "placement width mismatch"
          assert (pl.height == glyph.bitmap.height) "placement height mismatch"
          assert (pl.x >= 0 && pl.y >= 0) "placement origin out of bounds"
          assert (pl.x + pl.width <= img.width) "placement width out of bounds"
          assert (pl.y + pl.height <= img.height) "placement height out of bounds"
          assert (pl.u0 >= 0 && pl.v0 >= 0 && pl.u1 <= 1 && pl.v1 <= 1) "placement UVs out of range"

testAtlasPowerOfTwo :: MSDFAtlas -> IO ()
testAtlasPowerOfTwo atlas =
  case atlas.atlas of
    Nothing -> assert False "atlas packing missing"
    Just img -> do
      assert (isPow2 img.width && isPow2 img.height) "atlas dimensions should be power of two"

testAtlasNonPowerOfTwo :: TTF -> IO ()
testAtlasNonPowerOfTwo ttf = do
  let cfg = defaultMSDFConfig
        { MSDF.pixelSize = 16
        , MSDF.glyphSet = GlyphSetCodepoints [65, 66]
        , MSDF.atlas = (defaultMSDFConfig.atlas
            { MSDF.packAtlas = True
            , MSDF.atlasPowerOfTwo = False
            , MSDF.atlasMinSize = 300
            , MSDF.atlasMaxSize = 300
            , MSDF.atlasPadding = 1
            })
        }
      atlas = generateMSDFFromTTF cfg ttf
  case atlas.atlas of
    Nothing -> assert False "non-power-of-two atlas should pack at fixed size"
    Just img -> do
      assert (img.width == 300) "expected non-power-of-two atlas width"
      assert (img.height > 0 && img.height <= 300) "atlas height out of bounds"

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

testRenderScaleHelpers :: MSDFAtlas -> IO ()
testRenderScaleHelpers atlas = do
  let px = fromIntegral atlas.pixelSize
      scale = scaleForPixelSize atlas px
      range = pixelRangeForAtlas atlas px
  assertApprox "scaleForPixelSize" 1.0 scale
  assertApprox "pixelRangeForAtlas" (fromIntegral atlas.range) range

testSDLAtlasInterop :: MSDFAtlas -> IO ()
testSDLAtlasInterop atlas = do
  case atlas.atlas of
    Nothing -> assert False "atlas packing missing"
    Just img -> do
      let src = img.pixels
          channels = bitmapChannels img.format
          srcLen = rangeSize (bounds src)
      assert (channels == 3 || channels == 4) "atlas channel count should be 3 or 4"
      assert (srcLen `mod` channels == 0) "atlas pixel buffer size mismatch"
      let rgba = bitmapToRgba img.format src
          rgbaLen = rangeSize (bounds rgba)
      assert (rgbaLen == (srcLen `div` channels) * 4) "atlas RGBA length mismatch"
      if rgbaLen >= 4
        then assert (rgba UA.! 3 == 255 || img.format == BitmapMTSDF) "atlas RGBA alpha should be 255 for MSDF"
        else assert False "atlas RGBA empty"
  case lookupCodepoint atlas 65 of
    Nothing -> pure ()
    Just gA -> do
      let glyphA = atlas.glyphs ! gA
          (u0, v0, u1, v1) = glyphUV glyphA
          (tu0, tv0, tu1, tv1) = glyphUVTopLeft glyphA
      assertApprox "glyphUVTopLeft u0" u0 tu0
      assertApprox "glyphUVTopLeft u1" u1 tu1
      assertApprox "glyphUVTopLeft v0" (1 - v1) tv0
      assertApprox "glyphUVTopLeft v1" (1 - v0) tv1

testUnpackedAtlas :: MSDFAtlas -> IO ()
testUnpackedAtlas atlas = do
  assert (isNothing atlas.atlas) "unpacked atlas should not include atlas image"
  let placements = [ pl | glyph <- elems atlas.glyphs, Just pl <- [glyph.placement] ]
  assert (null placements) "unpacked atlas should not include placements"
  case lookupCodepoint atlas 65 of
    Nothing -> pure ()
    Just gA -> do
      let glyphA = atlas.glyphs ! gA
          (u0, v0, u1, v1) = glyphUV glyphA
      assert (u0 == 0 && v0 == 0 && u1 == 1 && v1 == 1) "glyphUV default mismatch (unpacked)"


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

testGlyphSetDedupesCodepoints :: TTF -> IO ()
testGlyphSetDedupesCodepoints ttf = do
  let mappings = ttf.cmap.mappings
      mA = lookupCodepointList 65 mappings
      mB = lookupCodepointList 66 mappings
  case (mA, mB) of
    (Just gA, Just gB)
      | gA /= gB -> do
          let dupMappings = (65, gB) : mappings
              ttfDup = ttf { cmap = Cmap dupMappings }
              cfg = defaultMSDFConfig
                { MSDF.pixelSize = 16
                , MSDF.glyphSet = GlyphSetCodepoints [65]
                , MSDF.atlas = defaultMSDFConfig.atlas { MSDF.packAtlas = False }
                }
              atlas = generateMSDFFromTTF cfg ttfDup
              glyphA = atlas.glyphs ! gA
              glyphB = atlas.glyphs ! gB
          assert (glyphA.bitmap.width > 0) "glyph A bitmap should be rendered"
          assert (glyphB.bitmap.width == 0) "duplicate mapping should not render non-canonical glyph"
    _ -> pure ()

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

testGlyphCacheLazy :: TTF -> MSDFConfig -> IO ()
testGlyphCacheLazy ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  cache <- prepareGlyphCacheLazy cfg ttf
  glyphCached <- renderGlyphMSDFCachedLazy cache cfg ttf gA
  glyphCached2 <- renderGlyphMSDFCachedLazy cache cfg ttf gA
  let glyphPlain = renderGlyphMSDF cfg ttf gA
  sigCached <- glyphSignatureSafe "cache lazy A" glyphCached
  sigCached2 <- glyphSignatureSafe "cache lazy A second" glyphCached2
  sigPlain <- glyphSignatureSafe "plain A" glyphPlain
  assertEq "cache lazy compare A" sigPlain sigCached "lazy glyph cache changed output"
  assertEq "cache lazy repeat" sigCached sigCached2 "lazy glyph cache changed between calls"

testBitmapHash :: TTF -> MSDFConfig -> IO ()
testBitmapHash ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let glyph1 = renderGlyphMSDF cfg ttf gA
      glyph2 = renderGlyphMSDF cfg ttf gA
      h1 = bitmapHash glyph1
      h2 = bitmapHash glyph2
  assertEq "bitmap hash stable" h1 h2 ("bitmap hash changed between runs: " ++ show h1 ++ " vs " ++ show h2)

testBitmapHashVariable :: TTF -> IO ()
testBitmapHashVariable ttf = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let cfgVar = defaultMSDFConfig
        { MSDF.pixelSize = 16
        , MSDF.variations = [("wght", 700), ("opsz", 32)]
        }
      glyph1 = renderGlyphMSDF cfgVar ttf gA
      glyph2 = renderGlyphMSDF cfgVar ttf gA
      h1 = bitmapHash glyph1
      h2 = bitmapHash glyph2
  assertEq "bitmap hash variable stable" h1 h2 ("bitmap hash variable changed between runs: " ++ show h1 ++ " vs " ++ show h2)

testBitmapHashItalic :: TTF -> IO ()
testBitmapHashItalic ttf = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let cfgVar = defaultMSDFConfig
        { MSDF.pixelSize = 16
        , MSDF.variations = [("wght", 700), ("opsz", 32)]
        }
      glyph1 = renderGlyphMSDF cfgVar ttf gA
      glyph2 = renderGlyphMSDF cfgVar ttf gA
      h1 = bitmapHash glyph1
      h2 = bitmapHash glyph2
  assertEq "bitmap hash italic stable" h1 h2 ("bitmap hash italic changed between runs: " ++ show h1 ++ " vs " ++ show h2)

testAtlasSpeckle :: TTF -> BitmapFormat -> IO ()
testAtlasSpeckle ttf fmt = do
  let sampleText = "masdiff"
      cfg = defaultMSDFConfig
        { MSDF.pixelSize = 128
        , MSDF.range = 12
        , MSDF.atlas = defaultMSDFConfig.atlas { MSDF.atlasPadding = 16, MSDF.packAtlas = True }
        , MSDF.glyphSet = GlyphSetCodepoints (map ord sampleText)
        , MSDF.outputFormat = fmt
        }
      atlas = generateMSDFFromTTF cfg ttf
  case atlas.atlas of
    Nothing -> assert False "speckle test: atlas packing failed"
    Just img -> do
      let specks = findSpecklePixels img atlas.range
      assert (null specks) ("speckle test: found " ++ show (length specks) ++ " pixels in " ++ show fmt ++ " (sample " ++ show (take 8 specks) ++ ")")

testCompositeVariation :: TTF -> IO ()
testCompositeVariation ttf =
  case ttf.variations of
    Nothing -> pure ()
    Just vars -> do
      let numGlyphs = ttf.maxp.numGlyphs
          compGlyphs = [ i | i <- [0 .. numGlyphs - 1], compositeComponentCount ttf i > 0 ]
      assert (not (null compGlyphs)) "no composite glyphs found"
      let locVar = normalizeLocation vars.fvar vars.avar [("wght", 900)]
          varies i =
            let baseContours = glyphOutlineAt Nothing ttf i
                varContours = glyphOutlineAt (Just locVar) ttf i
            in contourHash baseContours /= contourHash varContours
      case find varies compGlyphs of
        Nothing -> assert False "no composite glyph varies under wght=900"
        Just _ -> pure ()


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

testMalformedTables :: FilePath -> IO ()
testMalformedTables path = do
  bb <- readByteBuffer path
  mapM_ (corruptAndExpectFail bb) ["gvar", "GPOS", "HVAR", "MVAR"]

corruptAndExpectFail :: ByteBuffer -> String -> IO ()
corruptAndExpectFail bb tag =
  case findTableRecordOffset bb tag of
    Nothing -> pure ()
    Just recOff -> do
      bb' <- copyByteBuffer bb
      let ByteBuffer { ptr = ptr0, len = len0, off = off0 } = bb'
          badLen = fromIntegral (len0 + 1024) :: Word32
      withForeignPtr ptr0 $ \p ->
        overwriteU32BE (p `plusPtr` off0) (recOff + 12) badLen
      let tmpPath = "/tmp/masdiff-bad-" ++ tag ++ ".ttf"
      writeByteBuffer tmpPath bb'
      result <- parseTTF tmpPath
      case result of
        Left _ -> pure ()
        Right ttf -> do
          forced <- try (evaluate (forceTable tag ttf)) :: IO (Either SomeException ())
          case forced of
            Left _ -> pure ()
            Right _ -> assert False ("corrupted " ++ tag ++ " table should fail to parse: " ++ tmpPath)

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

isPow2 :: Int -> Bool
isPow2 n = n > 0 && (n .&. (n - 1)) == 0

findTableRecordOffset :: ByteBuffer -> String -> Maybe Int
findTableRecordOffset bb tag =
  let numTables = fromIntegral (readU16BE bb 4) :: Int
      start = 12
      findRec i
        | i >= numTables = Nothing
        | readTag bb (start + i * 16) == tag = Just (start + i * 16)
        | otherwise = findRec (i + 1)
  in findRec 0

copyByteBuffer :: ByteBuffer -> IO ByteBuffer
copyByteBuffer bb = do
  let ByteBuffer { ptr = srcPtr, len = len0, off = off0 } = bb
  fptr <- mallocForeignPtrBytes len0
  withForeignPtr srcPtr $ \src ->
    withForeignPtr fptr $ \dst ->
      copyBytes dst (src `plusPtr` off0) len0
  pure (ByteBuffer fptr len0 0)

writeByteBuffer :: FilePath -> ByteBuffer -> IO ()
writeByteBuffer path bb =
  let ByteBuffer { ptr = ptr0, len = len0, off = off0 } = bb
  in withBinaryFile path WriteMode $ \h ->
    withForeignPtr ptr0 $ \p ->
      hPutBuf h (p `plusPtr` off0) len0

overwriteU32BE :: Ptr Word8 -> Int -> Word32 -> IO ()
overwriteU32BE p off value = do
  pokeByteOff p off     (fromIntegral (value `shiftR` 24) :: Word8)
  pokeByteOff p (off+1) (fromIntegral (value `shiftR` 16) :: Word8)
  pokeByteOff p (off+2) (fromIntegral (value `shiftR` 8) :: Word8)
  pokeByteOff p (off+3) (fromIntegral value :: Word8)

forceTable :: String -> TTF -> ()
forceTable tag ttf =
  case tag of
    "gvar" -> forceGvar ttf
    "HVAR" -> forceHvar ttf
    "MVAR" -> forceMvar ttf
    "GPOS" -> forceGpos ttf
    _ -> ()

forceGvar :: TTF -> ()
forceGvar ttf =
  case ttf.variations of
    Nothing -> ()
    Just vars ->
      case vars.gvar of
        Nothing -> ()
        Just (Gvar _ _ offs _ _ _ _) ->
          let (_lo, _hi) = bounds offs
          in ()

forceHvar :: TTF -> ()
forceHvar ttf =
  case ttf.variations of
    Nothing -> ()
    Just vars ->
      case vars.hvar of
        Nothing -> ()
        Just hvar -> hvar `seq` ()

forceMvar :: TTF -> ()
forceMvar ttf =
  case ttf.variations of
    Nothing -> ()
    Just vars ->
      case vars.mvar of
        Nothing -> ()
        Just mvar -> mvar `seq` ()

forceGpos :: TTF -> ()
forceGpos ttf =
  let GPOSMarksRaw markToBase' markToMark' = ttf.gposMarks
  in length ttf.gpos `seq` length markToBase' `seq` length markToMark' `seq` ()

assertEq :: (Eq a, Show a) => String -> a -> a -> String -> IO ()
assertEq label a b msg = do
  result <- tryAny (evaluate (a == b))
  case result of
    Left e -> error (label ++ ": " ++ show e)
    Right ok -> assert ok msg

assertApprox :: String -> Double -> Double -> IO ()
assertApprox label expected actual =
  let eps = 1e-9
      ok = abs (expected - actual) <= eps
  in assert ok (label ++ ": expected " ++ show expected ++ " got " ++ show actual)

findSpecklePixels :: AtlasImage -> Int -> [(Int, Int)]
findSpecklePixels img range =
  let w = img.width
      h = img.height
      channels = bitmapChannels img.format
      stableThreshold = 1 / fromIntegral (2 * max 1 range)
      neighbors8 =
        [ (-1, -1), (0, -1), (1, -1)
        , (-1,  0),          (1,  0)
        , (-1,  1), (0,  1), (1,  1)
        ]
      sdAt x y =
        let base = (y * w + x) * channels
            chan i = fromIntegral (img.pixels UA.! (base + i)) / 255
        in case img.format of
             BitmapMSDF ->
               let r = chan 0
                   g = chan 1
                   b = chan 2
               in median3 r g b - 0.5
             BitmapMTSDF ->
               let a = chan 3
               in a - 0.5
      isInside sd = sd > 0
      stable sd = abs sd >= stableThreshold
  in if w < 3 || h < 3
     then []
     else
       [ (x, y)
       | y <- [1 .. h - 2]
       , x <- [1 .. w - 2]
       , let sd = sdAt x y
       , stable sd
       , not (isInside sd)
       , let neighborSds = [ sdAt (x + dx) (y + dy) | (dx, dy) <- neighbors8 ]
       , let stableNeighbors = [ sdN | sdN <- neighborSds, stable sdN ]
       , length stableNeighbors >= 7
       , let insideNeighbors = length [ () | sdN <- stableNeighbors, isInside sdN ]
       , insideNeighbors >= 7
       ]

median3 :: Double -> Double -> Double -> Double
median3 a b c = max (min a b) (min (max a b) c)

bitmapToRgba :: BitmapFormat -> UA.UArray Int Word8 -> UA.UArray Int Word8
bitmapToRgba fmt arr =
  let rgbaList = go fmt (UA.elems arr)
  in if null rgbaList
     then UA.listArray (0, -1) []
     else UA.listArray (0, length rgbaList - 1) rgbaList
  where
    go BitmapMSDF (r:g:b:rest) = r:g:b:255:go BitmapMSDF rest
    go BitmapMTSDF (r:g:b:a:rest) = r:g:b:a:go BitmapMTSDF rest
    go _ [] = []
    go BitmapMSDF _ = error "bitmapToRgba: invalid MSDF RGB length"
    go BitmapMTSDF _ = error "bitmapToRgba: invalid MTSDF RGBA length"

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

glyphVariesOrPhantom :: TTF -> Gvar -> VariationLocation -> Int -> IO Bool
glyphVariesOrPhantom ttf gvar loc glyphIndex = do
  let baseContours = glyphOutlineAt Nothing ttf glyphIndex
  result <- try (evaluate (applyGvarToContours gvar loc glyphIndex baseContours))
  case result of
    Left (_ :: SomeException) -> pure False
    Right (varContours, (dL, dR, dT, dB)) ->
      let changed = contourHash baseContours /= contourHash varContours
          phantom = dL /= 0 || dR /= 0 || dT /= 0 || dB /= 0
      in pure (changed || phantom)

gvarHasData :: Gvar -> Int -> Bool
gvarHasData gvar i =
  let offsets = gvar.offsets
      (lo, hi) = bounds offsets
  in i >= lo && i + 1 <= hi && offsets ! i < offsets ! (i + 1)

gvarGlyphsWithData :: Gvar -> Int -> [Int]
gvarGlyphsWithData gvar maxGlyph =
  let offsets = gvar.offsets
      (lo, hi) = bounds offsets
      maxIdx = min maxGlyph (hi - 1)
  in [ i | i <- [lo .. maxIdx], gvarHasData gvar i ]

findRepeatFlagGlyph :: TTF -> Maybe (Int, ByteBuffer, [Int], [Word8], Int, Int)
findRepeatFlagGlyph ttf = go 0
  where
    numGlyphs = ttf.maxp.numGlyphs
    Loca { offsets = locaOffsets } = ttf.loca
    glyf = ttf.glyf
    go i
      | i >= numGlyphs = Nothing
      | otherwise =
          let start = locaOffsets ! i
              end = locaOffsets ! (i + 1)
          in if end <= start
             then go (i + 1)
             else
               let bb = slice glyf start (end - start)
                   numContours = fromIntegral (readS16BE bb 0) :: Int
               in if numContours < 0
                  then go (i + 1)
                  else
                    case parseSimpleGlyphFlags bb numContours of
                      Nothing -> go (i + 1)
                      Just (endPts, flags, flagsOff, bytesConsumed, usedRepeat) ->
                        if usedRepeat && bytesConsumed /= length flags
                        then Just (i, bb, endPts, flags, flagsOff, bytesConsumed)
                        else go (i + 1)

parseSimpleGlyphFlags :: ByteBuffer -> Int -> Maybe ([Int], [Word8], Int, Int, Bool)
parseSimpleGlyphFlags bb numContours =
  let endPtsOff = 10
      headerBytes = numContours * 2 + 2
  in if not (withinTest bb endPtsOff headerBytes)
     then Nothing
     else
       let endPts = [ fromIntegral (readU16BE bb (endPtsOff + i * 2))
                    | i <- [0 .. numContours - 1] ]
           numPoints = if null endPts then 0 else last endPts + 1
           instructionLength = fromIntegral (readU16BE bb (endPtsOff + numContours * 2))
           flagsOff = endPtsOff + numContours * 2 + 2 + instructionLength
       in if flagsOff > bb.len
          then Nothing
          else
            let (flags, _nextOff, usedRepeat, bytesConsumed) = readFlagsCompressedTest bb flagsOff numPoints
            in Just (endPts, flags, flagsOff, bytesConsumed, usedRepeat)

readFlagsCompressedTest :: ByteBuffer -> Int -> Int -> ([Word8], Int, Bool, Int)
readFlagsCompressedTest bb off count =
  let (flags, next, usedRepeat) = go off count [] False
  in (reverse flags, next, usedRepeat, next - off)
  where
    go idx 0 acc used = (acc, idx, used)
    go idx n acc used =
      if not (withinTest bb idx 1)
      then (acc, idx, used)
      else
        let flag = readU8 bb idx
            hasRepeat = testBit flag 3 && withinTest bb (idx + 1) 1
            repeatCount = if hasRepeat
                          then fromIntegral (readU8 bb (idx + 1))
                          else 0
            toTake = min n (repeatCount + 1)
            acc' = replicate toTake flag ++ acc
            idx' = idx + 1 + if hasRepeat then 1 else 0
            used' = used || hasRepeat
        in go idx' (n - toTake) acc' used'

withinTest :: ByteBuffer -> Int -> Int -> Bool
withinTest bb off size =
  let ByteBuffer { len = len0 } = bb
  in off >= 0 && size >= 0 && off + size <= len0

readCoordsTest :: ByteBuffer -> Int -> [Word8] -> Bool -> ([Int], Int)
readCoordsTest bb off flags isX =
  go flags off 0 []
  where
    go [] idx _ acc = (reverse acc, idx)
    go (flag:rest) idx lastVal acc =
      let isShort = testBit flag (if isX then 1 else 2)
          isSame = testBit flag (if isX then 4 else 5)
          (delta, idx') =
            if isShort
            then
              if withinTest bb idx 1
              then
                let b = fromIntegral (readU8 bb idx)
                    d = if isSame then b else -b
                in (d, idx + 1)
              else (0, idx)
            else
              if isSame
              then (0, idx)
              else if withinTest bb idx 2
                   then (fromIntegral (readS16BE bb idx), idx + 2)
                   else (0, bb.len)
          val = lastVal + delta
      in go rest idx' val (val : acc)

splitContoursTest :: [Point] -> [Int] -> [[Point]]
splitContoursTest points endPts =
  let indices = map (+1) endPts
  in go points indices
  where
    go _ [] = []
    go pts (n:ns) =
      let (a, b) = splitAt n pts
      in a : go b (map (subtract n) ns)

contourHash :: [[Point]] -> Double
contourHash contours =
  foldl' (\acc (Point px py _) -> acc + px * 0.7 + py * 0.3) 0 [ p | c <- contours, p <- c ]

bitmapHash :: GlyphMSDF -> Word64
bitmapHash glyph =
  let bmp = glyph.bitmap
      h0 = fnvWord64 fnvOffset (fromIntegral bmp.width)
      h1 = fnvWord64 h0 (fromIntegral bmp.height)
  in foldl' fnvByte h1 (UA.elems bmp.pixels)

fnvOffset :: Word64
fnvOffset = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

fnvByte :: Word64 -> Word8 -> Word64
fnvByte h b = (h `xor` fromIntegral b) * fnvPrime

fnvWord64 :: Word64 -> Word64 -> Word64
fnvWord64 h w =
  foldl' step h [0 .. 7]
  where
    step acc i =
      let byte = fromIntegral ((w `shiftR` (i * 8)) .&. 0xff)
      in fnvByte acc byte

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)
