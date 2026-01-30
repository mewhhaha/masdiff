module Main (main) where

import Control.Exception (SomeException, try, evaluate)
import Data.Array (elems, (!))
import Data.Array.IArray (bounds, rangeSize)
import Data.Bits (testBit, (.&.))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import MSDF.Generated (generateMSDFWithConfig)
import MSDF.MSDF (MSDFConfig(..), defaultMSDFConfig, renderGlyphMSDF, glyphMetricsOnly, GlyphSet(..))
import qualified MSDF.MSDF as MSDF
import MSDF.Binary (ByteBuffer, readS16BE, readU16BE, slice)
import MSDF.Outline (Point(..))
import MSDF.TTF.Parser
import MSDF.TTF.Variations (Fvar(..), FvarAxis(..), Gvar(..))
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
  subsetAtlas <- requireRight "generateMSDFWithConfig" =<< generateMSDFWithConfig (cfgSmall { MSDF.glyphSet = GlyphSetCodepoints [65, 66] }) fontPath
  results <- sequence
    [ runTest "parseTTF basic" (testParseTTF ttf)
    , runTest "cmap includes" (testCmap ttf)
    , runTest "glyph outline A" (testOutlineA ttf)
    , runTest "composite glyph" (testCompositeGlyph ttf)
    , runTest "render glyph MSDF" (testRenderGlyph ttf cfgSmall)
    , runTest "variable font axes" (testVariableFont ttf)
    , runTest "subset atlas" (testGenerateSubset subsetAtlas)
    , runTest "kerning sorted" (testKerningSorted subsetAtlas)
    , runTest "metrics only" (testMetricsOnly ttf cfgSmall)
    , runTest "composite point match" (testCompositePointMatch ttf)
    ]
  if and results
    then putStrLn "All tests passed."
    else exitFailure

runTest :: String -> IO () -> IO Bool
runTest name action = do
  putStrLn ("[test] " ++ name)
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
    Just (Variations fvar avar gvar) -> do
      let Fvar axes _ = fvar
      assert (not (null axes)) "fvar has no axes"
      case find (\a -> case a of FvarAxis t _ _ _ _ -> t == "wght") axes of
        Nothing -> assert False "wght axis missing"
        Just (FvarAxis _ minV defV maxV _) ->
          assert (minV < defV && defV < maxV) "wght axis values invalid"
      let loc@(VariationLocation locCoords) = normalizeLocation fvar avar [("wght", 900)]
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
      case gvar of
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


testKerningSorted :: MSDFAtlas -> IO ()
testKerningSorted atlas = do
  let ks = elems (atlas.kerning)
  assert (isSortedBy kernKey ks) "kerning pairs not sorted"
  assert (all (not . isNaN . (\k -> k.xAdvance)) ks) "kerning contains NaN"
  where
    kernKey k = (k.left, k.right)


testMetricsOnly :: TTF -> MSDFConfig -> IO ()
testMetricsOnly ttf cfg = do
  let mappings = ttf.cmap.mappings
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let glyph = glyphMetricsOnly cfg ttf gA
  assert (glyph.advance /= 0) "advance should be non-zero"
  assert (glyph.bitmap.width == 0) "metrics-only should have empty bitmap"

-- Helpers -------------------------------------------------------------------

isSortedBy :: Ord b => (a -> b) -> [a] -> Bool
isSortedBy _ [] = True
isSortedBy key (x:xs) = go (key x) xs
  where
    go _ [] = True
    go prev (y:ys) =
      let k = key y
      in if prev <= k then go k ys else False

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
