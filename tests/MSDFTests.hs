module Main (main) where

import Control.Exception (SomeException, try)
import Data.Array (elems, (!))
import Data.Array.IArray (bounds, rangeSize)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import MSDF.Generated (generateMSDFWithConfig)
import MSDF.MSDF (MSDFConfig(..), defaultMSDFConfig, renderGlyphMSDF, glyphMetricsOnly, GlyphSet(..))
import MSDF.TTF.Parser
import MSDF.Types

ttfPath :: FilePath
ttfPath = "assets/Inter/Inter-VariableFont_opsz,wght.ttf"

main :: IO ()
main = do
  ttf <- parseTTF ttfPath
  let cfgSmall = defaultMSDFConfig { cfgPixelSize = 16 }
  subsetAtlas <- generateMSDFWithConfig (cfgSmall { cfgGlyphSet = GlyphSetCodepoints [65, 66] }) ttfPath
  results <- sequence
    [ runTest "parseTTF basic" (testParseTTF ttf)
    , runTest "cmap includes" (testCmap ttf)
    , runTest "glyph outline A" (testOutlineA ttf)
    , runTest "composite glyph" (testCompositeGlyph ttf)
    , runTest "render glyph MSDF" (testRenderGlyph ttf cfgSmall)
    , runTest "subset atlas" (testGenerateSubset subsetAtlas)
    , runTest "kerning sorted" (testKerningSorted subsetAtlas)
    , runTest "metrics only" (testMetricsOnly ttf cfgSmall)
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

lookupCodepointList :: Int -> [(Int, Int)] -> Maybe Int
lookupCodepointList cp mappings = lookup cp mappings

-- Tests ---------------------------------------------------------------------

testParseTTF :: TTF -> IO ()
testParseTTF ttf = do
  assert (headUnitsPerEm (ttfHead ttf) > 0) "unitsPerEm should be > 0"
  assert (maxpNumGlyphs (ttfMaxp ttf) > 0) "numGlyphs should be > 0"
  assert (not (null (cmapMappings (ttfCmap ttf)))) "cmap mappings should not be empty"


testCmap :: TTF -> IO ()
testCmap ttf = do
  let mappings = cmapMappings (ttfCmap ttf)
      numGlyphs = maxpNumGlyphs (ttfMaxp ttf)
  assert (lookupCodepointList 32 mappings /= Nothing) "cmap missing U+0020"
  assert (lookupCodepointList 65 mappings /= Nothing) "cmap missing U+0041"
  let gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0 && gA < numGlyphs) "glyph index for A out of range"


testOutlineA :: TTF -> IO ()
testOutlineA ttf = do
  let mappings = cmapMappings (ttfCmap ttf)
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let contours = glyphOutline ttf gA
  assert (not (null contours)) "glyph A has no contours"
  assert (any (not . null) contours) "glyph A contours empty"


testCompositeGlyph :: TTF -> IO ()
testCompositeGlyph ttf = do
  let mappings = cmapMappings (ttfCmap ttf)
      cp = 0x00C5 -- Å
      mGlyph = lookupCodepointList cp mappings
  assert (mGlyph /= Nothing) "cmap missing U+00C5 (Å)"
  let g = fromMaybe (-1) mGlyph
      contours = glyphOutline ttf g
  assert (not (null contours)) "glyph Å has no contours"


testRenderGlyph :: TTF -> MSDFConfig -> IO ()
testRenderGlyph ttf cfg = do
  let mappings = cmapMappings (ttfCmap ttf)
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let mGlyph = renderGlyphMSDF cfg ttf gA
  assert (mGlyph /= Nothing) "renderGlyphMSDF returned Nothing"
  let glyph = fromMaybe (error "missing glyph") mGlyph
      bmp = glyphBitmap glyph
  assert (bmpWidth bmp > 0 && bmpHeight bmp > 0) "bitmap should have positive dimensions"
  let (lo, hi) = bounds (bmpPixels bmp)
      size = rangeSize (lo, hi)
      expected = bmpWidth bmp * bmpHeight bmp * 3
  assert (size == expected) "bitmap pixel buffer size mismatch"


testGenerateSubset :: MSDFAtlas -> IO ()
testGenerateSubset atlas = do
  let mA = lookupCodepoint atlas 65
      mB = lookupCodepoint atlas 66
  assert (mA /= Nothing) "atlas missing U+0041"
  assert (mB /= Nothing) "atlas missing U+0042"
  let gA = fromMaybe (-1) mA
      gB = fromMaybe (-1) mB
      glyphA = msdfGlyphs atlas ! gA
      glyphB = msdfGlyphs atlas ! gB
  assert (bmpWidth (glyphBitmap glyphA) > 0) "glyph A bitmap should be rendered"
  assert (bmpWidth (glyphBitmap glyphB) > 0) "glyph B bitmap should be rendered"
  let mC = lookupCodepoint atlas 67
  case mC of
    Nothing -> pure ()
    Just gC ->
      if gC == gA || gC == gB
      then pure ()
      else do
        let glyphC = msdfGlyphs atlas ! gC
        assert (bmpWidth (glyphBitmap glyphC) == 0) "glyph C bitmap should be empty in subset"


testKerningSorted :: MSDFAtlas -> IO ()
testKerningSorted atlas = do
  let ks = elems (msdfKerning atlas)
  assert (isSortedBy kernKey ks) "kerning pairs not sorted"
  assert (all (not . isNaN . kernXAdvance) ks) "kerning contains NaN"
  where
    kernKey k = (kernLeft k, kernRight k)


testMetricsOnly :: TTF -> MSDFConfig -> IO ()
testMetricsOnly ttf cfg = do
  let mappings = cmapMappings (ttfCmap ttf)
      gA = fromMaybe (-1) (lookupCodepointList 65 mappings)
  assert (gA >= 0) "glyph index for A not found"
  let glyph = glyphMetricsOnly cfg ttf gA
  assert (glyphAdvance glyph /= 0) "advance should be non-zero"
  assert (bmpWidth (glyphBitmap glyph) == 0) "metrics-only should have empty bitmap"

-- Helpers -------------------------------------------------------------------

isSortedBy :: Ord b => (a -> b) -> [a] -> Bool
isSortedBy _ [] = True
isSortedBy key (x:xs) = go (key x) xs
  where
    go _ [] = True
    go prev (y:ys) =
      let k = key y
      in if prev <= k then go k ys else False
