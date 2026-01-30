module MSDF.Generated
  ( generateMSDF
  , generateMSDFWithConfig
  , generateMSDFOrThrow
  ) where

import Data.Array (Array, array, listArray, accumArray, bounds, (!))
import Data.List (sortOn, sort)
import MSDF.MSDF
import MSDF.TTF.GPOS (KerningPairRaw(..))
import MSDF.TTF.Parser
import MSDF.Types

-- | Generate an MSDF atlas from a TTF file with default config.
generateMSDF :: FilePath -> IO (Either ParseError MSDFAtlas)
generateMSDF = generateMSDFWithConfig defaultMSDFConfig

-- | Generate an MSDF atlas from a TTF file with a custom config.
generateMSDFWithConfig :: MSDFConfig -> FilePath -> IO (Either ParseError MSDFAtlas)
generateMSDFWithConfig cfg path = do
  parsed <- parseTTF path
  case parsed of
    Left err -> pure (Left err)
    Right ttf -> pure (Right (buildAtlas cfg ttf))

generateMSDFOrThrow :: FilePath -> IO MSDFAtlas
generateMSDFOrThrow path = do
  result <- generateMSDF path
  case result of
    Left err -> error (peContext err ++ ": " ++ peMessage err)
    Right atlas -> pure atlas

buildAtlas :: MSDFConfig -> TTF -> MSDFAtlas
buildAtlas cfg ttf =
  let numGlyphs = maxpNumGlyphs (ttfMaxp ttf)
      unitsPerEm = headUnitsPerEm (ttfHead ttf)
      scale = fromIntegral (cfgPixelSize cfg) / fromIntegral unitsPerEm
      mappings = cmapMappings (ttfCmap ttf)
      codepointArr = accumCodepoints numGlyphs mappings
      codepointEntries = map (uncurry CodepointMapEntry) (sortOn fst mappings)
      codepointIndex = arrayFromList codepointEntries
      selectedGlyphs = glyphSelection cfg mappings
      glyphs = [ buildGlyph ttf cfg codepointArr selectedGlyphs i | i <- [0 .. numGlyphs - 1] ]
      glyphArray = array (0, numGlyphs - 1) (zip [0..] glyphs)
      kernPairs = buildKerning scale (ttfGpos ttf) (ttfKern ttf)
      kernArray = arrayFromList kernPairs
      fontName = buildFontName (ttfName ttf)
      ascent = round (fromIntegral (hheaAscent (ttfHhea ttf)) * scale)
      descent = round (fromIntegral (hheaDescent (ttfHhea ttf)) * scale)
      lineGap = round (fromIntegral (hheaLineGap (ttfHhea ttf)) * scale)
  in MSDFAtlas
       { msdfFontName = fontName
       , msdfUnitsPerEm = unitsPerEm
       , msdfAscent = ascent
       , msdfDescent = descent
       , msdfLineGap = lineGap
       , msdfPixelSize = cfgPixelSize cfg
       , msdfRange = cfgRange cfg
       , msdfScale = scale
       , msdfGlyphs = glyphArray
       , msdfCodepointIndex = codepointIndex
       , msdfKerning = kernArray
       }

buildGlyph :: TTF -> MSDFConfig -> Array Int [Int] -> [Int] -> Int -> GlyphMSDF
buildGlyph ttf cfg codepointArr selected glyphIndex =
  let codepoints = reverse (codepointArr ! glyphIndex)
      render = if null selected then True else glyphIndex `elem` selected
      base = if render
             then renderGlyphMSDF cfg ttf glyphIndex
             else glyphMetricsOnly cfg ttf glyphIndex
  in base { glyphCodepoints = codepoints }

accumCodepoints :: Int -> [(Int, Int)] -> Array Int [Int]
accumCodepoints numGlyphs mappings =
  let pairs = [ (g, c) | (c, g) <- mappings, g >= 0, g < numGlyphs ]
  in accumArray (flip (:)) [] (0, numGlyphs - 1) pairs

arrayFromList :: [a] -> Array Int a
arrayFromList xs =
  if null xs
  then array (0, -1) []
  else listArray (0, length xs - 1) xs

kernKey :: KerningPair -> (Int, Int)
kernKey k = (kernLeft k, kernRight k)

buildKerning :: Double -> [KerningPairRaw] -> [KerningPairRaw] -> [KerningPair]
buildKerning scale gpos kern =
  let gposPairs = sortOn kernKey (map (toKerning scale) gpos)
      kernPairs = sortOn kernKey (map (toKerning scale) kern)
  in mergeKerning gposPairs kernPairs

mergeKerning :: [KerningPair] -> [KerningPair] -> [KerningPair]
mergeKerning [] ys = ys
mergeKerning xs [] = xs
mergeKerning (x:xs) (y:ys) =
  case compare (kernKey x) (kernKey y) of
    LT -> x : mergeKerning xs (y:ys)
    GT -> y : mergeKerning (x:xs) ys
    EQ -> x : mergeKerning xs ys

toKerning :: Double -> KerningPairRaw -> KerningPair
toKerning scale kp = KerningPair
  { kernLeft = kpLeft kp
  , kernRight = kpRight kp
  , kernXAdvance = fromIntegral (kpXAdvance kp) * scale
  }

buildFontName :: NameTable -> String
buildFontName nt =
  let fam = nameFamily nt
      sty = nameStyle nt
  in case (fam, sty) of
       ("", "") -> ""
       (f, "") -> f
       ("", s) -> s
       (f, s) -> f ++ " " ++ s

-- | Select glyph indices for rendering based on config.
glyphSelection :: MSDFConfig -> [(Int, Int)] -> [Int]
glyphSelection cfg mappings =
  case cfgGlyphSet cfg of
    GlyphSetAll -> []
    GlyphSetCodepoints cps ->
      let cpsSorted = sort cps
          cpsArr = if null cpsSorted then Nothing else Just (listArray (0, length cpsSorted - 1) cpsSorted)
      in uniqueSorted [ g | (c, g) <- mappings, memberSorted cpsArr c ]

memberSorted :: Maybe (Array Int Int) -> Int -> Bool
memberSorted Nothing _ = False
memberSorted (Just arr) x = go lo hi
  where
    (lo, hi) = bounds arr
    go l h
      | l > h = False
      | otherwise =
          let mid = (l + h) `div` 2
              v = arr ! mid
          in if x == v
             then True
             else if x < v
                  then go l (mid - 1)
                  else go (mid + 1) h

uniqueSorted :: [Int] -> [Int]
uniqueSorted xs =
  case sort xs of
    [] -> []
    (y:ys) -> y : go y ys
  where
    go _ [] = []
    go prev (z:zs)
      | z == prev = go prev zs
      | otherwise = z : go z zs
