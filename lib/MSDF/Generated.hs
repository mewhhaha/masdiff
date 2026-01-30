module MSDF.Generated
  ( generateMSDF
  , generateMSDFWithConfig
  ) where

import Data.Array (Array, array, listArray, accumArray, (!))
import Data.List (sortOn)
import MSDF.MSDF
import MSDF.TTF.GPOS (KerningPairRaw(..))
import MSDF.TTF.Parser
import MSDF.Types

-- | Generate an MSDF atlas from a TTF file with default config.
generateMSDF :: FilePath -> IO MSDFAtlas
generateMSDF = generateMSDFWithConfig defaultMSDFConfig

-- | Generate an MSDF atlas from a TTF file with a custom config.
generateMSDFWithConfig :: MSDFConfig -> FilePath -> IO MSDFAtlas
generateMSDFWithConfig cfg path = do
  ttf <- parseTTF path
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
      kernArray = arrayFromList (sortOn kernKey kernPairs)
      fontName = buildFontName (ttfName ttf)
      ascent = round (fromIntegral (hheaAscent (ttfHhea ttf)) * scale)
      descent = round (fromIntegral (hheaDescent (ttfHhea ttf)) * scale)
      lineGap = round (fromIntegral (hheaLineGap (ttfHhea ttf)) * scale)
  pure MSDFAtlas
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
             then case renderGlyphMSDF cfg ttf glyphIndex of
                    Just g -> g
                    Nothing -> error "renderGlyphMSDF failed"
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
  let gposPairs = map (toKerning scale) gpos
      gposKeys = map kernKey gposPairs
      kernPairs = [ k' | k <- kern
                       , let k' = toKerning scale k
                       , kernKey k' `notElem` gposKeys ]
  in gposPairs ++ kernPairs

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
    GlyphSetCodepoints cps -> uniqueSorted [ g | (c, g) <- mappings, c `elem` cps ]

uniqueSorted :: [Int] -> [Int]
uniqueSorted = foldl' (\acc x -> if null acc || last acc /= x then acc ++ [x] else acc) [] . sort
  where
    sort = foldl' insert []
    insert [] x = [x]
    insert (y:ys) x
      | x <= y = x:y:ys
      | otherwise = y : insert ys x
