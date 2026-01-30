module MSDF.Generated
  ( generateMSDF
  , generateMSDFWithConfig
  , generateMSDFOrThrow
  ) where

import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)
import Data.Array (Array, array, listArray, accumArray, bounds, (!))
import Data.List (groupBy, sortOn, sort)
import MSDF.MSDF
import MSDF.TTF.GPOS (KerningPairRaw(..))
import MSDF.TTF.Parser
import MSDF.TTF.Variations (mvarHheaDeltas)
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
    Left err -> error (err.context ++ ": " ++ err.message)
    Right atlas -> pure atlas

buildAtlas :: MSDFConfig -> TTF -> MSDFAtlas
buildAtlas cfg ttf =
  let numGlyphs = ttf.maxp.numGlyphs
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      loc = case ttf.variations of
        Nothing -> Nothing
        Just vars -> Just (normalizeLocation vars.fvar vars.avar cfg.variations)
      mappings = ttf.cmap.mappings
      mappingsUnique = dedupeMappings mappings
      codepointArr = accumCodepoints numGlyphs mappings
      codepointEntries = map (uncurry CodepointMapEntry) mappingsUnique
      codepointIndex = arrayFromList codepointEntries
      selector = glyphSelector cfg.glyphSet mappings
      glyphs = renderGlyphs cfg ttf codepointArr selector numGlyphs
      glyphArray = array (0, numGlyphs - 1) (zip [0..] glyphs)
      kernPairs = buildKerning scale ttf.gpos ttf.kern
      kernArray = arrayFromList kernPairs
      fontName = buildFontName ttf.name
      baseAscent = fromIntegral ttf.hhea.ascent
      baseDescent = fromIntegral ttf.hhea.descent
      baseLineGap = fromIntegral ttf.hhea.lineGap
      (deltaAscent, deltaDescent, deltaLineGap) =
        case (loc, ttf.variations) of
          (Just loc', Just vars) ->
            case vars.mvar of
              Just mv -> mvarHheaDeltas mv loc'
              Nothing -> (0, 0, 0)
          _ -> (0, 0, 0)
      ascent = round ((baseAscent + deltaAscent) * scale)
      descent = round ((baseDescent + deltaDescent) * scale)
      lineGap = round ((baseLineGap + deltaLineGap) * scale)
  in MSDFAtlas
       { fontName = fontName
       , unitsPerEm = unitsPerEm
       , ascent = ascent
       , descent = descent
       , lineGap = lineGap
       , pixelSize = cfg.pixelSize
       , range = cfg.range
       , scale = scale
       , glyphs = glyphArray
       , codepointIndex = codepointIndex
       , kerning = kernArray
       }

buildGlyph :: TTF -> MSDFConfig -> Array Int [Int] -> (Int -> Bool) -> Int -> GlyphMSDF
buildGlyph ttf cfg codepointArr shouldRender glyphIndex =
  let codepoints = reverse (codepointArr ! glyphIndex)
      base = if shouldRender glyphIndex
             then renderGlyphMSDF cfg ttf glyphIndex
             else glyphMetricsOnly cfg ttf glyphIndex
  in base { codepoints = codepoints }

accumCodepoints :: Int -> [(Int, Int)] -> Array Int [Int]
accumCodepoints numGlyphs mappings =
  let pairs = [ (g, c) | (c, g) <- mappings, g >= 0, g < numGlyphs ]
      arr = accumArray (flip (:)) [] (0, numGlyphs - 1) pairs
  in fmap uniqueSorted arr

arrayFromList :: [a] -> Array Int a
arrayFromList xs =
  if null xs
  then array (0, -1) []
  else listArray (0, length xs - 1) xs

dedupeMappings :: [(Int, Int)] -> [(Int, Int)]
dedupeMappings mappings =
  let sorted = sortOn fst mappings
      groups = groupBy (\(c, _) (c', _) -> c == c') sorted
      pick grp =
        case grp of
          [] -> Nothing
          ((cp, _):_) ->
            let gid = minimum (map snd grp)
            in Just (cp, gid)
  in [ x | Just x <- map pick groups ]

kernKey :: KerningPair -> (Int, Int)
kernKey k = (k.left, k.right)

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
  { left = kp.left
  , right = kp.right
  , xAdvance = fromIntegral kp.xAdvance * scale
  }

buildFontName :: NameTable -> String
buildFontName nt =
  let fam = nt.family
      sty = nt.style
  in case (fam, sty) of
       ("", "") -> ""
       (f, "") -> f
       ("", s) -> s
       (f, s) -> f ++ " " ++ s

-- | Select glyph indices for rendering based on config.
glyphSelector :: GlyphSet -> [(Int, Int)] -> (Int -> Bool)
glyphSelector set mappings =
  case set of
    GlyphSetAll -> const True
    GlyphSetNone -> const False
    GlyphSetCodepoints cps ->
      let cpsSorted = sort cps
          cpsArr = if null cpsSorted then Nothing else Just (listArray (0, length cpsSorted - 1) cpsSorted)
          allowed = uniqueSorted [ g | (c, g) <- mappings, memberSorted cpsArr c ]
          allowedArr = if null allowed then Nothing else Just (listArray (0, length allowed - 1) allowed)
      in memberSorted allowedArr

renderGlyphs :: MSDFConfig -> TTF -> Array Int [Int] -> (Int -> Bool) -> Int -> [GlyphMSDF]
renderGlyphs cfg ttf codepointArr shouldRender numGlyphs =
  let glyphs = [ buildGlyph ttf cfg codepointArr shouldRender i | i <- [0 .. numGlyphs - 1] ]
      chunk = cfg.parallelism
  in if chunk > 0
     then withStrategy (parListChunk chunk rdeepseq) glyphs
     else glyphs

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
