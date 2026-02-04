{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module MSDF.Generated
  ( generateMSDF
  , generateMSDFWithConfig
  , generateMSDFFromBytes
  , generateMSDFFromTTF
  , generateMSDFFromTTFWithTimings
  , generateMSDFOrThrow
  , generateMTSDF
  , generateMTSDFWithConfig
  , generateMTSDFFromBytes
  , generateMTSDFFromTTF
  , generateMTSDFOrThrow
  , BuildTimings(..)
  ) where

import Control.Exception (SomeException, evaluate, try, bracket_)
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Control.Parallel.Strategies (parListChunk, rdeepseq, rseq, withStrategy)
import Data.Array (Array, array, listArray, accumArray, bounds, (!), (//))
import Data.Array.Base (STUArray(..), UArray(..))
import Data.Array.ST (newArray, freeze)
import qualified Data.Array.Unboxed as UA
import Data.List (groupBy, sortOn, sort)
import Control.Monad.ST (ST, runST)
import Data.Word (Word8)
import GHC.Exts (Int(I#), Int#, copyByteArray#)
import GHC.ST (ST(..))
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import System.CPUTime (getCPUTime)
import MSDF.MSDF (MSDFConfig(..), AtlasConfig(..), GlyphSet(..), defaultMSDFConfig, glyphMetricsOnlyAt, renderGlyphMSDF)
import MSDF.TTF.GPOS
  ( KerningPairRaw(..)
  , AnchorRaw(..)
  , MarkGlyphRaw(..)
  , BaseGlyphRaw(..)
  , MarkToBaseRaw(..)
  , MarkToMarkRaw(..)
  , GPOSMarksRaw(..)
  )
import MSDF.TTF.Parser
import MSDF.TTF.Variations (mvarHheaDeltas, mvarVheaDeltas)
import MSDF.Types

data BuildTimings = BuildTimings
  { totalMs :: Double
  , renderMs :: Double
  , packPlaceMs :: Double
  , packImageMs :: Double
  , kerningMs :: Double
  , marksMs :: Double
  , packRectCount :: Int
  , atlasSize :: Maybe (Int, Int)
  } deriving (Eq, Show)

-- | Generate an MSDF atlas from a TTF file with default config.
generateMSDF :: FilePath -> IO (Either ParseError MSDFAtlas)
generateMSDF = generateMSDFWithConfig defaultMSDFConfig

-- | Generate an MSDF atlas from a TTF file with a custom config.
generateMSDFWithConfig :: MSDFConfig -> FilePath -> IO (Either ParseError MSDFAtlas)
generateMSDFWithConfig cfg path = do
  withCapabilities cfg.parallelism $ do
    parsed <- parseTTF path
    case parsed of
      Left err -> pure (Left err)
      Right ttf -> do
        result <- try (evaluate (forceAtlas (buildAtlas cfg ttf)))
        case result of
          Left (e :: SomeException) ->
            pure (Left (ParseError { context = "buildAtlas", message = show e }))
          Right atlas -> pure (Right atlas)

-- | Generate an MSDF atlas from a parsed TTF.
generateMSDFFromTTF :: MSDFConfig -> TTF -> MSDFAtlas
generateMSDFFromTTF cfg ttf = buildAtlas cfg ttf

generateMSDFFromTTFWithTimings :: MSDFConfig -> TTF -> IO (MSDFAtlas, BuildTimings)
generateMSDFFromTTFWithTimings cfg ttf =
  withCapabilities cfg.parallelism (buildAtlasWithTimings cfg ttf)

withCapabilities :: Int -> IO a -> IO a
withCapabilities caps action
  | caps <= 0 = action
  | otherwise = do
      current <- getNumCapabilities
      if current == caps
        then action
        else bracket_ (setNumCapabilities caps) (setNumCapabilities current) action

-- | Generate an MSDF atlas from a TTF ByteString.
generateMSDFFromBytes :: MSDFConfig -> BS.ByteString -> Either ParseError MSDFAtlas
generateMSDFFromBytes cfg bs =
  case parseTTFBytes bs of
    Left err -> Left err
    Right ttf -> Right (buildAtlas cfg ttf)

generateMSDFOrThrow :: FilePath -> IO MSDFAtlas
generateMSDFOrThrow path = do
  result <- generateMSDF path
  case result of
    Left err -> error (err.context ++ ": " ++ err.message)
    Right atlas -> pure atlas

-- | Generate an MTSDF atlas from a TTF file with default config.
generateMTSDF :: FilePath -> IO (Either ParseError MSDFAtlas)
generateMTSDF = generateMSDFWithConfig (defaultMSDFConfig { outputFormat = BitmapMTSDF })

-- | Generate an MTSDF atlas from a TTF file with a custom config.
generateMTSDFWithConfig :: MSDFConfig -> FilePath -> IO (Either ParseError MSDFAtlas)
generateMTSDFWithConfig cfg = generateMSDFWithConfig (cfg { outputFormat = BitmapMTSDF })

-- | Generate an MTSDF atlas from a parsed TTF.
generateMTSDFFromTTF :: MSDFConfig -> TTF -> MSDFAtlas
generateMTSDFFromTTF cfg = generateMSDFFromTTF (cfg { outputFormat = BitmapMTSDF })

-- | Generate an MTSDF atlas from a TTF ByteString.
generateMTSDFFromBytes :: MSDFConfig -> BS.ByteString -> Either ParseError MSDFAtlas
generateMTSDFFromBytes cfg bs = generateMSDFFromBytes (cfg { outputFormat = BitmapMTSDF }) bs

generateMTSDFOrThrow :: FilePath -> IO MSDFAtlas
generateMTSDFOrThrow path = do
  result <- generateMTSDF path
  case result of
    Left err -> error (err.context ++ ": " ++ err.message)
    Right atlas -> pure atlas

forceAtlas :: MSDFAtlas -> MSDFAtlas
forceAtlas atlas = atlas `deepseq` atlas

buildAtlas :: MSDFConfig -> TTF -> MSDFAtlas
buildAtlas cfg ttf =
  let numGlyphs = ttf.maxp.numGlyphs
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      AtlasConfig { packAtlas = packAtlas'
                  , atlasPadding = atlasPadding'
                  } = cfg.atlas
      loc = case ttf.variations of
        Nothing -> Nothing
        Just vars -> Just (normalizeLocation vars.fvar vars.avar cfg.variations)
      mappings = ttf.cmap.mappings
      mappingsUnique = dedupeMappings mappings
      codepointArr = accumCodepoints numGlyphs mappingsUnique
      codepointEntries = map (uncurry CodepointMapEntry) mappingsUnique
      codepointIndex = arrayFromList codepointEntries
      selector = glyphSelector cfg.glyphSet mappingsUnique
      glyphs = renderGlyphs cfg ttf codepointArr selector numGlyphs
      (glyphsPacked, atlasImage) = if packAtlas'
                                   then packAtlas cfg glyphs
                                   else (glyphs, Nothing)
      glyphArray = array (0, numGlyphs - 1) (zip [0..] glyphsPacked)
      kernPairs = buildKerning scale ttf.gpos ttf.kern
      kernArray = arrayFromList kernPairs
      (markToBase', markToMark') = scaleMarks scale ttf.gposMarks
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
      (vAscent, vDescent, vLineGap) =
        case ttf.vhea of
          Nothing -> (Nothing, Nothing, Nothing)
          Just vhea ->
            let baseVAscent = fromIntegral vhea.ascent
                baseVDescent = fromIntegral vhea.descent
                baseVLineGap = fromIntegral vhea.lineGap
                (deltaVAscent, deltaVDescent, deltaVLineGap) =
                  case (loc, ttf.variations) of
                    (Just loc', Just vars) ->
                      case vars.mvar of
                        Just mv -> mvarVheaDeltas mv loc'
                        Nothing -> (0, 0, 0)
                    _ -> (0, 0, 0)
                asc = round ((baseVAscent + deltaVAscent) * scale)
                desc = round ((baseVDescent + deltaVDescent) * scale)
                gap = round ((baseVLineGap + deltaVLineGap) * scale)
            in (Just asc, Just desc, Just gap)
  in MSDFAtlas
       { fontName = fontName
       , unitsPerEm = unitsPerEm
       , ascent = ascent
       , descent = descent
       , lineGap = lineGap
       , vAscent = vAscent
       , vDescent = vDescent
       , vLineGap = vLineGap
       , pixelSize = cfg.pixelSize
       , range = cfg.range
       , scale = scale
       , atlasPadding = atlasPadding'
       , atlas = atlasImage
      , glyphs = glyphArray
      , codepointIndex = codepointIndex
      , kerning = kernArray
      , markToBase = markToBase'
      , markToMark = markToMark'
      }

buildAtlasWithTimings :: MSDFConfig -> TTF -> IO (MSDFAtlas, BuildTimings)
buildAtlasWithTimings cfg ttf = do
  startTotal <- getCPUTime
  let numGlyphs = ttf.maxp.numGlyphs
      unitsPerEm = ttf.head.unitsPerEm
      scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
      AtlasConfig { packAtlas = packAtlas'
                  , atlasPadding = atlasPadding'
                  , buildAtlasImage = buildAtlasImage'
                  } = cfg.atlas
      loc = case ttf.variations of
        Nothing -> Nothing
        Just vars -> Just (normalizeLocation vars.fvar vars.avar cfg.variations)
      mappings = ttf.cmap.mappings
      mappingsUnique = dedupeMappings mappings
      codepointArr = accumCodepoints numGlyphs mappingsUnique
      codepointEntries = map (uncurry CodepointMapEntry) mappingsUnique
      codepointIndex = arrayFromList codepointEntries
      selector = glyphSelector cfg.glyphSet mappingsUnique
  (glyphs, tRender) <- timeEval (renderGlyphs cfg ttf codepointArr selector numGlyphs)
  let pad = max 0 atlasPadding'
      rects =
        [ PackRect i (bmp.width + 2 * pad) (bmp.height + 2 * pad) bmp.width bmp.height
        | (i, g) <- zip [0..] glyphs
        , let bmp = g.bitmap
        , bmp.width > 0
        , bmp.height > 0
        ]
  (glyphsPacked, atlasImage, tPlace, tImage, packRectCount', atlasSize') <-
    if packAtlas' && not (null rects)
    then do
      (placementRes, tPlace') <- timeEval (chooseAtlasSize cfg rects)
      case placementRes of
        Nothing -> pure (glyphs, Nothing, tPlace', 0, length rects, Nothing)
        Just (atlasW, atlasH, placements) -> do
          let placementArr = placementsToArray (length glyphs) atlasW atlasH pad placements
              glyphs' = [ applyPlacement g (placementArr ! i) | (i, g) <- zip [0..] glyphs ]
          if buildAtlasImage'
            then do
              (atlasImage', tImage') <- timeEval (buildAtlasImage cfg.outputFormat atlasW atlasH pad cfg.parallelism placements glyphs)
              pure (glyphs', Just atlasImage', tPlace', tImage', length rects, Just (atlasW, atlasH))
            else pure (glyphs', Nothing, tPlace', 0, length rects, Just (atlasW, atlasH))
    else pure (glyphs, Nothing, 0, 0, length rects, Nothing)
  (kernPairs, tKerning) <- timeEval (buildKerning scale ttf.gpos ttf.kern)
  let kernArray = arrayFromList kernPairs
  ((markToBase', markToMark'), tMarks) <- timeEval (scaleMarks scale ttf.gposMarks)
  let fontName = buildFontName ttf.name
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
      (vAscent, vDescent, vLineGap) =
        case ttf.vhea of
          Nothing -> (Nothing, Nothing, Nothing)
          Just vhea ->
            let baseVAscent = fromIntegral vhea.ascent
                baseVDescent = fromIntegral vhea.descent
                baseVLineGap = fromIntegral vhea.lineGap
                (deltaVAscent, deltaVDescent, deltaVLineGap) =
                  case (loc, ttf.variations) of
                    (Just loc', Just vars) ->
                      case vars.mvar of
                        Just mv -> mvarVheaDeltas mv loc'
                        Nothing -> (0, 0, 0)
                    _ -> (0, 0, 0)
                asc = round ((baseVAscent + deltaVAscent) * scale)
                desc = round ((baseVDescent + deltaVDescent) * scale)
                gap = round ((baseVLineGap + deltaVLineGap) * scale)
            in (Just asc, Just desc, Just gap)
      glyphArray = array (0, numGlyphs - 1) (zip [0..] glyphsPacked)
      atlas = MSDFAtlas
        { fontName = fontName
        , unitsPerEm = unitsPerEm
        , ascent = ascent
        , descent = descent
        , lineGap = lineGap
        , vAscent = vAscent
        , vDescent = vDescent
        , vLineGap = vLineGap
        , pixelSize = cfg.pixelSize
        , range = cfg.range
        , scale = scale
        , atlasPadding = atlasPadding'
        , atlas = atlasImage
        , glyphs = glyphArray
        , codepointIndex = codepointIndex
        , kerning = kernArray
        , markToBase = markToBase'
        , markToMark = markToMark'
        }
  atlas `deepseq` pure ()
  endTotal <- getCPUTime
  let totalMs' = fromIntegral (endTotal - startTotal) / 1.0e9
      timings = BuildTimings
        { totalMs = totalMs'
        , renderMs = tRender
        , packPlaceMs = tPlace
        , packImageMs = tImage
        , kerningMs = tKerning
        , marksMs = tMarks
        , packRectCount = packRectCount'
        , atlasSize = atlasSize'
        }
  pure (atlas, timings)

timeEval :: NFData a => a -> IO (a, Double)
timeEval thunk = do
  start <- getCPUTime
  result <- evaluate thunk
  result `deepseq` pure ()
  end <- getCPUTime
  let elapsedMs = fromIntegral (end - start) / 1.0e9
  pure (result, elapsedMs)

buildGlyph :: TTF -> MSDFConfig -> Array Int [Int] -> (Int -> Bool) -> Int -> GlyphMSDF
buildGlyph ttf cfg codepointArr shouldRender glyphIndex =
  let codepoints = reverse (codepointArr ! glyphIndex)
      base = if shouldRender glyphIndex
             then renderGlyphMSDF cfg ttf glyphIndex
             else glyphMetricsOnlyAt Nothing cfg ttf glyphIndex
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

scaleMarks :: Double -> GPOSMarksRaw -> ([MarkToBase], [MarkToMark])
scaleMarks scale marks =
  let base = map (scaleMarkToBase scale) marks.markToBase
      mkmk = map (scaleMarkToMark scale) marks.markToMark
  in (base, mkmk)

scaleMarkToBase :: Double -> MarkToBaseRaw -> MarkToBase
scaleMarkToBase scale m =
  MarkToBase
    { classCount = m.classCount
    , marks = fmap (fmap (scaleMarkGlyph scale)) m.marks
    , bases = fmap (fmap (scaleBaseGlyph scale)) m.bases
    }

scaleMarkToMark :: Double -> MarkToMarkRaw -> MarkToMark
scaleMarkToMark scale m =
  MarkToMark
    { classCount = m.classCount
    , marks1 = fmap (fmap (scaleMarkGlyph scale)) m.marks1
    , marks2 = fmap (fmap (scaleBaseGlyph scale)) m.marks2
    }

scaleMarkGlyph :: Double -> MarkGlyphRaw -> MarkGlyph
scaleMarkGlyph scale m =
  MarkGlyph
    { markClass = m.markClass
    , anchor = scaleAnchor scale m.anchor
    }

scaleBaseGlyph :: Double -> BaseGlyphRaw -> BaseGlyph
scaleBaseGlyph scale b =
  BaseGlyph
    { anchors = fmap (fmap (scaleAnchor scale)) b.anchors
    }

scaleAnchor :: Double -> AnchorRaw -> Anchor
scaleAnchor scale a =
  Anchor
    { x = fromIntegral a.x * scale
    , y = fromIntegral a.y * scale
    }

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
      caps = cfg.parallelism
      chunk =
        if caps > 0
        then max 1 ((numGlyphs + (caps * 2) - 1) `div` (caps * 2))
        else 0
  in if chunk > 0
     then withStrategy (parListChunk chunk rdeepseq) glyphs
     else glyphs

-- Atlas packing -------------------------------------------------------------

data PackRect = PackRect
  { glyphIndex :: Int
  , slotW :: Int
  , slotH :: Int
  , bmpW :: Int
  , bmpH :: Int
  }

data PackPlacement = PackPlacement
  { glyphIndex :: Int
  , slotX :: Int
  , slotY :: Int
  , slotW :: Int
  , slotH :: Int
  , bmpW :: Int
  , bmpH :: Int
  }

instance NFData PackPlacement where
  rnf p =
    p.glyphIndex `seq`
    p.slotX `seq`
    p.slotY `seq`
    p.slotW `seq`
    p.slotH `seq`
    p.bmpW `seq`
    p.bmpH `seq`
    ()

data SkylineNode = SkylineNode
  { skyX :: Int
  , skyY :: Int
  , skyW :: Int
  } deriving (Eq, Show)

packAtlas :: MSDFConfig -> [GlyphMSDF] -> ([GlyphMSDF], Maybe AtlasImage)
packAtlas cfg glyphs =
  let pad = max 0 cfg.atlas.atlasPadding
      buildImage = cfg.atlas.buildAtlasImage
      rects = [ PackRect i (bmp.width + 2 * pad) (bmp.height + 2 * pad) bmp.width bmp.height
              | (i, g) <- zip [0..] glyphs
              , let bmp = g.bitmap
              , bmp.width > 0
              , bmp.height > 0
              ]
  in if null rects
     then (glyphs, Nothing)
     else
       case chooseAtlasSize cfg rects of
         Nothing -> (glyphs, Nothing)
         Just (atlasW, atlasH, placements) ->
           let placementArr = placementsToArray (length glyphs) atlasW atlasH pad placements
               glyphs' = [ applyPlacement g (placementArr ! i) | (i, g) <- zip [0..] glyphs ]
               atlasImage =
                 if buildImage
                 then Just (buildAtlasImage cfg.outputFormat atlasW atlasH pad cfg.parallelism placements glyphs)
                 else Nothing
           in (glyphs', atlasImage)

applyPlacement :: GlyphMSDF -> Maybe GlyphPlacement -> GlyphMSDF
applyPlacement glyph placement =
  glyph { placement = placement }

placementsToArray :: Int -> Int -> Int -> Int -> [PackPlacement] -> Array Int (Maybe GlyphPlacement)
placementsToArray count atlasW atlasH pad placements =
  let base = array (0, count - 1) [ (i, Nothing) | i <- [0 .. count - 1] ]
      entries = [ (p.glyphIndex, Just (toPlacement atlasW atlasH pad p)) | p <- placements ]
  in base // entries

toPlacement :: Int -> Int -> Int -> PackPlacement -> GlyphPlacement
toPlacement atlasW atlasH pad p =
  let gx = p.slotX + pad
      gy = p.slotY + pad
      u0 = fromIntegral gx / fromIntegral atlasW
      v0 = fromIntegral gy / fromIntegral atlasH
      u1 = fromIntegral (gx + p.bmpW) / fromIntegral atlasW
      v1 = fromIntegral (gy + p.bmpH) / fromIntegral atlasH
  in GlyphPlacement
     { x = gx
     , y = gy
     , width = p.bmpW
     , height = p.bmpH
     , u0 = u0
     , v0 = v0
     , u1 = u1
     , v1 = v1
     }

chooseAtlasSize :: MSDFConfig -> [PackRect] -> Maybe (Int, Int, [PackPlacement])
chooseAtlasSize cfg rects =
  let AtlasConfig { atlasMaxSize = atlasMaxSize'
                  , atlasMinSize = atlasMinSize'
                  , atlasPowerOfTwo = atlasPowerOfTwo'
                  } = cfg.atlas
      maxDim = max 1 atlasMaxSize'
      minDim0 = max atlasMinSize' (maximum [ r.slotW | r <- rects ])
      minDim = if atlasPowerOfTwo' then nextPow2 minDim0 else minDim0
      sizes = if atlasPowerOfTwo'
              then takeWhile (<= maxDim) (iterate (*2) minDim)
              else [minDim .. maxDim]
      sorted = sortOn (\r -> (-r.slotH, -r.slotW, r.glyphIndex)) rects
      trySize [] = Nothing
      trySize (w:ws) =
        case packWithWidth w sorted of
          Nothing -> trySize ws
          Just (placements, hUsed) ->
            let h = if atlasPowerOfTwo' then nextPow2 hUsed else hUsed
            in if h <= maxDim then Just (w, h, placements) else trySize ws
  in trySize sizes

packWithWidth :: Int -> [PackRect] -> Maybe ([PackPlacement], Int)
packWithWidth width rects = go [SkylineNode 0 0 width] [] 0 rects
  where
    go _ acc maxH [] = Just (reverse acc, maxH)
    go skyline acc maxH (r:rs) =
      case placeRect width skyline r of
        Nothing -> Nothing
        Just (placement, skyline') ->
          let maxH' = max maxH (placement.slotY + placement.slotH)
          in go skyline' (placement:acc) maxH' rs

placeRect :: Int -> [SkylineNode] -> PackRect -> Maybe (PackPlacement, [SkylineNode])
placeRect width skyline r
  | r.slotW > width = Nothing
  | otherwise =
      case findPosition width skyline r of
        Nothing -> Nothing
        Just (idx, xPos, yPos) ->
          let placement = PackPlacement
                { glyphIndex = r.glyphIndex
                , slotX = xPos
                , slotY = yPos
                , slotW = r.slotW
                , slotH = r.slotH
                , bmpW = r.bmpW
                , bmpH = r.bmpH
                }
              newNode = SkylineNode xPos (yPos + r.slotH) r.slotW
              (before, after) = splitAt idx skyline
              skyline' = mergeSkyline (before ++ [newNode] ++ shrinkAfter newNode after)
          in Just (placement, skyline')

findPosition :: Int -> [SkylineNode] -> PackRect -> Maybe (Int, Int, Int)
findPosition width skyline r =
  let candidates = zip [0..] skyline
      pick best [] = best
      pick best ((idx, node):rest)
        | node.skyX + r.slotW > width = pick best rest
        | otherwise =
            case fitAt idx r.slotW skyline of
              Nothing -> pick best rest
              Just y ->
                case best of
                  Nothing -> pick (Just (idx, node.skyX, y)) rest
                  Just (_bIdx, bX, bY) ->
                    if y < bY || (y == bY && node.skyX < bX)
                    then pick (Just (idx, node.skyX, y)) rest
                    else pick best rest
  in pick Nothing candidates

fitAt :: Int -> Int -> [SkylineNode] -> Maybe Int
fitAt idx rectW skyline =
  case drop idx skyline of
    [] -> Nothing
    (n:ns) -> go rectW (n.skyY) (n:ns)
  where
    go widthLeft y nodes
      | widthLeft <= 0 = Just y
      | otherwise =
          case nodes of
            [] -> Nothing
            (n:ns) ->
              let y' = max y (n.skyY)
                  widthLeft' = widthLeft - n.skyW
              in go widthLeft' y' ns

shrinkAfter :: SkylineNode -> [SkylineNode] -> [SkylineNode]
shrinkAfter newNode nodes =
  let newEnd = newNode.skyX + newNode.skyW
  in case nodes of
       [] -> []
       (n:ns)
         | n.skyX >= newEnd -> n : ns
         | otherwise ->
             let overlap = newEnd - n.skyX
             in if overlap < n.skyW
                then n { skyX = n.skyX + overlap, skyW = n.skyW - overlap } : ns
                else shrinkAfter newNode ns

mergeSkyline :: [SkylineNode] -> [SkylineNode]
mergeSkyline [] = []
mergeSkyline (n:ns) = go n ns
  where
    go cur [] = [cur]
    go cur (n':rest)
      | cur.skyY == n'.skyY && cur.skyX + cur.skyW == n'.skyX =
          let merged = cur { skyW = cur.skyW + n'.skyW }
          in go merged rest
      | otherwise = cur : go n' rest

nextPow2 :: Int -> Int
nextPow2 n
  | n <= 1 = 1
  | otherwise = go 1
  where
    go k = if k >= n then k else go (k * 2)

buildAtlasImage :: BitmapFormat -> Int -> Int -> Int -> Int -> [PackPlacement] -> [GlyphMSDF] -> AtlasImage
buildAtlasImage fmt width height pad parallelism placements glyphs =
  let channels = bitmapChannels fmt
      glyphMap = array (0, length glyphs - 1) (zip [0..] glyphs)
      pixels =
        if parallelism > 1 && height > 1 && length placements > 1
        then buildAtlasImageParallel fmt width height pad channels parallelism placements glyphMap
        else buildAtlasImageSeq fmt width height pad channels placements glyphMap
  in AtlasImage { width = width, height = height, format = fmt, pixels = pixels }

buildAtlasImageSeq :: BitmapFormat -> Int -> Int -> Int -> Int -> [PackPlacement] -> Array Int GlyphMSDF -> UA.UArray Int Word8
buildAtlasImageSeq _fmt width height pad channels placements glyphMap =
  let total = width * height * channels
  in runST $ do
    -- Fill with black so padding represents "outside" (negative distance).
    arr <- (newArray (0, total - 1) 0 :: ST s (STUArray s Int Word8))
    forM_ placements $ \p -> do
      let glyph = glyphMap ! p.glyphIndex
          bmp = glyph.bitmap
          bw = p.bmpW
          bh = p.bmpH
      when (bw > 0 && bh > 0) $ do
        let src = bmp.pixels
            dstX = p.slotX + pad
            dstY = p.slotY + pad
            rowBytes = bw * channels
        forM_ [0 .. bh - 1] $ \y -> do
          let srcBase = y * rowBytes
              dstBase = ((dstY + y) * width + dstX) * channels
          copyBytesST arr dstBase src srcBase rowBytes
    freeze arr

buildAtlasImageParallel :: BitmapFormat -> Int -> Int -> Int -> Int -> Int -> [PackPlacement] -> Array Int GlyphMSDF -> UA.UArray Int Word8
buildAtlasImageParallel fmt width height pad channels parallelism placements glyphMap =
  let bands = bandsForHeight height (min parallelism height)
      buildBand (yStart, bandH) =
        AtlasBand yStart bandH (buildAtlasImageBand fmt width pad channels yStart bandH placements glyphMap)
      bandsBuilt = withStrategy (parListChunk 1 rseq) (map buildBand bands)
      total = width * height * channels
      pixels = runST $ do
        arr <- (newArray (0, total - 1) 0 :: ST s (STUArray s Int Word8))
        forM_ bandsBuilt $ \band -> do
          let dstOff = band.startRow * width * channels
              len = band.bandHeight * width * channels
          copyBytesST arr dstOff band.pixels 0 len
        freeze arr
  in pixels

data AtlasBand = AtlasBand
  { startRow :: Int
  , bandHeight :: Int
  , pixels :: UA.UArray Int Word8
  }

bandsForHeight :: Int -> Int -> [(Int, Int)]
bandsForHeight height bands =
  let bands' = max 1 (min bands height)
      bandH = (height + bands' - 1) `div` bands'
      starts = [0, bandH .. height - 1]
  in [ (y, min bandH (height - y)) | y <- starts ]

buildAtlasImageBand :: BitmapFormat -> Int -> Int -> Int -> Int -> Int -> [PackPlacement] -> Array Int GlyphMSDF -> UA.UArray Int Word8
buildAtlasImageBand _fmt width pad channels bandStart bandH placements glyphMap =
  let total = bandH * width * channels
      bandEnd = bandStart + bandH
  in runST $ do
    arr <- (newArray (0, total - 1) 0 :: ST s (STUArray s Int Word8))
    forM_ placements $ \p -> do
      let glyph = glyphMap ! p.glyphIndex
          bmp = glyph.bitmap
          bw = p.bmpW
          bh = p.bmpH
      when (bw > 0 && bh > 0) $ do
        let dstX = p.slotX + pad
            dstY = p.slotY + pad
            glyphTop = dstY
            glyphBottom = dstY + bh
        when (glyphBottom > bandStart && glyphTop < bandEnd) $ do
          let startY = max 0 (bandStart - glyphTop)
              endY = min bh (bandEnd - glyphTop)
              rowBytes = bw * channels
              src = bmp.pixels
          forM_ [startY .. endY - 1] $ \y -> do
            let srcBase = y * rowBytes
                dstBase = ((glyphTop + y - bandStart) * width + dstX) * channels
            copyBytesST arr dstBase src srcBase rowBytes
    freeze arr

copyBytesST :: STUArray s Int Word8 -> Int -> UA.UArray Int Word8 -> Int -> Int -> ST s ()
copyBytesST (STUArray _ _ _ dst#) dstOff (UArray _ _ _ src#) srcOff len =
  ST $ \s ->
    case copyByteArray# src# (intToInt# srcOff) dst# (intToInt# dstOff) (intToInt# len) s of
      s' -> (# s', () #)

intToInt# :: Int -> Int#
intToInt# (I# i#) = i#

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
