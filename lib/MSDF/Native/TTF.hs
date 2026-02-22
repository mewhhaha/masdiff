{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Native.TTF
  ( VariationAxes (..),
    loadOutlineIO,
    loadOutlinesIO,
  )
where

import qualified Data.ByteString as BS
import Data.Bits ((.&.), (.|.), shiftL, testBit)
import Data.Char (chr)
import Data.List (find, findIndex)
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16, Word32)
import MSDF.Native.Types
  ( Contour (..),
    Edge (..),
    Outline (..),
    Pt (..),
  )
import MSDF.Types
  ( GlyphCode,
    unGlyphCode,
  )

data VariationAxes = VariationAxes
  { wght :: Maybe Double,
    opsz :: Maybe Double
  }
  deriving stock (Eq, Show)

data Tbl = Tbl
  { off :: !Int,
    len :: !Int
  }
  deriving stock (Eq, Show)

data Sfnt = Sfnt
  { bs :: !BS.ByteString,
    tabs :: !(Map.Map String Tbl)
  }
  deriving stock (Eq, Show)

data Head = Head
  { upem :: !Double,
    locFmt :: !Int
  }
  deriving stock (Eq, Show)

data Font = Font
  { sfnt :: !Sfnt,
    head :: !Head,
    cmap :: !Cmap,
    numGlyphs :: !Int,
    numHMetrics :: !Int,
    loca :: ![Int],
    glyf :: !Tbl,
    hmtx :: !Tbl,
    var :: !(Maybe Var)
  }
  deriving stock (Eq, Show)

data Cmap
  = CMap4 !Cmap4
  | CMap12 !Cmap12
  deriving stock (Eq, Show)

data Cmap4 = Cmap4
  { tab :: !BS.ByteString,
    segCnt :: !Int,
    endCodes :: ![Int],
    startCodes :: ![Int],
    idDeltas :: ![Int],
    idRangeOffsets :: ![Int],
    idRangeOffPos :: !Int
  }
  deriving stock (Eq, Show)

data Cmap12 = Cmap12
  { groups :: ![(Int, Int, Int)]
  }
  deriving stock (Eq, Show)

data RawPt = RawPt
  { x :: !Double,
    y :: !Double,
    on :: !Bool
  }
  deriving stock (Eq, Show)

data Seg
  = SegLine !RawPt !RawPt
  | SegQuad !RawPt !RawPt !RawPt
  deriving stock (Eq, Show)

data Comp = Comp
  { flg :: !Int,
    gid :: !Int,
    dx :: !Double,
    dy :: !Double,
    a :: !Double,
    b :: !Double,
    c :: !Double,
    d :: !Double
  }
  deriving stock (Eq, Show)

data AxisRange = AxisRange
  { tag :: !Word32,
    lo :: !Double,
    def :: !Double,
    hi :: !Double
  }
  deriving stock (Eq, Show)

data Var = Var
  { loc :: ![Double],
    gvar :: !Gvar
  }
  deriving stock (Eq, Show)

data Gvar = Gvar
  { axisCnt :: !Int,
    glyphCnt :: !Int,
    sharedTuples :: ![[Double]],
    offs :: ![Int],
    dataOff :: !Int,
    tab :: !BS.ByteString
  }
  deriving stock (Eq, Show)

data TupleHdr = TupleHdr
  { dataSize :: !Int,
    peak :: ![Double],
    inter :: !(Maybe ([Double], [Double])),
    privatePts :: !Bool
  }
  deriving stock (Eq, Show)

data TupleRec = TupleRec
  { hdr :: !TupleHdr,
    pts :: ![Int],
    dx :: ![Int],
    dy :: ![Int]
  }
  deriving stock (Eq, Show)

loadOutlineIO :: FilePath -> VariationAxes -> GlyphCode -> IO (Either String Outline)
loadOutlineIO fontPath axes glyph = do
  raw <- BS.readFile fontPath
  pure (decodeOutline raw axes glyph)

loadOutlinesIO :: FilePath -> VariationAxes -> [GlyphCode] -> IO [Either String Outline]
loadOutlinesIO fontPath axes glyphs = do
  raw <- BS.readFile fontPath
  pure (decodeOutlines raw axes glyphs)

decodeOutline :: BS.ByteString -> VariationAxes -> GlyphCode -> Either String Outline
decodeOutline raw axes glyph = do
  font <- parseFont raw axes
  decodeOutlineFromFont font glyph

decodeOutlines :: BS.ByteString -> VariationAxes -> [GlyphCode] -> [Either String Outline]
decodeOutlines raw axes glyphs =
  case parseFont raw axes of
    Left err -> fmap (const (Left err)) glyphs
    Right font -> fmap (decodeOutlineFromFont font) glyphs

decodeOutlineFromFont :: Font -> GlyphCode -> Either String Outline
decodeOutlineFromFont font glyph = do
  gid <- glyphIndex font.cmap (unGlyphCode glyph)
  advBase <- glyphAdvance font gid
  adv <- applyAdvanceVariation font gid advBase
  cnts <- glyphContours font gid
  contours <- traverse flattenContour cnts
  let k = legacyFontScale
  let adv' = adv * k
  let contours' = fmap (scaleContour k) contours
  let b = boundsOfContours contours'
  pure
    Outline
      { adv = adv',
        bounds = b,
        unitsPerEm = font.head.upem,
        contours = contours'
      }

parseFont :: BS.ByteString -> VariationAxes -> Either String Font
parseFont raw axes = do
  sfnt <- parseSfnt raw
  headInfo <- parseHead sfnt
  cmapInfo <- parseCmap sfnt
  numGlyphs <- parseMaxp sfnt
  numHMetrics <- parseNumHMetrics sfnt
  loca <- parseLoca sfnt headInfo.locFmt numGlyphs
  glyfTbl <- requireTable sfnt "glyf"
  hmtxTbl <- requireTable sfnt "hmtx"
  varInfo <- parseVariations sfnt numGlyphs axes
  pure
    Font
      { sfnt = sfnt,
        head = headInfo,
        cmap = cmapInfo,
        numGlyphs = numGlyphs,
        numHMetrics = numHMetrics,
        loca = loca,
        glyf = glyfTbl,
        hmtx = hmtxTbl,
        var = varInfo
      }

parseSfnt :: BS.ByteString -> Either String Sfnt
parseSfnt raw = do
  ensureMin "font" 12 raw
  tableCount <- u16 raw 4
  let n = fromIntegral tableCount
  recs <- traverse (parseRec raw) [0 .. n - 1]
  pure
    Sfnt
      { bs = raw,
        tabs = Map.fromList recs
      }
  where
    parseRec bs i = do
      let p = 12 + (i * 16)
      ensureAtLeast "table record" bs p 16
      tag <- tag4 bs p
      off <- u32 bs (p + 8)
      len <- u32 bs (p + 12)
      let o = fromIntegral off
      let l = fromIntegral len
      ensureSlice "table" bs o l
      pure (tag, Tbl {off = o, len = l})

requireTable :: Sfnt -> String -> Either String Tbl
requireTable sfnt tagName =
  case Map.lookup tagName sfnt.tabs of
    Just t -> Right t
    Nothing -> Left ("Missing required OpenType table: " <> tagName)

tableBytes :: Sfnt -> String -> Either String BS.ByteString
tableBytes sfnt tagName = do
  tbl <- requireTable sfnt tagName
  slice sfnt.bs tbl.off tbl.len

parseHead :: Sfnt -> Either String Head
parseHead sfnt = do
  headBs <- tableBytes sfnt "head"
  ensureMin "head" 54 headBs
  upem <- fromIntegral <$> u16 headBs 18
  locFmt <- fromIntegral <$> i16 headBs 50
  pure
    Head
      { upem = upem,
        locFmt = locFmt
      }

parseMaxp :: Sfnt -> Either String Int
parseMaxp sfnt = do
  maxp <- tableBytes sfnt "maxp"
  ensureMin "maxp" 6 maxp
  fromIntegral <$> u16 maxp 4

parseNumHMetrics :: Sfnt -> Either String Int
parseNumHMetrics sfnt = do
  hhea <- tableBytes sfnt "hhea"
  ensureMin "hhea" 36 hhea
  fromIntegral <$> u16 hhea 34

parseLoca :: Sfnt -> Int -> Int -> Either String [Int]
parseLoca sfnt locFmt numGlyphs = do
  locaBs <- tableBytes sfnt "loca"
  let count = numGlyphs + 1
  if locFmt == 0
    then do
      let need = count * 2
      ensureMin "loca(short)" need locaBs
      traverse
        (\i -> do
           w <- u16 locaBs (i * 2)
           pure (fromIntegral w * 2)
        )
        [0 .. count - 1]
    else
      if locFmt == 1
        then do
          let need = count * 4
          ensureMin "loca(long)" need locaBs
          traverse
            (\i -> fromIntegral <$> u32 locaBs (i * 4))
            [0 .. count - 1]
        else Left ("Unsupported indexToLocFormat in head table: " <> show locFmt)

parseCmap :: Sfnt -> Either String Cmap
parseCmap sfnt = do
  cmapBs <- tableBytes sfnt "cmap"
  ensureMin "cmap" 4 cmapBs
  subCount <- fromIntegral <$> u16 cmapBs 2
  recs <- traverse (parseCmapRec cmapBs) [0 .. subCount - 1]
  chooseCmap cmapBs recs
  where
    parseCmapRec bs i = do
      let p = 4 + (i * 8)
      ensureAtLeast "cmap encoding record" bs p 8
      pid <- fromIntegral <$> u16 bs p
      eid <- fromIntegral <$> u16 bs (p + 2)
      off <- fromIntegral <$> u32 bs (p + 4)
      pure (pid, eid, off)

chooseCmap :: BS.ByteString -> [(Int, Int, Int)] -> Either String Cmap
chooseCmap cmapBs recs =
  case pick12 of
    Just (_, _, off12) -> parseCmap12 cmapBs off12
    Nothing ->
      case pick4 of
        Just (_, _, off4) -> parseCmap4 cmapBs off4
        Nothing -> Left "No supported cmap subtable (need format 12 or 4)."
  where
    pick12 = firstWhere isFmt12 recs
    pick4 = firstWhere isFmt4 recs

    isFmt12 (pid, eid, off) =
      unicodeLike pid eid
        && fmtAt off == Just (12 :: Word16)

    isFmt4 (pid, eid, off) =
      unicodeLike pid eid
        && fmtAt off == Just (4 :: Word16)

    fmtAt off =
      if off + 2 <= BS.length cmapBs
        then either (const Nothing) (Just . fromIntegral) (u16 cmapBs off)
        else Nothing

    unicodeLike pid eid =
      pid == 0
        || (pid == 3 && (eid == 1 || eid == 10))

firstWhere :: (a -> Bool) -> [a] -> Maybe a
firstWhere predFn = go
  where
    go [] = Nothing
    go (x : xs)
      | predFn x = Just x
      | otherwise = go xs

parseCmap4 :: BS.ByteString -> Int -> Either String Cmap
parseCmap4 cmapBs off = do
  ensureAtLeast "cmap format 4 header" cmapBs off 16
  fmt <- u16 cmapBs off
  if fmt /= 4
    then Left "cmap subtable offset does not point to format 4 table."
    else do
      len <- fromIntegral <$> u16 cmapBs (off + 2)
      table <- slice cmapBs off len
      segCountX2 <- fromIntegral <$> u16 table 6
      let segCount = segCountX2 `div` 2
      let endPos = 14
      let startPos = endPos + (segCount * 2) + 2
      let deltaPos = startPos + (segCount * 2)
      let rangePos = deltaPos + (segCount * 2)
      ensureAtLeast "cmap format 4 arrays" table 0 (rangePos + (segCount * 2))
      endCodes <- traverse (\i -> fromIntegral <$> u16 table (endPos + i * 2)) [0 .. segCount - 1]
      startCodes <- traverse (\i -> fromIntegral <$> u16 table (startPos + i * 2)) [0 .. segCount - 1]
      deltas <- traverse (\i -> fromIntegral <$> i16 table (deltaPos + i * 2)) [0 .. segCount - 1]
      ranges <- traverse (\i -> fromIntegral <$> u16 table (rangePos + i * 2)) [0 .. segCount - 1]
      pure
        ( CMap4
            Cmap4
              { tab = table,
                segCnt = segCount,
                endCodes = endCodes,
                startCodes = startCodes,
                idDeltas = deltas,
                idRangeOffsets = ranges,
                idRangeOffPos = rangePos
              }
        )

parseCmap12 :: BS.ByteString -> Int -> Either String Cmap
parseCmap12 cmapBs off = do
  ensureAtLeast "cmap format 12 header" cmapBs off 16
  fmt <- u16 cmapBs off
  if fmt /= 12
    then Left "cmap subtable offset does not point to format 12 table."
    else do
      len <- fromIntegral <$> u32 cmapBs (off + 4)
      table <- slice cmapBs off len
      groupCount <- fromIntegral <$> u32 table 12
      let g0 = 16
      ensureAtLeast "cmap format 12 groups" table g0 (groupCount * 12)
      groups <-
        traverse
          (\i -> do
             let p = g0 + (i * 12)
             s <- fromIntegral <$> u32 table p
             e <- fromIntegral <$> u32 table (p + 4)
             g <- fromIntegral <$> u32 table (p + 8)
             pure (s, e, g)
          )
          [0 .. groupCount - 1]
      pure (CMap12 Cmap12 {groups = groups})

glyphIndex :: Cmap -> Int -> Either String Int
glyphIndex cmap codepoint =
  let cp = clamp0 codepoint
      gid =
        case cmap of
          CMap12 c12 -> lookupCmap12 c12 cp
          CMap4 c4 -> lookupCmap4 c4 cp
   in pure (maybe 0 id gid)

lookupCmap12 :: Cmap12 -> Int -> Maybe Int
lookupCmap12 c12 cp =
  go c12.groups
  where
    go [] = Nothing
    go ((s, e, g0) : rest)
      | cp < s = Nothing
      | cp <= e = Just (g0 + (cp - s))
      | otherwise = go rest

lookupCmap4 :: Cmap4 -> Int -> Maybe Int
lookupCmap4 c4 cp =
  case findIndex inSeg [0 .. c4.segCnt - 1] of
    Nothing -> Nothing
    Just i ->
      let start = c4.startCodes !! i
          delta = c4.idDeltas !! i
          rOff = c4.idRangeOffsets !! i
       in if rOff == 0
            then Just ((cp + delta) .&. 0xFFFF)
            else
              let wordAddr = c4.idRangeOffPos + (i * 2)
                  glyphWordAddr = wordAddr + rOff + ((cp - start) * 2)
               in if glyphWordAddr + 2 > BS.length c4.tab
                    then Nothing
                    else
                      case u16 c4.tab glyphWordAddr of
                        Left _ -> Nothing
                        Right 0 -> Just 0
                        Right g -> Just (((fromIntegral g) + delta) .&. 0xFFFF)
  where
    inSeg i =
      let s = c4.startCodes !! i
          e = c4.endCodes !! i
       in cp >= s && cp <= e

glyphAdvance :: Font -> Int -> Either String Double
glyphAdvance font gidRaw = do
  let gid = clamp gidRaw 0 (font.numGlyphs - 1)
  let hmtxBs = font.sfnt.bs
  let base = font.hmtx.off
  if font.numHMetrics <= 0
    then Left "hhea reports zero horizontal metrics."
    else
      if gid < font.numHMetrics
        then do
          let p = base + gid * 4
          ensureAtLeast "hmtx metric" hmtxBs p 2
          fromIntegral <$> u16 hmtxBs p
        else do
          let p = base + (font.numHMetrics - 1) * 4
          ensureAtLeast "hmtx fallback metric" hmtxBs p 2
          fromIntegral <$> u16 hmtxBs p

applyAdvanceVariation :: Font -> Int -> Double -> Either String Double
applyAdvanceVariation font gid baseAdv =
  case font.var of
    Nothing -> Right baseAdv
    Just varInfo -> do
      pointCount <- glyphPointCount font gid
      tuples <- glyphTupleData varInfo.gvar gid pointCount
      let (dxs, _) = applyCompositeTupleDeltas varInfo.loc pointCount tuples
      let p0 = pointCount - 4
      let p1 = pointCount - 3
      if p0 >= 0 && p1 >= 0 && p1 < length dxs
        then pure (baseAdv + (dxs !! p1) - (dxs !! p0))
        else pure baseAdv

glyphPointCount :: Font -> Int -> Either String Int
glyphPointCount font gid
  | gid < 0 || gid >= font.numGlyphs = Left ("Glyph index out of range: " <> show gid)
  | otherwise = do
      gl <- glyphBytes font gid
      nCnt <- i16 gl 0
      if nCnt >= 0
        then do
          endPts <- traverse (\i -> fromIntegral <$> u16 gl (10 + i * 2)) [0 .. fromIntegral nCnt - 1]
          let baseCount =
                case reverse endPts of
                  [] -> 0
                  x : _ -> x + 1
          pure (baseCount + 4)
        else
          if nCnt == (-1)
            then do
              (comps, _) <- readCompositeComponents gl
              pure (length comps + 4)
            else Left ("Unsupported glyf numberOfContours value: " <> show nCnt)

glyphBytes :: Font -> Int -> Either String BS.ByteString
glyphBytes font gid = do
  let startOff = font.loca !! gid
  let endOff = font.loca !! (gid + 1)
  if endOff <= startOff
    then Right (BS.empty)
    else do
      let absStart = font.glyf.off + startOff
      let absLen = endOff - startOff
      ensureSlice "glyf glyph" font.sfnt.bs absStart absLen
      slice font.sfnt.bs absStart absLen

glyphContours :: Font -> Int -> Either String [[RawPt]]
glyphContours font gidRaw =
  parseGlyphContours font (clamp gidRaw 0 (font.numGlyphs - 1)) 0

parseGlyphContours :: Font -> Int -> Int -> Either String [[RawPt]]
parseGlyphContours font gid depth
  | depth > 32 = Left "Composite glyph recursion limit exceeded."
  | gid < 0 || gid >= font.numGlyphs = Left ("Glyph index out of range: " <> show gid)
  | otherwise = do
      let startOff = font.loca !! gid
      let endOff = font.loca !! (gid + 1)
      if endOff <= startOff
        then Right []
        else do
          let absStart = font.glyf.off + startOff
          let absLen = endOff - startOff
          ensureSlice "glyf glyph" font.sfnt.bs absStart absLen
          gl <- slice font.sfnt.bs absStart absLen
          nCnt <- i16 gl 0
          if nCnt >= 0
            then parseSimpleGlyph font gid gl (fromIntegral nCnt)
            else
              if nCnt == (-1)
                then parseCompositeGlyph font gid gl depth
                else Left ("Unsupported glyf numberOfContours value: " <> show nCnt)

parseSimpleGlyph :: Font -> Int -> BS.ByteString -> Int -> Either String [[RawPt]]
parseSimpleGlyph font gid gl contourCount = do
  ensureAtLeast "simple glyph header" gl 0 10
  endPts <- traverse (\i -> fromIntegral <$> u16 gl (10 + i * 2)) [0 .. contourCount - 1]
  let pointCount = case reverse endPts of
        [] -> 0
        x : _ -> x + 1
  let instrLenPos = 10 + contourCount * 2
  instrLen <- fromIntegral <$> u16 gl instrLenPos
  let flagPos = instrLenPos + 2 + instrLen
  (flags, xPos) <- decodeFlags gl flagPos pointCount
  (xs, yPos) <- decodeCoordsX gl xPos flags
  (ys, _) <- decodeCoordsY gl yPos flags
  let pts0 = zipWith3 mkRaw xs ys flags
  pts <- applySimpleVariation font gid endPts pts0
  splitByEndPts endPts pts
  where
    mkRaw x y fl =
      RawPt
        { x = fromIntegral x,
          y = fromIntegral y,
          on = (fl .&. 0x01) /= 0
        }

applySimpleVariation :: Font -> Int -> [Int] -> [RawPt] -> Either String [RawPt]
applySimpleVariation font gid endPts pts =
  case font.var of
    Nothing -> Right pts
    Just varInfo -> do
      let baseCount = length pts
      let allPts = pts <> replicate 4 RawPt {x = 0, y = 0, on = True}
      let fullCount = length allPts
      tuples <- glyphTupleData varInfo.gvar gid fullCount
      let contourEnds =
            if fullCount > baseCount
              then endPts <> [fullCount - 1]
              else endPts
      let (dxs, dys) = applyTupleDeltas varInfo.loc contourEnds allPts fullCount tuples
      pure $
        zipWith3
          (\p dx dy -> RawPt {x = p.x + dx, y = p.y + dy, on = p.on})
          pts
          (take baseCount dxs)
          (take baseCount dys)

applyTupleDeltas :: [Double] -> [Int] -> [RawPt] -> Int -> [TupleRec] -> ([Double], [Double])
applyTupleDeltas loc0 contourEnds pts pointCount tuples =
  foldl' step (zeroes, zeroes) tuples
  where
    zeroes = replicate pointCount 0
    coordsX = fmap (.x) pts
    coordsY = fmap (.y) pts

    step (accX, accY) tuple =
      let s = tupleScalar loc0 tuple.hdr
       in if s == 0
            then (accX, accY)
            else
              let (dx, dy) = tupleDeltasFull contourEnds coordsX coordsY pointCount tuple
               in (zipWith (+) accX (fmap (s *) dx), zipWith (+) accY (fmap (s *) dy))

tupleDeltasFull ::
  [Int] ->
  [Double] ->
  [Double] ->
  Int ->
  TupleRec ->
  ([Double], [Double])
tupleDeltasFull contourEnds coordsX coordsY pointCount tuple =
  let xMap = Map.fromList [(i, fromIntegral d) | (i, d) <- zip tuple.pts tuple.dx, i >= 0, i < pointCount]
      yMap = Map.fromList [(i, fromIntegral d) | (i, d) <- zip tuple.pts tuple.dy, i >= 0, i < pointCount]
      sparseX = fmap (\i -> Map.lookup i xMap) [0 .. pointCount - 1]
      sparseY = fmap (\i -> Map.lookup i yMap) [0 .. pointCount - 1]
      directX = fmap (maybe 0 id) sparseX
      directY = fmap (maybe 0 id) sparseY
   in if length tuple.pts < pointCount
        then
          ( inferDeltas contourEnds coordsX sparseX,
            inferDeltas contourEnds coordsY sparseY
          )
        else (directX, directY)

inferDeltas :: [Int] -> [Double] -> [Maybe Double] -> [Double]
inferDeltas contourEnds coords sparse =
  let n = length coords
      ranges = contourRanges contourEnds n
   in if null ranges
        then fmap (maybe 0 id) sparse
        else concatMap (\(s, e) -> inferContour (sliceRange s e coords) (sliceRange s e sparse)) ranges

contourRanges :: [Int] -> Int -> [(Int, Int)]
contourRanges ends n = go 0 ends
  where
    go start rest =
      case rest of
        [] ->
          if start < n
            then [(start, n - 1)]
            else []
        e : tailEnds ->
          if e < start
            then go start tailEnds
            else
              let e' = min e (n - 1)
                  next = e + 1
               in (start, e') : go next tailEnds

sliceRange :: Int -> Int -> [a] -> [a]
sliceRange start end xs =
  let n = max 0 ((end - start) + 1)
   in take n (drop start xs)

inferContour :: [Double] -> [Maybe Double] -> [Double]
inferContour coords sparse =
  case explicit of
    [] -> replicate n 0
    [(_, d)] -> replicate n d
    _ -> fmap valueAt [0 .. n - 1]
  where
    n = length coords
    explicit = [(i, d) | (i, Just d) <- zip [0 ..] sparse]
    expIdx = fmap fst explicit
    expMap = Map.fromList explicit
    expExt = expIdx <> fmap (+ n) expIdx
    pairs = zip expExt (drop 1 expExt)
    headIdx =
      case expIdx of
        [] -> 0
        i : _ -> i

    valueAt j =
      case sparse !! j of
        Just d -> d
        Nothing ->
          let j' = if j < headIdx then j + n else j
              picked =
                case find (\(lo, hi) -> j' >= lo && j' <= hi) pairs of
                  Just p -> p
                  Nothing -> (headIdx, headIdx + n)
              a = fst picked
              b = snd picked
              ia = a `mod` n
              ib = b `mod` n
              rc1 = coords !! ia
              rc2 = coords !! ib
              rd1 = maybe 0 id (Map.lookup ia expMap)
              rd2 = maybe 0 id (Map.lookup ib expMap)
           in iupInterpolate rc1 rd1 rc2 rd2 (coords !! j)

iupInterpolate :: Double -> Double -> Double -> Double -> Double -> Double
iupInterpolate rc1 rd1 rc2 rd2 c
  | rc1 == rc2 =
      if rd1 == rd2
        then rd1
        else
          if c <= rc1
            then min rd1 rd2
            else max rd1 rd2
  | rc1 > rc2 = iupInterpolate rc2 rd2 rc1 rd1 c
  | c <= rc1 = rd1
  | c >= rc2 = rd2
  | otherwise =
      let t = (c - rc1) / (rc2 - rc1)
       in rd1 + ((rd2 - rd1) * t)

parseCompositeGlyph :: Font -> Int -> BS.ByteString -> Int -> Either String [[RawPt]]
parseCompositeGlyph font selfGid gl depth = do
  (comps0, posAfter) <- readCompositeComponents gl
  comps <- applyCompositeVariation font selfGid comps0
  mapped <- traverse mapComp comps
  let hasInstructions =
        case reverse comps of
          [] -> False
          compLast : _ -> (compLast.flg .&. flagInstructions) /= 0
  if hasInstructions
    then do
      ensureAtLeast "composite glyph instructions length" gl posAfter 2
      instrLen <- fromIntegral <$> u16 gl posAfter
      let _ = posAfter + 2 + instrLen
      pure (concat mapped)
    else pure (concat mapped)
  where
    mapComp comp = do
      subs <- parseGlyphContours font comp.gid (depth + 1)
      pure (fmap (fmap (transformPt comp.a comp.b comp.c comp.d comp.dx comp.dy)) subs)

readComponentArgs :: BS.ByteString -> Int -> Int -> Either String (Double, Double, Int)
readComponentArgs gl flags pos =
  if (flags .&. flagArgsAreXY) == 0
    then Left "Composite glyph uses point-matching arguments (unsupported)."
    else
      if (flags .&. flagArgsAreWords) /= 0
        then do
          dx <- fromIntegral <$> i16 gl pos
          dy <- fromIntegral <$> i16 gl (pos + 2)
          pure (dx, dy, pos + 4)
        else do
          dx <- fromIntegral <$> i8 gl pos
          dy <- fromIntegral <$> i8 gl (pos + 1)
          pure (dx, dy, pos + 2)

readComponentTransform :: BS.ByteString -> Int -> Int -> Either String (Double, Double, Double, Double, Int)
readComponentTransform gl flags pos
  | (flags .&. flagTwoByTwo) /= 0 = do
      a <- f2dot14 gl pos
      b <- f2dot14 gl (pos + 2)
      c <- f2dot14 gl (pos + 4)
      d <- f2dot14 gl (pos + 6)
      pure (a, b, c, d, pos + 8)
  | (flags .&. flagXYScale) /= 0 = do
      a <- f2dot14 gl pos
      d <- f2dot14 gl (pos + 2)
      pure (a, 0, 0, d, pos + 4)
  | (flags .&. flagScale) /= 0 = do
      s <- f2dot14 gl pos
      pure (s, 0, 0, s, pos + 2)
  | otherwise =
      pure (1, 0, 0, 1, pos)

readCompositeComponents :: BS.ByteString -> Either String ([Comp], Int)
readCompositeComponents gl = go 10 []
  where
    go pos acc = do
      ensureAtLeast "composite glyph component" gl pos 4
      flags <- fromIntegral <$> u16 gl pos
      subGlyph <- fromIntegral <$> u16 gl (pos + 2)
      let posArgs = pos + 4
      (dx, dy, posAfterArgs) <- readComponentArgs gl flags posArgs
      (a, b, c, d, posAfterTransform) <- readComponentTransform gl flags posAfterArgs
      let comp =
            Comp
              { flg = flags,
                gid = subGlyph,
                dx = dx,
                dy = dy,
                a = a,
                b = b,
                c = c,
                d = d
              }
      if (flags .&. flagMoreComponents) /= 0
        then go posAfterTransform (acc <> [comp])
        else Right (acc <> [comp], posAfterTransform)

applyCompositeVariation :: Font -> Int -> [Comp] -> Either String [Comp]
applyCompositeVariation font selfGid comps0 =
  case font.var of
    Nothing -> Right comps0
    Just varInfo -> do
      let pointCount = length comps0 + 4
      tuples <- glyphTupleData varInfo.gvar selfGid pointCount
      let (dxs, dys) = applyCompositeTupleDeltas varInfo.loc pointCount tuples
      pure $
        fmap
          (\(ix, comp) ->
             Comp
               { flg = comp.flg,
                 gid = comp.gid,
                 dx = comp.dx + (dxs !! ix),
                 dy = comp.dy + (dys !! ix),
                 a = comp.a,
                 b = comp.b,
                 c = comp.c,
                 d = comp.d
               }
          )
          (zip [0 ..] comps0)

applyCompositeTupleDeltas :: [Double] -> Int -> [TupleRec] -> ([Double], [Double])
applyCompositeTupleDeltas loc0 pointCount tuples =
  foldl' step (zeroes, zeroes) tuples
  where
    zeroes = replicate pointCount 0
    step (accX, accY) tuple =
      let s = tupleScalar loc0 tuple.hdr
       in if s == 0
            then (accX, accY)
            else
              let xAdd = foldl' (addDelta tuple.dx s) zeroes (zip [0 ..] tuple.pts)
                  yAdd = foldl' (addDelta tuple.dy s) zeroes (zip [0 ..] tuple.pts)
               in (zipWith (+) accX xAdd, zipWith (+) accY yAdd)

    addDelta deltas s acc (k, ptIx)
      | ptIx < 0 || ptIx >= pointCount = acc
      | otherwise =
          let d =
                if k < length deltas
                  then fromIntegral (deltas !! k) * s
                  else 0
           in replaceAt ptIx ((acc !! ptIx) + d) acc

transformPt :: Double -> Double -> Double -> Double -> Double -> Double -> RawPt -> RawPt
transformPt a b c d dx dy pt =
  let x' = (a * pt.x) + (b * pt.y) + dx
      y' = (c * pt.x) + (d * pt.y) + dy
   in RawPt {x = x', y = y', on = pt.on}

decodeFlags :: BS.ByteString -> Int -> Int -> Either String ([Int], Int)
decodeFlags gl pos0 pointCount = go pos0 [] 0
  where
    go pos acc n
      | n >= pointCount = Right (reverse acc, pos)
      | otherwise = do
          fl <- fromIntegral <$> u8 gl pos
          if (fl .&. 0x08) /= 0
            then do
              rep <- fromIntegral <$> u8 gl (pos + 1)
              let k = rep + 1
              go (pos + 2) (replicate k fl <> acc) (n + k)
            else go (pos + 1) (fl : acc) (n + 1)

decodeCoordsX :: BS.ByteString -> Int -> [Int] -> Either String ([Int], Int)
decodeCoordsX gl pos0 flags = go pos0 0 [] flags
  where
    go pos _ acc [] = Right (reverse acc, pos)
    go pos cur acc (fl : rest)
      | (fl .&. 0x02) /= 0 = do
          deltaU <- fromIntegral <$> u8 gl pos
          let delta = if (fl .&. 0x10) /= 0 then deltaU else negate deltaU
          let next = cur + delta
          go (pos + 1) next (next : acc) rest
      | (fl .&. 0x10) /= 0 =
          let next = cur
           in go pos next (next : acc) rest
      | otherwise = do
          delta <- fromIntegral <$> i16 gl pos
          let next = cur + delta
          go (pos + 2) next (next : acc) rest

decodeCoordsY :: BS.ByteString -> Int -> [Int] -> Either String ([Int], Int)
decodeCoordsY gl pos0 flags = go pos0 0 [] flags
  where
    go pos _ acc [] = Right (reverse acc, pos)
    go pos cur acc (fl : rest)
      | (fl .&. 0x04) /= 0 = do
          deltaU <- fromIntegral <$> u8 gl pos
          let delta = if (fl .&. 0x20) /= 0 then deltaU else negate deltaU
          let next = cur + delta
          go (pos + 1) next (next : acc) rest
      | (fl .&. 0x20) /= 0 =
          let next = cur
           in go pos next (next : acc) rest
      | otherwise = do
          delta <- fromIntegral <$> i16 gl pos
          let next = cur + delta
          go (pos + 2) next (next : acc) rest

splitByEndPts :: [Int] -> [RawPt] -> Either String [[RawPt]]
splitByEndPts ends pts =
  go 0 ends
  where
    go _ [] = Right []
    go start (e : rest)
      | e < start = Left "Invalid simple glyph contour endpoints."
      | e >= length pts = Left "Simple glyph contour endpoint exceeds point count."
      | otherwise = do
          let n = (e - start) + 1
          segment <- takeN n (drop start pts)
          tailContours <- go (e + 1) rest
          pure (segment : tailContours)

takeN :: Int -> [a] -> Either String [a]
takeN n xs =
  let ys = take n xs
   in if length ys == n
        then Right ys
        else Left "Unexpected short list while splitting glyph contours."

replaceAt :: Int -> a -> [a] -> [a]
replaceAt ix val xs =
  case splitAt ix xs of
    (pre, _ : post) -> pre <> [val] <> post
    _ -> xs

flattenContour :: [RawPt] -> Either String Contour
flattenContour raw
  | length raw < 2 = Left "Contour must contain at least two points."
  | otherwise = do
      segs <- buildSegmentsTrueType raw
      if null segs
        then Left "Contour has no drawable segments."
        else
          let poly = segmentsToPolyline segs
              edgeSegs = fmap segToEdge segs
           in pure
                Contour
                  { pts = poly,
                    segs = edgeSegs,
                    grps = replicate (length edgeSegs) 1
                  }

buildSegmentsTrueType :: [RawPt] -> Either String [Seg]
buildSegmentsTrueType pts
  | n < 2 = Left "Contour must contain at least two points."
  | otherwise = Right (go startPt ix0 0 [])
  where
    n = length pts
    firstPt = pts !! 0
    lastPt = pts !! (n - 1)
    (startPt, ix0)
      | firstPt.on = (firstPt, 1)
      | lastPt.on = (lastPt, 0)
      | otherwise = (midpoint lastPt firstPt, 0)

    at i = pts !! (i `mod` n)

    go cur ix used acc
      | used >= n = reverse acc
      | otherwise =
          let p = at ix
           in if p.on
                then go p (ix + 1) (used + 1) (SegLine cur p : acc)
                else
                  let q = at (ix + 1)
                   in if q.on
                        then go q (ix + 2) (used + 2) (SegQuad cur p q : acc)
                        else
                          let mid = midpoint p q
                           in go mid (ix + 1) (used + 1) (SegQuad cur p mid : acc)

midpoint :: RawPt -> RawPt -> RawPt
midpoint a b =
  RawPt
    -- FreeType parity: implied on-curve insertion uses integer midpoint truncation.
    { x = avgInt a.x b.x,
      y = avgInt a.y b.y,
      on = True
    }
  where
    avgInt p q =
      let ip = truncate p :: Integer
          iq = truncate q :: Integer
       in fromIntegral ((ip + iq) `quot` 2)

segmentsToPolyline :: [Seg] -> [Pt]
segmentsToPolyline segs =
  case segs of
    [] -> []
    firstSeg : _ ->
      let startPt = segStart firstSeg
          tailPts = concatMap emit (zip [0 ..] segs)
       in ptOf startPt : tailPts
  where
    total = length segs

    emit (i, seg) =
      let closing = i == total - 1
       in case seg of
            SegLine _ b ->
              if closing
                then []
                else [ptOf b]
            SegQuad a c b ->
              let ts =
                    if closing
                      then [1 .. quadSteps - 1]
                      else [1 .. quadSteps]
               in fmap
                    (\k ->
                       let t = fromIntegral k / fromIntegral quadSteps
                        in ptOf (quadAt a c b t)
                    )
                    ts

quadSteps :: Int
quadSteps = 64

segToEdge :: Seg -> Edge
segToEdge seg =
  case seg of
    SegLine a b ->
      Edge
        { a = ptOf a,
          b = ptOf b,
          c = Nothing,
          col = 7
        }
    SegQuad a c b ->
      let dx0 = c.x - a.x
          dy0 = c.y - a.y
          dx1 = b.x - c.x
          dy1 = b.y - c.y
          isCollinear = (dx0 * dy1 - dy0 * dx1) == 0
       in Edge
            { a = ptOf a,
              b = ptOf b,
              c = if isCollinear then Nothing else Just (ptOf c),
              col = 7
            }

segStart :: Seg -> RawPt
segStart seg =
  case seg of
    SegLine a _ -> a
    SegQuad a _ _ -> a

ptOf :: RawPt -> Pt
ptOf rp = Pt {x = rp.x, y = rp.y}

quadAt :: RawPt -> RawPt -> RawPt -> Double -> RawPt
quadAt p c q t =
  let u = 1.0 - t
      x' = (u * u * p.x) + (2.0 * u * t * c.x) + (t * t * q.x)
      y' = (u * u * p.y) + (2.0 * u * t * c.y) + (t * t * q.y)
   in RawPt
        { x = x',
          y = y',
          on = True
        }

scaleContour :: Double -> Contour -> Contour
scaleContour k contour =
  contour
    { pts = fmap (scalePt k) contour.pts,
      segs = fmap (scaleEdge k) contour.segs
    }

scaleEdge :: Double -> Edge -> Edge
scaleEdge k edge =
  Edge
    { a = scalePt k edge.a,
      b = scalePt k edge.b,
      c = fmap (scalePt k) edge.c,
      col = edge.col
    }

scalePt :: Double -> Pt -> Pt
scalePt k pt =
  Pt
    { x = pt.x * k,
      y = pt.y * k
    }

legacyFontScale :: Double
legacyFontScale = 1.0 / 64.0

boundsOfContours :: [Contour] -> (Double, Double, Double, Double)
boundsOfContours contours =
  case concatMap (.segs) contours of
    [] -> (0, 0, 0, 0)
    edge0 : rest ->
      foldl' (\acc edge -> mergeBounds acc (edgeBounds edge)) (edgeBounds edge0) rest
  where
    mergeBounds (xmin0, ymin0, xmax0, ymax0) (xmin1, ymin1, xmax1, ymax1) =
      ( min xmin0 xmin1,
        min ymin0 ymin1,
        max xmax0 xmax1,
        max ymax0 ymax1
      )

    edgeBounds edge =
      case edge.c of
        Nothing ->
          boundsFromPoints [edge.a, edge.b]
        Just ctrl ->
          let ts =
                [ t
                  | t <- [quadExtremumT edge.a.x ctrl.x edge.b.x, quadExtremumT edge.a.y ctrl.y edge.b.y],
                    t > 0.0,
                    t < 1.0
                ]
              extPts = fmap (quadPoint edge.a ctrl edge.b) ts
           in boundsFromPoints ([edge.a, edge.b] <> extPts)

    boundsFromPoints points =
      case points of
        [] -> (0.0, 0.0, 0.0, 0.0)
        p0 : ps ->
          foldl' stepPoint (p0.x, p0.y, p0.x, p0.y) ps

    stepPoint (xmin, ymin, xmax, ymax) p =
      ( min xmin p.x,
        min ymin p.y,
        max xmax p.x,
        max ymax p.y
      )

    quadExtremumT p0 p1 p2 =
      let denom = p0 - (2.0 * p1) + p2
       in if abs denom <= 1.0e-18
            then -1.0
            else (p0 - p1) / denom

    quadPoint p0 p1 p2 t =
      let u = 1.0 - t
          w0 = u * u
          w1 = 2.0 * u * t
          w2 = t * t
       in Pt
            { x = (w0 * p0.x) + (w1 * p1.x) + (w2 * p2.x),
              y = (w0 * p0.y) + (w1 * p1.y) + (w2 * p2.y)
            }

parseVariations :: Sfnt -> Int -> VariationAxes -> Either String (Maybe Var)
parseVariations sfnt glyphCount axes = do
  axisRanges <- parseFvarAxes sfnt
  checkAxis axisRanges "wght" axes.wght
  checkAxis axisRanges "opsz" axes.opsz
  if null axisRanges
    then pure Nothing
    else do
      avarMaps <- parseAvarMaps sfnt (length axisRanges)
      let loc0 =
            fmap
              (\axis -> normalizeAxis axis (axisValue axis axes))
              axisRanges
      let loc1 = zipWith applyAvar avarMaps loc0
      if all nearZero loc1
        then pure Nothing
        else do
          mgv <- parseGvarMaybe sfnt glyphCount
          case mgv of
            Nothing -> Left "Variable font location requested but gvar table is missing."
            Just gv ->
              pure
                ( Just
                    Var
                      { loc = padWith 0 gv.axisCnt loc1,
                        gvar = gv
                      }
                )
  where
    checkAxis ranges nm mv =
      case mv of
        Nothing -> Right ()
        Just _ ->
          if hasAxis ranges nm
            then Right ()
            else Left ("Requested variation axis is not present in font: " <> nm)

    hasAxis ranges nm = any (\axis -> axis.tag == tagWord nm) ranges

    axisValue axis ax
      | axis.tag == tagWord "wght" =
          case ax.wght of
            Just v -> v
            Nothing -> axis.def
      | axis.tag == tagWord "opsz" =
          case ax.opsz of
            Just v -> v
            Nothing -> axis.def
      | otherwise = axis.def

parseFvarAxes :: Sfnt -> Either String [AxisRange]
parseFvarAxes sfnt =
  case Map.lookup "fvar" sfnt.tabs of
    Nothing -> Right []
    Just _ -> do
      fvar <- tableBytes sfnt "fvar"
      ensureMin "fvar" 16 fvar
      axisOffset <- fromIntegral <$> u16 fvar 4
      axisCount <- fromIntegral <$> u16 fvar 8
      axisSize <- fromIntegral <$> u16 fvar 10
      if axisSize < 20
        then Left "Invalid fvar axis size."
        else
          traverse
            (\i -> do
               let base = axisOffset + i * axisSize
               tag <- u32 fvar base
               lo <- fixed16dot16 fvar (base + 4)
               def <- fixed16dot16 fvar (base + 8)
               hi <- fixed16dot16 fvar (base + 12)
               pure
                 AxisRange
                   { tag = tag,
                     lo = lo,
                     def = def,
                     hi = hi
                   }
            )
            [0 .. axisCount - 1]

parseAvarMaps :: Sfnt -> Int -> Either String [[(Double, Double)]]
parseAvarMaps sfnt axisCount
  | axisCount <= 0 = Right []
  | otherwise =
      case Map.lookup "avar" sfnt.tabs of
        Nothing -> Right (replicate axisCount [])
        Just _ -> do
          avar <- tableBytes sfnt "avar"
          ensureMin "avar" 8 avar
          axisCountAvar <- fromIntegral <$> u16 avar 6
          if axisCountAvar /= axisCount
            then Left "avar axis count does not match fvar axis count."
            else readAxisMaps avar 8 axisCount []
  where
    readAxisMaps _ _ 0 acc = Right (reverse acc)
    readAxisMaps avar pos n acc = do
      ensureAtLeast "avar axis map header" avar pos 2
      segCount <- fromIntegral <$> u16 avar pos
      let segBase = pos + 2
      segs <-
        traverse
          (\i -> do
             let p = segBase + i * 4
             fromCoord <- f2dot14 avar p
             toCoord <- f2dot14 avar (p + 2)
             pure (fromCoord, toCoord)
          )
          [0 .. segCount - 1]
      readAxisMaps avar (segBase + segCount * 4) (n - 1) (segs : acc)

normalizeAxis :: AxisRange -> Double -> Double
normalizeAxis axis raw =
  let v = clampD axis.lo axis.hi raw
   in if v < axis.def
        then
          if axis.def == axis.lo
            then 0
            else (v - axis.def) / (axis.def - axis.lo)
        else
          if v > axis.def
            then
              if axis.hi == axis.def
                then 0
                else (v - axis.def) / (axis.hi - axis.def)
            else 0

applyAvar :: [(Double, Double)] -> Double -> Double
applyAvar segs x =
  case segs of
    [] -> x
    first : rest ->
      if x <= fst first
        then snd first
        else walk first rest
  where
    walk prev remaining =
      case remaining of
        [] -> snd prev
        cur : tailSegs ->
          if x <= fst cur
            then interpolate prev cur x
            else walk cur tailSegs

    interpolate (x0, y0) (x1, y1) x'
      | x1 == x0 = y0
      | otherwise =
          let t = (x' - x0) / (x1 - x0)
           in y0 + ((y1 - y0) * t)

parseGvarMaybe :: Sfnt -> Int -> Either String (Maybe Gvar)
parseGvarMaybe sfnt glyphCount =
  case Map.lookup "gvar" sfnt.tabs of
    Nothing -> Right Nothing
    Just _ -> Just <$> parseGvar sfnt glyphCount

parseGvar :: Sfnt -> Int -> Either String Gvar
parseGvar sfnt glyphCount = do
  gvar <- tableBytes sfnt "gvar"
  ensureMin "gvar" 20 gvar
  axisCnt <- fromIntegral <$> u16 gvar 4
  sharedTupleCount <- fromIntegral <$> u16 gvar 6
  sharedTupleOffset <- fromIntegral <$> u32 gvar 8
  glyphCountRaw <- fromIntegral <$> u16 gvar 12
  flags <- (fromIntegral <$> u16 gvar 14 :: Either String Int)
  dataOff <- fromIntegral <$> u32 gvar 16
  let glyphCnt = min glyphCount glyphCountRaw
  let offsetCount = glyphCnt + 1
  offs <-
    if (flags .&. 0x0001) /= 0
      then do
        ensureAtLeast "gvar glyph variation offsets" gvar 20 (offsetCount * 4)
        traverse (\i -> fromIntegral <$> u32 gvar (20 + i * 4)) [0 .. offsetCount - 1]
      else do
        ensureAtLeast "gvar glyph variation offsets" gvar 20 (offsetCount * 2)
        traverse
          (\i -> do
             shortOff <- fromIntegral <$> u16 gvar (20 + i * 2)
             pure (shortOff * 2)
          )
          [0 .. offsetCount - 1]
  sharedTuples <-
    traverse
      (\i -> do
         let base = sharedTupleOffset + i * axisCnt * 2
         traverse (\k -> f2dot14 gvar (base + k * 2)) [0 .. axisCnt - 1]
      )
      [0 .. sharedTupleCount - 1]
  pure
    Gvar
      { axisCnt = axisCnt,
        glyphCnt = glyphCnt,
        sharedTuples = sharedTuples,
        offs = offs,
        dataOff = dataOff,
        tab = gvar
      }

glyphTupleData :: Gvar -> Int -> Int -> Either String [TupleRec]
glyphTupleData gvar gid pointCount
  | gid < 0 || gid >= gvar.glyphCnt = Right []
  | gid + 1 >= length gvar.offs = Right []
  | otherwise = do
      let start = gvar.offs !! gid
      let end = gvar.offs !! (gid + 1)
      if end <= start
        then Right []
        else do
          let absOff = gvar.dataOff + start
          let len = end - start
          bytes <- slice gvar.tab absOff len
          parseGlyphTupleData bytes gvar pointCount

parseGlyphTupleData :: BS.ByteString -> Gvar -> Int -> Either String [TupleRec]
parseGlyphTupleData bytes gvar pointCount =
  if BS.length bytes < 4
    then Right []
    else do
      tupleCountRaw <- u16 bytes 0
      let tupleCount = fromIntegral (tupleCountRaw .&. 0x0FFF)
      let hasSharedPts = testBit tupleCountRaw 15
      dataOffset <- fromIntegral <$> u16 bytes 2
      (headers, headerEnd) <- readTupleHeaders bytes gvar tupleCount 4
      if dataOffset < headerEnd || dataOffset > BS.length bytes
        then Left "Invalid gvar glyph data offset."
        else do
          (sharedPts, tupleDataStart) <-
            if hasSharedPts
              then readPackedPointsE bytes dataOffset pointCount
              else Right ([], dataOffset)
          readTupleRecs bytes pointCount sharedPts tupleDataStart headers

readTupleRecs ::
  BS.ByteString ->
  Int ->
  [Int] ->
  Int ->
  [TupleHdr] ->
  Either String [TupleRec]
readTupleRecs _ _ _ _ [] = Right []
readTupleRecs bytes pointCount sharedPts pos (hdr : rest) = do
  tupleBytes <- slice bytes pos hdr.dataSize
  (pts, deltaPos) <-
    if hdr.privatePts
      then readPackedPointsE tupleBytes 0 pointCount
      else Right (sharedPts, 0)
  (dx, dyPos) <- readPackedDeltasE tupleBytes deltaPos (length pts)
  (dy, _) <- readPackedDeltasE tupleBytes dyPos (length pts)
  tailRecs <- readTupleRecs bytes pointCount sharedPts (pos + hdr.dataSize) rest
  pure (TupleRec {hdr = hdr, pts = pts, dx = dx, dy = dy} : tailRecs)

readTupleHeaders :: BS.ByteString -> Gvar -> Int -> Int -> Either String ([TupleHdr], Int)
readTupleHeaders _ _ 0 pos = Right ([], pos)
readTupleHeaders bytes gvar count pos = do
  ensureAtLeast "gvar tuple header" bytes pos 4
  dataSize <- fromIntegral <$> u16 bytes pos
  tupleIndex <- u16 bytes (pos + 2)
  let pos0 = pos + 4
  (peak, pos1) <-
    if testBit tupleIndex 15
      then readTupleCoords bytes pos0 gvar.axisCnt
      else do
        let idx = fromIntegral (tupleIndex .&. 0x0FFF)
        if idx < 0 || idx >= length gvar.sharedTuples
          then Left "gvar tuple index references out-of-range shared tuple."
          else Right (gvar.sharedTuples !! idx, pos0)
  (inter, pos2) <-
    if testBit tupleIndex 14
      then do
        (startTuple, posS) <- readTupleCoords bytes pos1 gvar.axisCnt
        (endTuple, posE) <- readTupleCoords bytes posS gvar.axisCnt
        Right (Just (startTuple, endTuple), posE)
      else Right (Nothing, pos1)
  let tupleHdr =
        TupleHdr
          { dataSize = dataSize,
            peak = peak,
            inter = inter,
            privatePts = testBit tupleIndex 13
          }
  (tailHeaders, endPos) <- readTupleHeaders bytes gvar (count - 1) pos2
  pure (tupleHdr : tailHeaders, endPos)

readTupleCoords :: BS.ByteString -> Int -> Int -> Either String ([Double], Int)
readTupleCoords bytes pos axisCount = do
  coords <- traverse (\i -> f2dot14 bytes (pos + i * 2)) [0 .. axisCount - 1]
  pure (coords, pos + axisCount * 2)

readPackedPointsE :: BS.ByteString -> Int -> Int -> Either String ([Int], Int)
readPackedPointsE bytes pos pointCount = do
  (count, pos0) <- readPackedCount bytes pos
  if count == 0
    then Right ([0 .. pointCount - 1], pos0)
    else
      if count < 0 || count > pointCount
        then Left "Invalid gvar packed point count."
        else go pos0 count 0 []
  where
    go pos' remaining prev acc
      | remaining <= 0 = Right (acc, pos')
      | otherwise = do
          header <- fromIntegral <$> u8 bytes pos'
          let isWord = testBit header 7
          let runCount = (header .&. 0x7F) + 1
          let k = min remaining runCount
          (vals, pos1) <- readUnsignedRun bytes (pos' + 1) k isWord
          let points = drop 1 (scanl (+) prev vals)
          let prev' =
                case reverse points of
                  [] -> prev
                  x : _ -> x
          go pos1 (remaining - k) prev' (acc <> points)

readUnsignedRun :: BS.ByteString -> Int -> Int -> Bool -> Either String ([Int], Int)
readUnsignedRun bytes pos count isWord = do
  vals <-
    if isWord
      then traverse (\i -> fromIntegral <$> u16 bytes (pos + i * 2)) [0 .. count - 1]
      else traverse (\i -> fromIntegral <$> u8 bytes (pos + i)) [0 .. count - 1]
  let size = if isWord then 2 else 1
  pure (vals, pos + count * size)

readPackedDeltasE :: BS.ByteString -> Int -> Int -> Either String ([Int], Int)
readPackedDeltasE bytes pos count = go pos count []
  where
    go pos' remaining acc
      | remaining <= 0 = Right (acc, pos')
      | otherwise = do
          header <- fromIntegral <$> u8 bytes pos'
          let sizeMask = header .&. 0xC0
          let runCount = (header .&. 0x3F) + 1
          let k = min remaining runCount
          case sizeMask of
            0x80 ->
              go (pos' + 1) (remaining - k) (acc <> replicate k 0)
            0x40 -> do
              (vals, pos1) <- readSignedRun bytes (pos' + 1) k 2
              go pos1 (remaining - k) (acc <> vals)
            0x00 -> do
              (vals, pos1) <- readSignedRun bytes (pos' + 1) k 1
              go pos1 (remaining - k) (acc <> vals)
            _ -> do
              (vals, pos1) <- readSignedRun bytes (pos' + 1) k 4
              go pos1 (remaining - k) (acc <> vals)

readSignedRun :: BS.ByteString -> Int -> Int -> Int -> Either String ([Int], Int)
readSignedRun bytes pos count width = do
  vals <-
    case width of
      1 -> traverse (\i -> i8 bytes (pos + i)) [0 .. count - 1]
      2 -> traverse (\i -> i16 bytes (pos + i * 2)) [0 .. count - 1]
      _ -> traverse (\i -> i32 bytes (pos + i * 4)) [0 .. count - 1]
  pure (vals, pos + count * width)

readPackedCount :: BS.ByteString -> Int -> Either String (Int, Int)
readPackedCount bytes pos = do
  b0 <- fromIntegral <$> u8 bytes pos
  if testBit b0 7
    then do
      b1 <- fromIntegral <$> u8 bytes (pos + 1)
      let count = ((b0 .&. 0x7F) `shiftL` 8) .|. b1
      pure (count, pos + 2)
    else pure (b0, pos + 1)

tupleScalar :: [Double] -> TupleHdr -> Double
tupleScalar loc0 hdr =
  case hdr.inter of
    Nothing ->
      foldl'
        (*)
        1
        (zipWith scalarAxis peak' loc')
    Just _ ->
      foldl'
        (*)
        1
        [ scalarAxisIntermediate pk st en lc
          | (pk, (st, (en, lc))) <- zip peak' (zip start' (zip end' loc'))
        ]
  where
    n = max (length loc0) (length hdr.peak)
    loc' = padWith 0 n loc0
    peak' = padWith 0 n hdr.peak
    start' =
      case hdr.inter of
        Nothing -> replicate n 0
        Just (xs, _) -> padWith 0 n xs
    end' =
      case hdr.inter of
        Nothing -> replicate n 0
        Just (_, ys) -> padWith 0 n ys

scalarAxis :: Double -> Double -> Double
scalarAxis peak coord
  | peak == 0 = 1
  | coord == 0 = 0
  | coord < 0 && peak > 0 = 0
  | coord > 0 && peak < 0 = 0
  | abs coord > abs peak = 0
  | otherwise = coord / peak

scalarAxisIntermediate :: Double -> Double -> Double -> Double -> Double
scalarAxisIntermediate peak start end coord
  | peak == 0 = 1
  | coord < min start end = 0
  | coord > max start end = 0
  | coord == peak = 1
  | coord < peak =
      if peak == start
        then 1
        else (coord - start) / (peak - start)
  | otherwise =
      if end == peak
        then 1
        else (end - coord) / (end - peak)

nearZero :: Double -> Bool
nearZero x = abs x <= 1.0e-12

padWith :: a -> Int -> [a] -> [a]
padWith x n xs = take n (xs <> replicate n x)

clampD :: Double -> Double -> Double -> Double
clampD lo hi x = max lo (min hi x)

tagWord :: String -> Word32
tagWord name =
  case fmap (fromIntegral . fromEnum) (take 4 name) of
    [a, b, c, d] -> (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
    _ -> 0

u8 :: BS.ByteString -> Int -> Either String Word8
u8 bs i =
  if i >= 0 && i < BS.length bs
    then Right (BS.index bs i)
    else Left ("Byte read out of bounds at offset " <> show i)

i8 :: BS.ByteString -> Int -> Either String Int
i8 bs i = do
  x <- fromIntegral <$> u8 bs i
  pure (if x >= 128 then x - 256 else x)

u16 :: BS.ByteString -> Int -> Either String Word16
u16 bs i = do
  ensureAtLeast "u16" bs i 2
  b0 <- fromIntegral <$> u8 bs i
  b1 <- fromIntegral <$> u8 bs (i + 1)
  pure ((b0 `shiftL` 8) .|. b1)

i16 :: BS.ByteString -> Int -> Either String Int
i16 bs i = do
  w <- fromIntegral <$> u16 bs i
  pure (if w >= 0x8000 then w - 0x10000 else w)

u32 :: BS.ByteString -> Int -> Either String Word32
u32 bs i = do
  ensureAtLeast "u32" bs i 4
  b0 <- fromIntegral <$> u8 bs i
  b1 <- fromIntegral <$> u8 bs (i + 1)
  b2 <- fromIntegral <$> u8 bs (i + 2)
  b3 <- fromIntegral <$> u8 bs (i + 3)
  pure ((b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3)

i32 :: BS.ByteString -> Int -> Either String Int
i32 bs i = do
  w <- fromIntegral <$> u32 bs i
  pure (if w >= 0x80000000 then w - 0x100000000 else w)

fixed16dot16 :: BS.ByteString -> Int -> Either String Double
fixed16dot16 bs i = do
  x <- i32 bs i
  pure (fromIntegral x / 65536.0)

f2dot14 :: BS.ByteString -> Int -> Either String Double
f2dot14 bs i = do
  x <- i16 bs i
  pure (fromIntegral x / 16384.0)

tag4 :: BS.ByteString -> Int -> Either String String
tag4 bs i = do
  b0 <- u8 bs i
  b1 <- u8 bs (i + 1)
  b2 <- u8 bs (i + 2)
  b3 <- u8 bs (i + 3)
  pure [chr (fromIntegral b0), chr (fromIntegral b1), chr (fromIntegral b2), chr (fromIntegral b3)]

slice :: BS.ByteString -> Int -> Int -> Either String BS.ByteString
slice bs off len = do
  ensureSlice "slice" bs off len
  pure (BS.take len (BS.drop off bs))

ensureSlice :: String -> BS.ByteString -> Int -> Int -> Either String ()
ensureSlice label bs off len
  | off < 0 || len < 0 = Left (label <> " has negative offset/length.")
  | off + len > BS.length bs = Left (label <> " extends beyond input buffer.")
  | otherwise = Right ()

ensureAtLeast :: String -> BS.ByteString -> Int -> Int -> Either String ()
ensureAtLeast label bs off need
  | off < 0 || need < 0 = Left (label <> " has negative offset/size.")
  | off + need > BS.length bs = Left (label <> " read exceeds input buffer.")
  | otherwise = Right ()

ensureMin :: String -> Int -> BS.ByteString -> Either String ()
ensureMin label n bs
  | BS.length bs < n = Left (label <> " table is too short.")
  | otherwise = Right ()

clamp0 :: Int -> Int
clamp0 x
  | x < 0 = 0
  | otherwise = x

clamp :: Int -> Int -> Int -> Int
clamp x lo hi
  | hi < lo = lo
  | x < lo = lo
  | x > hi = hi
  | otherwise = x

flagArgsAreWords :: Int
flagArgsAreWords = 0x0001

flagArgsAreXY :: Int
flagArgsAreXY = 0x0002

flagScale :: Int
flagScale = 0x0008

flagMoreComponents :: Int
flagMoreComponents = 0x0020

flagXYScale :: Int
flagXYScale = 0x0040

flagTwoByTwo :: Int
flagTwoByTwo = 0x0080

flagInstructions :: Int
flagInstructions = 0x0100
