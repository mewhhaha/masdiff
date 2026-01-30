module MSDF.TTF.Parser
  ( TTF(..)
  , Head(..)
  , Hhea(..)
  , Maxp(..)
  , Hmtx(..)
  , Loca(..)
  , Cmap(..)
  , NameTable(..)
  , ParseError(..)
  , parseTTF
  , parseTTFUnsafe
  , glyphOutline
  , glyphBBoxRaw
  , compositeMetricsGlyph
  ) where

import Control.Exception (SomeException, try)
import Data.Array (Array, array, (!))
import Data.Bits (testBit, shiftL, (.|.), (.&.))
import Data.Char (chr)
import Data.Int (Int16)
import Data.List (sortOn)
import Data.Word (Word8, Word16, Word32)
import MSDF.Binary
import MSDF.Outline (Point(..))
import MSDF.TTF.GPOS (KerningPairRaw(..), parseGPOS)

-- | Parsed TrueType font data (subset).
data TTF = TTF
  { ttfHead :: Head
  , ttfHhea :: Hhea
  , ttfMaxp :: Maxp
  , ttfHmtx :: Hmtx
  , ttfLoca :: Loca
  , ttfGlyf :: ByteBuffer
  , ttfCmap :: Cmap
  , ttfKern :: [KerningPairRaw]
  , ttfGpos :: [KerningPairRaw]
  , ttfName :: NameTable
  }

-- | head table

data Head = Head
  { headUnitsPerEm :: Int
  , headIndexToLocFormat :: Int
  , headXMin :: Int
  , headYMin :: Int
  , headXMax :: Int
  , headYMax :: Int
  }

-- | hhea table

data Hhea = Hhea
  { hheaAscent :: Int
  , hheaDescent :: Int
  , hheaLineGap :: Int
  , hheaNumberOfHMetrics :: Int
  }

-- | maxp table

data Maxp = Maxp
  { maxpNumGlyphs :: Int
  }

-- | hmtx table

data Hmtx = Hmtx
  { hmtxAdvances :: Array Int Int
  , hmtxLSB :: Array Int Int
  }

-- | loca table

data Loca = Loca
  { locaOffsets :: Array Int Int
  }

-- | cmap table

data Cmap = Cmap
  { cmapMappings :: [(Int, Int)]
  }

-- | name table

data NameTable = NameTable
  { nameFamily :: String
  , nameStyle :: String
  }

data ParseError = ParseError
  { peContext :: String
  , peMessage :: String
  } deriving (Eq, Show)

parseTTF :: FilePath -> IO (Either ParseError TTF)
parseTTF path = do
  result <- try (parseTTFUnsafe path)
  case result of
    Left (e :: SomeException) ->
      pure (Left (ParseError { peContext = "parseTTF", peMessage = show e }))
    Right ttf -> pure (Right ttf)

parseTTFUnsafe :: FilePath -> IO TTF
parseTTFUnsafe path = do
  bb <- readByteBuffer path
  let tables = parseTableDirectory bb
      headTbl = requireTable "head" tables bb
      hheaTbl = requireTable "hhea" tables bb
      maxpTbl = requireTable "maxp" tables bb
      hmtxTbl = requireTable "hmtx" tables bb
      locaTbl = requireTable "loca" tables bb
      glyfTbl = requireTable "glyf" tables bb
      cmapTbl = requireTable "cmap" tables bb
      kernTbl = optionalTable "kern" tables bb
      gposTbl = optionalTable "GPOS" tables bb
      nameTbl = optionalTable "name" tables bb
      headInfo = parseHead headTbl
      hhea = parseHhea hheaTbl
      maxp = parseMaxp maxpTbl
      hmtx = parseHmtx hmtxTbl (hheaNumberOfHMetrics hhea) (maxpNumGlyphs maxp)
      loca = parseLoca locaTbl (headIndexToLocFormat headInfo) (maxpNumGlyphs maxp)
      cmap = parseCmap cmapTbl
      kern = maybe [] parseKern kernTbl
      gpos = maybe [] (parseGPOS (maxpNumGlyphs maxp)) gposTbl
      name = maybe (NameTable "" "") parseName nameTbl
  pure TTF
    { ttfHead = headInfo
    , ttfHhea = hhea
    , ttfMaxp = maxp
    , ttfHmtx = hmtx
    , ttfLoca = loca
    , ttfGlyf = glyfTbl
    , ttfCmap = cmap
    , ttfKern = kern
    , ttfGpos = gpos
    , ttfName = name
    }

-- Table directory -----------------------------------------------------------

type TableDirectory = [(String, (Int, Int))]

parseTableDirectory :: ByteBuffer -> TableDirectory
parseTableDirectory bb =
  let numTables = fromIntegral (readU16BE bb 4)
      start = 12
  in [ (readTag bb (start + i * 16),
        (fromIntegral (readU32BE bb (start + i * 16 + 8)),
         fromIntegral (readU32BE bb (start + i * 16 + 12))))
     | i <- [0 .. numTables - 1] ]

requireTable :: String -> TableDirectory -> ByteBuffer -> ByteBuffer
requireTable tag tables bb =
  case lookup tag tables of
    Nothing -> error ("Missing table: " ++ tag)
    Just (off, len) -> slice bb off len

optionalTable :: String -> TableDirectory -> ByteBuffer -> Maybe ByteBuffer
optionalTable tag tables bb =
  case lookup tag tables of
    Nothing -> Nothing
    Just (off, len) -> Just (slice bb off len)

-- head ---------------------------------------------------------------------

parseHead :: ByteBuffer -> Head
parseHead bb =
  let unitsPerEm = fromIntegral (readU16BE bb 18)
      xMin = fromIntegral (readS16BE bb 36)
      yMin = fromIntegral (readS16BE bb 38)
      xMax = fromIntegral (readS16BE bb 40)
      yMax = fromIntegral (readS16BE bb 42)
      indexToLocFormat = fromIntegral (readS16BE bb 50)
  in Head unitsPerEm indexToLocFormat xMin yMin xMax yMax

-- hhea ---------------------------------------------------------------------

parseHhea :: ByteBuffer -> Hhea
parseHhea bb =
  let ascent = fromIntegral (readS16BE bb 4)
      descent = fromIntegral (readS16BE bb 6)
      lineGap = fromIntegral (readS16BE bb 8)
      numberOfHMetrics = fromIntegral (readU16BE bb 34)
  in Hhea ascent descent lineGap numberOfHMetrics

-- maxp ---------------------------------------------------------------------

parseMaxp :: ByteBuffer -> Maxp
parseMaxp bb =
  let numGlyphs = fromIntegral (readU16BE bb 4)
  in Maxp numGlyphs

-- hmtx ---------------------------------------------------------------------

parseHmtx :: ByteBuffer -> Int -> Int -> Hmtx
parseHmtx bb numberOfHMetrics numGlyphs =
  let metrics = [ (fromIntegral (readU16BE bb (i * 4)), fromIntegral (readS16BE bb (i * 4 + 2)))
                | i <- [0 .. numberOfHMetrics - 1] ]
      advances = map fst metrics
      lsbs = map snd metrics
      lastAdvance = if null advances then 0 else last advances
      remaining = numGlyphs - numberOfHMetrics
      extraLsbs = [ fromIntegral (readS16BE bb (numberOfHMetrics * 4 + i * 2))
                  | i <- [0 .. remaining - 1] ]
      advancesAll = advances ++ replicate remaining lastAdvance
      lsbAll = lsbs ++ extraLsbs
  in Hmtx
     { hmtxAdvances = array (0, numGlyphs - 1) (zip [0..] advancesAll)
     , hmtxLSB = array (0, numGlyphs - 1) (zip [0..] lsbAll)
     }

-- loca ---------------------------------------------------------------------

parseLoca :: ByteBuffer -> Int -> Int -> Loca
parseLoca bb indexToLocFormat numGlyphs =
  let count = numGlyphs + 1
      offsets = case indexToLocFormat of
        0 -> [ fromIntegral (readU16BE bb (i * 2)) * 2 | i <- [0 .. count - 1] ]
        _ -> [ fromIntegral (readU32BE bb (i * 4)) | i <- [0 .. count - 1] ]
  in Loca { locaOffsets = array (0, count - 1) (zip [0..] offsets) }

-- cmap ---------------------------------------------------------------------

parseCmap :: ByteBuffer -> Cmap
parseCmap bb =
  let numTables = fromIntegral (readU16BE bb 2)
      records = [ (fromIntegral (readU16BE bb (4 + i * 8)) :: Int,
                   fromIntegral (readU16BE bb (4 + i * 8 + 2)) :: Int,
                   fromIntegral (readU32BE bb (4 + i * 8 + 4)))
                | i <- [0 .. numTables - 1] ]
      -- Each record: (platformID, encodingID, offset)
      subTables = [ (pid, eid, off, fromIntegral (readU16BE bb (fromIntegral off)))
                  | (pid, eid, off) <- records ]
      chosen = chooseCmapSubtable subTables
      mappings = case chosen of
        Nothing -> []
        Just (_, _, off, fmt) ->
          case fmt of
            4 -> parseCmapFormat4 (slice bb (fromIntegral off) (bbLen bb - fromIntegral off))
            6 -> parseCmapFormat6 (slice bb (fromIntegral off) (bbLen bb - fromIntegral off))
            10 -> parseCmapFormat10 (slice bb (fromIntegral off) (bbLen bb - fromIntegral off))
            12 -> parseCmapFormat12 (slice bb (fromIntegral off) (bbLen bb - fromIntegral off))
            _ -> []
  in Cmap { cmapMappings = sortOn fst mappings }

chooseCmapSubtable :: [(Int, Int, Word32, Int)] -> Maybe (Int, Int, Word32, Int)
chooseCmapSubtable subs =
  let pick p e f = filter (\(pid, eid, _off, fmt) -> pid == p && eid == e && fmt == f) subs
      candidates = concat
        [ pick 3 10 12
        , pick 0 4 12
        , pick 3 10 10
        , pick 0 4 10
        , pick 3 1 4
        , pick 0 3 4
        , pick 0 0 4
        , pick 3 1 6
        , pick 0 3 6
        ]
  in case candidates of
       (x:_) -> Just x
       [] -> Nothing

parseCmapFormat4 :: ByteBuffer -> [(Int, Int)]
parseCmapFormat4 bb =
  let segCountX2 = fromIntegral (readU16BE bb 6)
      segCount = segCountX2 `div` 2
      endCodeOff = 14
      endCodes = [ fromIntegral (readU16BE bb (endCodeOff + i * 2)) | i <- [0 .. segCount - 1] ]
      startCodeOff = endCodeOff + segCount * 2 + 2
      startCodes = [ fromIntegral (readU16BE bb (startCodeOff + i * 2)) | i <- [0 .. segCount - 1] ]
      idDeltaOff = startCodeOff + segCount * 2
      idDeltas = [ fromIntegral (readS16BE bb (idDeltaOff + i * 2)) | i <- [0 .. segCount - 1] ]
      idRangeOff = idDeltaOff + segCount * 2
      idRangeOffsets = [ fromIntegral (readU16BE bb (idRangeOff + i * 2)) | i <- [0 .. segCount - 1] ]
  in concat
     [ mappingsForSegment start end delta rangeOff base
     | i <- [0 .. segCount - 1]
     , let start = startCodes !! i
           end = endCodes !! i
           delta = idDeltas !! i
           rangeOff = idRangeOffsets !! i
           base = idRangeOff + i * 2
     , end /= 0xFFFF
     , start <= end
     ]
  where
    mappingsForSegment start end delta rangeOff base =
      [ (code, glyphIndex code start delta rangeOff base)
      | code <- [start .. end] ]

    glyphIndex code start delta rangeOff base =
      if rangeOff == 0
      then (code + delta) `mod` 65536
      else
        let glyphOff = base + rangeOff + 2 * (code - start)
            gid = fromIntegral (readU16BE bb glyphOff)
        in if gid == 0 then 0 else (gid + delta) `mod` 65536

parseCmapFormat12 :: ByteBuffer -> [(Int, Int)]
parseCmapFormat12 bb =
  let nGroups = fromIntegral (readU32BE bb 12)
      groupsOff = 16
      groupMappings i =
        let off = groupsOff + i * 12
            startChar = fromIntegral (readU32BE bb off) :: Int
            endChar = fromIntegral (readU32BE bb (off + 4)) :: Int
            startGlyph = fromIntegral (readU32BE bb (off + 8)) :: Int
        in [ (code, startGlyph + (code - startChar)) | code <- [startChar .. endChar] ]
  in concat
     [ groupMappings i
     | i <- [0 .. nGroups - 1]
     ]

parseCmapFormat6 :: ByteBuffer -> [(Int, Int)]
parseCmapFormat6 bb =
  let firstCode = fromIntegral (readU16BE bb 6)
      entryCount = fromIntegral (readU16BE bb 8)
      glyphOff = 10
      glyphs = [ fromIntegral (readU16BE bb (glyphOff + i * 2)) | i <- [0 .. entryCount - 1] ]
  in [ (firstCode + i, g) | (i, g) <- zip [0..] glyphs ]

parseCmapFormat10 :: ByteBuffer -> [(Int, Int)]
parseCmapFormat10 bb =
  let startChar = fromIntegral (readU32BE bb 12) :: Int
      numChars = fromIntegral (readU32BE bb 16) :: Int
      glyphOff = 20
      glyphs = [ fromIntegral (readU16BE bb (glyphOff + i * 2)) | i <- [0 .. numChars - 1] ]
  in [ (startChar + i, g) | (i, g) <- zip [0..] glyphs ]

-- kern ---------------------------------------------------------------------

parseKern :: ByteBuffer -> [KerningPairRaw]
parseKern bb =
  let nTables = (fromIntegral (readU16BE bb 2)) :: Int
      subTablesOff = 4
  in concat [ parseKernSubtable bb (subTablesOff + offset) | offset <- subTableOffsets nTables subTablesOff ]
  where
    subTableOffsets n off =
      let go _ 0 acc = reverse acc
          go cur n' acc =
            let lengthTbl = fromIntegral (readU16BE bb (cur + 2))
            in go (cur + lengthTbl) (n' - 1) (cur - off : acc)
      in go off n []

parseKernSubtable :: ByteBuffer -> Int -> [KerningPairRaw]
parseKernSubtable bb off =
  let coverage = readU16BE bb (off + 4)
      format = (fromIntegral (coverage .&. 0xFF)) :: Int
      horizontal = testBit coverage 0
  in if format == 0 && horizontal
     then
       let nPairs = fromIntegral (readU16BE bb (off + 6))
           pairsOff = off + 14
       in [ KerningPairRaw
             { kpLeft = fromIntegral (readU16BE bb (pairsOff + i * 6))
             , kpRight = fromIntegral (readU16BE bb (pairsOff + i * 6 + 2))
             , kpXAdvance = fromIntegral (readS16BE bb (pairsOff + i * 6 + 4))
             }
          | i <- [0 .. nPairs - 1] ]
     else []

-- name ---------------------------------------------------------------------

parseName :: ByteBuffer -> NameTable
parseName bb =
  let count = fromIntegral (readU16BE bb 2)
      stringOffset = fromIntegral (readU16BE bb 4)
      records = [ nameRecord bb (6 + i * 12) stringOffset | i <- [0 .. count - 1] ]
      family = pickName 1 records
      style = pickName 2 records
  in NameTable family style

nameRecord :: ByteBuffer -> Int -> Int -> (Int, Int, Int, Int, String)
nameRecord bb off stringOffset =
  let platformID = fromIntegral (readU16BE bb off)
      encodingID = fromIntegral (readU16BE bb (off + 2))
      languageID = fromIntegral (readU16BE bb (off + 4))
      nameID = fromIntegral (readU16BE bb (off + 6))
      lengthBytes = fromIntegral (readU16BE bb (off + 8))
      offsetBytes = fromIntegral (readU16BE bb (off + 10))
      strOff = stringOffset + offsetBytes
      bytes = [ readU8 bb (strOff + i) | i <- [0 .. lengthBytes - 1] ]
      str = decodeName platformID encodingID bytes
  in (platformID, encodingID, languageID, nameID, str)

pickName :: Int -> [(Int, Int, Int, Int, String)] -> String
pickName nameID records =
  let matches = [ r | r@(_p,_e,_l,n,_s) <- records, n == nameID ]
      byPriority = preferEnglishUnicode matches
  in case byPriority of
       ((_,_,_,_,s):_) -> s
       [] -> ""

preferEnglishUnicode :: [(Int, Int, Int, Int, String)] -> [(Int, Int, Int, Int, String)]
preferEnglishUnicode records =
  let isEnglish (_,_,lang,_,_) = lang == 0x0409
      isWinUnicode (p,_,_,_,_) = p == 3
      isUnicode (p,_,_,_,_) = p == 0
      score :: (Int, Int, Int, Int, String) -> (Int, Int)
      score r =
        ( if isEnglish r then 0 else 1
        , if isWinUnicode r then 0 else if isUnicode r then 1 else 2
        )
  in sortOn score records

decodeName :: Int -> Int -> [Word8] -> String
decodeName platformID _encodingID bytes
  | platformID == 3 || platformID == 0 = decodeUTF16BE bytes
  | otherwise = map (chr . fromIntegral) bytes

decodeUTF16BE :: [Word8] -> String
decodeUTF16BE [] = []
decodeUTF16BE [_] = []
decodeUTF16BE (b0:b1:rest) =
  let w = (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1 :: Word16
      ch = chr (fromIntegral w)
  in ch : decodeUTF16BE rest

-- Glyph outlines ------------------------------------------------------------

glyphOutline :: TTF -> Int -> [[Point]]
glyphOutline ttf glyphIndex =
  glyphContoursAt ttf glyphIndex 0

glyphContoursAt :: TTF -> Int -> Int -> [[Point]]
glyphContoursAt ttf glyphIndex depth =
  let glyf = ttfGlyf ttf
      loca = ttfLoca ttf
      offsets = locaOffsets loca
      start = offsets ! glyphIndex
      end = offsets ! (glyphIndex + 1)
  in if end <= start
     then []
     else parseGlyphContours glyf start (end - start) depth ttf glyphIndex

-- | Raw bounding box for a glyph (font units).
glyphBBoxRaw :: TTF -> Int -> (Int, Int, Int, Int)
glyphBBoxRaw ttf glyphIndex =
  let glyf = ttfGlyf ttf
      loca = ttfLoca ttf
      offsets = locaOffsets loca
      start = offsets ! glyphIndex
      end = offsets ! (glyphIndex + 1)
  in if end <= start
     then (0,0,0,0)
     else
       let bb = slice glyf start (end - start)
           xMin = fromIntegral (readS16BE bb 2)
           yMin = fromIntegral (readS16BE bb 4)
           xMax = fromIntegral (readS16BE bb 6)
           yMax = fromIntegral (readS16BE bb 8)
       in (xMin, yMin, xMax, yMax)

parseGlyphContours :: ByteBuffer -> Int -> Int -> Int -> TTF -> Int -> [[Point]]
parseGlyphContours glyf off len depth ttf glyphIndex =
  let bb = slice glyf off len
      numContours = fromIntegral (readS16BE bb 0) :: Int
  in if numContours >= 0
     then parseSimpleGlyph bb numContours
     else if depth > 16
          then []
          else parseCompositeGlyph bb depth ttf glyphIndex

parseSimpleGlyph :: ByteBuffer -> Int -> [[Point]]
parseSimpleGlyph bb numContours =
  let endPtsOff = 10
      endPts = [ fromIntegral (readU16BE bb (endPtsOff + i * 2))
               | i <- [0 .. numContours - 1] ]
      numPoints = if null endPts then 0 else last endPts + 1
      instructionLength = fromIntegral (readU16BE bb (endPtsOff + numContours * 2))
      flagsOff = endPtsOff + numContours * 2 + 2 + instructionLength
      flags = readFlags bb flagsOff numPoints
      xOff = flagsOff + length flags
      (xs, yOff) = readCoords bb xOff flags True
      (ys, _) = readCoords bb yOff flags False
      points = [ Point (fromIntegral (xs !! i)) (fromIntegral (ys !! i)) (testBit (flags !! i) 0)
               | i <- [0 .. numPoints - 1] ]
  in splitContours points endPts

readFlags :: ByteBuffer -> Int -> Int -> [Word8]
readFlags bb off count = reverse (go off count [])
  where
    go _ 0 acc = acc
    go idx n acc =
      let flag = readU8 bb idx
          repeatCount = if testBit flag 3
                        then fromIntegral (readU8 bb (idx + 1))
                        else 0
          toTake = min n (repeatCount + 1)
          acc' = replicate toTake flag ++ acc
          idx' = idx + 1 + if repeatCount > 0 then 1 else 0
      in go idx' (n - toTake) acc'

readCoords :: ByteBuffer -> Int -> [Word8] -> Bool -> ([Int], Int)
readCoords bb off flags isX =
  go flags off 0 []
  where
    go [] idx _ acc = (reverse acc, idx)
    go (flag:rest) idx lastVal acc =
      let isShort = testBit flag (if isX then 1 else 2)
          isSame = testBit flag (if isX then 4 else 5)
          (delta, idx') = if isShort
                          then
                            let v = fromIntegral (readU8 bb idx) :: Int
                            in if isSame then (v, idx + 1) else (-v, idx + 1)
                          else if isSame
                               then (0, idx)
                               else (fromIntegral (readS16BE bb idx), idx + 2)
          val = lastVal + delta
      in go rest idx' val (val : acc)

splitContours :: [Point] -> [Int] -> [[Point]]
splitContours points endPts =
  let indices = map (+1) endPts
      slices = go points indices
  in slices
  where
    go _ [] = []
    go pts (n:ns) =
      let (a, b) = splitAt n pts
      in a : go b (map (subtract n) ns)

parseCompositeGlyph :: ByteBuffer -> Int -> TTF -> Int -> [[Point]]
parseCompositeGlyph bb depth ttf _glyphIndex =
  let flagsOff = 10
  in go flagsOff []
  where
    go off acc =
      let flags = readU16BE bb off
          glyphIndex = fromIntegral (readU16BE bb (off + 2))
          (arg1, arg2, offArgs) =
            if testBit flags 0
            then (fromIntegral (readS16BE bb (off + 4)), fromIntegral (readS16BE bb (off + 6)), off + 8)
            else (fromIntegral (readU8 bb (off + 4)), fromIntegral (readU8 bb (off + 5)), off + 6)
          (transform, offTrans) = readTransform bb offArgs flags
          contours = glyphContoursAt ttf glyphIndex (depth + 1)
          transformedNoTrans = map (map (applyTransform transform 0 0)) contours
          (dx, dy) = if testBit flags 1
                     then applyComponentOffset flags transform (fromIntegral arg1, fromIntegral arg2)
                     else pointMatchOffset acc transformedNoTrans arg1 arg2
          (dx', dy') = if testBit flags 2
                       then (fromIntegral (round dx :: Int), fromIntegral (round dy :: Int))
                       else (dx, dy)
          transformed = map (map (applyTransform transform dx' dy')) contours
          acc' = acc ++ transformed
      in if testBit flags 5
         then go offTrans acc'
         else acc'

readTransform :: ByteBuffer -> Int -> Word16 -> ((Double, Double, Double, Double), Int)
readTransform bb off flags
  | testBit flags 3 =
      let scale = f2dot14 (readS16BE bb off)
      in ((scale, 0, 0, scale), off + 2)
  | testBit flags 6 =
      let xscale = f2dot14 (readS16BE bb off)
          yscale = f2dot14 (readS16BE bb (off + 2))
      in ((xscale, 0, 0, yscale), off + 4)
  | testBit flags 7 =
      let a = f2dot14 (readS16BE bb off)
          b = f2dot14 (readS16BE bb (off + 2))
          c = f2dot14 (readS16BE bb (off + 4))
          d = f2dot14 (readS16BE bb (off + 6))
      in ((a, b, c, d), off + 8)
  | otherwise = ((1, 0, 0, 1), off)

applyTransform :: (Double, Double, Double, Double) -> Double -> Double -> Point -> Point
applyTransform (a,b,c,d) dx dy p =
  let x' = a * px p + b * py p + dx
      y' = c * px p + d * py p + dy
  in p { px = x', py = y' }

f2dot14 :: Int16 -> Double
f2dot14 v =
  let i = fromIntegral v :: Double
  in i / 16384.0

applyComponentOffset :: Word16 -> (Double, Double, Double, Double) -> (Double, Double) -> (Double, Double)
applyComponentOffset flags (a,b,c,d) (dx, dy)
  | testBit flags 12 = (dx, dy)
  | testBit flags 11 = (a * dx + b * dy, c * dx + d * dy)
  | hasScale = (a * dx + b * dy, c * dx + d * dy)
  | otherwise = (dx, dy)
  where
    hasScale = testBit flags 3 || testBit flags 6 || testBit flags 7

pointMatchOffset :: [[Point]] -> [[Point]] -> Int -> Int -> (Double, Double)
pointMatchOffset compositeContours componentContours compPointIndex compGlyphPointIndex =
  case (pointAt componentContours compPointIndex, pointAt compositeContours compGlyphPointIndex) of
    (Just compPt, Just basePt) ->
      let dx = px basePt - px compPt
          dy = py basePt - py compPt
      in (dx, dy)
    _ -> (0, 0)

pointAt :: [[Point]] -> Int -> Maybe Point
pointAt contours idx
  | idx < 0 = Nothing
  | otherwise = go contours idx
  where
    go [] _ = Nothing
    go (c:cs) n
      | n < length c = Just (c !! n)
      | otherwise = go cs (n - length c)

compositeMetricsGlyph :: TTF -> Int -> Maybe Int
compositeMetricsGlyph ttf glyphIndex =
  let glyf = ttfGlyf ttf
      offsets = locaOffsets (ttfLoca ttf)
      start = offsets ! glyphIndex
      end = offsets ! (glyphIndex + 1)
  in if end <= start
     then Nothing
     else
       let bb = slice glyf start (end - start)
           numContours = fromIntegral (readS16BE bb 0) :: Int
       in if numContours >= 0
          then Nothing
          else compositeUseMyMetrics bb

compositeUseMyMetrics :: ByteBuffer -> Maybe Int
compositeUseMyMetrics bb = go 10
  where
    go off =
      let flags = readU16BE bb off
          glyphIndex = fromIntegral (readU16BE bb (off + 2))
          argsAreWords = testBit flags 0
          offArgs = off + 2 + if argsAreWords then 4 else 2
          offTrans = offArgs + transformSize flags
      in if testBit flags 9
         then Just glyphIndex
         else if testBit flags 5
              then go offTrans
              else Nothing

transformSize :: Word16 -> Int
transformSize flags
  | testBit flags 3 = 2
  | testBit flags 6 = 4
  | testBit flags 7 = 8
  | otherwise = 0
