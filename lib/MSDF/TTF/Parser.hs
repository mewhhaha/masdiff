module MSDF.TTF.Parser
  ( TTF(..)
  , Head(..)
  , Hhea(..)
  , Vhea(..)
  , Maxp(..)
  , Hmtx(..)
  , Vmtx(..)
  , Loca(..)
  , Cmap(..)
  , NameTable(..)
  , ParseError(..)
  , Variations(..)
  , VariationLocation(..)
  , normalizeLocation
  , defaultLocation
  , parseTTF
  , parseTTFBytes
  , parseTTFUnsafe
  , glyphOutline
  , glyphOutlineAt
  , glyphContoursAtVar
  , glyphBBoxRaw
  , glyphBBoxRawAt
  , compositeMetricsGlyph
  , compositeComponentCount
  ) where

import Control.Exception (SomeException, try, evaluate)
import qualified Data.ByteString as BS
import Data.Array (Array, array, bounds, inRange, (!))
import Data.Bits (testBit, shiftL, (.|.), (.&.))
import Data.Char (chr)
import Data.Int (Int16, Int8)
import Data.List (sortOn)
import Data.Word (Word8, Word16, Word32)
import MSDF.Binary
import MSDF.Outline (Point(..))
import MSDF.TTF.GPOS (KerningPairRaw(..), GPOSAll(..), GPOSMarksRaw(..), parseGPOSAll)
import MSDF.TTF.Variations
import System.IO.Unsafe (unsafePerformIO)

within :: ByteBuffer -> Int -> Int -> Bool
within bb off size =
  off >= 0 && size >= 0 && off + size <= bb.len

-- | Parsed TrueType font data (subset).
data TTF = TTF
  { head :: Head
  , hhea :: Hhea
  , vhea :: Maybe Vhea
  , maxp :: Maxp
  , hmtx :: Hmtx
  , vmtx :: Maybe Vmtx
  , loca :: Loca
  , glyf :: ByteBuffer
  , cmap :: Cmap
  , kern :: [KerningPairRaw]
  , gpos :: [KerningPairRaw]
  , gposMarks :: GPOSMarksRaw
  , name :: NameTable
  , variations :: Maybe Variations
  }

-- | head table

data Head = Head
  { unitsPerEm :: Int
  , indexToLocFormat :: Int
  , xMin :: Int
  , yMin :: Int
  , xMax :: Int
  , yMax :: Int
  }

-- | hhea table

data Hhea = Hhea
  { ascent :: Int
  , descent :: Int
  , lineGap :: Int
  , numberOfHMetrics :: Int
  }

-- | vhea table
data Vhea = Vhea
  { ascent :: Int
  , descent :: Int
  , lineGap :: Int
  , numberOfVMetrics :: Int
  }

-- | maxp table

data Maxp = Maxp
  { numGlyphs :: Int
  }

-- | hmtx table

data Hmtx = Hmtx
  { advances :: Array Int Int
  , lsb :: Array Int Int
  }

-- | vmtx table
data Vmtx = Vmtx
  { advances :: Array Int Int
  , tsb :: Array Int Int
  }

-- | loca table

data Loca = Loca
  { offsets :: Array Int Int
  }

-- | cmap table

data Cmap = Cmap
  { mappings :: [(Int, Int)]
  }

-- | name table

data NameTable = NameTable
  { family :: String
  , style :: String
  }

data ParseError = ParseError
  { context :: String
  , message :: String
  } deriving (Eq, Show)

parseTTF :: FilePath -> IO (Either ParseError TTF)
parseTTF path = do
  result <- try (parseTTFUnsafe path)
  case result of
    Left (e :: SomeException) ->
      pure (Left (ParseError { context = "parseTTF", message = show e }))
    Right ttf -> pure (Right ttf)

parseTTFBytes :: BS.ByteString -> Either ParseError TTF
parseTTFBytes bs =
  unsafePerformIO $ do
    result <- try (evaluate (parseTTFBuffer (readByteBufferBytes bs)))
    case result of
      Left (e :: SomeException) ->
        pure (Left (ParseError { context = "parseTTFBytes", message = show e }))
      Right ttf -> pure (Right ttf)
{-# NOINLINE parseTTFBytes #-}

parseTTFUnsafe :: FilePath -> IO TTF
parseTTFUnsafe path = do
  bb <- readByteBuffer path
  pure (parseTTFBuffer bb)

parseTTFBuffer :: ByteBuffer -> TTF
parseTTFBuffer bb =
  let tables = parseTableDirectory bb
      headTbl = requireTable "head" tables bb
      hheaTbl = requireTable "hhea" tables bb
      maxpTbl = requireTable "maxp" tables bb
      hmtxTbl = requireTable "hmtx" tables bb
      vheaTbl = optionalTable "vhea" tables bb
      vmtxTbl = optionalTable "vmtx" tables bb
      locaTbl = requireTable "loca" tables bb
      glyfTbl = requireTable "glyf" tables bb
      cmapTbl = requireTable "cmap" tables bb
      kernTbl = optionalTable "kern" tables bb
      gposTbl = optionalTable "GPOS" tables bb
      nameTbl = optionalTable "name" tables bb
      fvarTbl = optionalTable "fvar" tables bb
      avarTbl = optionalTable "avar" tables bb
      gvarTbl = optionalTable "gvar" tables bb
      hvarTbl = optionalTable "HVAR" tables bb
      vvarTbl = optionalTable "VVAR" tables bb
      mvarTbl = optionalTable "MVAR" tables bb
      headInfo = parseHead headTbl
      hhea = parseHhea hheaTbl
      vhea = fmap parseVhea vheaTbl
      maxp = parseMaxp maxpTbl
      hmtx = parseHmtx hmtxTbl hhea.numberOfHMetrics maxp.numGlyphs
      vmtx = case (vhea, vmtxTbl) of
               (Just vhea', Just vmtxTable) -> Just (parseVmtx vmtxTable vhea'.numberOfVMetrics maxp.numGlyphs)
               _ -> Nothing
      loca = parseLoca locaTbl headInfo.indexToLocFormat maxp.numGlyphs
      cmap = parseCmap cmapTbl
      kern = maybe [] parseKern kernTbl
      gposAll = maybe (GPOSAll [] (GPOSMarksRaw [] [])) (parseGPOSAll maxp.numGlyphs) gposTbl
      gpos = gposAll.kernPairs
      gposMarks = gposAll.marks
      name = maybe (NameTable "" "") parseName nameTbl
      variations =
        case fvarTbl of
          Nothing -> Nothing
          Just fvarTable ->
            let fvar = parseFvar fvarTable
                avar = fmap parseAvar avarTbl
                gvar = fmap (parseGvar maxp.numGlyphs) gvarTbl
                hvar = hvarTbl >>= parseHvar
                vvar = vvarTbl >>= parseVvar
                mvar = mvarTbl >>= parseMvar
            in Just (Variations fvar avar gvar hvar vvar mvar)
  in TTF
      { head = headInfo
      , hhea = hhea
      , vhea = vhea
      , maxp = maxp
      , hmtx = hmtx
      , vmtx = vmtx
      , loca = loca
      , glyf = glyfTbl
      , cmap = cmap
      , kern = kern
      , gpos = gpos
      , gposMarks = gposMarks
      , name = name
      , variations = variations
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
    Just (off, len) ->
      let bb' = slice bb off len
      in bb' `seq` bb'

optionalTable :: String -> TableDirectory -> ByteBuffer -> Maybe ByteBuffer
optionalTable tag tables bb =
  case lookup tag tables of
    Nothing -> Nothing
    Just (off, len) ->
      let bb' = slice bb off len
      in bb' `seq` Just bb'

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

parseVhea :: ByteBuffer -> Vhea
parseVhea bb =
  let ascent = fromIntegral (readS16BE bb 4)
      descent = fromIntegral (readS16BE bb 6)
      lineGap = fromIntegral (readS16BE bb 8)
      numberOfVMetrics = fromIntegral (readU16BE bb 34)
  in Vhea ascent descent lineGap numberOfVMetrics

-- maxp ---------------------------------------------------------------------

parseMaxp :: ByteBuffer -> Maxp
parseMaxp bb =
  let numGlyphs = fromIntegral (readU16BE bb 4)
  in Maxp numGlyphs

-- hmtx ---------------------------------------------------------------------

parseHmtx :: ByteBuffer -> Int -> Int -> Hmtx
parseHmtx bb numberOfHMetrics numGlyphs =
  let (advancesAll, lsbAll) = parseMetrics bb numberOfHMetrics numGlyphs
  in Hmtx
     { advances = array (0, numGlyphs - 1) (zip [0..] advancesAll)
     , lsb = array (0, numGlyphs - 1) (zip [0..] lsbAll)
     }

parseVmtx :: ByteBuffer -> Int -> Int -> Vmtx
parseVmtx bb numberOfVMetrics numGlyphs =
  let (advancesAll, tsbAll) = parseMetrics bb numberOfVMetrics numGlyphs
  in Vmtx
     { advances = array (0, numGlyphs - 1) (zip [0..] advancesAll)
     , tsb = array (0, numGlyphs - 1) (zip [0..] tsbAll)
     }

parseMetrics :: ByteBuffer -> Int -> Int -> ([Int], [Int])
parseMetrics bb numberOfMetrics numGlyphs =
  let metricsCount0 = min numberOfMetrics numGlyphs
      maxMetrics = min metricsCount0 (bb.len `div` 4)
      metrics = [ (fromIntegral (readU16BE bb (i * 4)), fromIntegral (readS16BE bb (i * 4 + 2)))
                | i <- [0 .. maxMetrics - 1] ]
      advances = map fst metrics
      sideBearings = map snd metrics
      lastAdvance = if null advances then 0 else last advances
      remaining = max 0 (numGlyphs - maxMetrics)
      extraOff = maxMetrics * 4
      maxExtra = if extraOff >= bb.len then 0 else min remaining ((bb.len - extraOff) `div` 2)
      extraSideBearings = [ fromIntegral (readS16BE bb (extraOff + i * 2))
                          | i <- [0 .. maxExtra - 1] ]
      extraAll = extraSideBearings ++ replicate (remaining - maxExtra) 0
      advancesAll = advances ++ replicate remaining lastAdvance
      sideBearingsAll = sideBearings ++ extraAll
  in (advancesAll, sideBearingsAll)

-- loca ---------------------------------------------------------------------

parseLoca :: ByteBuffer -> Int -> Int -> Loca
parseLoca bb indexToLocFormat numGlyphs =
  let count = numGlyphs + 1
      offsets = case indexToLocFormat of
        0 -> [ fromIntegral (readU16BE bb (i * 2)) * 2 | i <- [0 .. count - 1] ]
        _ -> [ fromIntegral (readU32BE bb (i * 4)) | i <- [0 .. count - 1] ]
  in Loca { offsets = array (0, count - 1) (zip [0..] offsets) }

-- cmap ---------------------------------------------------------------------

parseCmap :: ByteBuffer -> Cmap
parseCmap bb =
  let numTablesRaw = if within bb 2 2 then fromIntegral (readU16BE bb 2) else 0
      maxTables = if bb.len < 4 then 0 else min numTablesRaw ((bb.len - 4) `div` 8)
      records = [ (fromIntegral (readU16BE bb (4 + i * 8)) :: Int,
                   fromIntegral (readU16BE bb (4 + i * 8 + 2)) :: Int,
                   fromIntegral (readU32BE bb (4 + i * 8 + 4)))
                | i <- [0 .. maxTables - 1] ]
      -- Each record: (platformID, encodingID, offset)
      subTables = [ (pid, eid, off, fromIntegral (readU16BE bb (fromIntegral off)))
                  | (pid, eid, off) <- records
                  , within bb (fromIntegral off) 2 ]
      chosen = chooseCmapSubtable subTables
      mappings = case chosen of
        Nothing -> []
        Just (_, _, off, fmt) ->
          let off' = fromIntegral off
          in if off' < 0 || off' >= bb.len
             then []
             else case fmt of
               4 -> parseCmapFormat4 (slice bb off' (bb.len - off'))
               6 -> parseCmapFormat6 (slice bb off' (bb.len - off'))
               10 -> parseCmapFormat10 (slice bb off' (bb.len - off'))
               12 -> parseCmapFormat12 (slice bb off' (bb.len - off'))
               _ -> []
  in Cmap { mappings = sortOn fst mappings }

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
  let segCountX2 = if within bb 6 2 then fromIntegral (readU16BE bb 6) else 0
      segCountRaw = segCountX2 `div` 2
      maxSegCount = if bb.len < 16 then 0 else (bb.len - 16) `div` 8
      segCount = min segCountRaw maxSegCount
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
            gid = if within bb glyphOff 2 then fromIntegral (readU16BE bb glyphOff) else 0
        in if gid == 0 then 0 else (gid + delta) `mod` 65536

parseCmapFormat12 :: ByteBuffer -> [(Int, Int)]
parseCmapFormat12 bb =
  let nGroupsRaw = if within bb 12 4 then fromIntegral (readU32BE bb 12) else 0
      groupsOff = 16
      maxGroups = if bb.len < groupsOff then 0 else (bb.len - groupsOff) `div` 12
      nGroups = min nGroupsRaw maxGroups
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
  let firstCode = if within bb 6 2 then fromIntegral (readU16BE bb 6) else 0
      entryCountRaw = if within bb 8 2 then fromIntegral (readU16BE bb 8) else 0
      glyphOff = 10
      maxEntries = if bb.len < glyphOff then 0 else (bb.len - glyphOff) `div` 2
      entryCount = min entryCountRaw maxEntries
      glyphs = [ fromIntegral (readU16BE bb (glyphOff + i * 2)) | i <- [0 .. entryCount - 1] ]
  in [ (firstCode + i, g) | (i, g) <- zip [0..] glyphs ]

parseCmapFormat10 :: ByteBuffer -> [(Int, Int)]
parseCmapFormat10 bb =
  let startChar = if within bb 12 4 then fromIntegral (readU32BE bb 12) else 0 :: Int
      numCharsRaw = if within bb 16 4 then fromIntegral (readU32BE bb 16) else 0 :: Int
      glyphOff = 20
      maxChars = if bb.len < glyphOff then 0 else (bb.len - glyphOff) `div` 2
      numChars = min numCharsRaw maxChars
      glyphs = [ fromIntegral (readU16BE bb (glyphOff + i * 2)) | i <- [0 .. numChars - 1] ]
  in [ (startChar + i, g) | (i, g) <- zip [0..] glyphs ]

-- kern ---------------------------------------------------------------------

parseKern :: ByteBuffer -> [KerningPairRaw]
parseKern bb =
  let nTables = if within bb 2 2 then (fromIntegral (readU16BE bb 2)) :: Int else 0
      subTablesOff = 4
  in concat [ parseKernSubtable bb (subTablesOff + offset) | offset <- subTableOffsets nTables subTablesOff ]
  where
    subTableOffsets n off =
      let go _ 0 acc = reverse acc
          go cur n' acc
            | not (within bb cur 4) = reverse acc
            | otherwise =
                let lengthTbl = fromIntegral (readU16BE bb (cur + 2))
                    next = cur + lengthTbl
                in if lengthTbl < 6 || not (within bb cur lengthTbl)
                   then reverse acc
                   else go next (n' - 1) (cur - off : acc)
      in go off n []

parseKernSubtable :: ByteBuffer -> Int -> [KerningPairRaw]
parseKernSubtable bb off =
  if not (within bb off 8)
  then []
  else
    let coverage = readU16BE bb (off + 4)
        format = (fromIntegral (coverage .&. 0xFF)) :: Int
        horizontal = testBit coverage 0
    in if format == 0 && horizontal
       then
         let nPairsRaw = fromIntegral (readU16BE bb (off + 6))
             pairsOff = off + 14
             maxPairs = if bb.len < pairsOff then 0 else (bb.len - pairsOff) `div` 6
             nPairs = min nPairsRaw maxPairs
         in [ KerningPairRaw
               { left = fromIntegral (readU16BE bb (pairsOff + i * 6))
               , right = fromIntegral (readU16BE bb (pairsOff + i * 6 + 2))
               , xAdvance = fromIntegral (readS16BE bb (pairsOff + i * 6 + 4))
               }
            | i <- [0 .. nPairs - 1] ]
       else []

-- name ---------------------------------------------------------------------

parseName :: ByteBuffer -> NameTable
parseName bb =
  if not (within bb 0 6)
  then NameTable "" ""
  else
    let countRaw = fromIntegral (readU16BE bb 2)
        stringOffset = fromIntegral (readU16BE bb 4)
        maxRecords = min countRaw ((bb.len - 6) `div` 12)
        records = [ nameRecord bb (6 + i * 12) stringOffset | i <- [0 .. maxRecords - 1] ]
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
      maxLen = max 0 (bb.len - strOff)
      length' = min lengthBytes maxLen
      bytes = [ readU8 bb (strOff + i) | i <- [0 .. length' - 1] ]
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
  glyphOutlineAt Nothing ttf glyphIndex

glyphOutlineAt :: Maybe VariationLocation -> TTF -> Int -> [[Point]]
glyphOutlineAt loc ttf glyphIndex =
  fst (glyphContoursAtVar loc ttf glyphIndex 0)

glyphContoursAtVar :: Maybe VariationLocation -> TTF -> Int -> Int -> ([[Point]], (Double, Double, Double, Double))
glyphContoursAtVar loc ttf glyphIndex depth =
  let numGlyphs = ttf.maxp.numGlyphs
      glyf = ttf.glyf
      loca = ttf.loca
      offsets = loca.offsets
      (offLo, offHi) = bounds offsets
      loc' = case loc of
        Just l | isDefaultLocation l -> Nothing
        _ -> loc
  in if glyphIndex < 0 || glyphIndex + 1 >= numGlyphs
        || glyphIndex < offLo || glyphIndex + 1 > offHi
     then ([], (0,0,0,0))
     else
       let start = offsets ! glyphIndex
           end = offsets ! (glyphIndex + 1)
       in if end <= start || end > glyf.len
         then ([], (0,0,0,0))
         else
           let bb = slice glyf start (end - start)
               numContours = if bb.len < 2 then 0 else fromIntegral (readS16BE bb 0) :: Int
               gvarMaybe = case ttf.variations of
                             Just vars -> vars.gvar
                             Nothing -> Nothing
           in if bb.len < 2
              then ([], (0,0,0,0))
           else if numContours >= 0
                   then
                     let baseContours = parseSimpleGlyph bb numContours
                     in case (loc', gvarMaybe) of
                          (Just loc', Just gv) -> applyGvarToContours gv loc' glyphIndex baseContours
                          _ -> (baseContours, (0,0,0,0))
                   else if depth > 16
                        then ([], (0,0,0,0))
                        else
                          parseCompositeGlyph loc' gvarMaybe bb depth ttf glyphIndex

-- | Raw bounding box for a glyph (font units).
glyphBBoxRaw :: TTF -> Int -> (Int, Int, Int, Int)
glyphBBoxRaw ttf glyphIndex =
  glyphBBoxRawAt Nothing ttf glyphIndex

glyphBBoxRawAt :: Maybe VariationLocation -> TTF -> Int -> (Int, Int, Int, Int)
glyphBBoxRawAt loc ttf glyphIndex =
  let numGlyphs = ttf.maxp.numGlyphs
      glyf = ttf.glyf
      loca = ttf.loca
      offsets = loca.offsets
      (offLo, offHi) = bounds offsets
      loc' = case loc of
        Just l | isDefaultLocation l -> Nothing
        _ -> loc
  in if glyphIndex < 0 || glyphIndex + 1 >= numGlyphs
        || glyphIndex < offLo || glyphIndex + 1 > offHi
     then (0,0,0,0)
     else
       let start = offsets ! glyphIndex
           end = offsets ! (glyphIndex + 1)
       in if end <= start || end > glyf.len
          then (0,0,0,0)
          else
            case loc' of
              Nothing ->
                let bb = slice glyf start (end - start)
                    xMin = fromIntegral (readS16BE bb 2)
                    yMin = fromIntegral (readS16BE bb 4)
                    xMax = fromIntegral (readS16BE bb 6)
                    yMax = fromIntegral (readS16BE bb 8)
                in (xMin, yMin, xMax, yMax)
              Just _ ->
                let (contours, _) = glyphContoursAtVar loc ttf glyphIndex 0
                in bboxFromContours contours

parseSimpleGlyph :: ByteBuffer -> Int -> [[Point]]
parseSimpleGlyph bb numContours =
  let endPtsOff = 10
  in if not (within bb endPtsOff (numContours * 2 + 2))
     then []
     else
       let endPts = [ fromIntegral (readU16BE bb (endPtsOff + i * 2))
                    | i <- [0 .. numContours - 1] ]
           numPoints = if null endPts then 0 else last endPts + 1
           instructionLength = fromIntegral (readU16BE bb (endPtsOff + numContours * 2))
           flagsOff = endPtsOff + numContours * 2 + 2 + instructionLength
       in if flagsOff > bb.len
          then []
          else
            let (flags, xOff) = readFlags bb flagsOff numPoints
                (xs, yOff) = readCoords bb xOff flags True
                (ys, _) = readCoords bb yOff flags False
                points = [ Point (fromIntegral (xs !! i)) (fromIntegral (ys !! i)) (testBit (flags !! i) 0)
                         | i <- [0 .. min (length flags - 1) (numPoints - 1)] ]
            in splitContours points endPts

bboxFromContours :: [[Point]] -> (Int, Int, Int, Int)
bboxFromContours contours =
  case [ (p.x, p.y) | c <- contours, p <- c ] of
    [] -> (0,0,0,0)
    pts ->
      let xs = map fst pts
          ys = map snd pts
          xMin' = floor (minimum xs)
          yMin' = floor (minimum ys)
          xMax' = ceiling (maximum xs)
          yMax' = ceiling (maximum ys)
      in (xMin', yMin', xMax', yMax')

readFlags :: ByteBuffer -> Int -> Int -> ([Word8], Int)
readFlags bb off count =
  let (flags, next) = go off count []
  in (reverse flags, next)
  where
    go idx 0 acc = (acc, idx)
    go idx n acc =
      if not (within bb idx 1)
      then (acc, idx)
      else
        let flag = readU8 bb idx
            hasRepeat = testBit flag 3 && within bb (idx + 1) 1
            repeatCount = if hasRepeat
                          then fromIntegral (readU8 bb (idx + 1))
                          else 0
            toTake = min n (repeatCount + 1)
            acc' = replicate toTake flag ++ acc
            idx' = idx + 1 + if hasRepeat then 1 else 0
        in go idx' (n - toTake) acc'

readCoords :: ByteBuffer -> Int -> [Word8] -> Bool -> ([Int], Int)
readCoords bb off flags isX =
  go flags off 0 []
  where
    go [] idx _ acc = (reverse acc, idx)
    go (flag:rest) idx lastVal acc =
      let isShort = testBit flag (if isX then 1 else 2)
          isSame = testBit flag (if isX then 4 else 5)
          (delta, idx') =
            if isShort
            then
              if within bb idx 1
              then
                let v = fromIntegral (readU8 bb idx) :: Int
                in if isSame then (v, idx + 1) else (-v, idx + 1)
              else (0, bb.len)
            else if isSame
                 then (0, idx)
                 else if within bb idx 2
                      then (fromIntegral (readS16BE bb idx), idx + 2)
                      else (0, bb.len)
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

data CompositeComponent = CompositeComponent
  { compFlags :: Word16
  , compGlyph :: Int
  , arg1 :: Int
  , arg2 :: Int
  , compTransform :: (Double, Double, Double, Double)
  }

parseCompositeGlyph :: Maybe VariationLocation -> Maybe Gvar -> ByteBuffer -> Int -> TTF -> Int -> ([[Point]], (Double, Double, Double, Double))
parseCompositeGlyph loc gvar bb depth ttf glyphIndex =
  let components = readCompositeComponents bb
      compCount = length components
      hasGvar = case gvar of
        Just gv -> gvarHasData gv glyphIndex
        Nothing -> False
      compLoc = loc
  in if depth > 16
     then ([], (0, 0, 0, 0))
     else
       let baseContoursNoVar = buildContours compLoc components (array (0, -1) []) (array (0, -1) [])
           numPoints = sum (map length baseContoursNoVar)
           pointCount = numPoints + 4
       in case (loc, gvar, hasGvar) of
            (Just loc', Just gv, True) ->
              if compositeUsesPointDeltas gv loc' glyphIndex compCount pointCount
                then applyGvarToContours gv loc' glyphIndex baseContoursNoVar
                else
                  let (compDxs, compDys, compPhantom) = componentDeltas gv loc' glyphIndex compCount
                      contours' = buildContours compLoc components compDxs compDys
                  in (contours', compPhantom)
            _ ->
              (baseContoursNoVar, (0, 0, 0, 0))
  where
    buildContours compLoc comps dxs dys =
      let go _ acc [] = acc
          go idx acc (c:cs) =
            let (contours, _) = glyphContoursAtVar compLoc ttf c.compGlyph (depth + 1)
                transformedNoTrans = map (map (applyTransform c.compTransform 0 0)) contours
                argsAreXY = testBit c.compFlags 1
                (dx0, dy0) =
                  if argsAreXY
                  then applyComponentOffset c.compFlags c.compTransform (fromIntegral c.arg1, fromIntegral c.arg2)
                  else pointMatchOffset acc transformedNoTrans c.arg1 c.arg2
                (dx1, dy1) =
                  if testBit c.compFlags 2
                  then (fromIntegral (round dx0 :: Int), fromIntegral (round dy0 :: Int))
                  else (dx0, dy0)
                dxVar = arrAt dxs idx
                dyVar = arrAt dys idx
                transformed = map (map (applyTransform c.compTransform (dx1 + dxVar) (dy1 + dyVar))) contours
                acc' = acc ++ transformed
            in go (idx + 1) acc' cs
      in go 0 [] comps

    arrAt arr i =
      let (lo, hi) = bounds arr
      in if i < lo || i > hi then 0 else arr ! i

    gvarHasData gv gi =
      let offs = gv.offsets
          (lo, hi) = bounds offs
      in if lo > hi || gi < lo || gi + 1 > hi
         then False
         else
           let start = offs ! gi
               end = offs ! (gi + 1)
           in end > start

readCompositeComponents :: ByteBuffer -> [CompositeComponent]
readCompositeComponents bb = go 10 []
  where
    go off acc =
      if not (within bb off 4)
      then reverse acc
      else
        let flags = readU16BE bb off
            glyphIndex = fromIntegral (readU16BE bb (off + 2))
            argsAreWords = testBit flags 0
            argsAreXY = testBit flags 1
            argsLen = if argsAreWords then 4 else 2
        in if not (within bb (off + 4) argsLen)
           then reverse acc
           else
             let (arg1', arg2', offArgs) =
                   if argsAreWords
                   then
                     if argsAreXY
                     then (fromIntegral (readS16BE bb (off + 4)), fromIntegral (readS16BE bb (off + 6)), off + 8)
                     else (fromIntegral (readU16BE bb (off + 4)), fromIntegral (readU16BE bb (off + 6)), off + 8)
                   else
                     if argsAreXY
                     then (fromIntegral (asInt8 (readU8 bb (off + 4))), fromIntegral (asInt8 (readU8 bb (off + 5))), off + 6)
                     else (fromIntegral (readU8 bb (off + 4)), fromIntegral (readU8 bb (off + 5)), off + 6)
                 transLen = transformSize flags
             in if not (within bb offArgs transLen)
                then reverse acc
                else
                  let (transform, offTrans) = readTransform bb offArgs flags
                      comp = CompositeComponent
                        { compFlags = flags
                        , compGlyph = glyphIndex
                        , arg1 = arg1'
                        , arg2 = arg2'
                        , compTransform = transform
                        }
                  in if testBit flags 5 && offTrans <= bb.len
                     then go offTrans (comp : acc)
                     else reverse (comp : acc)
    asInt8 :: Word8 -> Int8
    asInt8 = fromIntegral

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
  let x' = a * p.x + b * p.y + dx
      y' = c * p.x + d * p.y + dy
  in p { x = x', y = y' }

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
      let dx = basePt.x - compPt.x
          dy = basePt.y - compPt.y
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
  let numGlyphs = ttf.maxp.numGlyphs
      glyf = ttf.glyf
      offsets = ttf.loca.offsets
      (offLo, offHi) = bounds offsets
  in if glyphIndex < 0 || glyphIndex + 1 >= numGlyphs
        || glyphIndex < offLo || glyphIndex + 1 > offHi
     then Nothing
     else
       let start = offsets ! glyphIndex
           end = offsets ! (glyphIndex + 1)
       in if end <= start || end > glyf.len
          then Nothing
          else
            let bb = slice glyf start (end - start)
            in if bb.len < 2
               then Nothing
               else
                 let numContours = fromIntegral (readS16BE bb 0) :: Int
                 in if numContours >= 0
                    then Nothing
                    else compositeUseMyMetrics bb

compositeComponentCount :: TTF -> Int -> Int
compositeComponentCount ttf glyphIndex =
  let numGlyphs = ttf.maxp.numGlyphs
      glyf = ttf.glyf
      offsets = ttf.loca.offsets
      (offLo, offHi) = bounds offsets
  in if glyphIndex < 0 || glyphIndex + 1 >= numGlyphs
        || glyphIndex < offLo || glyphIndex + 1 > offHi
     then 0
     else
       let start = offsets ! glyphIndex
           end = offsets ! (glyphIndex + 1)
       in if end <= start || end > glyf.len
          then 0
          else
            let bb = slice glyf start (end - start)
            in if bb.len < 2
               then 0
               else
                 let numContours = fromIntegral (readS16BE bb 0) :: Int
                 in if numContours >= 0
                    then 0
                    else length (readCompositeComponents bb)

compositeUseMyMetrics :: ByteBuffer -> Maybe Int
compositeUseMyMetrics bb = go 10
  where
    go off =
      if not (within bb off 4)
      then Nothing
      else
        let flags = readU16BE bb off
            glyphIndex = fromIntegral (readU16BE bb (off + 2))
            argsAreWords = testBit flags 0
            offArgs = off + 2 + if argsAreWords then 4 else 2
            offTrans = offArgs + transformSize flags
        in if testBit flags 9
           then Just glyphIndex
           else if testBit flags 5 && offTrans <= bb.len
                then go offTrans
                else Nothing

transformSize :: Word16 -> Int
transformSize flags
  | testBit flags 3 = 2
  | testBit flags 6 = 4
  | testBit flags 7 = 8
  | otherwise = 0
