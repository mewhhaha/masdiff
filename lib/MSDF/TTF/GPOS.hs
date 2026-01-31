module MSDF.TTF.GPOS
  ( KerningPairRaw(..)
  , AnchorRaw(..)
  , MarkGlyphRaw(..)
  , BaseGlyphRaw(..)
  , MarkToBaseRaw(..)
  , MarkToMarkRaw(..)
  , GPOSMarksRaw(..)
  , GPOSAll(..)
  , parseGPOS
  , parseGPOSAll
  ) where

import Data.Array (Array, array, accumArray, (!), (//))
import Data.Bits (testBit)
import Data.List (sort)
import Data.Word (Word16)
import MSDF.Binary

-- | Kerning pair in font units.
data KerningPairRaw = KerningPairRaw
  { left :: Int
  , right :: Int
  , xAdvance :: Int
  } deriving (Eq, Show)

data AnchorRaw = AnchorRaw
  { x :: Int
  , y :: Int
  } deriving (Eq, Show)

data MarkGlyphRaw = MarkGlyphRaw
  { markClass :: Int
  , anchor :: AnchorRaw
  } deriving (Eq, Show)

data BaseGlyphRaw = BaseGlyphRaw
  { anchors :: Array Int (Maybe AnchorRaw)
  } deriving (Eq, Show)

data MarkToBaseRaw = MarkToBaseRaw
  { classCount :: Int
  , marks :: Array Int (Maybe MarkGlyphRaw)
  , bases :: Array Int (Maybe BaseGlyphRaw)
  } deriving (Eq, Show)

data MarkToMarkRaw = MarkToMarkRaw
  { classCount :: Int
  , marks1 :: Array Int (Maybe MarkGlyphRaw)
  , marks2 :: Array Int (Maybe BaseGlyphRaw)
  } deriving (Eq, Show)

data GPOSMarksRaw = GPOSMarksRaw
  { markToBase :: [MarkToBaseRaw]
  , markToMark :: [MarkToMarkRaw]
  } deriving (Eq, Show)

data GPOSAll = GPOSAll
  { kernPairs :: [KerningPairRaw]
  , marks :: GPOSMarksRaw
  } deriving (Eq, Show)

parseGPOS :: Int -> ByteBuffer -> [KerningPairRaw]
parseGPOS numGlyphs bb =
  (parseGPOSAll numGlyphs bb).kernPairs

parseGPOSAll :: Int -> ByteBuffer -> GPOSAll
parseGPOSAll numGlyphs bb =
  let scriptListOff = fromIntegral (readU16BE bb 4)
      featureListOff = fromIntegral (readU16BE bb 6)
      lookupListOff = fromIntegral (readU16BE bb 8)
      scriptOff = chooseScript bb scriptListOff
      kernFeatures = chooseFeatureIndicesByTags bb featureListOff scriptOff ["kern"]
      markFeatures = chooseFeatureIndicesByTags bb featureListOff scriptOff ["mark"]
      mkmkFeatures = chooseFeatureIndicesByTags bb featureListOff scriptOff ["mkmk"]
      kernLookups = uniqueSorted (concatMap (featureLookupIndices bb featureListOff) kernFeatures)
      markLookups = uniqueSorted (concatMap (featureLookupIndices bb featureListOff) markFeatures)
      mkmkLookups = uniqueSorted (concatMap (featureLookupIndices bb featureListOff) mkmkFeatures)
      kernPairs = concatMap (parseLookupPairs bb lookupListOff numGlyphs) kernLookups
      markToBase = concatMap (parseLookupMarkToBase bb lookupListOff numGlyphs) markLookups
      markToMark = concatMap (parseLookupMarkToMark bb lookupListOff numGlyphs) mkmkLookups
  in GPOSAll
       { kernPairs = kernPairs
       , marks = GPOSMarksRaw markToBase markToMark
       }

chooseScript :: ByteBuffer -> Int -> Int
chooseScript bb scriptListOff =
  let count = fromIntegral (readU16BE bb scriptListOff)
      recordsOff = scriptListOff + 2
      records = [ (readTag bb (recordsOff + i * 6), fromIntegral (readU16BE bb (recordsOff + i * 6 + 4)))
                | i <- [0 .. count - 1] ]
      pickTag tag = lookup tag records
  in case records of
       [] -> scriptListOff
       (firstRecord:_) ->
         let fallback = snd firstRecord
             selected = case pickTag "DFLT" of
                          Just off -> off
                          Nothing -> case pickTag "latn" of
                            Just off -> off
                            Nothing -> fallback
         in scriptListOff + selected

chooseFeatureIndicesByTags :: ByteBuffer -> Int -> Int -> [String] -> [Int]
chooseFeatureIndicesByTags bb featureListOff scriptOff tags =
  let defaultLangSysOff = fromIntegral (readU16BE bb scriptOff)
      langSysCount = fromIntegral (readU16BE bb (scriptOff + 2))
      langSysRecordsOff = scriptOff + 4
      langSysOffsets = [ fromIntegral (readU16BE bb (langSysRecordsOff + i * 6 + 4))
                       | i <- [0 .. langSysCount - 1] ]
      langSysOff = if defaultLangSysOff /= 0 then Just (scriptOff + defaultLangSysOff)
                   else case langSysOffsets of
                          [] -> Nothing
                          (o:_) -> Just (scriptOff + o)
  in case langSysOff of
       Nothing -> []
       Just langOff ->
         let reqFeatureIndex = fromIntegral (readU16BE bb (langOff + 2))
             featureIndexCount = fromIntegral (readU16BE bb (langOff + 4))
             featureIndices = [ fromIntegral (readU16BE bb (langOff + 6 + i * 2))
                              | i <- [0 .. featureIndexCount - 1] ]
             featureIndices' = if reqFeatureIndex /= 0xFFFF then reqFeatureIndex : featureIndices else featureIndices
         in filter (featureTagIsAny bb featureListOff tags) featureIndices'

featureTagIsAny :: ByteBuffer -> Int -> [String] -> Int -> Bool
featureTagIsAny bb featureListOff tags featureIndex =
  let featureCount = fromIntegral (readU16BE bb featureListOff)
      recordsOff = featureListOff + 2
      recordOff = recordsOff + featureIndex * 6
  in if featureIndex < 0 || featureIndex >= featureCount
     then False
     else readTag bb recordOff `elem` tags

featureLookupIndices :: ByteBuffer -> Int -> Int -> [Int]
featureLookupIndices bb featureListOff featureIndex =
  let recordOff = featureListOff + 2 + featureIndex * 6
      featureOff = fromIntegral (readU16BE bb (recordOff + 4))
      featureTableOff = featureListOff + featureOff
      lookupIndexCount = fromIntegral (readU16BE bb (featureTableOff + 2))
  in [ fromIntegral (readU16BE bb (featureTableOff + 4 + i * 2))
     | i <- [0 .. lookupIndexCount - 1] ]

parseLookupPairs :: ByteBuffer -> Int -> Int -> Int -> [KerningPairRaw]
parseLookupPairs bb lookupListOff numGlyphs lookupIndex =
  let lookupCount = fromIntegral (readU16BE bb lookupListOff)
      lookupOffsetsOff = lookupListOff + 2
  in if lookupIndex < 0 || lookupIndex >= lookupCount
     then []
     else
       let lookupOff = fromIntegral (readU16BE bb (lookupOffsetsOff + lookupIndex * 2))
           lookupTableOff = lookupListOff + lookupOff
           lookupType = (fromIntegral (readU16BE bb lookupTableOff)) :: Int
           subTableCount = fromIntegral (readU16BE bb (lookupTableOff + 4))
           subTableOffsets = [ fromIntegral (readU16BE bb (lookupTableOff + 6 + i * 2))
                             | i <- [0 .. subTableCount - 1] ]
       in if lookupType == 2
          then concatMap (parsePairPos bb (lookupTableOff) numGlyphs) subTableOffsets
          else if lookupType == 9
               then concatMap (parseExtensionLookupPairs bb lookupTableOff numGlyphs) subTableOffsets
               else []

parseExtensionLookupPairs :: ByteBuffer -> Int -> Int -> Int -> [KerningPairRaw]
parseExtensionLookupPairs bb lookupTableOff numGlyphs subOff =
  let subTableOff = lookupTableOff + subOff
      posFormat = fromIntegral (readU16BE bb subTableOff) :: Int
  in if posFormat /= 1
     then []
     else
       let extensionLookupType = fromIntegral (readU16BE bb (subTableOff + 2)) :: Int
           extensionOffset = fromIntegral (readU32BE bb (subTableOff + 4)) :: Int
           extensionSubOff = subTableOff + extensionOffset
       in if extensionLookupType == 2
          then parsePairPos bb extensionSubOff numGlyphs 0
          else []

parseLookupMarkToBase :: ByteBuffer -> Int -> Int -> Int -> [MarkToBaseRaw]
parseLookupMarkToBase bb lookupListOff numGlyphs lookupIndex =
  parseLookupByType 4 (parseMarkToBase bb numGlyphs) bb lookupListOff lookupIndex

parseLookupMarkToMark :: ByteBuffer -> Int -> Int -> Int -> [MarkToMarkRaw]
parseLookupMarkToMark bb lookupListOff numGlyphs lookupIndex =
  parseLookupByType 6 (parseMarkToMark bb numGlyphs) bb lookupListOff lookupIndex

parseLookupByType :: Int -> (Int -> Int -> [a]) -> ByteBuffer -> Int -> Int -> [a]
parseLookupByType desired parseSub bb lookupListOff lookupIndex =
  let lookupCount = fromIntegral (readU16BE bb lookupListOff)
      lookupOffsetsOff = lookupListOff + 2
  in if lookupIndex < 0 || lookupIndex >= lookupCount
     then []
     else
       let lookupOff = fromIntegral (readU16BE bb (lookupOffsetsOff + lookupIndex * 2))
           lookupTableOff = lookupListOff + lookupOff
           lookupType = (fromIntegral (readU16BE bb lookupTableOff)) :: Int
           subTableCount = fromIntegral (readU16BE bb (lookupTableOff + 4))
           subTableOffsets = [ fromIntegral (readU16BE bb (lookupTableOff + 6 + i * 2))
                             | i <- [0 .. subTableCount - 1] ]
       in if lookupType == desired
          then concatMap (parseSub lookupTableOff) subTableOffsets
          else if lookupType == 9
               then concatMap (parseExtensionLookupByType desired parseSub bb lookupTableOff) subTableOffsets
               else []

parseExtensionLookupByType :: Int -> (Int -> Int -> [a]) -> ByteBuffer -> Int -> Int -> [a]
parseExtensionLookupByType desired parseSub bb lookupTableOff subOff =
  let subTableOff = lookupTableOff + subOff
      posFormat = fromIntegral (readU16BE bb subTableOff) :: Int
  in if posFormat /= 1
     then []
     else
       let extensionLookupType = fromIntegral (readU16BE bb (subTableOff + 2)) :: Int
           extensionOffset = fromIntegral (readU32BE bb (subTableOff + 4)) :: Int
           extensionSubOff = subTableOff + extensionOffset
       in if extensionLookupType == desired
          then parseSub extensionSubOff 0
          else []

parsePairPos :: ByteBuffer -> Int -> Int -> Int -> [KerningPairRaw]
parsePairPos bb lookupTableOff numGlyphs subOff =
  let subTableOff = lookupTableOff + subOff
      posFormat = (fromIntegral (readU16BE bb subTableOff)) :: Int
  in case posFormat of
       1 -> parsePairPos1 bb subTableOff
       2 -> parsePairPos2 bb subTableOff numGlyphs
       _ -> []

parsePairPos1 :: ByteBuffer -> Int -> [KerningPairRaw]
parsePairPos1 bb subTableOff =
  let coverageOff = fromIntegral (readU16BE bb (subTableOff + 2))
      valueFormat1 = readU16BE bb (subTableOff + 4)
      valueFormat2 = readU16BE bb (subTableOff + 6)
      pairSetCount = fromIntegral (readU16BE bb (subTableOff + 8))
      pairSetOffsets = [ fromIntegral (readU16BE bb (subTableOff + 10 + i * 2))
                       | i <- [0 .. pairSetCount - 1] ]
      coverageGlyphs = parseCoverage bb (subTableOff + coverageOff)
  in concat [ parsePairSet bb (subTableOff + pairSetOffsets !! i) (coverageGlyphs !! i) valueFormat1 valueFormat2
           | i <- [0 .. min (pairSetCount - 1) (length coverageGlyphs - 1)] ]

parsePairSet :: ByteBuffer -> Int -> Int -> Word16 -> Word16 -> [KerningPairRaw]
parsePairSet bb pairSetOff firstGlyph valueFormat1 valueFormat2 =
  let pairValueCount = fromIntegral (readU16BE bb pairSetOff)
      pairValueOff = pairSetOff + 2
      recordSize = valueRecordSize valueFormat1 + valueRecordSize valueFormat2 + 2
      pairValueAt j =
        let recordOff = pairValueOff + j * recordSize
            secondGlyph = fromIntegral (readU16BE bb recordOff)
            (v1, off1) = readValueRecord bb (recordOff + 2) valueFormat1
            (v2, _off2) = readValueRecord bb off1 valueFormat2
            xAdv = v1.xAdvance + v2.xAdvance
        in KerningPairRaw firstGlyph secondGlyph xAdv
  in [ pairValueAt j | j <- [0 .. pairValueCount - 1] ]

parsePairPos2 :: ByteBuffer -> Int -> Int -> [KerningPairRaw]
parsePairPos2 bb subTableOff numGlyphs =
  let coverageOff = fromIntegral (readU16BE bb (subTableOff + 2))
      valueFormat1 = readU16BE bb (subTableOff + 4)
      valueFormat2 = readU16BE bb (subTableOff + 6)
      classDef1Off = fromIntegral (readU16BE bb (subTableOff + 8))
      classDef2Off = fromIntegral (readU16BE bb (subTableOff + 10))
      class1Count = fromIntegral (readU16BE bb (subTableOff + 12))
      class2Count = fromIntegral (readU16BE bb (subTableOff + 14))
      classDef1 = parseClassDef bb (subTableOff + classDef1Off)
      classDef2 = parseClassDef bb (subTableOff + classDef2Off)
      coverageGlyphs = parseCoverage bb (subTableOff + coverageOff)
      class2Glyphs = buildClassGlyphs numGlyphs class2Count (classOf classDef2)
      recordsOff = subTableOff + 16
  in concatMap (pairsForLeft bb recordsOff valueFormat1 valueFormat2 class1Count class2Count classDef1 class2Glyphs) coverageGlyphs

pairsForLeft :: ByteBuffer -> Int -> Word16 -> Word16 -> Int -> Int -> ClassDef -> Array Int [Int] -> Int -> [KerningPairRaw]
pairsForLeft bb recordsOff valueFormat1 valueFormat2 class1Count class2Count classDef1 class2Glyphs leftGlyph =
  let class1 = classOf classDef1 leftGlyph
      class1' = if class1 < class1Count then class1 else 0
      rowOff = recordsOff + class1' * (class2Count * (valueRecordSize valueFormat1 + valueRecordSize valueFormat2))
      recordSize = valueRecordSize valueFormat1 + valueRecordSize valueFormat2
  in concat [ pairsForClass2 (rowOff + class2 * recordSize) class2 | class2 <- [0 .. class2Count - 1] ]
  where
    pairsForClass2 recordOff class2 =
      let (v1, off1) = readValueRecord bb recordOff valueFormat1
          (v2, _off2) = readValueRecord bb off1 valueFormat2
          xAdv = v1.xAdvance + v2.xAdvance
      in if xAdv == 0
         then []
         else [ KerningPairRaw leftGlyph rightGlyph xAdv
               | rightGlyph <- class2Glyphs ! class2 ]

-- Mark-to-base and mark-to-mark --------------------------------------------

parseMarkToBase :: ByteBuffer -> Int -> Int -> Int -> [MarkToBaseRaw]
parseMarkToBase bb numGlyphs lookupTableOff subOff =
  let subTableOff = lookupTableOff + subOff
      posFormat = (fromIntegral (readU16BE bb subTableOff)) :: Int
  in if posFormat /= 1
     then []
     else
       let markCoverageOff = fromIntegral (readU16BE bb (subTableOff + 2))
           baseCoverageOff = fromIntegral (readU16BE bb (subTableOff + 4))
           markArrayOff = fromIntegral (readU16BE bb (subTableOff + 6))
           baseArrayOff = fromIntegral (readU16BE bb (subTableOff + 8))
           markCoverage = parseCoverage bb (subTableOff + markCoverageOff)
           baseCoverage = parseCoverage bb (subTableOff + baseCoverageOff)
           (marksArr, classCount) = parseMarkArray bb (subTableOff + markArrayOff) markCoverage numGlyphs
           basesArr = parseBaseArray bb (subTableOff + baseArrayOff) baseCoverage numGlyphs classCount
       in [MarkToBaseRaw classCount marksArr basesArr]

parseMarkToMark :: ByteBuffer -> Int -> Int -> Int -> [MarkToMarkRaw]
parseMarkToMark bb numGlyphs lookupTableOff subOff =
  let subTableOff = lookupTableOff + subOff
      posFormat = (fromIntegral (readU16BE bb subTableOff)) :: Int
  in if posFormat /= 1
     then []
     else
       let mark1CoverageOff = fromIntegral (readU16BE bb (subTableOff + 2))
           mark2CoverageOff = fromIntegral (readU16BE bb (subTableOff + 4))
           mark1ArrayOff = fromIntegral (readU16BE bb (subTableOff + 6))
           mark2ArrayOff = fromIntegral (readU16BE bb (subTableOff + 8))
           mark1Coverage = parseCoverage bb (subTableOff + mark1CoverageOff)
           mark2Coverage = parseCoverage bb (subTableOff + mark2CoverageOff)
           (marks1Arr, classCount) = parseMarkArray bb (subTableOff + mark1ArrayOff) mark1Coverage numGlyphs
           marks2Arr = parseBaseArray bb (subTableOff + mark2ArrayOff) mark2Coverage numGlyphs classCount
       in [MarkToMarkRaw classCount marks1Arr marks2Arr]

parseMarkArray :: ByteBuffer -> Int -> [Int] -> Int -> (Array Int (Maybe MarkGlyphRaw), Int)
parseMarkArray bb off coverage numGlyphs =
  if not (within bb off 2)
  then (emptyGlyphArray numGlyphs Nothing, 0)
  else
    let markCount = fromIntegral (readU16BE bb off)
        recordsOff = off + 2
        maxMarks = min markCount (length coverage)
        entries = [ parseMarkRecord recordsOff i | i <- [0 .. maxMarks - 1] ]
        classCount = case [ c | Just (_, MarkGlyphRaw c _) <- entries ] of
          [] -> 0
          cs -> 1 + maximum cs
        arr = emptyGlyphArray numGlyphs Nothing
        updates = [ (gid, Just mark) | Just (gid, mark) <- entries ]
    in (arr // updates, classCount)
  where
    parseMarkRecord recordsOff i =
      let recOff = recordsOff + i * 4
      in if not (within bb recOff 4)
         then Nothing
         else
           let cls = fromIntegral (readU16BE bb recOff)
               anchorOff = fromIntegral (readU16BE bb (recOff + 2))
               gid = coverage !! i
               anchorM = if anchorOff == 0 then Nothing else parseAnchor bb (off + anchorOff)
           in case anchorM of
                Nothing -> Nothing
                Just anchor -> Just (gid, MarkGlyphRaw cls anchor)

parseBaseArray :: ByteBuffer -> Int -> [Int] -> Int -> Int -> Array Int (Maybe BaseGlyphRaw)
parseBaseArray bb off coverage numGlyphs classCount =
  if classCount <= 0 || not (within bb off 2)
  then emptyGlyphArray numGlyphs Nothing
  else
    let baseCount = fromIntegral (readU16BE bb off)
        recordsOff = off + 2
        recordSize = classCount * 2
        maxBases = min baseCount (length coverage)
        arr = emptyGlyphArray numGlyphs Nothing
        updates = [ (coverage !! i, Just (parseBaseRecord (recordsOff + i * recordSize)))
                  | i <- [0 .. maxBases - 1]
                  , within bb (recordsOff + i * recordSize) recordSize
                  ]
    in arr // updates
  where
    parseBaseRecord recOff =
      let anchorsArr = array (0, classCount - 1)
            [ (cls, anchorAt recOff cls) | cls <- [0 .. classCount - 1] ]
      in BaseGlyphRaw anchorsArr

    anchorAt recOff cls =
      let off' = recOff + cls * 2
          anchorOff = fromIntegral (readU16BE bb off')
      in if anchorOff == 0 then Nothing else parseAnchor bb (off + anchorOff)

parseAnchor :: ByteBuffer -> Int -> Maybe AnchorRaw
parseAnchor bb off =
  if not (within bb off 6)
  then Nothing
  else
    let fmt = fromIntegral (readU16BE bb off) :: Int
        x' = fromIntegral (readS16BE bb (off + 2))
        y' = fromIntegral (readS16BE bb (off + 4))
    in case fmt of
         1 -> Just (AnchorRaw x' y')
         2 -> if within bb off 8 then Just (AnchorRaw x' y') else Nothing
         3 -> if within bb off 10 then Just (AnchorRaw x' y') else Nothing
         _ -> Nothing

-- Coverage tables -----------------------------------------------------------

parseCoverage :: ByteBuffer -> Int -> [Int]
parseCoverage bb off =
  let format = fromIntegral (readU16BE bb off) :: Int
  in case format of
       1 ->
         let count = fromIntegral (readU16BE bb (off + 2))
         in [ fromIntegral (readU16BE bb (off + 4 + i * 2)) | i <- [0 .. count - 1] ]
       2 ->
         let rangeCount = fromIntegral (readU16BE bb (off + 2))
             ranges = [ (fromIntegral (readU16BE bb (off + 4 + i * 6))
                        ,fromIntegral (readU16BE bb (off + 4 + i * 6 + 2)))
                      | i <- [0 .. rangeCount - 1] ]
         in concatMap (\(s,e) -> [s..e]) ranges
       _ -> []

-- Class definition ----------------------------------------------------------

data ClassDef = ClassDef [(Int, Int, Int)]

parseClassDef :: ByteBuffer -> Int -> ClassDef
parseClassDef bb off =
  let format = fromIntegral (readU16BE bb off) :: Int
  in case format of
       1 ->
         let startGlyph = fromIntegral (readU16BE bb (off + 2))
             glyphCount = fromIntegral (readU16BE bb (off + 4))
             classes = [ fromIntegral (readU16BE bb (off + 6 + i * 2))
                       | i <- [0 .. glyphCount - 1] ]
             ranges = [ (startGlyph + i, startGlyph + i, cls) | (i, cls) <- zip [0..] classes ]
         in ClassDef ranges
       2 ->
         let rangeCount = fromIntegral (readU16BE bb (off + 2))
             ranges = [ (fromIntegral (readU16BE bb (off + 4 + i * 6))
                        ,fromIntegral (readU16BE bb (off + 4 + i * 6 + 2))
                        ,fromIntegral (readU16BE bb (off + 4 + i * 6 + 4)))
                      | i <- [0 .. rangeCount - 1] ]
         in ClassDef ranges
       _ -> ClassDef []

classOf :: ClassDef -> Int -> Int
classOf (ClassDef ranges) glyph =
  case [cls | (s,e,cls) <- ranges, glyph >= s, glyph <= e] of
    (cls:_) -> cls
    [] -> 0

buildClassGlyphs :: Int -> Int -> (Int -> Int) -> Array Int [Int]
buildClassGlyphs numGlyphs classCount classFn =
  let pairs = [ (classFn g, g) | g <- [0 .. numGlyphs - 1], classFn g < classCount ]
  in accumArray (flip (:)) [] (0, classCount - 1) pairs

-- Value records -------------------------------------------------------------

data ValueRecord = ValueRecord
  { xAdvance :: Int
  }

readValueRecord :: ByteBuffer -> Int -> Word16 -> (ValueRecord, Int)
readValueRecord bb off format =
  let (off1, _xPlacement) = readIf 0 off
      (off2, _yPlacement) = readIf 1 off1
      (off3, xAdv) = if testBit format 2
                     then (off2 + 2, fromIntegral (readS16BE bb off2))
                     else (off2, 0)
      (off4, _yAdv) = readIf 3 off3
      off5 = off4 + if testBit format 4 then 2 else 0
      off6 = off5 + if testBit format 5 then 2 else 0
      off7 = off6 + if testBit format 6 then 2 else 0
      off8 = off7 + if testBit format 7 then 2 else 0
  in (ValueRecord xAdv, off8)
  where
    readIf bit off' =
      if testBit format bit
      then (off' + 2, fromIntegral (readS16BE bb off') :: Int)
      else (off', 0 :: Int)

valueRecordSize :: Word16 -> Int
valueRecordSize format =
  let hasXPlacement = testBit format 0
      hasYPlacement = testBit format 1
      hasXAdvance = testBit format 2
      hasYAdvance = testBit format 3
      hasXPlaDevice = testBit format 4
      hasYPlaDevice = testBit format 5
      hasXAdvDevice = testBit format 6
      hasYAdvDevice = testBit format 7
  in sum
     [ if hasXPlacement then 2 else 0
     , if hasYPlacement then 2 else 0
     , if hasXAdvance then 2 else 0
     , if hasYAdvance then 2 else 0
     , if hasXPlaDevice then 2 else 0
     , if hasYPlaDevice then 2 else 0
     , if hasXAdvDevice then 2 else 0
     , if hasYAdvDevice then 2 else 0
     ]

within :: ByteBuffer -> Int -> Int -> Bool
within bb off size =
  off >= 0 && size >= 0 && off + size <= bb.len

emptyGlyphArray :: Int -> a -> Array Int a
emptyGlyphArray n val =
  if n <= 0
  then array (0, -1) []
  else array (0, n - 1) [ (i, val) | i <- [0 .. n - 1] ]

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
