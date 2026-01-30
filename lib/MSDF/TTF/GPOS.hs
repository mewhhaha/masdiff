module MSDF.TTF.GPOS
  ( KerningPairRaw(..)
  , parseGPOS
  ) where

import Data.Array (Array, accumArray, (!))
import Data.Bits (testBit)
import Data.Word (Word16)
import MSDF.Binary

-- | Kerning pair in font units.
data KerningPairRaw = KerningPairRaw
  { kpLeft :: Int
  , kpRight :: Int
  , kpXAdvance :: Int
  } deriving (Eq, Show)

parseGPOS :: Int -> ByteBuffer -> [KerningPairRaw]
parseGPOS numGlyphs bb =
  let scriptListOff = fromIntegral (readU16BE bb 4)
      featureListOff = fromIntegral (readU16BE bb 6)
      lookupListOff = fromIntegral (readU16BE bb 8)
      scriptOff = chooseScript bb scriptListOff
      featureIndices = chooseFeatureIndices bb featureListOff scriptOff
      lookupIndices = concatMap (featureLookupIndices bb featureListOff) featureIndices
  in concatMap (parseLookup bb lookupListOff numGlyphs) lookupIndices

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

chooseFeatureIndices :: ByteBuffer -> Int -> Int -> [Int]
chooseFeatureIndices bb featureListOff scriptOff =
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
         in filter (featureTagIsKern bb featureListOff) featureIndices'

featureTagIsKern :: ByteBuffer -> Int -> Int -> Bool
featureTagIsKern bb featureListOff featureIndex =
  let featureCount = fromIntegral (readU16BE bb featureListOff)
      recordsOff = featureListOff + 2
      recordOff = recordsOff + featureIndex * 6
  in if featureIndex < 0 || featureIndex >= featureCount
     then False
     else readTag bb recordOff == "kern"

featureLookupIndices :: ByteBuffer -> Int -> Int -> [Int]
featureLookupIndices bb featureListOff featureIndex =
  let recordOff = featureListOff + 2 + featureIndex * 6
      featureOff = fromIntegral (readU16BE bb (recordOff + 4))
      featureTableOff = featureListOff + featureOff
      lookupIndexCount = fromIntegral (readU16BE bb (featureTableOff + 2))
  in [ fromIntegral (readU16BE bb (featureTableOff + 4 + i * 2))
     | i <- [0 .. lookupIndexCount - 1] ]

parseLookup :: ByteBuffer -> Int -> Int -> Int -> [KerningPairRaw]
parseLookup bb lookupListOff numGlyphs lookupIndex =
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
               then concatMap (parseExtensionLookup bb lookupTableOff numGlyphs) subTableOffsets
               else []

parseExtensionLookup :: ByteBuffer -> Int -> Int -> Int -> [KerningPairRaw]
parseExtensionLookup bb lookupTableOff numGlyphs subOff =
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
            xAdv = valueXAdvance v1 + valueXAdvance v2
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
          xAdv = valueXAdvance v1 + valueXAdvance v2
      in if xAdv == 0
         then []
         else [ KerningPairRaw leftGlyph rightGlyph xAdv
              | rightGlyph <- class2Glyphs ! class2 ]

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
  { valueXAdvance :: Int
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
