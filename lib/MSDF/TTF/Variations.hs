module MSDF.TTF.Variations
  ( Fvar(..)
  , FvarAxis(..)
  , FvarInstance(..)
  , Avar(..)
  , AxisMap
  , Gvar(..)
  , Variations(..)
  , VariationLocation(..)
  , parseFvar
  , parseAvar
  , parseGvar
  , defaultLocation
  , normalizeLocation
  , applyGvarToContours
  ) where

import Data.Array (Array, array, accumArray, bounds, inRange, (!))
import Data.Bits ((.&.), (.|.), shiftL, testBit)
import Data.Int (Int8, Int16, Int32)
import Data.List (zipWith4)
import Data.Ix (Ix)
import Data.Word (Word16)
import MSDF.Binary
import MSDF.Outline (Point(..))

data Fvar = Fvar
  { axes :: [FvarAxis]
  , instances :: [FvarInstance]
  } deriving (Eq, Show)

data FvarAxis = FvarAxis
  { tag :: String
  , minValue :: Double
  , defaultValue :: Double
  , maxValue :: Double
  , nameId :: Int
  } deriving (Eq, Show)

data FvarInstance = FvarInstance
  { nameId :: Int
  , coordinates :: [Double]
  } deriving (Eq, Show)

type AxisMap = [(Double, Double)]

data Avar = Avar
  { maps :: [AxisMap]
  } deriving (Eq, Show)

data Gvar = Gvar
  { axisCount :: Int
  , sharedTuples :: [[Double]]
  , offsets :: Array Int Int
  , dataOffset :: Int
  , flags :: Word16
  , buffer :: ByteBuffer
  }

data Variations = Variations
  { fvar :: Fvar
  , avar :: Maybe Avar
  , gvar :: Maybe Gvar
  }

newtype VariationLocation = VariationLocation
  { coords :: [Double]
  } deriving (Eq, Show)

parseFvar :: ByteBuffer -> Fvar
parseFvar bb =
  if bb.len < 16
  then Fvar [] []
  else
    let offsetToData = fromIntegral (readU16BE bb 4)
        axisCountRaw = fromIntegral (readU16BE bb 8)
        axisSize = fromIntegral (readU16BE bb 10)
        instanceCountRaw = fromIntegral (readU16BE bb 12)
        instanceSize = fromIntegral (readU16BE bb 14)
        maxAxes = if axisSize <= 0 || offsetToData >= bb.len
                  then 0
                  else min axisCountRaw ((bb.len - offsetToData) `div` axisSize)
        axes = [ readAxis (offsetToData + i * axisSize) | i <- [0 .. maxAxes - 1] ]
        instancesOff = offsetToData + maxAxes * axisSize
        minInstanceSize = 4 + maxAxes * 4
        maxInstances = if instanceSize < minInstanceSize || instancesOff >= bb.len
                       then 0
                       else min instanceCountRaw ((bb.len - instancesOff) `div` instanceSize)
        instances = [ readInstance (instancesOff + i * instanceSize) maxAxes | i <- [0 .. maxInstances - 1] ]
    in Fvar axes instances
  where
    readAxis off =
      let axisTag = readTag bb off
          minVal = readFixed bb (off + 4)
          defVal = readFixed bb (off + 8)
          maxVal = readFixed bb (off + 12)
          nameId' = fromIntegral (readU16BE bb (off + 18))
      in FvarAxis axisTag minVal defVal maxVal nameId'

    readInstance off count =
      let nameId' = fromIntegral (readU16BE bb off)
          coords' = [ readFixed bb (off + 4 + i * 4) | i <- [0 .. count - 1] ]
      in FvarInstance nameId' coords'

parseAvar :: ByteBuffer -> Avar
parseAvar bb =
  if bb.len < 8
  then Avar []
  else
    let axisCount = fromIntegral (readU16BE bb 4) :: Int
        (maps', _) = readMaps bb 8 axisCount []
    in Avar maps'
  where
    readMaps _ off 0 acc = (reverse acc, off)
    readMaps bb' off n acc =
      if not (within bb' off 2)
      then (reverse acc, off)
      else
        let segCount = fromIntegral (readU16BE bb' off)
            segOff = off + 2
            maxSegs = if segOff >= bb'.len then 0 else min segCount ((bb'.len - segOff) `div` 4)
            segments = [ (f2dot14 (readS16BE bb' (segOff + i * 4))
                         , f2dot14 (readS16BE bb' (segOff + i * 4 + 2)))
                       | i <- [0 .. maxSegs - 1] ]
            off' = segOff + maxSegs * 4
        in readMaps bb' off' (n - 1) (segments : acc)

parseGvar :: Int -> ByteBuffer -> Gvar
parseGvar glyphCount bb =
  if bb.len < 20
  then Gvar 0 [] (array (0, -1) []) 0 0 bb
  else
    let axisCount' = fromIntegral (readU16BE bb 4)
        sharedTupleCount = fromIntegral (readU16BE bb 6)
        sharedTupleOffset = fromIntegral (readU32BE bb 8)
        glyphCountTable = fromIntegral (readU16BE bb 12)
        flags' = readU16BE bb 14
        dataOffset' = fromIntegral (readU32BE bb 16)
        glyphCount' = min glyphCount glyphCountTable
        offsetsOff = 20
        longOffsets = testBit flags' 0
        offsetsSize = if longOffsets then 4 else 2
        offsetsCount = glyphCount' + 1
        maxOffsets = if offsetsOff >= bb.len
                     then 0
                     else min offsetsCount ((bb.len - offsetsOff) `div` offsetsSize)
        offsetsList =
          if longOffsets
          then [ fromIntegral (readU32BE bb (offsetsOff + i * 4)) | i <- [0 .. maxOffsets - 1] ]
          else [ fromIntegral (readU16BE bb (offsetsOff + i * 2)) * 2 | i <- [0 .. maxOffsets - 1] ]
        offsetsArr = if null offsetsList
                     then array (0, -1) []
                     else array (0, length offsetsList - 1) (zip [0..] offsetsList)
        sharedTuples' = readSharedTuples bb sharedTupleOffset axisCount' sharedTupleCount
    in Gvar axisCount' sharedTuples' offsetsArr dataOffset' flags' bb

readSharedTuples :: ByteBuffer -> Int -> Int -> Int -> [[Double]]
readSharedTuples bb off axisCount' tupleCount =
  if axisCount' <= 0 || tupleCount <= 0 || off >= bb.len
  then []
  else
    let tupleSize = axisCount' * 2
        maxTuples = min tupleCount ((bb.len - off) `div` tupleSize)
    in [ [ f2dot14 (readS16BE bb (off + i * tupleSize + j * 2)) | j <- [0 .. axisCount' - 1] ]
       | i <- [0 .. maxTuples - 1] ]

defaultLocation :: Fvar -> VariationLocation
defaultLocation fvar' =
  VariationLocation (replicate (length fvar'.axes) 0)

normalizeLocation :: Fvar -> Maybe Avar -> [(String, Double)] -> VariationLocation
normalizeLocation fvar' avar' settings =
  let axisValues = map (axisCoord settings) fvar'.axes
      normalized = zipWith normalizeAxis fvar'.axes axisValues
      mapped = case avar' of
        Nothing -> normalized
        Just av -> zipWith applyAvar (av.maps ++ repeat []) normalized
  in VariationLocation mapped

axisCoord :: [(String, Double)] -> FvarAxis -> Double
axisCoord settings axis =
  case lookup axis.tag settings of
    Just v -> clamp axis.minValue axis.maxValue v
    Nothing -> axis.defaultValue

normalizeAxis :: FvarAxis -> Double -> Double
normalizeAxis axis v
  | v == axis.defaultValue = 0
  | v < axis.defaultValue =
      let denom = axis.defaultValue - axis.minValue
      in if denom == 0 then 0 else (v - axis.defaultValue) / denom
  | otherwise =
      let denom = axis.maxValue - axis.defaultValue
      in if denom == 0 then 0 else (v - axis.defaultValue) / denom

applyAvar :: AxisMap -> Double -> Double
applyAvar [] v = v
applyAvar segments v =
  let sorted = segments
      first = case sorted of
        [] -> (0, v)
        (x:_) -> x
      last' = foldl' (\_ x -> x) first sorted
      (f0, t0) = first
      (fLast, tLast) = last'
  in if v <= f0 then t0
     else if v >= fLast then tLast
     else interp sorted v
  where
    interp ((f1, t1):(f2, t2):rest) v'
      | v' >= f1 && v' <= f2 =
          if f2 == f1 then t1 else t1 + (v' - f1) * (t2 - t1) / (f2 - f1)
      | otherwise = interp ((f2, t2):rest) v'
    interp _ v' = v'

applyGvarToContours :: Gvar -> VariationLocation -> Int -> [[Point]] -> ([[Point]], (Double, Double, Double, Double))
applyGvarToContours gvar loc glyphIndex contours =
  let numPoints = sum (map length contours)
      pointCount = numPoints + 4
      (dxs, dys) = glyphDeltas gvar loc glyphIndex pointCount
      (dLeft, dRight, dTop, dBottom) =
        if pointCount >= 4
        then (dxs ! (pointCount - 4), dxs ! (pointCount - 3), dys ! (pointCount - 2), dys ! (pointCount - 1))
        else (0, 0, 0, 0)
      contours' = applyDeltas contours dxs dys
  in (contours', (dLeft, dRight, dTop, dBottom))

applyDeltas :: [[Point]] -> Array Int Double -> Array Int Double -> [[Point]]
applyDeltas contours dxs dys = go 0 contours
  where
    go _ [] = []
    go idx (c:cs) =
      let (c', idx') = applyContour idx c
      in c' : go idx' cs

    applyContour idx pts =
      let (pts', idx') = foldl' step ([], idx) pts
      in (reverse pts', idx')

    step (acc, i) p =
      let dx = if inBounds dxs i then dxs ! i else 0
          dy = if inBounds dys i then dys ! i else 0
          p' = p { x = p.x + dx, y = p.y + dy }
      in (p' : acc, i + 1)

glyphDeltas :: Gvar -> VariationLocation -> Int -> Int -> (Array Int Double, Array Int Double)
glyphDeltas gvar loc glyphIndex pointCount =
  let bounds' = (0, pointCount - 1)
  in case glyphVariationData gvar loc glyphIndex pointCount of
       Nothing -> (array bounds' [], array bounds' [])
       Just tuples ->
         let contribX = concatMap fst tuples
             contribY = concatMap snd tuples
             arrX = accumArray' (+) 0 bounds' contribX
             arrY = accumArray' (+) 0 bounds' contribY
         in (arrX, arrY)

glyphVariationData :: Gvar -> VariationLocation -> Int -> Int -> Maybe ([([(Int, Double)], [(Int, Double)])])
glyphVariationData gvar loc glyphIndex pointCount =
  let offs = gvar.offsets
      (lo, hi) = boundsSafe offs
  in if lo > hi || glyphIndex < lo || glyphIndex + 1 > hi
     then Nothing
     else
       let start = offs ! glyphIndex
           end = offs ! (glyphIndex + 1)
           dataStart = gvar.dataOffset + start
           dataLen = end - start
       in if dataLen <= 0 || dataStart < 0 || dataStart + dataLen > gvar.buffer.len
          then Nothing
          else Just (parseGlyphData (slice gvar.buffer dataStart dataLen) gvar loc pointCount)

parseGlyphData :: ByteBuffer -> Gvar -> VariationLocation -> Int -> [([(Int, Double)], [(Int, Double)])]
parseGlyphData bb gvar loc pointCount =
  if bb.len < 4
  then []
  else
    let tupleCountRaw = readU16BE bb 0
        tupleCount = fromIntegral (tupleCountRaw .&. 0x0FFF) :: Int
        tupleFlags = tupleCountRaw .&. 0xF000
        dataOffset = fromIntegral (readU16BE bb 2)
        (headers, headersEnd) = readTupleHeaders bb 4 tupleCount gvar
        (sharedPoints, _) =
          if testBit tupleFlags 15
          then readPackedPoints bb headersEnd pointCount
          else ([], headersEnd)
        dataBase = dataOffset
    in concatMap (tupleContrib bb gvar loc pointCount dataBase sharedPoints) headers

data TupleHeader = TupleHeader
  { tupleIndex :: Word16
  , tupleSize :: Int
  , tupleDataOffset :: Int
  , peakTuple :: [Double]
  , intermediate :: Maybe ([Double], [Double])
  }

readTupleHeaders :: ByteBuffer -> Int -> Int -> Gvar -> ([TupleHeader], Int)
readTupleHeaders bb off count gvar =
  go off count []
  where
    go cur 0 acc = (reverse acc, cur)
    go cur n acc =
      if not (within bb cur 6)
      then (reverse acc, cur)
      else
        let tIndex = readU16BE bb cur
            tSize = fromIntegral (readU16BE bb (cur + 2))
            tOff = fromIntegral (readU16BE bb (cur + 4))
            cur1 = cur + 6
            (peak, cur2) =
              if testBit tIndex 15
              then readTuple bb cur1 gvar.axisCount
              else (sharedTuple gvar (fromIntegral (tIndex .&. 0x0FFF)), cur1)
            (inter, cur3) =
              if testBit tIndex 14
              then
                let (start, curS) = readTuple bb cur2 gvar.axisCount
                    (end, curE) = readTuple bb curS gvar.axisCount
                in (Just (start, end), curE)
              else (Nothing, cur2)
            header = TupleHeader tIndex tSize tOff peak inter
        in go cur3 (n - 1) (header : acc)

readTuple :: ByteBuffer -> Int -> Int -> ([Double], Int)
readTuple bb off count =
  let size = count * 2
      coords' = [ f2dot14 (readS16BE bb (off + i * 2)) | i <- [0 .. count - 1] ]
  in (coords', off + size)

sharedTuple :: Gvar -> Int -> [Double]
sharedTuple gvar idx =
  if idx < 0 || idx >= length gvar.sharedTuples
  then replicate gvar.axisCount 0
  else gvar.sharedTuples !! idx

tupleContrib :: ByteBuffer -> Gvar -> VariationLocation -> Int -> Int -> [Int] -> TupleHeader -> [([(Int, Double)], [(Int, Double)])]
tupleContrib bb gvar loc pointCount dataBase sharedPoints header =
  let scalar = tupleScalar (coordsForAxis gvar.axisCount loc) header
  in if scalar == 0
     then []
     else
       let dataStart = dataBase + header.tupleDataOffset
           dataLen = header.tupleSize
       in if dataStart < 0 || dataStart + dataLen > bb.len
          then []
          else
            let tb = slice bb dataStart dataLen
                (points, dataOff) =
                  if testBit header.tupleIndex 13
                  then readPackedPoints tb 0 pointCount
                  else if null sharedPoints then ([0 .. pointCount - 1], 0) else (sharedPoints, 0)
                (dxs, offX) = readPackedDeltas tb dataOff (length points)
                (dys, _) = readPackedDeltas tb offX (length points)
                pts = zip points dxs
                ptsY = zip points dys
                xs = [ (p, scalar * fromIntegral d) | (p, d) <- pts, p >= 0, p < pointCount ]
                ys = [ (p, scalar * fromIntegral d) | (p, d) <- ptsY, p >= 0, p < pointCount ]
            in [(xs, ys)]
  where
    coordsForAxis n loc' =
      let cs = loc'.coords
      in if length cs >= n then cs else cs ++ replicate (n - length cs) 0

tupleScalar :: [Double] -> TupleHeader -> Double
tupleScalar coords' header =
  let peak = header.peakTuple
      axisCount' = max (length coords') (length peak)
      coords'' = pad axisCount' coords'
      peak' = pad axisCount' peak
  in case header.intermediate of
       Nothing -> foldl' (*) 1 (zipWith scalarAxis peak' coords'')
       Just (start, end) ->
         let start' = pad axisCount' start
             end' = pad axisCount' end
             factors = zipWith4 scalarAxisIntermediate peak' start' end' coords''
         in foldl' (*) 1 factors
  where
    pad n xs = xs ++ replicate (n - length xs) 0

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
  | coord < min start end = 0
  | coord > max start end = 0
  | coord == peak = 1
  | coord < peak =
      let denom = peak - start
      in if denom == 0 then 0 else (coord - start) / denom
  | otherwise =
      let denom = end - peak
      in if denom == 0 then 0 else (end - coord) / denom

readPackedPoints :: ByteBuffer -> Int -> Int -> ([Int], Int)
readPackedPoints bb off pointCount =
  let (count, off1) = readCount bb off
  in if count == 0
     then ([0 .. pointCount - 1], off1)
     else readPointRuns bb off1 count count 0 []

readPointRuns :: ByteBuffer -> Int -> Int -> Int -> Int -> [Int] -> ([Int], Int)
readPointRuns _ off 0 _ _ acc = (reverse acc, off)
readPointRuns bb off remaining total prev acc =
  if not (within bb off 1)
  then (reverse acc, off)
  else
    let header = readU8 bb off
        isWord = testBit header 7
        runCount = fromIntegral (header .&. 0x7F) + 1
        (vals, off') = readRunDeltas bb (off + 1) runCount isWord
        points = case scanl (+) prev vals of
          [] -> []
          (_:ps) -> ps
        acc' = reverse points ++ acc
        prev' = if null points then prev else last points
        remaining' = remaining - runCount
    in if remaining' <= 0
       then (reverse (take total (reverse acc')), off')
       else readPointRuns bb off' remaining' total prev' acc'

readPackedDeltas :: ByteBuffer -> Int -> Int -> ([Int], Int)
readPackedDeltas bb off count = go off count []
  where
    go idx 0 acc = (reverse acc, idx)
    go idx n acc =
      if not (within bb idx 1)
      then (reverse (replicate n 0 ++ acc), idx)
      else
        let header = readU8 bb idx
            isWord = testBit header 7
            isZero = testBit header 6
            runCount = fromIntegral (header .&. 0x3F) + 1
            n' = min n runCount
        in if isZero
           then go (idx + 1) (n - n') (replicate n' 0 ++ acc)
           else
             let (vals, idx') = readRunSigned bb (idx + 1) n' isWord
             in go idx' (n - n') (reverse vals ++ acc)

readRunSigned :: ByteBuffer -> Int -> Int -> Bool -> ([Int], Int)
readRunSigned bb off count isWord =
  let size = if isWord then 2 else 1
      maxCount = if off >= bb.len then 0 else min count ((bb.len - off) `div` size)
      vals = [ if isWord
               then fromIntegral (readS16BE bb (off + i * 2)) :: Int
               else fromIntegral (fromIntegral (readU8 bb (off + i)) :: Int8)
             | i <- [0 .. maxCount - 1] ]
      off' = off + maxCount * size
      padding = replicate (count - maxCount) 0
  in (vals ++ padding, off')

readRunDeltas :: ByteBuffer -> Int -> Int -> Bool -> ([Int], Int)
readRunDeltas bb off count isWord =
  let size = if isWord then 2 else 1
      maxCount = if off >= bb.len then 0 else min count ((bb.len - off) `div` size)
      vals = [ if isWord
               then fromIntegral (readU16BE bb (off + i * 2)) :: Int
               else fromIntegral (readU8 bb (off + i)) :: Int
             | i <- [0 .. maxCount - 1] ]
      off' = off + maxCount * size
      padding = replicate (count - maxCount) 0
  in (vals ++ padding, off')

readCount :: ByteBuffer -> Int -> (Int, Int)
readCount bb off =
  if not (within bb off 1)
  then (0, off)
  else
    let b0 = readU8 bb off
    in if testBit b0 7
       then
         if within bb (off + 1) 1
         then
           let b1 = readU8 bb (off + 1)
               count = ((fromIntegral (b0 .&. 0x7F) :: Int) `shiftL` 8) .|. fromIntegral b1
           in (count, off + 2)
         else (0, off + 1)
       else (fromIntegral b0, off + 1)

accumArray' :: (Ix i) => (e -> e -> e) -> e -> (i, i) -> [(i, e)] -> Array i e
accumArray' f z bnds xs =
  if null xs then array bnds [] else accumArray f z bnds xs

boundsSafe :: Array Int a -> (Int, Int)
boundsSafe arr =
  let (l, h) = bounds arr
  in (l, h)

inBounds :: Array Int a -> Int -> Bool
inBounds arr i = inRange (bounds arr) i

within :: ByteBuffer -> Int -> Int -> Bool
within bb off size =
  off >= 0 && size >= 0 && off + size <= bb.len

readFixed :: ByteBuffer -> Int -> Double
readFixed bb off =
  let raw = readS32BE bb off
  in fromIntegral (raw :: Int32) / 65536.0

f2dot14 :: Int16 -> Double
f2dot14 v =
  let i = fromIntegral v :: Double
  in i / 16384.0

clamp :: Ord a => a -> a -> a -> a
clamp lo hi v = max lo (min hi v)
