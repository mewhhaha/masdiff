module MSDF.Types
  ( MSDFAtlas(..)
  , GlyphMSDF(..)
  , MSDFBitmap(..)
  , BBox(..)
  , BBoxUnion(..)
  , bboxUnion
  , bboxUnionMaybe
  , CodepointMapEntry(..)
  , KerningPair(..)
  , lookupCodepoint
  , lookupKerning
  ) where

import Control.DeepSeq (NFData(..))
import Data.Array (Array, (!), bounds)
import Data.Array.Unboxed (UArray)
import Data.Word (Word8)

-- | Complete MSDF data for a font.
type GlyphIndex = Int

data MSDFAtlas = MSDFAtlas
  { msdfFontName :: String
  , msdfUnitsPerEm :: Int
  , msdfAscent :: Int
  , msdfDescent :: Int
  , msdfLineGap :: Int
  , msdfPixelSize :: Int
  , msdfRange :: Int
  , msdfScale :: Double
  , msdfGlyphs :: Array GlyphIndex GlyphMSDF
  , msdfCodepointIndex :: Array Int CodepointMapEntry
  , msdfKerning :: Array Int KerningPair
  }

-- | Map from Unicode codepoint to glyph index.
data CodepointMapEntry = CodepointMapEntry
  { cmapCodepoint :: Int
  , cmapGlyphIndex :: Int
  } deriving (Eq, Show)

-- | Kerning pair adjustment in pixels.
data KerningPair = KerningPair
  { kernLeft :: Int
  , kernRight :: Int
  , kernXAdvance :: Double
  } deriving (Eq, Show)

-- | Per-glyph MSDF data.
data GlyphMSDF = GlyphMSDF
  { glyphIndex :: GlyphIndex
  , glyphCodepoints :: [Int]
  , glyphAdvance :: Double
  , glyphBearingX :: Double
  , glyphBearingY :: Double
  , glyphBBox :: BBox
  , glyphBitmap :: MSDFBitmap
  } deriving (Eq, Show)

-- | Glyph bounding box in pixels.
data BBox = BBox
  { bboxXMin :: Double
  , bboxYMin :: Double
  , bboxXMax :: Double
  , bboxYMax :: Double
  } deriving (Eq, Show)

newtype BBoxUnion = BBoxUnion { getBBoxUnion :: Maybe BBox }
  deriving (Eq, Show)

bboxUnion :: BBox -> BBox -> BBox
bboxUnion a b = BBox
  { bboxXMin = min (bboxXMin a) (bboxXMin b)
  , bboxYMin = min (bboxYMin a) (bboxYMin b)
  , bboxXMax = max (bboxXMax a) (bboxXMax b)
  , bboxYMax = max (bboxYMax a) (bboxYMax b)
  }

bboxUnionMaybe :: Maybe BBox -> BBox -> Maybe BBox
bboxUnionMaybe Nothing b = Just b
bboxUnionMaybe (Just a) b = Just (bboxUnion a b)

instance Semigroup BBoxUnion where
  BBoxUnion a <> BBoxUnion b =
    BBoxUnion (case (a, b) of
      (Nothing, x) -> x
      (x, Nothing) -> x
      (Just x, Just y) -> Just (bboxUnion x y))

instance Monoid BBoxUnion where
  mempty = BBoxUnion Nothing

-- | Packed RGB bitmap data.
data MSDFBitmap = MSDFBitmap
  { bmpWidth :: Int
  , bmpHeight :: Int
  , bmpOffsetX :: Double
  , bmpOffsetY :: Double
  , bmpPixels :: UArray Int Word8
  } deriving (Eq, Show)

instance NFData BBox where
  rnf (BBox a b c d) = a `seq` b `seq` c `seq` d `seq` ()

instance NFData BBoxUnion where
  rnf (BBoxUnion m) = rnf m

instance NFData CodepointMapEntry where
  rnf (CodepointMapEntry a b) = a `seq` b `seq` ()

instance NFData KerningPair where
  rnf (KerningPair a b c) = a `seq` b `seq` c `seq` ()

instance NFData MSDFBitmap where
  rnf (MSDFBitmap w h ox oy px) = w `seq` h `seq` ox `seq` oy `seq` px `seq` ()

instance NFData GlyphMSDF where
  rnf (GlyphMSDF i cps adv bx by bb bm) =
    i `seq` cps `seq` adv `seq` bx `seq` by `seq` bb `seq` bm `seq` ()

instance NFData MSDFAtlas where
  rnf (MSDFAtlas n u a d l p r s g c k) =
    n `seq` u `seq` a `seq` d `seq` l `seq` p `seq` r `seq` s `seq` g `seq` c `seq` k `seq` ()

lookupCodepoint :: MSDFAtlas -> Int -> Maybe Int
lookupCodepoint atlas codepoint =
  let arr = msdfCodepointIndex atlas
      (lo, hi) = boundsSafe arr
  in if lo > hi then Nothing else binarySearchCodepoint arr codepoint lo hi

lookupKerning :: MSDFAtlas -> Int -> Int -> Double
lookupKerning atlas left right =
  let arr = msdfKerning atlas
      (lo, hi) = boundsSafe arr
  in if lo > hi then 0 else binarySearchKerning arr left right lo hi

boundsSafe :: Array Int a -> (Int, Int)
boundsSafe arr =
  let (l, h) = bounds arr
  in (l, h)

binarySearchCodepoint :: Array Int CodepointMapEntry -> Int -> Int -> Int -> Maybe Int
binarySearchCodepoint arr codepoint lo hi
  | lo > hi = Nothing
  | otherwise =
      let mid = (lo + hi) `div` 2
          entry = arr ! mid
          c = cmapCodepoint entry
      in if codepoint == c
         then Just (cmapGlyphIndex entry)
         else if codepoint < c
              then binarySearchCodepoint arr codepoint lo (mid - 1)
              else binarySearchCodepoint arr codepoint (mid + 1) hi

binarySearchKerning :: Array Int KerningPair -> Int -> Int -> Int -> Int -> Double
binarySearchKerning arr left right lo hi
  | lo > hi = 0
  | otherwise =
      let mid = (lo + hi) `div` 2
          entry = arr ! mid
          keyL = kernLeft entry
          keyR = kernRight entry
      in if left == keyL && right == keyR
         then kernXAdvance entry
         else if (left < keyL) || (left == keyL && right < keyR)
              then binarySearchKerning arr left right lo (mid - 1)
              else binarySearchKerning arr left right (mid + 1) hi
