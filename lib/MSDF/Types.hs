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
  { fontName :: String
  , unitsPerEm :: Int
  , ascent :: Int
  , descent :: Int
  , lineGap :: Int
  , pixelSize :: Int
  , range :: Int
  , scale :: Double
  , glyphs :: Array GlyphIndex GlyphMSDF
  , codepointIndex :: Array Int CodepointMapEntry
  , kerning :: Array Int KerningPair
  }

-- | Map from Unicode codepoint to glyph index.
data CodepointMapEntry = CodepointMapEntry
  { codepoint :: Int
  , glyphIndex :: Int
  } deriving (Eq, Show)

-- | Kerning pair adjustment in pixels.
data KerningPair = KerningPair
  { left :: Int
  , right :: Int
  , xAdvance :: Double
  } deriving (Eq, Show)

-- | Per-glyph MSDF data.
data GlyphMSDF = GlyphMSDF
  { index :: GlyphIndex
  , codepoints :: [Int]
  , advance :: Double
  , bearingX :: Double
  , bearingY :: Double
  , bbox :: BBox
  , bitmap :: MSDFBitmap
  } deriving (Eq, Show)

-- | Glyph bounding box in pixels.
data BBox = BBox
  { xMin :: Double
  , yMin :: Double
  , xMax :: Double
  , yMax :: Double
  } deriving (Eq, Show)

newtype BBoxUnion = BBoxUnion { getBBoxUnion :: Maybe BBox }
  deriving (Eq, Show)

bboxUnion :: BBox -> BBox -> BBox
bboxUnion a b = BBox
  { xMin = min a.xMin b.xMin
  , yMin = min a.yMin b.yMin
  , xMax = max a.xMax b.xMax
  , yMax = max a.yMax b.yMax
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
  { width :: Int
  , height :: Int
  , offsetX :: Double
  , offsetY :: Double
  , pixels :: UArray Int Word8
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
lookupCodepoint atlas cp =
  let arr = atlas.codepointIndex
      (lo, hi) = boundsSafe arr
  in if lo > hi
     then Nothing
     else
       case binarySearchCodepoint arr cp lo hi of
         Nothing -> Nothing
         Just gid ->
           let (glo, ghi) = boundsSafe atlas.glyphs
           in if gid < glo || gid > ghi then Nothing else Just gid

lookupKerning :: MSDFAtlas -> Int -> Int -> Double
lookupKerning atlas l r =
  let arr = atlas.kerning
      (lo, hi) = boundsSafe arr
  in if lo > hi then 0 else binarySearchKerning arr l r lo hi

boundsSafe :: Array Int a -> (Int, Int)
boundsSafe arr =
  let (l, h) = bounds arr
  in (l, h)

binarySearchCodepoint :: Array Int CodepointMapEntry -> Int -> Int -> Int -> Maybe Int
binarySearchCodepoint arr cp lo hi
  | lo > hi = Nothing
  | otherwise =
      let mid = (lo + hi) `div` 2
          entry = arr ! mid
          c = entry.codepoint
      in if cp == c
         then Just entry.glyphIndex
         else if cp < c
              then binarySearchCodepoint arr cp lo (mid - 1)
              else binarySearchCodepoint arr cp (mid + 1) hi

binarySearchKerning :: Array Int KerningPair -> Int -> Int -> Int -> Int -> Double
binarySearchKerning arr l r lo hi
  | lo > hi = 0
  | otherwise =
      let mid = (lo + hi) `div` 2
          entry = arr ! mid
          keyL = entry.left
          keyR = entry.right
      in if l == keyL && r == keyR
         then entry.xAdvance
         else if (l < keyL) || (l == keyL && r < keyR)
              then binarySearchKerning arr l r lo (mid - 1)
              else binarySearchKerning arr l r (mid + 1) hi
