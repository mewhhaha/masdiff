module MSDF.Types
  ( MSDFAtlas(..)
  , GlyphMSDF(..)
  , GlyphPlacement(..)
  , VerticalMetrics(..)
  , AtlasImage(..)
  , MSDFBitmap(..)
  , BBox(..)
  , BBoxUnion(..)
  , bboxUnion
  , bboxUnionMaybe
  , CodepointMapEntry(..)
  , KerningPair(..)
  , Anchor(..)
  , MarkGlyph(..)
  , BaseGlyph(..)
  , MarkToBase(..)
  , MarkToMark(..)
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
  , vAscent :: Maybe Int
  , vDescent :: Maybe Int
  , vLineGap :: Maybe Int
  , pixelSize :: Int
  , range :: Int
  , scale :: Double
  , atlasPadding :: Int
  , atlas :: Maybe AtlasImage
  , glyphs :: Array GlyphIndex GlyphMSDF
  , codepointIndex :: Array Int CodepointMapEntry
  , kerning :: Array Int KerningPair
  , markToBase :: [MarkToBase]
  , markToMark :: [MarkToMark]
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

-- | Anchor position in pixels.
data Anchor = Anchor
  { x :: Double
  , y :: Double
  } deriving (Eq, Show)

-- | Mark glyph anchor and class.
data MarkGlyph = MarkGlyph
  { markClass :: Int
  , anchor :: Anchor
  } deriving (Eq, Show)

-- | Base glyph anchors per mark class.
data BaseGlyph = BaseGlyph
  { anchors :: Array Int (Maybe Anchor)
  } deriving (Eq, Show)

data MarkToBase = MarkToBase
  { classCount :: Int
  , marks :: Array Int (Maybe MarkGlyph)
  , bases :: Array Int (Maybe BaseGlyph)
  } deriving (Eq, Show)

data MarkToMark = MarkToMark
  { classCount :: Int
  , marks1 :: Array Int (Maybe MarkGlyph)
  , marks2 :: Array Int (Maybe BaseGlyph)
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
  , vertical :: Maybe VerticalMetrics
  , placement :: Maybe GlyphPlacement
  } deriving (Eq, Show)

-- | Optional vertical metrics for vertical writing.
data VerticalMetrics = VerticalMetrics
  { advance :: Double
  , topSideBearing :: Double
  } deriving (Eq, Show)

-- | Glyph placement inside an atlas (pixel and UV space).
data GlyphPlacement = GlyphPlacement
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , u0 :: Double
  , v0 :: Double
  , u1 :: Double
  , v1 :: Double
  } deriving (Eq, Show)

-- | Packed atlas image (RGB).
data AtlasImage = AtlasImage
  { width :: Int
  , height :: Int
  , pixels :: UArray Int Word8
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

instance NFData Anchor where
  rnf (Anchor a b) = a `seq` b `seq` ()

instance NFData MarkGlyph where
  rnf (MarkGlyph a b) = a `seq` b `seq` ()

instance NFData BaseGlyph where
  rnf (BaseGlyph a) = rnf a

instance NFData MarkToBase where
  rnf (MarkToBase c m b) = c `seq` rnf m `seq` rnf b `seq` ()

instance NFData MarkToMark where
  rnf (MarkToMark c m1 m2) = c `seq` rnf m1 `seq` rnf m2 `seq` ()

instance NFData MSDFBitmap where
  rnf (MSDFBitmap w h ox oy px) = w `seq` h `seq` ox `seq` oy `seq` px `seq` ()

instance NFData GlyphMSDF where
  rnf (GlyphMSDF i cps adv bx by bb bm vm pl) =
    i `seq` cps `seq` adv `seq` bx `seq` by `seq` bb `seq` bm `seq` vm `seq` pl `seq` ()

instance NFData VerticalMetrics where
  rnf (VerticalMetrics a b) = a `seq` b `seq` ()

instance NFData MSDFAtlas where
  rnf (MSDFAtlas n u a d l va vd vg p r s pad at g c k mb mm) =
    n `seq` u `seq` a `seq` d `seq` l `seq` va `seq` vd `seq` vg `seq` p `seq` r `seq` s `seq`
    pad `seq` at `seq` g `seq` c `seq` k `seq` mb `seq` mm `seq` ()

instance NFData GlyphPlacement where
  rnf (GlyphPlacement a b c d e f g h) =
    a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` ()

instance NFData AtlasImage where
  rnf (AtlasImage w h px) = w `seq` h `seq` px `seq` ()

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
