module MSDF.Types
  ( MSDFAtlas(..)
  , GlyphMSDF(..)
  , MSDFBitmap(..)
  , BBox(..)
  , CodepointMapEntry(..)
  , KerningPair(..)
  , lookupCodepoint
  , lookupKerning
  ) where

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

-- | Packed RGB bitmap data.
data MSDFBitmap = MSDFBitmap
  { bmpWidth :: Int
  , bmpHeight :: Int
  , bmpOffsetX :: Double
  , bmpOffsetY :: Double
  , bmpPixels :: UArray Int Word8
  } deriving (Eq, Show)

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
