module MSDF.Config
  ( MSDFConfig(..)
  , AtlasConfig(..)
  , OutlineConfig(..)
  , EdgeColoringConfig(..)
  , ColoringStrategy(..)
  , DistanceConfig(..)
  , FillRule(..)
  , SignMode(..)
  , CorrectionConfig(..)
  , GlyphSet(..)
  ) where

import Data.List (sort)
import MSDF.Types (BitmapFormat(..))

data MSDFConfig = MSDFConfig
  { pixelSize :: Int
  , range :: Int
  , glyphSet :: GlyphSet
  , variations :: [(String, Double)]
  , outputFormat :: BitmapFormat
  , parallelism :: Int
  , atlas :: AtlasConfig
  , outline :: OutlineConfig
  , coloring :: EdgeColoringConfig
  , distance :: DistanceConfig
  , correction :: CorrectionConfig
  }

data AtlasConfig = AtlasConfig
  { packAtlas :: Bool
  , atlasPadding :: Int
  , atlasMinSize :: Int
  , atlasMaxSize :: Int
  , atlasPowerOfTwo :: Bool
  }

data OutlineConfig = OutlineConfig
  { windingFlatness :: Double
  , contourEpsilon :: Double
  , normalizeOrientation :: Bool
  }

data EdgeColoringConfig = EdgeColoringConfig
  { cornerAngleDeg :: Double
  , minSegments :: Int
  , conflictDistance :: Double
  , coloringSeed :: Int
  , strategy :: ColoringStrategy
  }

data ColoringStrategy
  = ColoringSimple
  | ColoringInktrap
  | ColoringDistance
  deriving (Eq, Show)

data DistanceConfig = DistanceConfig
  { pseudoDistance :: Bool
  , gridCellSize :: Double
  , signEpsilon :: Double
  , fillRule :: FillRule
  , signMode :: SignMode
  , overlapSupport :: Bool
  , overlapEpsilon :: Double
  }

data FillRule
  = FillNonZero
  | FillOdd
  | FillPositive
  | FillNegative
  deriving (Eq, Show)

data SignMode
  = SignWinding
  | SignScanline
  deriving (Eq, Show)

data CorrectionConfig = CorrectionConfig
  { enableCorrection :: Bool
  , channelThreshold :: Double
  , edgeThreshold :: Double
  , hardThreshold :: Double
  }

data GlyphSet
  = GlyphSetNone
  | GlyphSetAll
  | GlyphSetCodepoints [Int]

instance Semigroup GlyphSet where
  GlyphSetAll <> _ = GlyphSetAll
  _ <> GlyphSetAll = GlyphSetAll
  GlyphSetNone <> x = x
  x <> GlyphSetNone = x
  GlyphSetCodepoints a <> GlyphSetCodepoints b = GlyphSetCodepoints (uniqueSorted (a ++ b))

instance Monoid GlyphSet where
  mempty = GlyphSetNone

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
