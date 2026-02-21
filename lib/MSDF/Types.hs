{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Types
  ( AxisMap,
    AxisTag (..),
    AxisVal (..),
    Dim,
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut (..),
    GlyphCode,
    ImgRGBA8 (..),
    Metrics (..),
    Mode (..),
    PxRange,
    mkDim,
    mkGlyphCode,
    mkImgRGBA8,
    mkPxRange,
    showGlyphCodeHex,
    unDim,
    unGlyphCode,
    unPxRange,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Text.Printf (printf)

type AxisMap = Map.Map AxisTag AxisVal

newtype AxisTag = AxisTag {tag :: Text}
  deriving stock (Eq, Ord, Show)

newtype AxisVal = AxisVal {value :: Double}
  deriving stock (Eq, Ord, Show)

data Mode = Mtsdf
  deriving stock (Eq, Show)

newtype Dim = Dim {value :: Int}
  deriving stock (Eq, Show)

mkDim :: Int -> Either String Dim
mkDim x
  | x <= 0 = Left "Dim must be greater than zero."
  | otherwise = Right (Dim x)

unDim :: Dim -> Int
unDim (Dim x) = x

newtype PxRange = PxRange {value :: Double}
  deriving stock (Eq, Ord, Show)

mkPxRange :: Double -> Either String PxRange
mkPxRange x
  | isFinite x && x > 0 = Right (PxRange x)
  | otherwise = Left "PxRange must be finite and greater than zero."

unPxRange :: PxRange -> Double
unPxRange (PxRange x) = x

newtype GlyphCode = GlyphCode {value :: Int}
  deriving stock (Eq, Ord, Show)

mkGlyphCode :: Int -> Either String GlyphCode
mkGlyphCode x
  | x < 0 = Left "GlyphCode must be >= 0."
  | x > 0x10FFFF = Left "GlyphCode must be <= 0x10FFFF."
  | otherwise = Right (GlyphCode x)

unGlyphCode :: GlyphCode -> Int
unGlyphCode (GlyphCode x) = x

showGlyphCodeHex :: GlyphCode -> String
showGlyphCodeHex glyph = printf "0x%X" (unGlyphCode glyph)

data FontSrc
  = FontFile
      { path :: FilePath
      }
  | VarFontFile
      { path :: FilePath,
        axes :: AxisMap
      }
  deriving stock (Eq, Show)

data GenCfg = GenCfg
  { mode :: Mode,
    dim :: Dim,
    pxr :: PxRange,
    seed :: Int,
    autoframe :: Bool,
    ovlp :: Bool
  }
  deriving stock (Eq, Show)

data Metrics = Metrics
  { adv :: Double,
    bounds :: (Double, Double, Double, Double),
    scale :: Maybe Double,
    translate :: Maybe (Double, Double),
    range :: Maybe (Double, Double)
  }
  deriving stock (Eq, Show)

data ImgRGBA8 = ImgRGBA8
  { w :: Int,
    h :: Int,
    px :: ByteString
  }
  deriving stock (Eq, Show)

mkImgRGBA8 :: Int -> Int -> ByteString -> Either String ImgRGBA8
mkImgRGBA8 w h px
  | w <= 0 || h <= 0 = Left "Image width and height must be > 0."
  | BS.length px /= expected = Left expectedMsg
  | otherwise = Right ImgRGBA8 {w = w, h = h, px = px}
  where
    expected = w * h * 4
    expectedMsg =
      "Invalid RGBA payload length. expected="
        <> show expected
        <> " actual="
        <> show (BS.length px)

data GenOut = GenOut
  { img :: ImgRGBA8,
    metrics :: Metrics
  }
  deriving stock (Eq, Show)

data GenErr
  = InvalidCfg String
  | MissingInput String
  | Unsupported String
  | ExecFailed String
  | ParseFailed String
  deriving stock (Eq, Show)

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)
