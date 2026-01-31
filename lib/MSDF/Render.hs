module MSDF.Render
  ( Origin(..)
  , YAxis(..)
  , SamplerColorSpace(..)
  , SamplerFilter(..)
  , SamplerWrap(..)
  , SamplerHints(..)
  , atlasOrigin
  , uvOrigin
  , glyphQuadSpace
  , msdfSamplerHints
  , pixelRange
  , scaleForPixelSize
  , pixelRangeForAtlas
  , glyphQuad
  , glyphUV
  , glyphUVTopLeft
  , glyphQuadYDown
  ) where

import MSDF.Types

data Origin
  = OriginTopLeft
  | OriginBottomLeft
  deriving (Eq, Show)

data YAxis
  = YUp
  | YDown
  deriving (Eq, Show)

data SamplerColorSpace
  = ColorSpaceLinear
  | ColorSpaceSRGB
  deriving (Eq, Show)

data SamplerFilter
  = FilterLinear
  | FilterNearest
  deriving (Eq, Show)

data SamplerWrap
  = WrapClamp
  | WrapRepeat
  deriving (Eq, Show)

data SamplerHints = SamplerHints
  { colorSpace :: SamplerColorSpace
  , filter :: SamplerFilter
  , wrapU :: SamplerWrap
  , wrapV :: SamplerWrap
  } deriving (Eq, Show)

atlasOrigin :: Origin
atlasOrigin = OriginBottomLeft

uvOrigin :: Origin
uvOrigin = OriginBottomLeft

glyphQuadSpace :: YAxis
glyphQuadSpace = YUp

msdfSamplerHints :: SamplerHints
msdfSamplerHints = SamplerHints
  { colorSpace = ColorSpaceLinear
  , filter = FilterLinear
  , wrapU = WrapClamp
  , wrapV = WrapClamp
  }

-- | Convert an MSDF range and screen-space scale to a pixel-range factor.
pixelRange :: Int -> Double -> Double
pixelRange range scale = fromIntegral range * scale

-- | Convert an atlas pixel size to a screen-space scale factor.
scaleForPixelSize :: MSDFAtlas -> Double -> Double
scaleForPixelSize atlas targetPx =
  if atlas.pixelSize <= 0
  then 1
  else targetPx / fromIntegral atlas.pixelSize

-- | Pixel-range helper based on atlas config and desired screen size.
pixelRangeForAtlas :: MSDFAtlas -> Double -> Double
pixelRangeForAtlas atlas targetPx =
  pixelRange atlas.range (scaleForPixelSize atlas targetPx)

-- | Compute the quad bounds for a glyph bitmap at a pen position (baseline).
glyphQuad :: GlyphMSDF -> (Double, Double) -> (Double, Double, Double, Double)
glyphQuad glyph (penX, penY) =
  let bmp = glyph.bitmap
      x0 = penX + glyph.bbox.xMin - bmp.offsetX
      y0 = penY - glyph.bbox.yMax - bmp.offsetY
      x1 = x0 + fromIntegral bmp.width
      y1 = y0 + fromIntegral bmp.height
  in (x0, y0, x1, y1)

-- | UVs for the glyph bitmap. Defaults to full quad if not in an atlas.
glyphUV :: GlyphMSDF -> (Double, Double, Double, Double)
glyphUV glyph =
  case glyph.placement of
    Just pl -> (pl.u0, pl.v0, pl.u1, pl.v1)
    Nothing -> (0, 0, 1, 1)

-- | UVs with a top-left origin (flip V from the default bottom-left).
glyphUVTopLeft :: GlyphMSDF -> (Double, Double, Double, Double)
glyphUVTopLeft glyph =
  let (u0, v0, u1, v1) = glyphUV glyph
  in (u0, 1 - v1, u1, 1 - v0)

-- | Quad bounds in a y-down coordinate system (baseline at penY).
glyphQuadYDown :: GlyphMSDF -> (Double, Double) -> (Double, Double, Double, Double)
glyphQuadYDown glyph (penX, penY) =
  let (x0, y0, x1, y1) = glyphQuad glyph (penX, penY)
      y0' = 2 * penY - y1
      y1' = 2 * penY - y0
  in (x0, y0', x1, y1')
