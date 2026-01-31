module MSDF.Render
  ( pixelRange
  , scaleForPixelSize
  , pixelRangeForAtlas
  , glyphQuad
  , glyphUV
  ) where

import MSDF.Types

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
