module MSDF.Generate.Native
  ( generateGlyphIO,
    generateGlyphBatchIO,
  )
where

import MSDF.Native
  ( generateGlyphBatchNativeIO,
    generateGlyphNativeIO,
  )
import MSDF.Types (FontSrc, GenCfg, GenErr, GenOut, GlyphCode)

generateGlyphIO :: GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphIO = generateGlyphNativeIO

generateGlyphBatchIO :: Int -> GenCfg -> FontSrc -> [GlyphCode] -> IO [Either GenErr GenOut]
generateGlyphBatchIO = generateGlyphBatchNativeIO
