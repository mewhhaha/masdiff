module MSDF.Generate.Native
  ( generateGlyphIO,
  )
where

import MSDF.Native (generateGlyphNativeIO)
import MSDF.Types (FontSrc, GenCfg, GenErr, GenOut, GlyphCode)

generateGlyphIO :: GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphIO = generateGlyphNativeIO
