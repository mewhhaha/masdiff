{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Encode
  ( decodeMsdfgenRgba,
    decodePngRGBA8,
    encodeMsdfgenRgba,
    encodePngRGBA8,
    readPngRGBA8File,
    writeMsdfgenRgbaFile,
    writePngRGBA8File,
  )
where

import Codec.Picture
import Data.Binary.Get (Get, getByteString, getWord32be, runGetOrFail)
import Data.Binary.Put (putByteString, putWord32be, runPut)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import MSDF.Types (ImgRGBA8 (..), mkImgRGBA8)
import System.IO (IOMode (WriteMode), withBinaryFile)

encodePngRGBA8 :: ImgRGBA8 -> ByteString
encodePngRGBA8 img =
  BL.toStrict $
    encodePng $
      generateImage mkPixel img.w img.h
  where
    mkPixel x y =
      let idx = ((y * img.w) + x) * 4
       in PixelRGBA8
            (BS.index img.px idx)
            (BS.index img.px (idx + 1))
            (BS.index img.px (idx + 2))
            (BS.index img.px (idx + 3))

decodePngRGBA8 :: ByteString -> Either String ImgRGBA8
decodePngRGBA8 pngBytes = do
  dyn <- first show (decodeImage pngBytes)
  let rgba8 = convertRGBA8 dyn
  mkImgRGBA8 rgba8.imageWidth rgba8.imageHeight (imageToByteString rgba8)

readPngRGBA8File :: FilePath -> IO (Either String ImgRGBA8)
readPngRGBA8File path = decodePngRGBA8 <$> BS.readFile path

writePngRGBA8File :: FilePath -> ImgRGBA8 -> IO ()
writePngRGBA8File path = BS.writeFile path . encodePngRGBA8

encodeMsdfgenRgba :: ImgRGBA8 -> ByteString
encodeMsdfgenRgba img =
  BL.toStrict $
    runPut $ do
      putHeader img.w img.h
      putByteString img.px
  where
    putHeader w h = do
      putByteString (C8.pack "RGBA")
      putWord32be (fromIntegral w)
      putWord32be (fromIntegral h)

decodeMsdfgenRgba :: ByteString -> Either String ImgRGBA8
decodeMsdfgenRgba raw = do
  (_, _, (w, h, px)) <- first (\(_, _, err) -> err) $
    runGetOrFail getMsdfgenRgba (BL.fromStrict raw)
  mkImgRGBA8 w h px

writeMsdfgenRgbaFile :: FilePath -> ImgRGBA8 -> IO ()
writeMsdfgenRgbaFile path img = withBinaryFile path WriteMode (\h -> BS.hPut h (encodeMsdfgenRgba img))

getMsdfgenRgba :: Get (Int, Int, ByteString)
getMsdfgenRgba = do
  magic <- getByteString 4
  if magic /= C8.pack "RGBA"
    then fail "Invalid msdfgen rgba payload: missing RGBA header."
    else do
      w <- fromIntegral <$> getWord32be
      h <- fromIntegral <$> getWord32be
      px <- getByteString (w * h * 4)
      pure (w, h, px)

imageToByteString :: Image PixelRGBA8 -> ByteString
imageToByteString image =
  BS.pack
    [ channel
      | y <- [0 .. image.imageHeight - 1],
        x <- [0 .. image.imageWidth - 1],
        channel <- pixelChannels (pixelAt image x y)
    ]
  where
    pixelChannels (PixelRGBA8 r g b a) = [r, g, b, a]
