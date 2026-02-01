{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Array ((!))
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, floatLE, word8, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.List (foldl')
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName, (<.>))
import Data.Word (Word8)

import qualified MSDF.MSDF as MSDF
import MSDF.MSDF (GlyphSet(..))
import MSDF.Generated (generateMSDFWithConfig)
import MSDF.Render (glyphQuad, glyphUV, pixelRangeForAtlas)
import MSDF.Types (AtlasImage(..), GlyphMSDF(..), MSDFAtlas(..), lookupCodepoint)
import MSDF.TTF.Parser (ParseError(..))
import Spirdo.Wesl (compile, renderCompileError, sourceFile, shaderSpirv)

shaderDirName :: FilePath
shaderDirName = "shaders"

outDirName :: FilePath
outDirName = "out"

screenW, screenH :: Double
screenW = 1280
screenH = 720

sampleText :: String
sampleText = "masdiff"

main :: IO ()
main = do
  baseDir <- resolveBaseDir
  let shaderDir = baseDir </> shaderDirName
      outDir = baseDir </> outDirName
  createDirectoryIfMissing True outDir
  compileWesl outDir (shaderDir </> "msdf.vert.wesl")
  compileWesl outDir (shaderDir </> "msdf.frag.wesl")

  fontPath <- resolveFontPath baseDir
  let cfg = MSDF.defaultMSDFConfig
        { MSDF.pixelSize = 128
        , MSDF.range = 12
        , MSDF.atlasPadding = 16
        , MSDF.msdfCorrectionThreshold = 0.05
        , MSDF.glyphSet = GlyphSetCodepoints (map ord sampleText)
        , MSDF.packAtlas = True
        }
  result <- generateMSDFWithConfig cfg fontPath
  case result of
    Left err -> do
      let ParseError { context = ctx, message = msg } = err
      putStrLn ("generateMSDFWithConfig failed: " <> ctx <> ": " <> msg)
      exitFailure
    Right atlas -> writeAtlas outDir atlas

resolveBaseDir :: IO FilePath
resolveBaseDir = do
  let local = "." </> shaderDirName </> "msdf.vert.wesl"
      fromRoot = "examples" </> "sdl_gpu_wesl" </> shaderDirName </> "msdf.vert.wesl"
  localOk <- doesFileExist local
  if localOk
    then pure "."
    else do
      rootOk <- doesFileExist fromRoot
      if rootOk
        then pure ("examples" </> "sdl_gpu_wesl")
        else do
          putStrLn "could not locate shaders; run from repo root or examples/sdl_gpu_wesl"
          exitFailure

resolveFontPath :: FilePath -> IO FilePath
resolveFontPath baseDir = do
  args <- getArgs
  let defaultPath = baseDir </> ".." </> ".." </> "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
      path =
        case args of
          ["--font", p] -> p
          [p] -> p
          _ -> defaultPath
  exists <- doesFileExist path
  if exists
    then pure path
    else do
      putStrLn ("font not found: " <> path)
      putStrLn "pass a font path: msdf-sdl-gen --font /path/to/font.ttf"
      exitFailure

compileWesl :: FilePath -> FilePath -> IO ()
compileWesl outDir src = do
  result <- compile [] (sourceFile src)
  case result of
    Left err -> do
      putStrLn (renderCompileError err)
      exitFailure
    Right bundle -> do
      let outPath = outDir </> (takeBaseName src <.> "spv")
      BS.writeFile outPath (shaderSpirv bundle)
      putStrLn ("wrote " <> outPath)

writeAtlas :: FilePath -> MSDFAtlas -> IO ()
writeAtlas outDir atlas =
  case atlas of
    MSDFAtlas { atlas = Nothing } -> do
      putStrLn "atlas packing failed (packAtlas was true)"
      exitFailure
    MSDFAtlas { atlas = Just img, pixelSize = ps } -> do
      let AtlasImage { width = aw, height = ah, pixels = px } = img
          rgbaBytes = rgbToRgbaBytes px
          pxRange = pixelRangeForAtlas atlas (fromIntegral ps :: Double)
          bounds = textBounds atlas sampleText
          penStart' = snapPen (centerPen (screenW, screenH) bounds)
          texel = (1 / fromIntegral aw, 1 / fromIntegral ah)
          verts = buildTextVertices atlas (screenW, screenH) penStart' texel sampleText
          verticesOut = outDir </> "vertices.bin"
          atlasOut = outDir </> "atlas.rgba"
          metaOut = outDir </> "meta.txt"
      BS.writeFile atlasOut rgbaBytes
      BL.writeFile verticesOut (toLazyByteString (floatListBuilder verts))
      writeFile metaOut $ unlines
        [ "atlasWidth " <> show aw
        , "atlasHeight " <> show ah
        , "pixelSize " <> show ps
        , "pxRange " <> show pxRange
        , "screenWidth " <> show screenW
        , "screenHeight " <> show screenH
        , "vertexCount " <> show (length verts `div` 4)
        ]
      putStrLn ("wrote " <> atlasOut)
      putStrLn ("wrote " <> verticesOut)
      putStrLn ("wrote " <> metaOut)

rgbToRgbaBytes :: UA.UArray Int Word8 -> BS.ByteString
rgbToRgbaBytes arr =
  BL.toStrict (toLazyByteString (go (UA.elems arr)))
  where
    go (r:g:b:rest) = word8 r <> word8 g <> word8 b <> word8 255 <> go rest
    go [] = mempty
    go _ = error "rgbToRgbaBytes: invalid RGB length"

floatListBuilder :: [Float] -> Builder
floatListBuilder = foldMap floatLE

buildTextVertices :: MSDFAtlas -> (Double, Double) -> (Double, Double) -> (Double, Double) -> String -> [Float]
buildTextVertices atlas screen (penX0, penY) texel text =
  let MSDFAtlas { glyphs = glyphsArr } = atlas
      step (acc, penX) ch =
        case lookupCodepoint atlas (ord ch) of
          Nothing -> (acc, penX)
          Just gi ->
            let glyph = glyphsArr ! gi
                GlyphMSDF { advance = adv } = glyph
                quad = quadVerts screen (penX, penY) texel glyph
                penX' = penX + adv
            in (quad : acc, penX')
      (chunks, _) = foldl' step ([], penX0) text
  in concat (reverse chunks)

quadVerts :: (Double, Double) -> (Double, Double) -> (Double, Double) -> GlyphMSDF -> [Float]
quadVerts screen pen texel glyph =
  let (x0, y0, x1, y1) = glyphQuad glyph pen
      (u0, v0, u1, v1) = insetUV texel (glyphUV glyph)
      (cx0, cy0) = toClip screen (x0, y0)
      (cx1, cy1) = toClip screen (x1, y1)
  in [ cx0, cy0, realToFrac u0, realToFrac v0
     , cx1, cy0, realToFrac u1, realToFrac v0
     , cx1, cy1, realToFrac u1, realToFrac v1
     , cx0, cy0, realToFrac u0, realToFrac v0
     , cx1, cy1, realToFrac u1, realToFrac v1
     , cx0, cy1, realToFrac u0, realToFrac v1
     ]

toClip :: (Double, Double) -> (Double, Double) -> (Float, Float)
toClip (w, h) (x, y) =
  ( realToFrac (x / (w * 0.5) - 1)
  , realToFrac (y / (h * 0.5) - 1)
  )

insetUV :: (Double, Double) -> (Double, Double, Double, Double) -> (Double, Double, Double, Double)
insetUV (du, dv) (u0, v0, u1, v1) =
  let insetU = du * 0.5
      insetV = dv * 0.5
  in (u0 + insetU, v0 + insetV, u1 - insetU, v1 - insetV)

textBounds :: MSDFAtlas -> String -> (Double, Double, Double, Double)
textBounds atlas text =
  let MSDFAtlas { glyphs = glyphsArr } = atlas
      step (penX, acc) ch =
        case lookupCodepoint atlas (ord ch) of
          Nothing -> (penX, acc)
          Just gi ->
            let glyph = glyphsArr ! gi
                GlyphMSDF { advance = adv } = glyph
                (x0, y0, x1, y1) = glyphQuad glyph (penX, 0)
                acc' = case acc of
                  Nothing -> Just (x0, y0, x1, y1)
                  Just (mnx, mny, mxx, mxy) ->
                    Just (min mnx x0, min mny y0, max mxx x1, max mxy y1)
            in (penX + adv, acc')
      (_, bounds) = foldl' step (0, Nothing) text
  in case bounds of
       Just b -> b
       Nothing -> (0, 0, 0, 0)

centerPen :: (Double, Double) -> (Double, Double, Double, Double) -> (Double, Double)
centerPen (w, h) (minX, minY, maxX, maxY) =
  let textW = maxX - minX
      textH = maxY - minY
      penX = (w - textW) * 0.5 - minX
      penY = (h - textH) * 0.5 - minY
  in (penX, penY)

snapPen :: (Double, Double) -> (Double, Double)
snapPen (x, y) = (fromIntegral (round x :: Int), fromIntegral (round y :: Int))
