{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import Data.Char (ord, toUpper)
import Data.Array (elems)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))

import qualified MSDF.MSDF as MSDF
import MSDF.MSDF (MSDFConfig(..), defaultMSDFConfig, renderGlyphMSDF)
import MSDF.TTF.Parser (Cmap(..), Head(..), ParseError(..), compositeComponentCount, glyphOutlineAt, parseTTF, TTF(..))
import MSDF.TTF.Variations
  ( Variations(..)
  , componentDeltas
  , compositeUsesPointDeltas
  , gvarDeltaSum
  , gvarTupleDeltaStats
  , gvarTuplePointStats
  , normalizeLocation
  )
import MSDF.Outline (Point(..))
import MSDF.Types (BitmapFormat(..), MSDFBitmap(..), GlyphMSDF(..), bitmapChannels)
import Paths_masdiff (getDataFileName)
import Control.Monad (when)


data Options = Options
  { optFontPath :: Maybe FilePath
  , optText :: String
  , optPixelSize :: Int
  , optRange :: Int
  , optPadding :: Int
  , optFormat :: BitmapFormat
  , optOutDir :: FilePath
  , optVars :: [(String, Double)]
  , optDebugGvar :: Bool
  , optSignMode :: Maybe MSDF.SignMode
  , optSplitIntersections :: Maybe Bool
  , optOverlapSupport :: Maybe Bool
  , optPseudoDistance :: Maybe Bool
  }

usage :: String
usage =
  unlines
    [ "msdf-dump --font PATH --text TEXT --out-dir DIR"
    , "          [--pixel-size N] [--range N] [--padding N]"
    , "          [--format msdf|mtsdf]"
    , "          [--var tag=value]..."
    , "          [--sign-mode winding|scanline]"
    , "          [--split-intersections | --no-split-intersections]"
    , "          [--overlap | --no-overlap]"
    , "          [--pseudo-distance | --no-pseudo-distance]"
    , "          [--debug-gvar]"
    ]

main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  fontPath <- resolveFontPath opts
  parsed <- parseTTF fontPath
  ttf <- case parsed of
    Left (ParseError ctx msg) -> die (ctx ++ ": " ++ msg)
    Right val -> pure val
  createDirectoryIfMissing True opts.optOutDir
  let cps = nub (map ord opts.optText)
      cfg0 = defaultMSDFConfig
      signMode =
        case opts.optSignMode of
          Just mode -> mode
          Nothing ->
            if opts.optDebugGvar
            then MSDF.SignWinding
            else cfg0.distance.signMode
      cfg = cfg0
        { MSDF.pixelSize = opts.optPixelSize
        , MSDF.range = opts.optRange
        , MSDF.atlas = (cfg0.atlas { MSDF.atlasPadding = opts.optPadding, MSDF.packAtlas = False })
        , MSDF.outputFormat = opts.optFormat
        , MSDF.variations = opts.optVars
        , MSDF.outline =
            cfg0.outline
              { MSDF.splitIntersections = fromMaybe cfg0.outline.splitIntersections opts.optSplitIntersections
              }
        , MSDF.distance =
            cfg0.distance
              { MSDF.signMode = signMode
              , MSDF.overlapSupport = fromMaybe cfg0.distance.overlapSupport opts.optOverlapSupport
              , MSDF.pseudoDistance = fromMaybe cfg0.distance.pseudoDistance opts.optPseudoDistance
              }
        }
  mapM_ (dumpGlyph ttf cfg opts.optOutDir opts.optDebugGvar) cps

parseArgs :: [String] -> IO Options
parseArgs args = go args defaultOpts
  where
    defaultOpts = Options
      { optFontPath = Nothing
      , optText = "masdiff"
      , optPixelSize = 128
      , optRange = 12
      , optPadding = 16
      , optFormat = BitmapMSDF
      , optOutDir = "out/msdfgen_compare/masdiff"
      , optVars = []
      , optDebugGvar = False
      , optSignMode = Nothing
      , optSplitIntersections = Nothing
      , optOverlapSupport = Nothing
      , optPseudoDistance = Nothing
      }

    go [] opts = pure opts
    go ("--font":path:rest) opts = go rest opts { optFontPath = Just path }
    go ("--text":txt:rest) opts = go rest opts { optText = txt }
    go ("--pixel-size":v:rest) opts = go rest opts { optPixelSize = read v }
    go ("--range":v:rest) opts = go rest opts { optRange = read v }
    go ("--padding":v:rest) opts = go rest opts { optPadding = read v }
    go ("--format":v:rest) opts = go rest opts { optFormat = parseFormat v }
    go ("--out-dir":dir:rest) opts = go rest opts { optOutDir = dir }
    go ("--var":spec:rest) opts =
      case parseVar spec of
        Nothing -> die ("invalid --var " ++ spec ++ " (expected tag=value)")
        Just v -> go rest opts { optVars = opts.optVars ++ [v] }
    go ("--sign-mode":v:rest) opts =
      case parseSignMode v of
        Nothing -> die ("invalid --sign-mode " ++ v ++ " (expected winding|scanline)")
        Just mode -> go rest opts { optSignMode = Just mode }
    go ("--split-intersections":rest) opts = go rest opts { optSplitIntersections = Just True }
    go ("--no-split-intersections":rest) opts = go rest opts { optSplitIntersections = Just False }
    go ("--overlap":rest) opts = go rest opts { optOverlapSupport = Just True }
    go ("--no-overlap":rest) opts = go rest opts { optOverlapSupport = Just False }
    go ("--pseudo-distance":rest) opts = go rest opts { optPseudoDistance = Just True }
    go ("--no-pseudo-distance":rest) opts = go rest opts { optPseudoDistance = Just False }
    go ("--debug-gvar":rest) opts = go rest opts { optDebugGvar = True }
    go ("-h":_) _ = die usage
    go ("--help":_) _ = die usage
    go (_:rest) opts = go rest opts

parseVar :: String -> Maybe (String, Double)
parseVar spec =
  case break (== '=') spec of
    (tag, '=':valStr)
      | not (null tag) ->
          case reads valStr of
            [(v, "")] -> Just (tag, v)
            _ -> Nothing
    _ -> Nothing

parseFormat :: String -> BitmapFormat
parseFormat v =
  case map toUpper v of
    "MSDF" -> BitmapMSDF
    "MTSDF" -> BitmapMTSDF
    _ -> BitmapMSDF

parseSignMode :: String -> Maybe MSDF.SignMode
parseSignMode v =
  case map toUpper v of
    "WINDING" -> Just MSDF.SignWinding
    "SCANLINE" -> Just MSDF.SignScanline
    _ -> Nothing

resolveFontPath :: Options -> IO FilePath
resolveFontPath opts =
  case opts.optFontPath of
    Just path -> do
      exists <- doesFileExist path
      if exists then pure path else die ("font not found: " ++ path)
    Nothing -> getDataFileName "assets/Inter/Inter-VariableFont_opsz,wght.ttf"


dumpGlyph :: TTF -> MSDFConfig -> FilePath -> Bool -> Int -> IO ()
dumpGlyph ttf cfg outDir debugGvar cp =
  case lookupCodepoint cp (ttfMappings ttf) of
    Nothing -> putStrLn ("missing codepoint U+" ++ padHex cp)
    Just glyphIndex -> do
      when debugGvar $
        dumpGvarStats ttf cfg cp glyphIndex
      let glyph = renderGlyphMSDF cfg ttf glyphIndex
          bmp = case glyph of
            GlyphMSDF { bitmap = b } -> b
          chans = bitmapChannels bmp.format
          rawPath = outDir </> (glyphName cp <.> "raw")
          metaPath = outDir </> (glyphName cp <.> "json")
          bytes = BS.pack (UA.elems bmp.pixels)
          unitsPerEm = case ttf of
            TTF { head = Head { unitsPerEm = upm } } -> upm
          scale = fromIntegral cfg.pixelSize / fromIntegral unitsPerEm
          translateX = if scale == 0 then 0 else (-bmp.offsetX / scale)
          translateY = if scale == 0 then 0 else (-bmp.offsetY / scale)
      BS.writeFile rawPath bytes
      writeFile metaPath (glyphMeta cp glyphIndex bmp cfg chans unitsPerEm scale translateX translateY)

ttfMappings :: TTF -> [(Int, Int)]
ttfMappings ttf =
  case ttf of
    TTF { cmap = Cmap ms } -> ms

lookupCodepoint :: Int -> [(Int, Int)] -> Maybe Int
lookupCodepoint cp mappings = lookup cp mappings

padHex :: Int -> String
padHex cp =
  let hex = map toUpper (showHex cp "")
      minWidth = if cp <= 0xFFFF then 4 else 6
      pad = replicate (max 0 (minWidth - length hex)) '0'
  in pad ++ hex

glyphName :: Int -> String
glyphName cp = "U+" ++ padHex cp

glyphMeta :: Int -> Int -> MSDFBitmap -> MSDFConfig -> Int -> Int -> Double -> Double -> Double -> String
glyphMeta cp glyphIndex bmp cfg chans unitsPerEm scale translateX translateY =
  let fmt = case bmp.format of
        BitmapMSDF -> "msdf"
        BitmapMTSDF -> "mtsdf"
  in unlines
      [ "{"
      , "  \"codepoint\": " ++ show cp ++ ","
      , "  \"glyphIndex\": " ++ show glyphIndex ++ ","
      , "  \"format\": \"" ++ fmt ++ "\","
      , "  \"width\": " ++ show bmp.width ++ ","
      , "  \"height\": " ++ show bmp.height ++ ","
      , "  \"channels\": " ++ show chans ++ ","
      , "  \"offsetX\": " ++ show bmp.offsetX ++ ","
      , "  \"offsetY\": " ++ show bmp.offsetY ++ ","
      , "  \"pixelSize\": " ++ show cfg.pixelSize ++ ","
      , "  \"range\": " ++ show cfg.range ++ ","
      , "  \"unitsPerEm\": " ++ show unitsPerEm ++ ","
      , "  \"scale\": " ++ show scale ++ ","
      , "  \"translateX\": " ++ show translateX ++ ","
      , "  \"translateY\": " ++ show translateY
      , "}"
      ]


die :: String -> IO a
die msg = do
  putStrLn msg
  exitFailure

dumpGvarStats :: TTF -> MSDFConfig -> Int -> Int -> IO ()
dumpGvarStats ttf cfg cp glyphIndex =
  case ttf.variations of
    Nothing -> pure ()
    Just vars -> do
      let loc = normalizeLocation vars.fvar vars.avar cfg.variations
          baseContours = glyphOutlineAt Nothing ttf glyphIndex
          varContours = glyphOutlineAt (Just loc) ttf glyphIndex
          delta = contourDeltaSum baseContours varContours
          pointCount = sum (map length baseContours) + 4
          baseCount = length baseContours
          varCount = length varContours
          basePoints = sum (map length baseContours)
          varPoints = sum (map length varContours)
      putStrLn ("gvar[" ++ glyphLabel cp ++ "]: deltaSum=" ++ show delta)
      putStrLn ("gvar[" ++ glyphLabel cp ++ "]: contours base=" ++ show baseCount ++ " var=" ++ show varCount
        ++ " points base=" ++ show basePoints ++ " var=" ++ show varPoints ++ " pointCount=" ++ show pointCount)
      let contourBBoxes = [ contourBBox c | c <- varContours, not (null c) ]
      when (not (null contourBBoxes)) $
        putStrLn ("gvar[" ++ glyphLabel cp ++ "]: contourBBoxes=" ++ show contourBBoxes)
      case vars.gvar of
        Nothing -> pure ()
        Just gv -> do
          putStrLn ("gvar[" ++ glyphLabel cp ++ "]: gvarDeltaSum=" ++ show (gvarDeltaSum gv loc glyphIndex pointCount))
          case gvarTuplePointStats gv glyphIndex pointCount of
            Nothing -> pure ()
            Just (minPt, maxPt, totalPts, invalidPts) ->
              putStrLn ("gvar[" ++ glyphLabel cp ++ "]: tuplePoints min=" ++ show minPt
                ++ " max=" ++ show maxPt ++ " total=" ++ show totalPts ++ " invalid=" ++ show invalidPts)
          case gvarTupleDeltaStats gv loc glyphIndex pointCount of
            Nothing -> pure ()
            Just stats ->
              let fmt (scalar, minDx, maxDx, minDy, maxDy) =
                    "  scalar=" ++ show scalar
                    ++ " dx=[" ++ show minDx ++ "," ++ show maxDx ++ "]"
                    ++ " dy=[" ++ show minDy ++ "," ++ show maxDy ++ "]"
              in do
                putStrLn ("gvar[" ++ glyphLabel cp ++ "]: tupleDeltas")
                mapM_ (putStrLn . fmt) stats
          let compCount = compositeComponentCount ttf glyphIndex
          if compCount <= 0
            then pure ()
            else do
              let (dxs, dys, phantom) = componentDeltas gv loc glyphIndex compCount
                  usesPoint = compositeUsesPointDeltas gv loc glyphIndex compCount pointCount
              putStrLn ("gvar[" ++ glyphLabel cp ++ "]: components=" ++ show compCount
                        ++ " dxs=" ++ show (elems dxs)
                        ++ " dys=" ++ show (elems dys)
                        ++ " phantom=" ++ show phantom
                        ++ " pointDeltas=" ++ show usesPoint)

contourBBox :: [Point] -> (Double, Double, Double, Double)
contourBBox pts =
  let xs = map (\p -> p.x) pts
      ys = map (\p -> p.y) pts
  in (minimum xs, minimum ys, maximum xs, maximum ys)

glyphLabel :: Int -> String
glyphLabel cp = "U+" ++ padHex cp

contourDeltaSum :: [[Point]] -> [[Point]] -> Double
contourDeltaSum a b =
  let pairs = zip a b
      sumPts acc (pa, pb) = acc + contourDelta pa pb
  in foldl sumPts 0 pairs

contourDelta :: [Point] -> [Point] -> Double
contourDelta a b =
  let pairs = zip a b
      sumPt acc (p1, p2) =
        let x1 = p1.x
            y1 = p1.y
            x2 = p2.x
            y2 = p2.y
        in acc + abs (x2 - x1) + abs (y2 - y1)
  in foldl sumPt 0 pairs
