{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Manifest
  ( Manifest (..),
    ManifestMeta (..),
    ManifestRow (..),
    loadManifest,
    manifestCfg,
  )
where

import Data.Bifunctor (first)
import MSDF.VarFont (parseVarFontSpecTyped, renderVarFontParseErr)
import MSDF.Types
  ( Dim,
    FontSrc (..),
    GenCfg (..),
    GlyphCode,
    Mode (..),
    PxRange,
    mkDim,
    mkGlyphCode,
    mkPxRange,
  )
import System.IO.Error (catchIOError, ioeGetErrorString)

data ManifestMeta = ManifestMeta
  { dim :: Dim,
    pxr :: PxRange,
    seed :: Int
  }
  deriving stock (Eq, Show)

data ManifestRow = ManifestRow
  { fontCase :: String,
    glyph :: GlyphCode,
    glyphHex :: String,
    outputPng :: FilePath,
    src :: FontSrc
  }
  deriving stock (Eq, Show)

data Manifest = Manifest
  { meta :: ManifestMeta,
    rows :: [ManifestRow]
  }
  deriving stock (Eq, Show)

loadManifest :: FilePath -> IO (Either String Manifest)
loadManifest path =
  catchIOError
    (parseManifest <$> readFile path)
    (\err -> pure (Left ("Failed to read manifest " <> path <> ": " <> ioeGetErrorString err)))

manifestCfg :: ManifestMeta -> GenCfg
manifestCfg meta =
  GenCfg
    { mode = Mtsdf,
      dim = meta.dim,
      pxr = meta.pxr,
      seed = meta.seed,
      autoframe = True,
      ovlp = False
    }

parseManifest :: String -> Either String Manifest
parseManifest raw = do
  dim <- parseHeaderDim headerLines
  pxr <- parseHeaderPxRange headerLines
  seed <- parseHeaderSeed headerLines
  parsedRows <- traverse parseRow dataLines
  pure
    Manifest
      { meta =
          ManifestMeta
            { dim = dim,
              pxr = pxr,
              seed = seed
            },
        rows = parsedRows
      }
  where
    ls = filter (not . null) (lines raw)
    headerLines = filter (\line -> take 1 line == "#") ls
    dataLines =
      filter
        (\line -> take 1 line /= "#" && line /= "font_case\tglyph\tglyph_hex\toutput_png\tinput_spec")
        ls

parseHeaderDim :: [String] -> Either String Dim
parseHeaderDim headers = do
  raw <- parseHeaderValue "dimensions" headers
  parsed <- parseInt raw
  first (("Invalid dimensions header: " <>) . show) (mkDim parsed)

parseHeaderPxRange :: [String] -> Either String PxRange
parseHeaderPxRange headers = do
  raw <- parseHeaderValue "pxrange" headers
  parsed <- parseDouble raw
  first (("Invalid pxrange header: " <>) . show) (mkPxRange parsed)

parseHeaderSeed :: [String] -> Either String Int
parseHeaderSeed headers = parseInt =<< parseHeaderValue "seed" headers

parseHeaderValue :: String -> [String] -> Either String String
parseHeaderValue key headers =
  case findHeader key headers of
    Nothing -> Left ("Missing manifest header: " <> key)
    Just value -> Right value

findHeader :: String -> [String] -> Maybe String
findHeader key = go
  where
    go [] = Nothing
    go (line : rest) =
      case splitOnce "=" (dropHeaderPrefix line) of
        Just (k, v) | trim k == key -> Just (trim v)
        _ -> go rest

dropHeaderPrefix :: String -> String
dropHeaderPrefix line =
  case line of
    '#' : rest -> trim rest
    _ -> line

parseRow :: String -> Either String ManifestRow
parseRow line = do
  case splitBy '\t' line of
    [caseId, _glyphLabel, glyphHex, outputPng, inputSpec] -> do
      glyph <- parseGlyphHex glyphHex
      src <- parseInputSpec inputSpec
      pure
        ManifestRow
          { fontCase = caseId,
            glyph = glyph,
            glyphHex = glyphHex,
            outputPng = outputPng,
            src = src
          }
    columns ->
      Left $
        "Expected 5 manifest columns, got "
          <> show (length columns)
          <> " in line: "
          <> line

parseGlyphHex :: String -> Either String GlyphCode
parseGlyphHex raw = do
  payload <-
    case splitOnce "U+" raw of
      Just ("", hexPart) -> Right hexPart
      _ -> Left ("Invalid glyph hex value: " <> raw)
  code <- parseHex payload
  first (("Invalid glyph code: " <>) . show) (mkGlyphCode code)

parseInputSpec :: String -> Either String FontSrc
parseInputSpec raw =
  case splitOnce ":" raw of
    Just ("font", path) ->
      Right $
        FontFile
          { path = path
          }
    Just ("varfont", spec) ->
      first renderVarFontParseErr (parseVarFontSpecTyped spec)
    _ -> Left ("Invalid input_spec: " <> raw)

parseInt :: String -> Either String Int
parseInt raw =
  case reads raw of
    [(x, "")] -> Right x
    _ -> Left ("Expected integer, got: " <> raw)

parseDouble :: String -> Either String Double
parseDouble raw =
  case reads raw of
    [(x, "")] -> Right x
    _ -> Left ("Expected floating-point value, got: " <> raw)

parseHex :: String -> Either String Int
parseHex raw =
  case reads ("0x" <> raw) of
    [(x, "")] -> Right x
    _ -> Left ("Expected hex value, got: " <> raw)

splitBy :: Char -> String -> [String]
splitBy delim = foldr step [""]
  where
    step c acc
      | c == delim = "" : acc
      | otherwise =
          case acc of
            [] -> [[c]]
            (x : xs) -> (c : x) : xs

splitOnce :: String -> String -> Maybe (String, String)
splitOnce token input = go [] input
  where
    go _ [] = Nothing
    go acc rest@(x : xs)
      | token `prefixOf` rest =
          Just (reverse acc, drop (length token) rest)
      | otherwise = go (x : acc) xs

prefixOf :: String -> String -> Bool
prefixOf prefix text = take (length prefix) text == prefix

trim :: String -> String
trim = dropWhile (`elem` [' ', '\t']) . dropWhileEnd (`elem` [' ', '\t', '\r', '\n'])

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse
