{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.Generate
  ( RuntimeCfg (..),
    BackendMode (..),
    defaultRuntimeCfg,
    parseBackendModeEnv,
    generateGlyphIO,
    generateGlyphBatchIO,
    renderMetrics,
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (mask, onException, throwIO)
import Data.Char (toLower)
import Data.List (find, intercalate, stripPrefix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import MSDF.Encode (decodeMsdfgenRgba)
import qualified MSDF.Generate.Native as Native
import MSDF.Types
  ( AxisMap,
    AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut (..),
    GlyphCode,
    Metrics (..),
    Mode (..),
    showGlyphCodeHex,
    unDim,
    unPxRange,
  )
import qualified Data.ByteString as BS
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

data BackendMode
  = BackendNative
  | BackendProcess
  deriving stock (Eq, Show)

data RuntimeCfg = RuntimeCfg
  { backend :: BackendMode,
    msdfgenBin :: FilePath
  }
  deriving stock (Eq, Show)

defaultRuntimeCfg :: RuntimeCfg
defaultRuntimeCfg =
  RuntimeCfg
    { backend = BackendNative,
      msdfgenBin = "msdfgen"
    }

parseBackendModeEnv :: Maybe String -> Either String BackendMode
parseBackendModeEnv raw =
  case fmap (fmap toLower . trim) raw of
    Nothing -> Right defaultRuntimeCfg.backend
    Just "native" -> Right BackendNative
    Just "oracle" -> Right BackendProcess
    Just "process" -> Right BackendProcess
    Just value ->
      Left
        ( "Invalid MASDIFF_BACKEND value: "
            <> value
            <> ". Supported values: native, process."
        )

generateGlyphIO :: RuntimeCfg -> GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphIO runtime cfg src glyph = do
  hasFont <- doesFileExist (fontPath src)
  if not hasFont
    then pure $ Left $ MissingInput ("Font file not found: " <> fontPath src)
    else
      case runtime.backend of
        BackendNative -> Native.generateGlyphIO cfg src glyph
        BackendProcess -> generateGlyphProcessIO runtime cfg src glyph

generateGlyphBatchIO :: RuntimeCfg -> Int -> GenCfg -> FontSrc -> [GlyphCode] -> IO [Either GenErr GenOut]
generateGlyphBatchIO runtime jobs cfg src glyphs
  | runtime.backend == BackendNative = Native.generateGlyphBatchIO jobs cfg src glyphs
  | jobs <= 1 = traverse runOne glyphs
  | otherwise = mapConcurrentlyBounded jobs runOne glyphs
  where
    runOne glyph = generateGlyphIO runtime cfg src glyph

mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded jobs action inputs = do
  sem <- newQSem (max 1 jobs)
  resultVars <- traverse (spawnOne sem) inputs
  results <- traverse takeMVar resultVars
  case firstFailure results of
    Just ex -> throwIO ex
    Nothing -> pure (collectRights results)
  where
    spawnOne sem input = do
      mask $ \restore -> do
        waitQSem sem
        resultVar <- newEmptyMVar
        let release = signalQSem sem
        _ <-
          forkFinally
            (restore (action input))
            (\result -> do
               putMVar resultVar result
               release
            )
            `onException` release
        pure resultVar

    firstFailure = foldr pickFirstFailure Nothing
    pickFirstFailure result acc =
      case result of
        Left ex -> Just ex
        Right _ -> acc
    collectRights = foldr collect []
    collect result acc =
      case result of
        Left _ -> acc
        Right value -> value : acc

generateGlyphProcessIO :: RuntimeCfg -> GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphProcessIO runtime cfg src glyph = do
  resolved <- findExecutable runtime.msdfgenBin
  case resolved of
    Nothing ->
      pure $
        Left $
          MissingInput $
            "Could not find msdfgen executable: " <> runtime.msdfgenBin
    Just _ -> do
      withSystemTempDirectory "masdiff-glyph" $ \tmpDir -> do
        let outRgba = tmpDir </> "glyph.rgba"
        let cmdArgs = buildArgs cfg src glyph outRgba
        (exitCode, stdoutText, stderrText) <- readProcessWithExitCode runtime.msdfgenBin cmdArgs ""
        case exitCode of
          ExitFailure code ->
            pure $
              Left $
                ExecFailed $
                  unlines
                    [ "msdfgen failed.",
                      "exit=" <> show code,
                      "command=" <> unwords (runtime.msdfgenBin : cmdArgs),
                      stdoutText,
                      stderrText
                    ]
          ExitSuccess -> do
            raw <- BS.readFile outRgba
            case decodeMsdfgenRgba raw of
              Left err ->
                pure $ Left $ ParseFailed ("Failed to decode rgba output: " <> err)
              Right img ->
                case parseMetrics (stdoutText <> "\n" <> stderrText) of
                  Left err ->
                    pure $ Left $ ParseFailed err
                  Right metrics ->
                    pure $
                      Right
                        GenOut
                          { img = img,
                            metrics = metrics
                          }

renderMetrics :: Metrics -> String
renderMetrics metrics =
  unlines $
    [ "advance = " <> show metrics.adv,
      "bounds = " <> render4 metrics.bounds
    ]
      <> maybe [] (\x -> ["scale = " <> show x]) metrics.scale
      <> maybe [] (\(tx, ty) -> ["translate = " <> show tx <> ", " <> show ty]) metrics.translate
      <> maybe [] (\(lo, hi) -> ["range = " <> show lo <> " to " <> show hi]) metrics.range
  where
    render4 (a, b, c, d) = intercalate ", " (fmap show [a, b, c, d])

buildArgs :: GenCfg -> FontSrc -> GlyphCode -> FilePath -> [String]
buildArgs cfg src glyph outRgba =
  [ modeArg cfg.mode
  ]
    <> fontArgs src
    <> [ showGlyphCodeHex glyph,
         "-dimensions",
         show (unDim cfg.dim),
         show (unDim cfg.dim),
         "-pxrange",
         show (unPxRange cfg.pxr),
         "-seed",
         show cfg.seed
       ]
    <> autoframeArg cfg.autoframe
    <> overlapFixArg cfg.ovlp
    <> [ "-printmetrics",
         "-format",
         "rgba",
         "-o",
         outRgba
       ]

overlapFixArg :: Bool -> [String]
-- Keep behavior stable for now; overlap-fix wiring is intentionally deferred.
overlapFixArg _ = []

modeArg :: Mode -> String
modeArg Mtsdf = "mtsdf"

autoframeArg :: Bool -> [String]
autoframeArg enabled =
  if enabled
    then ["-autoframe"]
    else []

fontArgs :: FontSrc -> [String]
fontArgs src =
  case src of
    FontFile path ->
      ["-font", path]
    VarFontFile path axes ->
      ["-varfont", path <> "?" <> renderAxes axes]

fontPath :: FontSrc -> FilePath
fontPath src =
  case src of
    FontFile path -> path
    VarFontFile path _ -> path

renderAxes :: AxisMap -> String
renderAxes axes =
  intercalate "&" $
    fmap renderPair (Map.toAscList axes)
  where
    renderPair (AxisTag tag, AxisVal value) =
      T.unpack tag <> "=" <> show value

parseMetrics :: String -> Either String Metrics
parseMetrics raw = do
  bounds <- parseBounds (findLine "bounds = ")
  adv <- parseSingleDouble (findLine "advance = ")
  let scale = parseSingleDoubleMaybe (findLine "scale = ")
  let translate = parseTranslateMaybe (findLine "translate = ")
  let range = parseRangeMaybe (findLine "range ")
  pure
    Metrics
      { adv = adv,
        bounds = bounds,
        scale = scale,
        translate = translate,
        range = range
      }
  where
    ls = fmap trim (lines raw)
    findLine prefix =
      case find (isPrefixOf prefix) ls of
        Nothing -> Left ("Missing metrics line: " <> prefix)
        Just x -> Right x

parseBounds :: Either String String -> Either String (Double, Double, Double, Double)
parseBounds lineResult = do
  line <- lineResult
  payload <- fromPrefix "bounds = " line
  let values = fmap trim (splitBy ',' payload)
  case traverse readMaybeDouble values of
    Right [a, b, c, d] -> Right (a, b, c, d)
    _ -> Left ("Invalid bounds line: " <> line)

parseSingleDouble :: Either String String -> Either String Double
parseSingleDouble lineResult = do
  line <- lineResult
  payload <- fromPrefixPrefixEq line
  readMaybeDouble payload
  where
    fromPrefixPrefixEq line =
      case break (== '=') line of
        (_, []) -> Left ("Expected '=' in line: " <> line)
        (_, _:rest) -> Right (trim rest)

parseSingleDoubleMaybe :: Either String String -> Maybe Double
parseSingleDoubleMaybe lineResult =
  case lineResult of
    Left _ -> Nothing
    Right line ->
      case break (== '=') line of
        (_, []) -> Nothing
        (_, _:rest) -> readMaybe (trim rest)

parseTranslateMaybe :: Either String String -> Maybe (Double, Double)
parseTranslateMaybe lineResult =
  case lineResult of
    Left _ -> Nothing
    Right line -> do
      payload <- stripPrefix "translate = " line
      let values = fmap trim (splitBy ',' payload)
      case traverse readMaybeDouble values of
        Right [x, y] -> Just (x, y)
        _ -> Nothing

parseRangeMaybe :: Either String String -> Maybe (Double, Double)
parseRangeMaybe lineResult =
  case lineResult of
    Left _ -> Nothing
    Right line -> do
      payload <- stripPrefix "range " line
      (lhs, rhsRaw) <- splitOnce " to " payload
      lo <- readMaybe (trim lhs)
      hi <- readMaybe (trim rhsRaw)
      pure (lo, hi)

readMaybeDouble :: String -> Either String Double
readMaybeDouble s =
  case readMaybe s of
    Nothing -> Left ("Expected floating point number, got: " <> s)
    Just x -> Right x

fromPrefix :: String -> String -> Either String String
fromPrefix prefix line =
  case stripPrefix prefix line of
    Nothing -> Left ("Expected line with prefix: " <> prefix <> " got: " <> line)
    Just payload -> Right payload

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
splitOnce token input =
  let (lhs, rhs) = breakOn token input
   in case rhs of
        [] -> Nothing
        _ -> Just (lhs, drop (length token) rhs)

breakOn :: String -> String -> (String, String)
breakOn needle haystack = go [] haystack
  where
    go acc rest
      | needle `isPrefixOf` rest = (reverse acc, rest)
      | otherwise =
          case rest of
            [] -> (reverse acc, [])
            (x : xs) -> go (x : acc) xs

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix value =
  take (length prefix) value == prefix

trim :: String -> String
trim = dropWhileEnd isSpaceChar . dropWhile isSpaceChar
  where
    isSpaceChar = (`elem` [' ', '\t', '\r', '\n'])

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse
