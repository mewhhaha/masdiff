{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.VarFont
  ( VarFontParseErr (..),
    renderVarFontParseErr,
    parseVarFontSpec,
    parseVarFontSpecTyped,
    parseAxisAssignments,
    parseAxisAssignmentsTyped,
    parseAxisQuery,
    parseAxisQueryTyped,
  )
where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import MSDF.Types (AxisMap, AxisTag (..), AxisVal (..), FontSrc (..))

data VarFontParseErr
  = VarFontAxisNameEmpty
  | VarFontAxisEntryInvalid String
  | VarFontAxisValueInvalid String
  | VarFontAxisTagDuplicate String
  deriving stock (Eq, Show)

renderVarFontParseErr :: VarFontParseErr -> String
renderVarFontParseErr err =
  case err of
    VarFontAxisNameEmpty ->
      "Invalid empty axis name in -varfont value."
    VarFontAxisEntryInvalid raw ->
      "Invalid axis entry in -varfont value: " <> raw
    VarFontAxisValueInvalid raw ->
      "Invalid axis value in -varfont value: " <> raw
    VarFontAxisTagDuplicate name ->
      "Duplicate axis tag in -varfont value: " <> name

parseVarFontSpec :: String -> Either String FontSrc
parseVarFontSpec = first renderVarFontParseErr . parseVarFontSpecTyped

parseVarFontSpecTyped :: String -> Either VarFontParseErr FontSrc
parseVarFontSpecTyped spec =
  case splitOnce "?" spec of
    Nothing ->
      Right
        VarFontFile
          { path = spec,
            axes = Map.empty
          }
    Just (path, query) -> do
      parsedAxes <- parseAxisQueryTyped query
      pure
        VarFontFile
          { path = path,
            axes = parsedAxes
          }

parseAxisQuery :: String -> Either String AxisMap
parseAxisQuery = first renderVarFontParseErr . parseAxisQueryTyped

parseAxisQueryTyped :: String -> Either VarFontParseErr AxisMap
parseAxisQueryTyped query
  | null query = Right Map.empty
  | otherwise = parseAxisAssignmentsTyped =<< traverse parseAxisEntry (splitBy '&' query)

parseAxisAssignments :: [(String, String)] -> Either String AxisMap
parseAxisAssignments = first renderVarFontParseErr . parseAxisAssignmentsTyped

parseAxisAssignmentsTyped :: [(String, String)] -> Either VarFontParseErr AxisMap
parseAxisAssignmentsTyped assignments = go Set.empty Map.empty assignments
  where
    go _ axisMap [] = Right axisMap
    go seen axisMap ((rawName, rawValue) : rest) = do
      let name = trim rawName
      if null name
        then Left VarFontAxisNameEmpty
        else do
          value <- parseFiniteDouble rawValue
          let foldTag = T.toCaseFold (T.pack name)
          if foldTag `Set.member` seen
            then Left (VarFontAxisTagDuplicate name)
            else
              go
                (Set.insert foldTag seen)
                (Map.insert (AxisTag (T.pack name)) (AxisVal value) axisMap)
                rest

parseAxisEntry :: String -> Either VarFontParseErr (String, String)
parseAxisEntry raw =
  case splitOnce "=" raw of
    Nothing -> Left (VarFontAxisEntryInvalid raw)
    Just (name, valueRaw) -> Right (name, valueRaw)

parseFiniteDouble :: String -> Either VarFontParseErr Double
parseFiniteDouble raw =
  case reads raw of
    [(x, "")] | isFinite x -> Right x
    _ -> Left (VarFontAxisValueInvalid raw)

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
    go acc rest@(x : _)
      | token `prefixOf` rest =
          Just (reverse acc, drop (length token) rest)
      | otherwise = go (x : acc) (drop 1 rest)

prefixOf :: String -> String -> Bool
prefixOf prefix text = take (length prefix) text == prefix

trim :: String -> String
trim = dropWhile (`elem` [' ', '\t']) . dropWhileEnd (`elem` [' ', '\t', '\r', '\n'])

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse

isFinite :: Double -> Bool
isFinite x = not (isNaN x || isInfinite x)
