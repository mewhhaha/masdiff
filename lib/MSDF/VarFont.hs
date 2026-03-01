{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MSDF.VarFont
  ( parseVarFontSpec,
    parseAxisAssignments,
    parseAxisQuery,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import MSDF.Types (AxisMap, AxisTag (..), AxisVal (..), FontSrc (..))

parseVarFontSpec :: String -> Either String FontSrc
parseVarFontSpec spec =
  case splitOnce "?" spec of
    Nothing ->
      Right
        VarFontFile
          { path = spec,
            axes = Map.empty
          }
    Just (path, query) -> do
      parsedAxes <- parseAxisQuery query
      pure
        VarFontFile
          { path = path,
            axes = parsedAxes
          }

parseAxisQuery :: String -> Either String AxisMap
parseAxisQuery query
  | null query = Right Map.empty
  | otherwise = parseAxisAssignments =<< traverse parseAxisEntry (splitBy '&' query)

parseAxisAssignments :: [(String, String)] -> Either String AxisMap
parseAxisAssignments assignments = go Set.empty Map.empty assignments
  where
    go _ axisMap [] = Right axisMap
    go seen axisMap ((rawName, rawValue) : rest) = do
      let name = trim rawName
      if null name
        then Left "Invalid empty axis name in -varfont value."
        else do
          value <- parseFiniteDouble rawValue
          let foldTag = T.toCaseFold (T.pack name)
          if foldTag `Set.member` seen
            then Left ("Duplicate axis tag in -varfont value: " <> name)
            else
              go
                (Set.insert foldTag seen)
                (Map.insert (AxisTag (T.pack name)) (AxisVal value) axisMap)
                rest

parseAxisEntry :: String -> Either String (String, String)
parseAxisEntry raw =
  case splitOnce "=" raw of
    Nothing -> Left ("Invalid axis entry in -varfont value: " <> raw)
    Just (name, valueRaw) -> Right (name, valueRaw)

parseFiniteDouble :: String -> Either String Double
parseFiniteDouble raw =
  case reads raw of
    [(x, "")] | isFinite x -> Right x
    _ -> Left ("Invalid axis value in -varfont value: " <> raw)

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
