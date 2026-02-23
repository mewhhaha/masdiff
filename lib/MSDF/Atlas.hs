{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MSDF.Atlas
  ( Atlas (..),
    AtlasCfg,
    AtlasEntry (..),
    AtlasPage (..),
    AtlasRect (..),
    defaultAtlasCfg,
    generateAtlasIO,
    generateAtlasWithRasterIO,
    mkAtlasCfg,
    packAtlas,
    renderAtlasTsv,
  )
where

import Control.Monad (forM_, foldM)
import Control.Exception (AsyncException, SomeException, displayException, fromException, throwIO, try)
import Data.Array.MArray (getElems, newArray, writeArray)
import Data.Array.ST (STUArray)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Control.Monad.ST (ST, runST)
import Data.Word (Word8)
import MSDF.Generate (RuntimeCfg, generateGlyphBatchIO)
import MSDF.Native (RasterPreparedIO, generateGlyphBatchNativeWithIO)
import MSDF.Types
  ( FontSrc,
    GenCfg,
    GenErr,
    GenOut (..),
    GlyphCode,
    ImgRGBA8 (..),
    Metrics (..),
    mkImgRGBA8,
    showGlyphCodeHex,
    unGlyphCode,
  )

data AtlasCfg = AtlasCfg
  { pw :: !Int,
    ph :: !Int,
    pad :: !Int
  }
  deriving stock (Eq, Show)

data AtlasRect = AtlasRect
  { x :: !Int,
    y :: !Int,
    w :: !Int,
    h :: !Int
  }
  deriving stock (Eq, Show)

data AtlasEntry = AtlasEntry
  { glyph :: !GlyphCode,
    page :: !Int,
    rect :: !AtlasRect,
    metrics :: !Metrics
  }
  deriving stock (Eq, Show)

data AtlasPage = AtlasPage
  { idx :: !Int,
    img :: !ImgRGBA8
  }
  deriving stock (Eq, Show)

data Atlas = Atlas
  { cfg :: !AtlasCfg,
    pages :: ![AtlasPage],
    entries :: ![AtlasEntry]
  }
  deriving stock (Eq, Show)

data Placement = Placement
  { page :: !Int,
    x :: !Int,
    y :: !Int
  }
  deriving stock (Eq, Show)

data PackState = PackState
  { page :: !Int,
    cx :: !Int,
    cy :: !Int,
    rh :: !Int,
    placed :: ![(GlyphCode, GenOut, Placement)]
  }
  deriving stock (Eq, Show)

defaultAtlasCfg :: AtlasCfg
defaultAtlasCfg = AtlasCfg {pw = 1024, ph = 1024, pad = 1}

mkAtlasCfg :: Int -> Int -> Int -> Either String AtlasCfg
mkAtlasCfg pw ph pad
  | pw <= 0 = Left "atlas width must be > 0."
  | ph <= 0 = Left "atlas height must be > 0."
  | pad < 0 = Left "atlas padding must be >= 0."
  | otherwise = Right AtlasCfg {pw = pw, ph = ph, pad = pad}

generateAtlasIO :: RuntimeCfg -> Int -> AtlasCfg -> GenCfg -> FontSrc -> [GlyphCode] -> IO (Either String Atlas)
generateAtlasIO runtime jobs atlasCfg genCfg src glyphs = do
  let uniqueGlyphs = dedupeGlyphs glyphs
  generatedResult <- try (generateGlyphBatchIO runtime jobs genCfg src uniqueGlyphs)
  assembleAtlasFromBatchIO atlasCfg uniqueGlyphs generatedResult

generateAtlasWithRasterIO :: Int -> RasterPreparedIO -> AtlasCfg -> GenCfg -> FontSrc -> [GlyphCode] -> IO (Either String Atlas)
generateAtlasWithRasterIO jobs raster atlasCfg genCfg src glyphs = do
  let uniqueGlyphs = dedupeGlyphs glyphs
  generatedResult <- try (generateGlyphBatchNativeWithIO jobs raster genCfg src uniqueGlyphs)
  assembleAtlasFromBatchIO atlasCfg uniqueGlyphs generatedResult

assembleAtlasFromBatchIO ::
  AtlasCfg ->
  [GlyphCode] ->
  Either SomeException [Either GenErr GenOut] ->
  IO (Either String Atlas)
assembleAtlasFromBatchIO atlasCfg uniqueGlyphs generatedResult =
  case generatedResult of
    Left ex ->
      case fromException ex of
        Just (_ :: AsyncException) -> throwIO ex
        Nothing ->
          pure
            ( Left
                ( "glyph batch generation threw exception: "
                    <> displayException ex
                )
            )
    Right generated ->
      pure (assemble uniqueGlyphs generated)
  where
    assemble ordered results =
      case firstFailure ordered results of
        Just err -> Left err
        Nothing ->
          let outs = collectRights results
              pairs = zip ordered outs
           in packAtlas atlasCfg pairs

firstFailure :: Show a => [GlyphCode] -> [Either a b] -> Maybe String
firstFailure glyphs results =
  foldl' step Nothing (zip glyphs results)
  where
    step acc (glyph, result) =
      case acc of
        Just _ -> acc
        Nothing ->
          case result of
            Left err ->
              Just ("glyph generation failed for " <> showGlyphCodeHex glyph <> ": " <> show err)
            Right _ -> Nothing

collectRights :: [Either a b] -> [b]
collectRights = foldr step []
  where
    step value acc =
      case value of
        Left _ -> acc
        Right x -> x : acc

packAtlas :: AtlasCfg -> [(GlyphCode, GenOut)] -> Either String Atlas
packAtlas atlasCfg glyphs =
  if null glyphs
    then
      Right
        Atlas
          { cfg = atlasCfg,
            pages = [],
            entries = []
          }
    else do
      finalState <- foldM (placeGlyph atlasCfg) initial glyphs
      let placements = reverse finalState.placed
      pageImgs <- buildPageImages atlasCfg placements
      let atlasPages =
            [ AtlasPage
                { idx = pageIdx,
                  img = img
                }
              | (pageIdx, img) <- pageImgs
            ]
      let atlasEntries =
            [ AtlasEntry
                { glyph = glyph,
                  page = placement.page,
                  rect =
                    AtlasRect
                      { x = placement.x,
                        y = placement.y,
                        w = out.img.w,
                        h = out.img.h
                      },
                  metrics = out.metrics
                }
              | (glyph, out, placement) <- placements
            ]
      Right
        Atlas
          { cfg = atlasCfg,
            pages = atlasPages,
            entries = atlasEntries
          }
  where
    initial =
      PackState
        { page = 0,
          cx = 0,
          cy = 0,
          rh = 0,
          placed = []
        }

placeGlyph :: AtlasCfg -> PackState -> (GlyphCode, GenOut) -> Either String PackState
placeGlyph atlasCfg st (glyph, out) = do
  let gw = out.img.w + (2 * atlasCfg.pad)
  let gh = out.img.h + (2 * atlasCfg.pad)
  if gw > atlasCfg.pw || gh > atlasCfg.ph
    then
      Left
        ( "glyph "
            <> showGlyphCodeHex glyph
            <> " does not fit atlas page (required "
            <> show gw
            <> "x"
            <> show gh
            <> ", page "
            <> show atlasCfg.pw
            <> "x"
            <> show atlasCfg.ph
            <> ")."
        )
    else do
      let stRow =
            if st.cx + gw > atlasCfg.pw
              then st {cx = 0, cy = st.cy + st.rh, rh = 0}
              else st
      let stPage =
            if stRow.cy + gh > atlasCfg.ph
              then stRow {page = stRow.page + 1, cx = 0, cy = 0, rh = 0}
              else stRow
      if stPage.cy + gh > atlasCfg.ph
        then
          Left
            ( "glyph "
                <> showGlyphCodeHex glyph
                <> " could not be placed after page rollover."
            )
        else do
          let placement =
                Placement
                  { page = stPage.page,
                    x = stPage.cx + atlasCfg.pad,
                    y = stPage.cy + atlasCfg.pad
                  }
          Right
            stPage
              { cx = stPage.cx + gw,
                rh = max stPage.rh gh,
                placed = (glyph, out, placement) : stPage.placed
              }

buildPageImages :: AtlasCfg -> [(GlyphCode, GenOut, Placement)] -> Either String [(Int, ImgRGBA8)]
buildPageImages atlasCfg placements = do
  let byPage = groupByPage placements
  traverse buildOnePage (IM.toAscList byPage)
  where
    buildOnePage (pageIdx, items) = do
      img <- blitPage atlasCfg (fmap (\(_, out, placement) -> (out.img, placement)) items)
      Right (pageIdx, img)

groupByPage :: [(GlyphCode, GenOut, Placement)] -> IM.IntMap [(GlyphCode, GenOut, Placement)]
groupByPage = foldl' step IM.empty
  where
    step acc item@(_, _, placement) =
      IM.insertWith (<>) placement.page [item] acc

blitPage :: AtlasCfg -> [(ImgRGBA8, Placement)] -> Either String ImgRGBA8
blitPage atlasCfg items = do
  let outW = atlasCfg.pw
  let outH = atlasCfg.ph
  let total = outW * outH * 4
  let px = BS.pack (buildBytes total)
  mkImgRGBA8 outW outH px
  where
    buildBytes total = runSTList total (\arr -> doBlits arr items)

    doBlits :: STUArray s Int Word8 -> [(ImgRGBA8, Placement)] -> ST s [Word8]
    doBlits arr pending = do
      forM_ pending (\(src, placement) -> blitOne arr src placement)
      getElems arr

    blitOne :: STUArray s Int Word8 -> ImgRGBA8 -> Placement -> ST s ()
    blitOne arr src placement = do
      let rowBytes = src.w * 4
      forM_ [0 .. src.h - 1] $ \iy -> do
        let srcBase = iy * rowBytes
        let dstBase = (((placement.y + iy) * atlasCfg.pw) + placement.x) * 4
        forM_ [0 .. rowBytes - 1] $ \ixByte -> do
          let srcIx = srcBase + ixByte
          let dstIx = dstBase + ixByte
          writeArray arr dstIx (BS.index src.px srcIx)

runSTList :: Int -> (forall s. STUArray s Int Word8 -> ST s [Word8]) -> [Word8]
runSTList total action = runST (newArray (0, total - 1) 0 >>= action)

dedupeGlyphs :: [GlyphCode] -> [GlyphCode]
dedupeGlyphs glyphs = reverse (snd (foldl' step (IM.empty, []) glyphs))
  where
    step (seen, out) glyph =
      let key = unGlyphCode glyph
       in case IM.lookup key seen of
            Just _ -> (seen, out)
            Nothing -> (IM.insert key () seen, glyph : out)

renderAtlasTsv :: Atlas -> String
renderAtlasTsv atlas =
  unlines
    ( header
        <> ["glyph\tglyph_hex\tpage\tx\ty\tw\th\tadv\tbounds_min_x\tbounds_min_y\tbounds_max_x\tbounds_max_y"]
        <> fmap renderEntry atlas.entries
    )
  where
    header =
      [ "# atlas_w=" <> show atlas.cfg.pw,
        "# atlas_h=" <> show atlas.cfg.ph,
        "# atlas_pad=" <> show atlas.cfg.pad,
        "# pages=" <> show (length atlas.pages)
      ]
    renderEntry entry =
      let (bx0, by0, bx1, by1) = entry.metrics.bounds
       in concatWithTab
            [ show (unGlyphCode entry.glyph),
              showGlyphCodeHex entry.glyph,
              show entry.page,
              show entry.rect.x,
              show entry.rect.y,
              show entry.rect.w,
              show entry.rect.h,
              show entry.metrics.adv,
              show bx0,
              show by0,
              show bx1,
              show by1
            ]

concatWithTab :: [String] -> String
concatWithTab values =
  case values of
    [] -> ""
    first : rest -> foldl' (\acc x -> acc <> "\t" <> x) first rest
