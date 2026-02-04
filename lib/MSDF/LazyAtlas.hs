module MSDF.LazyAtlas
  ( LazyAtlasConfig(..)
  , AtlasUpdate(..)
  , LazyAtlas
  , defaultLazyAtlasConfig
  , newLazyAtlas
  , ensureGlyph
  , snapshotAtlasImage
  ) where

import Control.Monad (forM_, void, when)
import System.CPUTime (getCPUTime)
import Data.Array.Base (unsafeWrite)
import Data.Array.IO (IOUArray, newArray, readArray)
import Data.Array.IArray ((!))
import Data.Array.Unboxed (UArray, listArray)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Word (Word8)

import MSDF.Config (MSDFConfig(..), AtlasConfig(..))
import MSDF.MSDF (renderGlyphMSDF)
import MSDF.Types (AtlasImage(..), GlyphMSDF(..), GlyphPlacement(..), MSDFBitmap(..), bitmapChannels)
import MSDF.TTF.Parser (TTF)

data LazyAtlasConfig = LazyAtlasConfig
  { atlasWidth :: Int
  , atlasHeight :: Int
  , atlasPadding :: Int
  , maxEntries :: Int
  , debugEnabled :: Bool
  } deriving (Eq, Show)

data AtlasUpdate = AtlasUpdate
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , pixels :: UArray Int Word8
  } deriving (Eq, Show)

data Rect = Rect
  { rx :: Int
  , ry :: Int
  , rw :: Int
  , rh :: Int
  } deriving (Eq, Show)

data AtlasEntry = AtlasEntry
  { glyph :: GlyphMSDF
  , rect :: Rect
  , lastUsed :: Int
  } deriving (Eq, Show)

data LazyAtlas = LazyAtlas
  { cfg :: MSDFConfig
  , ttf :: TTF
  , width :: Int
  , height :: Int
  , padding :: Int
  , channels :: Int
  , pixelsRef :: IOUArray Int Word8
  , entriesRef :: IORef (IntMap.IntMap AtlasEntry)
  , freeRectsRef :: IORef [Rect]
  , useCounterRef :: IORef Int
  , maxEntriesLimit :: Int
  , debugEnabled :: Bool
  }

defaultLazyAtlasConfig :: MSDFConfig -> LazyAtlasConfig
defaultLazyAtlasConfig cfg =
  let AtlasConfig { atlasMaxSize = maxSize, atlasPadding = pad } = cfg.atlas
  in LazyAtlasConfig
       { atlasWidth = max 1 maxSize
       , atlasHeight = max 1 maxSize
       , atlasPadding = max 0 pad
       , maxEntries = 0
       , debugEnabled = False
       }

newLazyAtlas :: MSDFConfig -> TTF -> LazyAtlasConfig -> IO LazyAtlas
newLazyAtlas cfg ttf lazyCfg = do
  let w = max 1 lazyCfg.atlasWidth
      h = max 1 lazyCfg.atlasHeight
      fmt = cfg.outputFormat
      ch = bitmapChannels fmt
      total = w * h * ch
  pixelsRef <- newArray (0, total - 1) 0 :: IO (IOUArray Int Word8)
  entriesRef <- newIORef IntMap.empty
  freeRectsRef <- newIORef [Rect 0 0 w h]
  useCounterRef <- newIORef 0
  pure LazyAtlas
    { cfg = cfg
    , ttf = ttf
    , width = w
    , height = h
    , padding = lazyCfg.atlasPadding
    , channels = ch
    , pixelsRef = pixelsRef
    , entriesRef = entriesRef
    , freeRectsRef = freeRectsRef
    , useCounterRef = useCounterRef
    , maxEntriesLimit = lazyCfg.maxEntries
    , debugEnabled = lazyCfg.debugEnabled
    }

ensureGlyph :: LazyAtlas -> Int -> IO (GlyphMSDF, Maybe AtlasUpdate)
ensureGlyph atlas glyphIndex = do
  entries <- readIORef atlas.entriesRef
  case IntMap.lookup glyphIndex entries of
    Just entry -> do
      touchEntry atlas glyphIndex entry
      pure (entry.glyph, Nothing)
    Nothing -> do
      glyph0 <-
        if atlas.debugEnabled
        then do
          start <- getCPUTime
          putStrLn ("lazy atlas: render glyph " <> show glyphIndex)
          let g = renderGlyphMSDF atlas.cfg atlas.ttf glyphIndex
              bmp = g.bitmap
          bmp.pixels `seq` pure ()
          end <- getCPUTime
          let secs :: Double
              secs = fromIntegral (end - start) / 1.0e12
          putStrLn ("lazy atlas: rendered glyph " <> show glyphIndex
            <> " " <> show bmp.width <> "x" <> show bmp.height
            <> " in " <> show secs <> "s")
          pure g
        else pure (renderGlyphMSDF atlas.cfg atlas.ttf glyphIndex)
      let MSDFBitmap { width = bw, height = bh, format = fmt } = glyph0.bitmap
      if bw <= 0 || bh <= 0
        then do
          let entry = AtlasEntry glyph0 (Rect 0 0 0 0) 0
          modifyIORef' atlas.entriesRef (IntMap.insert glyphIndex entry)
          pure (glyph0, Nothing)
        else if fmt /= atlas.cfg.outputFormat
          then pure (glyph0, Nothing)
          else do
            let neededW = bw + 2 * atlas.padding
                neededH = bh + 2 * atlas.padding
            enforceLimit atlas
            rect <- allocRect atlas neededW neededH
            case rect of
              Nothing -> pure (glyph0, Nothing)
              Just r -> do
                let dstX = r.rx + atlas.padding
                    dstY = r.ry + atlas.padding
                blitBitmap atlas dstX dstY glyph0.bitmap
                let placement = mkPlacement atlas dstX dstY bw bh
                    glyph1 = glyph0 { placement = Just placement }
                use <- nextUse atlas
                let entry = AtlasEntry glyph1 r use
                modifyIORef' atlas.entriesRef (IntMap.insert glyphIndex entry)
                pure (glyph1, Just AtlasUpdate
                  { x = dstX
                  , y = dstY
                  , width = bw
                  , height = bh
                  , pixels = glyph0.bitmap.pixels
                  })

snapshotAtlasImage :: LazyAtlas -> IO AtlasImage
snapshotAtlasImage atlas = do
  let total = atlas.width * atlas.height * atlas.channels
  vals <- mapM (readArray atlas.pixelsRef) [0 .. total - 1]
  pure AtlasImage
    { width = atlas.width
    , height = atlas.height
    , format = atlas.cfg.outputFormat
    , pixels = listArray (0, total - 1) vals
    }

-- Internal helpers ----------------------------------------------------------

nextUse :: LazyAtlas -> IO Int
nextUse atlas = do
  n <- readIORef atlas.useCounterRef
  let n' = n + 1
  writeIORef' atlas.useCounterRef n'
  pure n'

writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref v = modifyIORef' ref (const v)

touchEntry :: LazyAtlas -> Int -> AtlasEntry -> IO ()
touchEntry atlas glyphIndex entry = do
  use <- nextUse atlas
  let entry' = entry { lastUsed = use }
  modifyIORef' atlas.entriesRef (IntMap.insert glyphIndex entry')

allocRect :: LazyAtlas -> Int -> Int -> IO (Maybe Rect)
allocRect atlas w h = do
  freeRects <- readIORef atlas.freeRectsRef
  case findFit freeRects w h of
    Nothing -> do
      evicted <- evictLRU atlas
      if not evicted then pure Nothing else allocRect atlas w h
    Just (r, rest) -> do
      let splits = splitRect r w h
      modifyIORef' atlas.freeRectsRef (const (splits ++ rest))
      pure (Just (Rect r.rx r.ry w h))

findFit :: [Rect] -> Int -> Int -> Maybe (Rect, [Rect])
findFit [] _ _ = Nothing
findFit (r:rs) w h
  | r.rw >= w && r.rh >= h = Just (r, rs)
  | otherwise = do
      (r', rs') <- findFit rs w h
      pure (r', r : rs')

splitRect :: Rect -> Int -> Int -> [Rect]
splitRect r w h =
  let rightW = r.rw - w
      bottomH = r.rh - h
      right = Rect (r.rx + w) r.ry rightW h
      bottom = Rect r.rx (r.ry + h) r.rw bottomH
      keep rect = rect.rw > 0 && rect.rh > 0
  in filter keep [right, bottom]

evictLRU :: LazyAtlas -> IO Bool
evictLRU atlas = do
  entries <- readIORef atlas.entriesRef
  if IntMap.null entries
    then pure False
    else do
      let entry = minimumBy (comparing (\e -> e.lastUsed)) (IntMap.elems entries)
          gid = entry.glyph.index
      modifyIORef' atlas.entriesRef (IntMap.delete gid)
      clearRect atlas entry.rect
      modifyIORef' atlas.freeRectsRef (entry.rect :)
      pure True

clearRect :: LazyAtlas -> Rect -> IO ()
clearRect atlas r = do
  let ch = atlas.channels
  forM_ [0 .. r.rh - 1] $ \y -> do
    let rowBase = ((r.ry + y) * atlas.width + r.rx) * ch
        rowLen = r.rw * ch
    forM_ [0 .. rowLen - 1] $ \i ->
      unsafeWrite atlas.pixelsRef (rowBase + i) 0

blitBitmap :: LazyAtlas -> Int -> Int -> MSDFBitmap -> IO ()
blitBitmap atlas dstX dstY bmp = do
  let ch = atlas.channels
      bw = bmp.width
      bh = bmp.height
      src = bmp.pixels
  forM_ [0 .. bh - 1] $ \y -> do
    let srcRow = y * bw
        dstRow = (dstY + y) * atlas.width + dstX
    forM_ [0 .. bw - 1] $ \x -> do
      let srcBase = (srcRow + x) * ch
          dstBase = (dstRow + x) * ch
      forM_ [0 .. ch - 1] $ \c ->
        unsafeWrite atlas.pixelsRef (dstBase + c) (src ! (srcBase + c))

mkPlacement :: LazyAtlas -> Int -> Int -> Int -> Int -> GlyphPlacement
mkPlacement atlas dstX dstY bw bh =
  let w = atlas.width
      h = atlas.height
      u0 = fromIntegral dstX / fromIntegral w
      v0 = fromIntegral dstY / fromIntegral h
      u1 = fromIntegral (dstX + bw) / fromIntegral w
      v1 = fromIntegral (dstY + bh) / fromIntegral h
  in GlyphPlacement
       { x = dstX
       , y = dstY
       , width = bw
       , height = bh
       , u0 = u0
       , v0 = v0
       , u1 = u1
       , v1 = v1
       }

enforceLimit :: LazyAtlas -> IO ()
enforceLimit atlas = do
  let limit = atlas.maxEntriesLimit
  if limit <= 0
    then pure ()
    else do
      entries <- readIORef atlas.entriesRef
      when (IntMap.size entries >= limit) $
        void (evictLRU atlas)
