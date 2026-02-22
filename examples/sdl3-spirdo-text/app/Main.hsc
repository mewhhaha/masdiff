{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception (bracket, finally)
import Control.Monad (foldM, forM_, unless, when)
import Data.Char (ord, toLower, toUpper)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.List (isPrefixOf, mapAccumL, nub, sort, stripPrefix)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Foreign
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..), CSize (..))
import MSDF.Atlas
  ( Atlas (..),
    AtlasCfg,
    AtlasEntry (..),
    AtlasPage (..),
    AtlasRect (..),
    generateAtlasIO,
    mkAtlasCfg,
  )
import MSDF.Encode (writePngRGBA8File)
import MSDF.Generate (defaultRuntimeCfg)
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GlyphCode,
    ImgRGBA8 (..),
    Metrics (..),
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkImgRGBA8,
    mkPxRange,
    unPxRange,
    unGlyphCode,
  )
import Spirdo.Wesl.Reflection
  ( BindingInfo (..),
    BindingKind (..),
    BindingPlan (..),
    Shader,
    ShaderStage (..),
    shaderPlan,
    shaderSpirv,
    shaderStageCached,
    weslShader,
  )
import System.Exit (die)
import System.Environment (lookupEnv)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>

data SDLWindow
data SDLGPUDevice
data SDLGPUShader
data SDLGPUGraphicsPipeline
data SDLGPUSampler
data SDLGPUTexture
data SDLGPUTransferBuffer
data SDLGPUCommandBuffer
data SDLGPUCopyPass
data SDLGPURenderPass
data SDLGPUFence

data FColor = FColor
  { r :: !CFloat,
    g :: !CFloat,
    b :: !CFloat,
    a :: !CFloat
  }

instance Storable FColor where
  sizeOf _ = #{size SDL_FColor}
  alignment _ = #{alignment SDL_FColor}
  peek ptr =
    FColor
      <$> peekByteOff ptr #{offset SDL_FColor, r}
      <*> peekByteOff ptr #{offset SDL_FColor, g}
      <*> peekByteOff ptr #{offset SDL_FColor, b}
      <*> peekByteOff ptr #{offset SDL_FColor, a}
  poke ptr c = do
    pokeByteOff ptr #{offset SDL_FColor, r} c.r
    pokeByteOff ptr #{offset SDL_FColor, g} c.g
    pokeByteOff ptr #{offset SDL_FColor, b} c.b
    pokeByteOff ptr #{offset SDL_FColor, a} c.a

data TextureSamplerBinding = TextureSamplerBinding
  { texture :: !(Ptr SDLGPUTexture),
    sampler :: !(Ptr SDLGPUSampler)
  }

instance Storable TextureSamplerBinding where
  sizeOf _ = #{size SDL_GPUTextureSamplerBinding}
  alignment _ = #{alignment SDL_GPUTextureSamplerBinding}
  peek ptr =
    TextureSamplerBinding
      <$> peekByteOff ptr #{offset SDL_GPUTextureSamplerBinding, texture}
      <*> peekByteOff ptr #{offset SDL_GPUTextureSamplerBinding, sampler}
  poke ptr t = do
    pokeByteOff ptr #{offset SDL_GPUTextureSamplerBinding, texture} t.texture
    pokeByteOff ptr #{offset SDL_GPUTextureSamplerBinding, sampler} t.sampler

data VsUniform = VsUniform
  { rx :: !CFloat,
    ry :: !CFloat,
    rw :: !CFloat,
    rh :: !CFloat,
    u0 :: !CFloat,
    v0 :: !CFloat,
    u1 :: !CFloat,
    v1 :: !CFloat,
    sw :: !CFloat,
    sh :: !CFloat,
    p0 :: !CFloat,
    p1 :: !CFloat
  }

instance Storable VsUniform where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr =
    VsUniform
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20
      <*> peekByteOff ptr 24
      <*> peekByteOff ptr 28
      <*> peekByteOff ptr 32
      <*> peekByteOff ptr 36
      <*> peekByteOff ptr 40
      <*> peekByteOff ptr 44
  poke ptr v = do
    pokeByteOff ptr 0 v.rx
    pokeByteOff ptr 4 v.ry
    pokeByteOff ptr 8 v.rw
    pokeByteOff ptr 12 v.rh
    pokeByteOff ptr 16 v.u0
    pokeByteOff ptr 20 v.v0
    pokeByteOff ptr 24 v.u1
    pokeByteOff ptr 28 v.v1
    pokeByteOff ptr 32 v.sw
    pokeByteOff ptr 36 v.sh
    pokeByteOff ptr 40 v.p0
    pokeByteOff ptr 44 v.p1

data FsUniform = FsUniform
  { fr :: !CFloat,
    fg :: !CFloat,
    fb :: !CFloat,
    fa :: !CFloat,
    br :: !CFloat,
    bg :: !CFloat,
    bb :: !CFloat,
    ba :: !CFloat,
    pxr :: !CFloat,
    thr :: !CFloat,
    fallback :: !CFloat,
    soft :: !CFloat
  }

instance Storable FsUniform where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr =
    FsUniform
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20
      <*> peekByteOff ptr 24
      <*> peekByteOff ptr 28
      <*> peekByteOff ptr 32
      <*> peekByteOff ptr 36
      <*> peekByteOff ptr 40
      <*> peekByteOff ptr 44
  poke ptr v = do
    pokeByteOff ptr 0 v.fr
    pokeByteOff ptr 4 v.fg
    pokeByteOff ptr 8 v.fb
    pokeByteOff ptr 12 v.fa
    pokeByteOff ptr 16 v.br
    pokeByteOff ptr 20 v.bg
    pokeByteOff ptr 24 v.bb
    pokeByteOff ptr 28 v.ba
    pokeByteOff ptr 32 v.pxr
    pokeByteOff ptr 36 v.thr
    pokeByteOff ptr 40 v.fallback
    pokeByteOff ptr 44 v.soft

data PxRect = PxRect
  { x :: !Double,
    y :: !Double,
    w :: !Double,
    h :: !Double
  }

data UvRect = UvRect
  { u0 :: !Double,
    v0 :: !Double,
    u1 :: !Double,
    v1 :: !Double
  }

data GlyphRun = GlyphRun
  { adv :: !Double,
    sc :: !Double,
    ox :: !Double,
    oy :: !Double,
    desc :: !Double,
    iw :: !Int,
    ih :: !Int,
    uv :: !UvRect
  }

data GlyphScaled = GlyphScaled
  { ch :: !Char,
    adv :: !Double,
    ox :: !Double,
    oy :: !Double,
    desc :: !Double,
    w :: !Double,
    h :: !Double,
    uv :: !UvRect,
    spr :: !Double
  }

data LineAtom
  = AtomGlyph !GlyphScaled
  | AtomSpace !Double

data PlacedGlyph = PlacedGlyph
  { ch :: !Char,
    rect :: !PxRect,
    uv :: !UvRect,
    spr :: !Double
  }

data LineBuild = LineBuild
  { atlas :: !ImgRGBA8,
    glyphs :: ![PlacedGlyph],
    w :: !Double,
    h :: !Double
  }

data DrawGlyph = DrawGlyph
  { ch :: !Char,
    tex :: !(Ptr SDLGPUTexture),
    rect :: !PxRect,
    uv :: !UvRect,
    spr :: !Double
  }

data LineSpec = LineSpec
  { src :: !FontSrc,
    txt :: !String,
    em :: !Double
  }

data ScenePreset
  = SceneDefault
  | SceneSingleRegular !Char
  | SceneSingleVarLight !Char
  | SceneSingleVarBold !Char
  deriving stock (Eq, Show)

data GpuShader = GpuShader
  { spv :: !BS.ByteString,
    stg :: !ShaderStage,
    binds :: ![BindingInfo]
  }

foreign import ccall unsafe "SDL_Init"
  c_sdlInit :: Word32 -> IO CBool

foreign import ccall unsafe "SDL_Quit"
  c_sdlQuit :: IO ()

foreign import ccall unsafe "SDL_GetError"
  c_sdlGetError :: IO CString

foreign import ccall unsafe "SDL_CreateWindow"
  c_sdlCreateWindow :: CString -> CInt -> CInt -> Word64 -> IO (Ptr SDLWindow)

foreign import ccall unsafe "SDL_DestroyWindow"
  c_sdlDestroyWindow :: Ptr SDLWindow -> IO ()

foreign import ccall unsafe "SDL_PollEvent"
  c_sdlPollEvent :: Ptr () -> IO CBool

foreign import ccall unsafe "SDL_Delay"
  c_sdlDelay :: Word32 -> IO ()

foreign import ccall unsafe "SDL_CreateGPUDevice"
  c_sdlCreateGPUDevice :: Word32 -> CBool -> CString -> IO (Ptr SDLGPUDevice)

foreign import ccall unsafe "SDL_DestroyGPUDevice"
  c_sdlDestroyGPUDevice :: Ptr SDLGPUDevice -> IO ()

foreign import ccall unsafe "SDL_ClaimWindowForGPUDevice"
  c_sdlClaimWindowForGPUDevice :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO CBool

foreign import ccall unsafe "SDL_ReleaseWindowFromGPUDevice"
  c_sdlReleaseWindowFromGPUDevice :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO ()

foreign import ccall unsafe "SDL_GetGPUSwapchainTextureFormat"
  c_sdlGetGPUSwapchainTextureFormat :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO CInt

foreign import ccall unsafe "SDL_CreateGPUShader"
  c_sdlCreateGPUShader :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUShader)

foreign import ccall unsafe "SDL_ReleaseGPUShader"
  c_sdlReleaseGPUShader :: Ptr SDLGPUDevice -> Ptr SDLGPUShader -> IO ()

foreign import ccall unsafe "SDL_CreateGPUGraphicsPipeline"
  c_sdlCreateGPUGraphicsPipeline :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUGraphicsPipeline)

foreign import ccall unsafe "SDL_ReleaseGPUGraphicsPipeline"
  c_sdlReleaseGPUGraphicsPipeline :: Ptr SDLGPUDevice -> Ptr SDLGPUGraphicsPipeline -> IO ()

foreign import ccall unsafe "SDL_CreateGPUSampler"
  c_sdlCreateGPUSampler :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUSampler)

foreign import ccall unsafe "SDL_ReleaseGPUSampler"
  c_sdlReleaseGPUSampler :: Ptr SDLGPUDevice -> Ptr SDLGPUSampler -> IO ()

foreign import ccall unsafe "SDL_CreateGPUTexture"
  c_sdlCreateGPUTexture :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUTexture)

foreign import ccall unsafe "SDL_ReleaseGPUTexture"
  c_sdlReleaseGPUTexture :: Ptr SDLGPUDevice -> Ptr SDLGPUTexture -> IO ()

foreign import ccall unsafe "SDL_CreateGPUTransferBuffer"
  c_sdlCreateGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUTransferBuffer)

foreign import ccall unsafe "SDL_ReleaseGPUTransferBuffer"
  c_sdlReleaseGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUTransferBuffer -> IO ()

foreign import ccall unsafe "SDL_MapGPUTransferBuffer"
  c_sdlMapGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUTransferBuffer -> CBool -> IO (Ptr ())

foreign import ccall unsafe "SDL_UnmapGPUTransferBuffer"
  c_sdlUnmapGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUTransferBuffer -> IO ()

foreign import ccall unsafe "SDL_AcquireGPUCommandBuffer"
  c_sdlAcquireGPUCommandBuffer :: Ptr SDLGPUDevice -> IO (Ptr SDLGPUCommandBuffer)

foreign import ccall unsafe "SDL_BeginGPUCopyPass"
  c_sdlBeginGPUCopyPass :: Ptr SDLGPUCommandBuffer -> IO (Ptr SDLGPUCopyPass)

foreign import ccall unsafe "SDL_UploadToGPUTexture"
  c_sdlUploadToGPUTexture :: Ptr SDLGPUCopyPass -> Ptr () -> Ptr () -> CBool -> IO ()

foreign import ccall unsafe "SDL_DownloadFromGPUTexture"
  c_sdlDownloadFromGPUTexture :: Ptr SDLGPUCopyPass -> Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "SDL_EndGPUCopyPass"
  c_sdlEndGPUCopyPass :: Ptr SDLGPUCopyPass -> IO ()

foreign import ccall unsafe "SDL_WaitAndAcquireGPUSwapchainTexture"
  c_sdlWaitAndAcquireGPUSwapchainTexture ::
    Ptr SDLGPUCommandBuffer ->
    Ptr SDLWindow ->
    Ptr (Ptr SDLGPUTexture) ->
    Ptr Word32 ->
    Ptr Word32 ->
    IO CBool

foreign import ccall unsafe "SDL_BeginGPURenderPass"
  c_sdlBeginGPURenderPass ::
    Ptr SDLGPUCommandBuffer ->
    Ptr () ->
    Word32 ->
    Ptr () ->
    IO (Ptr SDLGPURenderPass)

foreign import ccall unsafe "SDL_BindGPUGraphicsPipeline"
  c_sdlBindGPUGraphicsPipeline :: Ptr SDLGPURenderPass -> Ptr SDLGPUGraphicsPipeline -> IO ()

foreign import ccall unsafe "SDL_BindGPUFragmentSamplers"
  c_sdlBindGPUFragmentSamplers :: Ptr SDLGPURenderPass -> Word32 -> Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "SDL_PushGPUVertexUniformData"
  c_sdlPushGPUVertexUniformData :: Ptr SDLGPUCommandBuffer -> Word32 -> Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "SDL_DrawGPUPrimitives"
  c_sdlDrawGPUPrimitives :: Ptr SDLGPURenderPass -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "SDL_EndGPURenderPass"
  c_sdlEndGPURenderPass :: Ptr SDLGPURenderPass -> IO ()

foreign import ccall unsafe "SDL_SubmitGPUCommandBuffer"
  c_sdlSubmitGPUCommandBuffer :: Ptr SDLGPUCommandBuffer -> IO CBool

foreign import ccall unsafe "SDL_SubmitGPUCommandBufferAndAcquireFence"
  c_sdlSubmitGPUCommandBufferAndAcquireFence :: Ptr SDLGPUCommandBuffer -> IO (Ptr SDLGPUFence)

foreign import ccall unsafe "SDL_WaitForGPUFences"
  c_sdlWaitForGPUFences :: Ptr SDLGPUDevice -> CBool -> Ptr (Ptr SDLGPUFence) -> Word32 -> IO CBool

foreign import ccall unsafe "SDL_ReleaseGPUFence"
  c_sdlReleaseGPUFence :: Ptr SDLGPUDevice -> Ptr SDLGPUFence -> IO ()

foreign import ccall unsafe "SDL_CancelGPUCommandBuffer"
  c_sdlCancelGPUCommandBuffer :: Ptr SDLGPUCommandBuffer -> IO CBool

fontPathRegular :: FilePath
fontPathRegular = "../../assets/Inter/static/Inter_24pt-Regular.ttf"

fontPathVar :: FilePath
fontPathVar = "../../assets/Inter/Inter-VariableFont_opsz,wght.ttf"

winW, winH :: Int
winW = 1024
winH = 1024

sdlInitVideo :: Word32
sdlInitVideo = #{const SDL_INIT_VIDEO}

sdlEventQuit :: Word32
sdlEventQuit = #{const SDL_EVENT_QUIT}

sdlEventBytes :: Int
sdlEventBytes = #{size SDL_Event}

sdlWindowResizable :: Word64
sdlWindowResizable = #{const SDL_WINDOW_RESIZABLE}

sdlGpuShaderFormatSpirv :: Word32
sdlGpuShaderFormatSpirv = #{const SDL_GPU_SHADERFORMAT_SPIRV}

sdlGpuShaderStageVertex :: CInt
sdlGpuShaderStageVertex = #{const SDL_GPU_SHADERSTAGE_VERTEX}

sdlGpuShaderStageFragment :: CInt
sdlGpuShaderStageFragment = #{const SDL_GPU_SHADERSTAGE_FRAGMENT}

sdlGpuTextureFormatRgba8Unorm :: CInt
sdlGpuTextureFormatRgba8Unorm = #{const SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM}

sdlGpuTextureType2d :: CInt
sdlGpuTextureType2d = #{const SDL_GPU_TEXTURETYPE_2D}

sdlGpuTextureUsageSampler :: Word32
sdlGpuTextureUsageSampler = #{const SDL_GPU_TEXTUREUSAGE_SAMPLER}

sdlGpuTransferUsageUpload :: CInt
sdlGpuTransferUsageUpload = #{const SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD}

sdlGpuTransferUsageDownload :: CInt
sdlGpuTransferUsageDownload = #{const SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD}

sdlGpuTextureFormatBgra8Unorm :: CInt
sdlGpuTextureFormatBgra8Unorm = #{const SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM}

sdlGpuPrimitiveTriangleList :: CInt
sdlGpuPrimitiveTriangleList = #{const SDL_GPU_PRIMITIVETYPE_TRIANGLELIST}

sdlGpuSampleCount1 :: CInt
sdlGpuSampleCount1 = #{const SDL_GPU_SAMPLECOUNT_1}

sdlGpuFillModeFill :: CInt
sdlGpuFillModeFill = #{const SDL_GPU_FILLMODE_FILL}

sdlGpuCullModeNone :: CInt
sdlGpuCullModeNone = #{const SDL_GPU_CULLMODE_NONE}

sdlGpuFrontFaceCcw :: CInt
sdlGpuFrontFaceCcw = #{const SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE}

sdlGpuFilterLinear :: CInt
sdlGpuFilterLinear = #{const SDL_GPU_FILTER_LINEAR}

sdlGpuSamplerMipmapNearest :: CInt
sdlGpuSamplerMipmapNearest = #{const SDL_GPU_SAMPLERMIPMAPMODE_NEAREST}

sdlGpuSamplerAddressClamp :: CInt
sdlGpuSamplerAddressClamp = #{const SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE}

sdlGpuCompareAlways :: CInt
sdlGpuCompareAlways = #{const SDL_GPU_COMPAREOP_ALWAYS}

sdlGpuBlendOpAdd :: CInt
sdlGpuBlendOpAdd = #{const SDL_GPU_BLENDOP_ADD}

sdlGpuBlendFactorSrcAlpha :: CInt
sdlGpuBlendFactorSrcAlpha = #{const SDL_GPU_BLENDFACTOR_SRC_ALPHA}

sdlGpuBlendFactorOneMinusSrcAlpha :: CInt
sdlGpuBlendFactorOneMinusSrcAlpha = #{const SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA}

sdlGpuBlendFactorOne :: CInt
sdlGpuBlendFactorOne = #{const SDL_GPU_BLENDFACTOR_ONE}

sdlGpuLoadOpClear :: CInt
sdlGpuLoadOpClear = #{const SDL_GPU_LOADOP_CLEAR}

sdlGpuStoreOpStore :: CInt
sdlGpuStoreOpStore = #{const SDL_GPU_STOREOP_STORE}

main :: IO ()
main = do
  sceneRaw <- lookupEnv "MASDIFF_SDL_SCENE"
  scenePreset <- either die pure (parseScenePreset sceneRaw)
  capturePathRaw <- lookupEnv "MASDIFF_SDL_CAPTURE"
  metaPathRaw <- lookupEnv "MASDIFF_SDL_META"
  noFit <- readBoolEnv "MASDIFF_SDL_NO_FIT"
  let capturePath =
        case capturePathRaw of
          Just p | not (null p) -> Just p
          _ -> Nothing
      metaPath =
        case metaPathRaw of
          Just p | not (null p) -> Just p
          _ -> Nothing
  scene <- buildScene scenePreset
  let vtx = vertexShader
  let frag = fragmentShader
  withSDL $
    withWindow "masdiff SDL3 (Spirdo + GPU MTSDF)" winW winH $ \win ->
      withGpuDevice $ \dev ->
        withClaimedWindow dev win $ do
          swapFmt <- c_sdlGetGPUSwapchainTextureFormat dev win
          withGpuShader dev vtx $ \vs ->
            withGpuShader dev frag $ \fs ->
              withGpuPipeline dev swapFmt vs fs $ \pipe ->
                withGpuSampler dev $ \smp ->
                  withLineTextures dev scene $ \lineTexs -> do
                    let draws0 = placeScene lineTexs
                    let draws =
                          if noFit
                            then draws0
                            else fitDrawsToWindow winW winH 24 draws0
                    case metaPath of
                      Nothing -> pure ()
                      Just path -> writeDrawMeta path draws
                    runLoop win dev pipe smp draws swapFmt 8.0 capturePath

buildScene :: ScenePreset -> IO [LineBuild]
buildScene scenePreset = do
  singleEm <- readPositiveDoubleEnvDefault "MASDIFF_SDL_SINGLE_EM" 640.0
  overlapSupport <- readBoolEnvDefault "MASDIFF_SDL_OVLP" False
  dim <- either (die . ("invalid dim: " <>)) pure (mkDim 192)
  pxr <- either (die . ("invalid px range: " <>)) pure (mkPxRange 6.0)
  atlasCfg <- either (die . ("invalid atlas cfg: " <>)) pure (mkAtlasCfg 2048 2048 12)
  let cfg =
        GenCfg
          { mode = Mtsdf,
            dim = dim,
            pxr = pxr,
            seed = 1,
            autoframe = True,
            ovlp = overlapSupport
          }
  let regular = FontFile {path = fontPathRegular}
  let varLight =
        VarFontFile
          { path = fontPathVar,
            axes =
              Map.fromList
                [ (AxisTag (T.pack "wght"), AxisVal 300),
                  (AxisTag (T.pack "opsz"), AxisVal 14)
                ]
          }
  let varBold =
        VarFontFile
          { path = fontPathVar,
            axes =
              Map.fromList
                [ (AxisTag (T.pack "wght"), AxisVal 900),
                  (AxisTag (T.pack "opsz"), AxisVal 32)
                ]
          }
  let specs =
        case scenePreset of
          SceneDefault ->
            [ LineSpec {src = regular, txt = "MASDIFF SDL3", em = 86},
              LineSpec {src = regular, txt = "AaRMPYgq 0123 ?!", em = 80},
              LineSpec {src = varLight, txt = "VAR 300/14: AaRMPY", em = 72},
              LineSpec {src = varBold, txt = "VAR 900/32: AaRMPY", em = 92}
            ]
          SceneSingleRegular ch ->
            [ LineSpec {src = regular, txt = [ch], em = singleEm} ]
          SceneSingleVarLight ch ->
            [ LineSpec {src = varLight, txt = [ch], em = singleEm} ]
          SceneSingleVarBold ch ->
            [ LineSpec {src = varBold, txt = [ch], em = singleEm} ]
  built <- traverse (buildLine cfg atlasCfg) specs
  either (die . ("scene build failed: " <>)) pure (sequence built)

buildLine :: GenCfg -> AtlasCfg -> LineSpec -> IO (Either String LineBuild)
buildLine cfg atlasCfg spec = do
  let atlasCodes = uniqueGlyphCodes spec.txt
  let glyphList = snd <$> atlasCodes
  case sequence glyphList of
    Left err -> pure (Left err)
    Right glyphs -> do
      atlasResult <- generateAtlasIO defaultRuntimeCfg 1 atlasCfg cfg spec.src glyphs
      case atlasResult of
        Left err -> pure (Left err)
        Right atlas -> pure (assemble atlas)
  where
    assemble :: Atlas -> Either String LineBuild
    assemble atlas =
      case atlas.pages of
        [] -> Left "atlas build produced no pages"
        [page0] -> do
          runMap <- runsByGlyph page0 atlas.entries
          atoms <- lineAtoms runMap
          layoutLine page0.img atoms
        _ -> Left "line atlas spilled to multiple pages; increase atlas size for this demo"

    runsByGlyph :: AtlasPage -> [AtlasEntry] -> Either String (IM.IntMap GlyphRun)
    runsByGlyph page0 entries =
      foldM step IM.empty entries
      where
        texW = fromIntegral page0.img.w
        texH = fromIntegral page0.img.h
        step m e = do
          let Metrics {adv = adv0, bounds = (xmin, ymin, xmax, _), scale = mScale, translate = mTranslate} = e.metrics
              gw = fromIntegral e.rect.w
              gh = fromIntegral e.rect.h
              fallbackSc = gw / max 1.0e-6 (xmax - xmin)
              sc0 = fromMaybe fallbackSc mScale
              (tx0, ty0) = fromMaybe (negate xmin, negate ymin) mTranslate
          if not (isFiniteD sc0) || sc0 <= 0
            then Left ("invalid glyph scale for codepoint " <> show (unGlyphCode e.glyph))
            else do
              let cropX0 = 0
                  cropY0 = 0
                  cW = e.rect.w
                  cH = e.rect.h
                  insetX = 0.0 :: Double
                  insetY = 0.0 :: Double
                  ox0 = (sc0 * tx0) - 0.5
                  oy0 = (gh - (sc0 * ty0)) - 0.5
                  desc0 = max 0 ((-ymin) * sc0)
                  uv0 =
                    UvRect
                      { u0 = (fromIntegral (e.rect.x + cropX0) + insetX) / texW,
                        v0 = (fromIntegral (e.rect.y + cropY0) + insetY) / texH,
                        u1 = (fromIntegral (e.rect.x + cropX0 + cW) - insetX) / texW,
                        v1 = (fromIntegral (e.rect.y + cropY0 + cH) - insetY) / texH
                      }
                  gr =
                    GlyphRun
                      { adv = adv0,
                        sc = sc0,
                        ox = ox0,
                        oy = oy0,
                        desc = desc0,
                        iw = cW,
                        ih = cH,
                        uv = uv0
                      }
              pure (IM.insert (unGlyphCode e.glyph) gr m)

    lineAtoms :: IM.IntMap GlyphRun -> Either String [LineAtom]
    lineAtoms runMap = do
      let runs = IM.elems runMap
      let sRef = spec.em
      let fallbackSpace = if null runs then 0.35 else median [r.adv | r <- runs]
      let spaceUnits = fallbackSpace
      let spacePx = max 0 (spaceUnits * sRef)
      traverse (toAtom sRef spacePx) spec.txt
      where
        toAtom :: Double -> Double -> Char -> Either String LineAtom
        toAtom _sRef spacePx ch
          | ch == ' ' = Right (AtomSpace spacePx)
        toAtom sRef _ ch = do
          code <- mkGlyphCode (ord ch)
          case IM.lookup (unGlyphCode code) runMap of
            Nothing -> Left ("missing atlas entry for codepoint " <> show (unGlyphCode code))
            Just run -> do
              let f = sRef / run.sc
              if not (isFiniteD f) || f <= 0
                then Left ("invalid glyph scale factor for codepoint " <> show (unGlyphCode code))
                else
                  Right
                    ( AtomGlyph
                        GlyphScaled
                          { ch = ch,
                            adv = run.adv * sRef,
                            ox = run.ox * f,
                            oy = run.oy * f,
                            desc = run.desc * f,
                            w = fromIntegral run.iw * f,
                            h = fromIntegral run.ih * f,
                            uv = run.uv,
                            spr = max 1.0 (unPxRange cfg.pxr)
                          }
                    )

uniqueGlyphCodes :: String -> [(Char, Either String GlyphCode)]
uniqueGlyphCodes txt =
  [ (ch, mkGlyphCode (ord ch))
    | ch <- nub txt,
      ch /= ' '
  ]

layoutLine :: ImgRGBA8 -> [LineAtom] -> Either String LineBuild
layoutLine atlasImg atoms =
  if null atoms
    then Left "line is empty"
    else do
      let glyphsOnly = [g | AtomGlyph g <- atoms]
      if null glyphsOnly
        then Left "line has no renderable glyphs"
        else do
          let descs = [g.desc | g <- glyphsOnly, isFiniteD g.desc]
              anchors = [g.oy - g.desc | g <- glyphsOnly, isFiniteD g.oy, isFiniteD g.desc]
              lineDesc = median (if null descs then [0] else descs)
              baseline = median (if null anchors then [maximum [g.oy | g <- glyphsOnly]] else anchors) + lineDesc
              (penEnd, placedMaybe) = mapAccumL (placeAtom baseline) 0 atoms
              placed = catMaybes placedMaybe
          case lineBounds placed of
            Nothing ->
              Right
                LineBuild
                  { atlas = atlasImg,
                    glyphs = [],
                    w = penEnd,
                    h = 1
                  }
            Just (minX, minY, maxX, maxY) -> do
              let padI = 1 :: Int
                  padD = fromIntegral padI
                  shifted =
                    [ PlacedGlyph
                        { ch = ch,
                          rect =
                            PxRect
                              { x = x - fromIntegral minX + padD,
                                y = y - fromIntegral minY + padD,
                                w = gW,
                                h = gH
                              },
                          uv = uv,
                          spr = spr
                        }
                      | (ch, x, y, gW, gH, uv, spr) <- placed
                    ]
              Right
                LineBuild
                  { atlas = atlasImg,
                    glyphs = shifted,
                    w = fromIntegral (maxX - minX + (2 * padI)),
                    h = fromIntegral (maxY - minY + (2 * padI))
                  }

placeAtom :: Double -> Double -> LineAtom -> (Double, Maybe (Char, Double, Double, Double, Double, UvRect, Double))
placeAtom baseline pen atom =
  case atom of
    AtomSpace dx -> (pen + dx, Nothing)
    AtomGlyph g ->
      let x = pen - g.ox
          y = baseline - g.oy
       in (pen + g.adv, Just (g.ch, x, y, g.w, g.h, g.uv, g.spr))

lineBounds :: [(Char, Double, Double, Double, Double, UvRect, Double)] -> Maybe (Int, Int, Int, Int)
lineBounds placed =
  case placed of
    [] -> Nothing
    (_, x0, y0, w0, h0, _, _) : rest ->
      Just (foldl' step initial rest)
      where
        initial = (floor x0, floor y0, ceiling (x0 + w0), ceiling (y0 + h0))
  where
    step (minX, minY, maxX, maxY) (_, x, y, w, h, _, _) =
      ( min minX (floor x),
        min minY (floor y),
        max maxX (ceiling (x + w)),
        max maxY (ceiling (y + h))
      )

placeScene :: [(Ptr SDLGPUTexture, LineBuild)] -> [DrawGlyph]
placeScene lineTexs = concat (snd (mapAccumL step startY lineTexs))
  where
    startX = 36.0
    startY = 48.0
    gapY = 18.0
    step y0 (tex, line) =
      let placed =
            [ DrawGlyph
                { ch = g.ch,
                  tex = tex,
                  rect =
                    PxRect
                      { x = startX + g.rect.x,
                        y = y0 + g.rect.y,
                        w = g.rect.w,
                        h = g.rect.h
                  },
                  uv = g.uv,
                  spr = g.spr
                }
              | g <- line.glyphs
            ]
          y1 = y0 + line.h + gapY
       in (y1, placed)

fitDrawsToWindow :: Int -> Int -> Double -> [DrawGlyph] -> [DrawGlyph]
fitDrawsToWindow outW outH margin draws =
  case drawBounds draws of
    Nothing -> draws
    Just (minX, minY, maxX, maxY) ->
      let extentW = max 1.0e-6 (maxX - minX)
          extentH = max 1.0e-6 (maxY - minY)
          availW = max 1.0 (fromIntegral outW - (2 * margin))
          availH = max 1.0 (fromIntegral outH - (2 * margin))
          scale = min 1.0 (min (availW / extentW) (availH / extentH))
          tx = ((fromIntegral outW - (extentW * scale)) * 0.5) - (minX * scale)
          ty = ((fromIntegral outH - (extentH * scale)) * 0.5) - (minY * scale)
       in fmap (scaleDraw scale tx ty) draws

drawBounds :: [DrawGlyph] -> Maybe (Double, Double, Double, Double)
drawBounds draws =
  case draws of
    [] -> Nothing
    d0 : rest ->
      Just (foldl' step start rest)
      where
        start =
          ( d0.rect.x,
            d0.rect.y,
            d0.rect.x + d0.rect.w,
            d0.rect.y + d0.rect.h
          )
        step (minX, minY, maxX, maxY) d =
          ( min minX d.rect.x,
            min minY d.rect.y,
            max maxX (d.rect.x + d.rect.w),
            max maxY (d.rect.y + d.rect.h)
          )

scaleDraw :: Double -> Double -> Double -> DrawGlyph -> DrawGlyph
scaleDraw k tx ty d =
  DrawGlyph
    { ch = d.ch,
      tex = d.tex,
      rect =
        PxRect
          { x = (d.rect.x * k) + tx,
            y = (d.rect.y * k) + ty,
            w = d.rect.w * k,
            h = d.rect.h * k
          },
      uv = d.uv,
      spr = d.spr
    }

withLineTextures :: Ptr SDLGPUDevice -> [LineBuild] -> ([(Ptr SDLGPUTexture, LineBuild)] -> IO a) -> IO a
withLineTextures dev lines0 action = bracket acquire release action
  where
    acquire = traverse mk lines0
    mk line = do
      tex <- uploadTexture dev line.atlas
      pure (tex, line)
    release pairs =
      forM_ pairs (\(tex, _) -> c_sdlReleaseGPUTexture dev tex)

uploadTexture :: Ptr SDLGPUDevice -> ImgRGBA8 -> IO (Ptr SDLGPUTexture)
uploadTexture dev img = do
  tex <- withTextureCreateInfo img.w img.h $ \ci -> requirePtr "SDL_CreateGPUTexture" (c_sdlCreateGPUTexture dev ci)
  tb <-
    withTransferBufferCreateInfo sdlGpuTransferUsageUpload (BS.length img.px) $ \ci ->
      requirePtr "SDL_CreateGPUTransferBuffer" (c_sdlCreateGPUTransferBuffer dev ci)
  mapped <- c_sdlMapGPUTransferBuffer dev tb 0
  when (mapped == nullPtr) (dieSdl "SDL_MapGPUTransferBuffer")
  BS.useAsCString img.px $ \src ->
    copyBytes mapped (castPtr src) (BS.length img.px)
  c_sdlUnmapGPUTransferBuffer dev tb
  cmd <- requirePtr "SDL_AcquireGPUCommandBuffer" (c_sdlAcquireGPUCommandBuffer dev)
  cp <- requirePtr "SDL_BeginGPUCopyPass" (c_sdlBeginGPUCopyPass cmd)
  withTextureTransferInfo tb img.w img.h $ \srcInfo ->
    withTextureRegion tex img.w img.h $ \dstRegion ->
      c_sdlUploadToGPUTexture cp srcInfo dstRegion 0
  c_sdlEndGPUCopyPass cp
  requireTrue "SDL_SubmitGPUCommandBuffer" (c_sdlSubmitGPUCommandBuffer cmd)
  c_sdlReleaseGPUTransferBuffer dev tb
  pure tex

runLoop ::
  Ptr SDLWindow ->
  Ptr SDLGPUDevice ->
  Ptr SDLGPUGraphicsPipeline ->
  Ptr SDLGPUSampler ->
  [DrawGlyph] ->
  CInt ->
  Double ->
  Maybe FilePath ->
  IO ()
runLoop win dev pipe smp draws swapFmt pxRange capturePath =
  allocaBytes sdlEventBytes (\eventBuf -> loop eventBuf False)
  where
    loop eventBuf captured = do
      quit <- pumpQuit eventBuf
      unless quit $ do
        capturedNow <-
          drawFrame
            win
            dev
            pipe
            smp
            draws
            swapFmt
            pxRange
            (if captured then Nothing else capturePath)
        if capturedNow
          then pure ()
          else do
            c_sdlDelay 16
            loop eventBuf (captured || capturedNow)

pumpQuit :: Ptr () -> IO Bool
pumpQuit eventBuf = do
  hasEvent <- c_sdlPollEvent eventBuf
  if not (asBool hasEvent)
    then pure False
    else do
      ty <- peek (castPtr eventBuf :: Ptr Word32)
      if ty == sdlEventQuit
        then pure True
        else pumpQuit eventBuf

drawFrame ::
  Ptr SDLWindow ->
  Ptr SDLGPUDevice ->
  Ptr SDLGPUGraphicsPipeline ->
  Ptr SDLGPUSampler ->
  [DrawGlyph] ->
  CInt ->
  Double ->
  Maybe FilePath ->
  IO Bool
drawFrame win dev pipe smp draws swapFmt _pxRange capturePath = do
  cmd <- requirePtr "SDL_AcquireGPUCommandBuffer" (c_sdlAcquireGPUCommandBuffer dev)
  alloca $ \swapTexPtr ->
    alloca $ \swPtr ->
      alloca $ \shPtr -> do
        ok <- c_sdlWaitAndAcquireGPUSwapchainTexture cmd win swapTexPtr swPtr shPtr
        if not (asBool ok)
          then requireTrue "SDL_CancelGPUCommandBuffer" (c_sdlCancelGPUCommandBuffer cmd) >> pure False
          else do
            swapTex <- peek swapTexPtr
            sw <- peek swPtr
            sh <- peek shPtr
            if swapTex == nullPtr
              then requireTrue "SDL_CancelGPUCommandBuffer" (c_sdlCancelGPUCommandBuffer cmd) >> pure False
              else do
                let bgCol = FColor 0.05 0.08 0.12 1.0
                withColorTargetInfo swapTex bgCol $ \ctInfo -> do
                  rp <- requirePtr "SDL_BeginGPURenderPass" (c_sdlBeginGPURenderPass cmd ctInfo 1 nullPtr)
                  c_sdlBindGPUGraphicsPipeline rp pipe
                  drawGlyphs cmd rp smp (fromIntegral sw) (fromIntegral sh) draws
                  c_sdlEndGPURenderPass rp
                case capturePath of
                  Nothing -> do
                    requireTrue "SDL_SubmitGPUCommandBuffer" (c_sdlSubmitGPUCommandBuffer cmd)
                    pure False
                  Just outPath -> do
                    bytes <- downloadSwapTexture dev cmd swapTex (fromIntegral sw) (fromIntegral sh)
                    img0 <-
                      case mkImgRGBA8 (fromIntegral sw) (fromIntegral sh) bytes of
                        Left err -> die ("capture image error: " <> err)
                        Right x -> pure x
                    let img = if swapFmt == sdlGpuTextureFormatBgra8Unorm then rgbaFromBgra img0 else img0
                    let outDir = takeDirectory outPath
                    unless (null outDir || outDir == ".") (createDirectoryIfMissing True outDir)
                    writePngRGBA8File outPath img
                    putStrLn ("Captured GPU frame: " <> outPath)
                    pure True

downloadSwapTexture ::
  Ptr SDLGPUDevice ->
  Ptr SDLGPUCommandBuffer ->
  Ptr SDLGPUTexture ->
  Int ->
  Int ->
  IO BS.ByteString
downloadSwapTexture dev cmd tex w h = do
  let byteCount = w * h * 4
  tb <-
    withTransferBufferCreateInfo sdlGpuTransferUsageDownload byteCount $ \ci ->
      requirePtr "SDL_CreateGPUTransferBuffer(download)" (c_sdlCreateGPUTransferBuffer dev ci)
  cp <- requirePtr "SDL_BeginGPUCopyPass(download)" (c_sdlBeginGPUCopyPass cmd)
  withTextureRegion tex w h $ \srcRegion ->
    withTextureTransferInfo tb w h $ \dstInfo ->
      c_sdlDownloadFromGPUTexture cp srcRegion dstInfo
  c_sdlEndGPUCopyPass cp
  fence <- requirePtr "SDL_SubmitGPUCommandBufferAndAcquireFence" (c_sdlSubmitGPUCommandBufferAndAcquireFence cmd)
  with fence $ \fencesPtr ->
    requireTrue "SDL_WaitForGPUFences" (c_sdlWaitForGPUFences dev 1 fencesPtr 1)
  mapped <- c_sdlMapGPUTransferBuffer dev tb 0
  when (mapped == nullPtr) (dieSdl "SDL_MapGPUTransferBuffer(download)")
  bytes <- BS.packCStringLen (castPtr mapped, byteCount)
  c_sdlUnmapGPUTransferBuffer dev tb
  c_sdlReleaseGPUFence dev fence
  c_sdlReleaseGPUTransferBuffer dev tb
  pure bytes

rgbaFromBgra :: ImgRGBA8 -> ImgRGBA8
rgbaFromBgra img =
  img {px = BS.pack (go (BS.unpack img.px))}
  where
    go (b : g : r : a : rest) = r : g : b : a : go rest
    go _ = []

drawGlyphs ::
  Ptr SDLGPUCommandBuffer ->
  Ptr SDLGPURenderPass ->
  Ptr SDLGPUSampler ->
  Double ->
  Double ->
  [DrawGlyph] ->
  IO ()
drawGlyphs cmd rp smp sw sh = go nullPtr
  where
    go _ [] = pure ()
    go currentTex (d : rest) = do
      nextTex <-
        if d.tex == currentTex
          then pure currentTex
          else do
            with (TextureSamplerBinding d.tex smp) $ \binding ->
              c_sdlBindGPUFragmentSamplers rp 0 (castPtr binding) 1
            pure d.tex
      let vsu =
            VsUniform
              { rx = realToFrac d.rect.x,
                ry = realToFrac d.rect.y,
                rw = realToFrac d.rect.w,
                rh = realToFrac d.rect.h,
                u0 = realToFrac d.uv.u0,
                v0 = realToFrac d.uv.v0,
                u1 = realToFrac d.uv.u1,
                v1 = realToFrac d.uv.v1,
                sw = realToFrac sw,
                sh = realToFrac sh,
                p0 = realToFrac d.spr,
                p1 = 0
              }
      with vsu $ \vsPtr ->
        c_sdlPushGPUVertexUniformData
          cmd
          0
          (castPtr vsPtr)
          (fromIntegral (sizeOf (undefined :: VsUniform)))
      c_sdlDrawGPUPrimitives rp 6 1 0 0
      go nextTex rest

withSDL :: IO a -> IO a
withSDL action = do
  ok <- c_sdlInit sdlInitVideo
  unless (asBool ok) (dieSdl "SDL_Init")
  finally action c_sdlQuit

withWindow :: String -> Int -> Int -> (Ptr SDLWindow -> IO a) -> IO a
withWindow title w h action =
  withCString title $ \cTitle ->
    bracket
      (requirePtr "SDL_CreateWindow" (c_sdlCreateWindow cTitle (fromIntegral w) (fromIntegral h) sdlWindowResizable))
      c_sdlDestroyWindow
      action

withGpuDevice :: (Ptr SDLGPUDevice -> IO a) -> IO a
withGpuDevice action =
  bracket
    (requirePtr "SDL_CreateGPUDevice" (c_sdlCreateGPUDevice sdlGpuShaderFormatSpirv 0 nullPtr))
    c_sdlDestroyGPUDevice
    action

withClaimedWindow :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO a -> IO a
withClaimedWindow dev win action = do
  requireTrue "SDL_ClaimWindowForGPUDevice" (c_sdlClaimWindowForGPUDevice dev win)
  finally action (c_sdlReleaseWindowFromGPUDevice dev win)

withGpuShader :: Ptr SDLGPUDevice -> GpuShader -> (Ptr SDLGPUShader -> IO a) -> IO a
withGpuShader dev bundle action =
  bracket create destroy action
  where
    create = do
      let (numSamplers, numStorageTextures, numStorageBuffers, numUniforms) = resourceCounts bundle
      stage <- stageToSdl bundle
      withShaderCreateInfo
        bundle.spv
        stage
        numSamplers
        numStorageTextures
        numStorageBuffers
        numUniforms
        (\ci -> requirePtr "SDL_CreateGPUShader" (c_sdlCreateGPUShader dev ci))
    destroy shader = c_sdlReleaseGPUShader dev shader

withGpuPipeline ::
  Ptr SDLGPUDevice ->
  CInt ->
  Ptr SDLGPUShader ->
  Ptr SDLGPUShader ->
  (Ptr SDLGPUGraphicsPipeline -> IO a) ->
  IO a
withGpuPipeline dev swapFmt vs fs action =
  bracket create destroy action
  where
    create =
      withPipelineCreateInfo swapFmt vs fs $ \ci ->
        requirePtr "SDL_CreateGPUGraphicsPipeline" (c_sdlCreateGPUGraphicsPipeline dev ci)
    destroy pipe = c_sdlReleaseGPUGraphicsPipeline dev pipe

withGpuSampler :: Ptr SDLGPUDevice -> (Ptr SDLGPUSampler -> IO a) -> IO a
withGpuSampler dev action =
  bracket create destroy action
  where
    create =
      withSamplerCreateInfo $ \ci ->
        requirePtr "SDL_CreateGPUSampler" (c_sdlCreateGPUSampler dev ci)
    destroy smp = c_sdlReleaseGPUSampler dev smp

withZeroStruct :: Int -> (Ptr () -> IO a) -> IO a
withZeroStruct n action =
  allocaBytes n $ \ptr -> do
    fillBytes ptr 0 n
    action (castPtr ptr)

withShaderCreateInfo ::
  BS.ByteString ->
  CInt ->
  Word32 ->
  Word32 ->
  Word32 ->
  Word32 ->
  (Ptr () -> IO a) ->
  IO a
withShaderCreateInfo code stage numSamplers numStorageTextures numStorageBuffers numUniforms action =
  BS.useAsCStringLen code $ \(spvPtr, spvLen) ->
    withCString "main" $ \entry ->
      withZeroStruct #{size SDL_GPUShaderCreateInfo} $ \ptr -> do
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, code_size} (fromIntegral spvLen :: CSize)
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, code} (castPtr spvPtr :: Ptr Word8)
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, entrypoint} entry
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, format} sdlGpuShaderFormatSpirv
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, stage} stage
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_samplers} numSamplers
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_storage_textures} numStorageTextures
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_storage_buffers} numStorageBuffers
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_uniform_buffers} numUniforms
        pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, props} (0 :: Word32)
        action ptr

withPipelineCreateInfo ::
  CInt ->
  Ptr SDLGPUShader ->
  Ptr SDLGPUShader ->
  (Ptr () -> IO a) ->
  IO a
withPipelineCreateInfo swapFmt vs fs action =
  withZeroStruct #{size SDL_GPUColorTargetDescription} $ \ctDesc -> do
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, format} swapFmt
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.src_color_blendfactor} sdlGpuBlendFactorSrcAlpha
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.dst_color_blendfactor} sdlGpuBlendFactorOneMinusSrcAlpha
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.color_blend_op} sdlGpuBlendOpAdd
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.src_alpha_blendfactor} sdlGpuBlendFactorOne
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.dst_alpha_blendfactor} sdlGpuBlendFactorOneMinusSrcAlpha
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.alpha_blend_op} sdlGpuBlendOpAdd
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.enable_blend} (1 :: CBool)
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.enable_color_write_mask} (0 :: CBool)
    withZeroStruct #{size SDL_GPUGraphicsPipelineCreateInfo} $ \ptr -> do
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, vertex_shader} vs
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, fragment_shader} fs
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, primitive_type} sdlGpuPrimitiveTriangleList
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, rasterizer_state.fill_mode} sdlGpuFillModeFill
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, rasterizer_state.cull_mode} sdlGpuCullModeNone
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, rasterizer_state.front_face} sdlGpuFrontFaceCcw
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, multisample_state.sample_count} sdlGpuSampleCount1
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, target_info.color_target_descriptions} ctDesc
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, target_info.num_color_targets} (1 :: Word32)
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, target_info.has_depth_stencil_target} (0 :: CBool)
      pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, props} (0 :: Word32)
      action ptr

withSamplerCreateInfo :: (Ptr () -> IO a) -> IO a
withSamplerCreateInfo action =
  withZeroStruct #{size SDL_GPUSamplerCreateInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, min_filter} sdlGpuFilterLinear
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mag_filter} sdlGpuFilterLinear
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mipmap_mode} sdlGpuSamplerMipmapNearest
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_u} sdlGpuSamplerAddressClamp
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_v} sdlGpuSamplerAddressClamp
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_w} sdlGpuSamplerAddressClamp
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, compare_op} sdlGpuCompareAlways
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, min_lod} (0 :: CFloat)
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, max_lod} (0 :: CFloat)
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, enable_anisotropy} (0 :: CBool)
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, enable_compare} (0 :: CBool)
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, props} (0 :: Word32)
    action ptr

withTextureCreateInfo :: Int -> Int -> (Ptr () -> IO a) -> IO a
withTextureCreateInfo w h action =
  withZeroStruct #{size SDL_GPUTextureCreateInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, type} sdlGpuTextureType2d
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, format} sdlGpuTextureFormatRgba8Unorm
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, usage} sdlGpuTextureUsageSampler
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, width} (fromIntegral w :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, height} (fromIntegral h :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, layer_count_or_depth} (1 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, num_levels} (1 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, sample_count} sdlGpuSampleCount1
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, props} (0 :: Word32)
    action ptr

withTransferBufferCreateInfo :: CInt -> Int -> (Ptr () -> IO a) -> IO a
withTransferBufferCreateInfo usage bytes action =
  withZeroStruct #{size SDL_GPUTransferBufferCreateInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUTransferBufferCreateInfo, usage} usage
    pokeByteOff ptr #{offset SDL_GPUTransferBufferCreateInfo, size} (fromIntegral bytes :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTransferBufferCreateInfo, props} (0 :: Word32)
    action ptr

withTextureTransferInfo :: Ptr SDLGPUTransferBuffer -> Int -> Int -> (Ptr () -> IO a) -> IO a
withTextureTransferInfo tb w h action =
  withZeroStruct #{size SDL_GPUTextureTransferInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, transfer_buffer} tb
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, offset} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, pixels_per_row} (fromIntegral w :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, rows_per_layer} (fromIntegral h :: Word32)
    action ptr

withTextureRegion :: Ptr SDLGPUTexture -> Int -> Int -> (Ptr () -> IO a) -> IO a
withTextureRegion tex w h action =
  withZeroStruct #{size SDL_GPUTextureRegion} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, texture} tex
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, mip_level} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, layer} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, x} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, y} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, z} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, w} (fromIntegral w :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, h} (fromIntegral h :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureRegion, d} (1 :: Word32)
    action ptr

withColorTargetInfo :: Ptr SDLGPUTexture -> FColor -> (Ptr () -> IO a) -> IO a
withColorTargetInfo tex clr action =
  withZeroStruct #{size SDL_GPUColorTargetInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, texture} tex
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, mip_level} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, layer_or_depth_plane} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, clear_color} clr
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, load_op} sdlGpuLoadOpClear
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, store_op} sdlGpuStoreOpStore
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, cycle} (0 :: CBool)
    action ptr

resourceCounts :: GpuShader -> (Word32, Word32, Word32, Word32)
resourceCounts bundle =
  ( slotCount sampleBindings isSampledTextureKind,
    slotCount sampleBindings isStorageTextureKind,
    slotCount sampleBindings isStorageBufferKind,
    slotCount uniformBindings isUniformKind
  )
  where
    stage = bundle.stg
    groupSample :: Word32
    groupSample = case stage of
      ShaderStageVertex -> 0
      ShaderStageFragment -> 2
      ShaderStageCompute -> 0
    groupUniform :: Word32
    groupUniform = case stage of
      ShaderStageVertex -> 1
      ShaderStageFragment -> 3
      ShaderStageCompute -> 2
    sampleBindings = [b | b <- bundle.binds, b.biGroup == groupSample]
    uniformBindings = [b | b <- bundle.binds, b.biGroup == groupUniform]
    slotCount bs predFn =
      if null indices
        then 0
        else fromIntegral (maximum indices + 1)
      where
        indices = [fromIntegral b.biBinding :: Int | b <- bs, predFn b.biKind]

isUniformKind :: BindingKind -> Bool
isUniformKind kind =
  case kind of
    BUniform -> True
    _ -> False

isStorageBufferKind :: BindingKind -> Bool
isStorageBufferKind kind =
  case kind of
    BStorageRead -> True
    BStorageReadWrite -> True
    _ -> False

isStorageTextureKind :: BindingKind -> Bool
isStorageTextureKind kind =
  case kind of
    BStorageTexture1D -> True
    BStorageTexture2D -> True
    BStorageTexture2DArray -> True
    BStorageTexture3D -> True
    _ -> False

isSampledTextureKind :: BindingKind -> Bool
isSampledTextureKind kind =
  case kind of
    BTexture1D -> True
    BTexture1DArray -> True
    BTexture2D -> True
    BTexture2DArray -> True
    BTexture3D -> True
    BTextureCube -> True
    BTextureCubeArray -> True
    BTextureMultisampled2D -> True
    BTextureDepth2D -> True
    BTextureDepth2DArray -> True
    BTextureDepthCube -> True
    BTextureDepthCubeArray -> True
    BTextureDepthMultisampled2D -> True
    _ -> False

stageToSdl :: GpuShader -> IO CInt
stageToSdl bundle =
  case bundle.stg of
    ShaderStageVertex -> pure sdlGpuShaderStageVertex
    ShaderStageFragment -> pure sdlGpuShaderStageFragment
    ShaderStageCompute -> die "compute shaders are not used in this demo"

asBool :: CBool -> Bool
asBool x = x /= 0

requireTrue :: String -> IO CBool -> IO ()
requireTrue label action = do
  ok <- action
  unless (asBool ok) (dieSdl label)

requirePtr :: String -> IO (Ptr a) -> IO (Ptr a)
requirePtr label action = do
  ptr <- action
  if ptr == nullPtr
    then dieSdl label
    else pure ptr

dieSdl :: String -> IO a
dieSdl label = do
  err <- sdlError
  die (label <> " failed: " <> err)

sdlError :: IO String
sdlError = do
  cstr <- c_sdlGetError
  if cstr == nullPtr
    then pure "unknown SDL error"
    else peekCString cstr

median :: [Double] -> Double
median xs =
  case sort xs of
    [] -> 0
    ys ->
      let n = length ys
          k = n `quot` 2
       in if odd n
            then ys !! k
            else 0.5 * ((ys !! (k - 1)) + (ys !! k))

isFiniteD :: Double -> Bool
isFiniteD x = not (isNaN x || isInfinite x)

parseScenePreset :: Maybe String -> Either String ScenePreset
parseScenePreset raw =
  case fmap (fmap toLower) raw of
    Nothing -> Right SceneDefault
    Just [] -> Right SceneDefault
    Just "default" -> Right SceneDefault
    Just s ->
      case () of
        _
          | Just rest <- stripPrefix "single-var-bold-" s ->
              parseSingle SceneSingleVarBold s rest
          | Just rest <- stripPrefix "single-var-light-" s ->
              parseSingle SceneSingleVarLight s rest
          | Just rest <- stripPrefix "single-" s ->
              parseSingle SceneSingleRegular s rest
          | otherwise ->
              badScene s
  where
    parseSingle ctor whole rest =
      case rest of
        [ch] | ch `elem` ['a', 'm', 'p', 'r', 'y'] -> Right (ctor (toUpper ch))
        _ -> badScene whole
    badScene other =
      Left
        ( "Unknown MASDIFF_SDL_SCENE value: "
            <> other
            <> " (expected: default, single-a|m|p|r|y, single-var-light-a|m|p|r|y, single-var-bold-a|m|p|r|y)"
        )

readBoolEnv :: String -> IO Bool
readBoolEnv key = do
  readBoolEnvDefault key False

readBoolEnvDefault :: String -> Bool -> IO Bool
readBoolEnvDefault key defVal = do
  raw <- lookupEnv key
  pure $
    case fmap (fmap toLower) raw of
      Nothing -> defVal
      Just "1" -> True
      Just "true" -> True
      Just "yes" -> True
      Just "on" -> True
      Just "0" -> False
      Just "false" -> False
      Just "no" -> False
      Just "off" -> False
      _ -> defVal

readPositiveDoubleEnvDefault :: String -> Double -> IO Double
readPositiveDoubleEnvDefault key defVal = do
  raw <- lookupEnv key
  case raw of
    Nothing -> pure defVal
    Just "" -> pure defVal
    Just txt ->
      case reads txt of
        [(x, "")] | isFiniteD x && x > 0 -> pure x
        _ -> die ("invalid " <> key <> " value: " <> txt <> " (expected finite > 0)")

writeDrawMeta :: FilePath -> [DrawGlyph] -> IO ()
writeDrawMeta path draws = do
  let outDir = takeDirectory path
  unless (null outDir || outDir == ".") (createDirectoryIfMissing True outDir)
  writeFile path (renderMeta draws)
  where
    renderMeta ds =
      unlines
        ( "idx\tch\tx\ty\tw\th"
            : [ show i
                  <> "\t"
                  <> [d.ch]
                  <> "\t"
                  <> show d.rect.x
                  <> "\t"
                  <> show d.rect.y
                  <> "\t"
                  <> show d.rect.w
                  <> "\t"
                  <> show d.rect.h
              | (i, d) <- zip ([0 ..] :: [Int]) ds
              ]
        )

mkGpuShader :: Shader mode iface -> GpuShader
mkGpuShader shader =
  GpuShader
    { spv = shaderSpirv shader,
      stg = shaderStageCached shader,
      binds = (shaderPlan shader).bpBindings
    }

vertexShader :: GpuShader
vertexShader =
  mkGpuShader
    [weslShader|
struct VsU {
  rect: vec4<f32>,
  uv: vec4<f32>,
  screen: vec2<f32>,
  _pad: vec2<f32>,
};

struct VsOut {
  @builtin(position) pos: vec4<f32>,
  @location(0) uv: vec2<f32>,
  @location(1) spr: f32,
  @location(2) uvLo: vec2<f32>,
  @location(3) uvHi: vec2<f32>,
};

@group(1) @binding(0) var<uniform> u: VsU;

fn corner(vid: u32) -> vec2<f32> {
  if (vid == 0u) {
    return vec2<f32>(0.0, 0.0);
  }
  if (vid == 1u || vid == 3u) {
    return vec2<f32>(1.0, 0.0);
  }
  if (vid == 2u || vid == 5u) {
    return vec2<f32>(0.0, 1.0);
  }
  return vec2<f32>(1.0, 1.0);
}

@vertex
fn main(@builtin(vertex_index) vid: u32) -> VsOut {
  let c = corner(vid);
  let px = vec2<f32>(
    u.rect.x + u.rect.z * c.x,
    u.rect.y + u.rect.w * c.y
  );
  let ndc = vec2<f32>(
    (px.x / u.screen.x) * 2.0 - 1.0,
    1.0 - (px.y / u.screen.y) * 2.0
  );
  var out: VsOut;
  out.pos = vec4<f32>(ndc, 0.0, 1.0);
  out.uv = vec2<f32>(
    mix(u.uv.x, u.uv.z, c.x),
    mix(u.uv.y, u.uv.w, c.y)
  );
  out.spr = max(1.0, u._pad.x);
  out.uvLo = vec2<f32>(min(u.uv.x, u.uv.z), min(u.uv.y, u.uv.w));
  out.uvHi = vec2<f32>(max(u.uv.x, u.uv.z), max(u.uv.y, u.uv.w));
  return out;
}
|]

fragmentShader :: GpuShader
fragmentShader =
  mkGpuShader
    [weslShader|
@group(2) @binding(0) var tx: texture_2d<f32>;
@group(2) @binding(1) var smp: sampler;

fn median3(a: f32, b: f32, c: f32) -> f32 {
  return max(min(a, b), min(max(a, b), c));
}

fn screenPxRange(uv: vec2<f32>, pxRange: f32) -> f32 {
  let dims = max(vec2<f32>(textureDimensions(tx)), vec2<f32>(1.0, 1.0));
  let unitRange = vec2<f32>(pxRange, pxRange) / dims;
  let screenTexSize = vec2<f32>(1.0, 1.0) / max(fwidth(uv), vec2<f32>(1.0e-6, 1.0e-6));
  return max(0.5 * dot(unitRange, screenTexSize), 1.0);
}

fn coverageFromMtsdf(s: vec4<f32>, range: f32) -> f32 {
  let msdfSd = median3(s.r, s.g, s.b) - 0.5;
  let sdfSd = s.a - 0.5;
  let msdfCov = clamp((range * msdfSd) + 0.5, 0.0, 1.0);
  let sdfCov = clamp((range * sdfSd) + 0.5, 0.0, 1.0);
  if (abs(msdfSd - sdfSd) > 0.0) {
    return max(msdfCov, sdfCov);
  }
  return msdfCov;
}

fn sampleCoverage(uv: vec2<f32>, uvLo: vec2<f32>, uvHi: vec2<f32>, halfTexel: vec2<f32>, range: f32) -> f32 {
  let uvSafe = clamp(uv, uvLo + halfTexel, uvHi - halfTexel);
  let s = textureSample(tx, smp, uvSafe);
  return coverageFromMtsdf(s, range);
}

fn healCoverage(c: f32, l: f32, r: f32, u: f32, d: f32, lu: f32, ru: f32, ld: f32, rd: f32) -> f32 {
  let nMin = min(min(min(l, r), min(u, d)), min(min(lu, ru), min(ld, rd)));
  let nMax = max(max(max(l, r), max(u, d)), max(max(lu, ru), max(ld, rd)));
  let orthMin = min(min(l, r), min(u, d));
  let orthAvg = 0.25 * (l + r + u + d);
  let diagMin = min(min(lu, ru), min(ld, rd));
  let orthHighCount =
    select(0u, 1u, l > 0.82)
    + select(0u, 1u, r > 0.82)
    + select(0u, 1u, u > 0.82)
    + select(0u, 1u, d > 0.82);
  let diagHighCount =
    select(0u, 1u, lu > 0.78)
    + select(0u, 1u, ru > 0.78)
    + select(0u, 1u, ld > 0.78)
    + select(0u, 1u, rd > 0.78);
  let orthLowCount =
    select(0u, 1u, l < 0.70)
    + select(0u, 1u, r < 0.70)
    + select(0u, 1u, u < 0.70)
    + select(0u, 1u, d < 0.70);
  let highCount =
    select(0u, 1u, l > 0.80)
    + select(0u, 1u, r > 0.80)
    + select(0u, 1u, u > 0.80)
    + select(0u, 1u, d > 0.80)
    + select(0u, 1u, lu > 0.80)
    + select(0u, 1u, ru > 0.80)
    + select(0u, 1u, ld > 0.80)
    + select(0u, 1u, rd > 0.80);
  let stable = (nMax - nMin) < 0.30;
  let isolatedHole = c < 0.60 && orthMin > 0.84 && diagMin > 0.72;
  let hardSpeck = c < 0.95 && highCount >= 7u && orthMin > 0.82 && (nMax - c) > 0.05;
  let microPit = c < 0.90 && orthMin > 0.82 && highCount >= 7u && (orthAvg - c) > 0.035;
  let pinhole =
    c < 0.72
    && stable
    && (nMax - c) > 0.14
    && highCount >= 5u
    && orthHighCount >= 2u
    && diagHighCount >= 1u
    && orthLowCount <= 1u;
  let cuspPair =
    (l > 0.82 && r > 0.82)
    || (l > 0.82 && u > 0.82)
    || (l > 0.82 && d > 0.82)
    || (r > 0.82 && u > 0.82)
    || (r > 0.82 && d > 0.82)
    || (u > 0.82 && d > 0.82);
  let cusp =
    c < 0.68
    && stable
    && (nMax - c) > 0.12
    && highCount >= 5u
    && orthHighCount >= 2u
    && diagHighCount >= 1u
    && orthLowCount <= 1u
    && cuspPair;
  let microSpeck =
    c < 0.985
    && orthMin > 0.965
    && diagMin > 0.94
    && (orthAvg - c) > 0.01;
  if (hardSpeck) {
    return nMax;
  }
  if (isolatedHole || microPit || pinhole || cusp || microSpeck) {
    return max(c, orthAvg);
  }
  return c;
}

@fragment
fn main(
  @location(0) uv: vec2<f32>,
  @location(1) sprIn: f32,
  @location(2) uvLo: vec2<f32>,
  @location(3) uvHi: vec2<f32>
) -> @location(0) vec4<f32> {
  let pxRange = max(1.0, sprIn);
  let dims = max(vec2<f32>(textureDimensions(tx)), vec2<f32>(1.0, 1.0));
  let texel = vec2<f32>(1.0, 1.0) / dims;
  let halfTexel = 0.5 * texel;
  let uvCenter = clamp(uv, uvLo + halfTexel, uvHi - halfTexel);
  let range = screenPxRange(uvCenter, pxRange);
  let du = 0.25 * dpdx(uvCenter);
  let dv = 0.25 * dpdy(uvCenter);
  let c0 = sampleCoverage(uvCenter - du - dv, uvLo, uvHi, halfTexel, range);
  let c1 = sampleCoverage(uvCenter + du - dv, uvLo, uvHi, halfTexel, range);
  let c2 = sampleCoverage(uvCenter - du + dv, uvLo, uvHi, halfTexel, range);
  let c3 = sampleCoverage(uvCenter + du + dv, uvLo, uvHi, halfTexel, range);
  let baseOpacity = clamp(0.25 * (c0 + c1 + c2 + c3), 0.0, 1.0);
  let pxU = dpdx(uvCenter);
  let pxV = dpdy(uvCenter);
  let l = sampleCoverage(uvCenter - pxU, uvLo, uvHi, halfTexel, range);
  let r = sampleCoverage(uvCenter + pxU, uvLo, uvHi, halfTexel, range);
  let u = sampleCoverage(uvCenter - pxV, uvLo, uvHi, halfTexel, range);
  let d = sampleCoverage(uvCenter + pxV, uvLo, uvHi, halfTexel, range);
  let lu = sampleCoverage(uvCenter - pxU - pxV, uvLo, uvHi, halfTexel, range);
  let ru = sampleCoverage(uvCenter + pxU - pxV, uvLo, uvHi, halfTexel, range);
  let ld = sampleCoverage(uvCenter - pxU + pxV, uvLo, uvHi, halfTexel, range);
  let rd = sampleCoverage(uvCenter + pxU + pxV, uvLo, uvHi, halfTexel, range);
  let opacity = clamp(healCoverage(baseOpacity, l, r, u, d, lu, ru, ld, rd), 0.0, 1.0);
  let fg = vec3<f32>(0.92, 0.94, 0.98);
  return vec4<f32>(fg, opacity);
}
|]
