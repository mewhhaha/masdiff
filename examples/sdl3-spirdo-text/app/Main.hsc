{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception (SomeException, bracket, displayException, finally, try)
import Control.Monad (foldM, forM_, unless, when)
import Data.Char (ord, toLower, toUpper)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (find, mapAccumL, nub, sort, stripPrefix)
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
    generateAtlasWithRasterIO,
    mkAtlasCfg,
  )
import MSDF.Encode (writePngRGBA8File)
import MSDF.Native
  ( PreparedLineSeg (..),
    PreparedGlyph,
    RasterPreparedIO,
    metricsPrepared,
    prepareGlyphBatchNativeIO,
    preparedLineSegs,
    requiresNonZeroWinding,
    rasterPreparedCpu,
  )
import MSDF.Types
  ( AxisTag (..),
    AxisVal (..),
    FontSrc (..),
    GenCfg (..),
    GenErr (..),
    GenOut (..),
    GlyphCode,
    ImgRGBA8 (..),
    Metrics (..),
    Mode (..),
    mkDim,
    mkGlyphCode,
    mkImgRGBA8,
    mkPxRange,
    unDim,
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
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>

data SDLWindow
data SDLGPUDevice
data SDLGPUShader
data SDLGPUGraphicsPipeline
data SDLGPUSampler
data SDLGPUTexture
data SDLGPUBuffer
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

data GpuViewport = GpuViewport
  { x :: !CFloat,
    y :: !CFloat,
    w :: !CFloat,
    h :: !CFloat,
    minDepth :: !CFloat,
    maxDepth :: !CFloat
  }

instance Storable GpuViewport where
  sizeOf _ = #{size SDL_GPUViewport}
  alignment _ = #{alignment SDL_GPUViewport}
  peek ptr =
    GpuViewport
      <$> peekByteOff ptr #{offset SDL_GPUViewport, x}
      <*> peekByteOff ptr #{offset SDL_GPUViewport, y}
      <*> peekByteOff ptr #{offset SDL_GPUViewport, w}
      <*> peekByteOff ptr #{offset SDL_GPUViewport, h}
      <*> peekByteOff ptr #{offset SDL_GPUViewport, min_depth}
      <*> peekByteOff ptr #{offset SDL_GPUViewport, max_depth}
  poke ptr v = do
    pokeByteOff ptr #{offset SDL_GPUViewport, x} v.x
    pokeByteOff ptr #{offset SDL_GPUViewport, y} v.y
    pokeByteOff ptr #{offset SDL_GPUViewport, w} v.w
    pokeByteOff ptr #{offset SDL_GPUViewport, h} v.h
    pokeByteOff ptr #{offset SDL_GPUViewport, min_depth} v.minDepth
    pokeByteOff ptr #{offset SDL_GPUViewport, max_depth} v.maxDepth

data SdlRect = SdlRect
  { x :: !CInt,
    y :: !CInt,
    w :: !CInt,
    h :: !CInt
  }

instance Storable SdlRect where
  sizeOf _ = #{size SDL_Rect}
  alignment _ = #{alignment SDL_Rect}
  peek ptr =
    SdlRect
      <$> peekByteOff ptr #{offset SDL_Rect, x}
      <*> peekByteOff ptr #{offset SDL_Rect, y}
      <*> peekByteOff ptr #{offset SDL_Rect, w}
      <*> peekByteOff ptr #{offset SDL_Rect, h}
  poke ptr r = do
    pokeByteOff ptr #{offset SDL_Rect, x} r.x
    pokeByteOff ptr #{offset SDL_Rect, y} r.y
    pokeByteOff ptr #{offset SDL_Rect, w} r.w
    pokeByteOff ptr #{offset SDL_Rect, h} r.h

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
  { atlasImg :: !(Maybe ImgRGBA8),
    atlasTex :: !(Maybe (Ptr SDLGPUTexture)),
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

data RasterMode
  = RasterModeGpu
  | RasterModeCpu
  deriving stock (Eq, Show)

data GenFragShaderMode
  = GenFragFlat
  | GenFragStruct
  | GenFragSanity
  deriving stock (Eq, Show)

data GpuRasterStats = GpuRasterStats
  { ok :: !Int,
    cacheHit :: !Int,
    badFrame :: !Int,
    segOverflow :: !Int,
    gpuErr :: !Int
  }
  deriving stock (Eq, Show)

data GpuRasterLimits = GpuRasterLimits
  { maxSegs :: !Int,
    maxPushBytes :: !Int
  }
  deriving stock (Eq, Show)

data GpuRasterCacheKey = GpuRasterCacheKey
  { dim :: !Int,
    pxr :: !Double,
    sc :: !Double,
    tx :: !Double,
    ty :: !Double,
    selectorSegs :: ![PreparedLineSeg],
    windingSegs :: ![PreparedLineSeg]
  }
  deriving stock (Eq, Show)

data GpuBatchCtx = GpuBatchCtx
  { debugLog :: !Bool,
    strictMode :: !Bool,
    limits :: !GpuRasterLimits,
    statsRef :: !(IORef GpuRasterStats),
    cacheRef :: !(IORef [(GpuRasterCacheKey, GenOut)]),
    dev :: !(Ptr SDLGPUDevice),
    genPipe :: !(Ptr SDLGPUGraphicsPipeline)
  }

data GpuAtlasDraw = GpuAtlasDraw
  { viewport :: !GpuViewport,
    scissor :: !SdlRect,
    dimX :: !Int,
    dimY :: !Int,
    scale :: !Double,
    tx :: !Double,
    ty :: !Double,
    selectorSegs :: ![PreparedLineSeg],
    windingSegs :: ![PreparedLineSeg]
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

foreign import ccall unsafe "SDL_CreateGPUBuffer"
  c_sdlCreateGPUBuffer :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUBuffer)

foreign import ccall unsafe "SDL_ReleaseGPUBuffer"
  c_sdlReleaseGPUBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUBuffer -> IO ()

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

foreign import ccall unsafe "SDL_UploadToGPUBuffer"
  c_sdlUploadToGPUBuffer :: Ptr SDLGPUCopyPass -> Ptr () -> Ptr () -> CBool -> IO ()

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

foreign import ccall unsafe "SDL_BindGPUFragmentStorageBuffers"
  c_sdlBindGPUFragmentStorageBuffers :: Ptr SDLGPURenderPass -> Word32 -> Ptr (Ptr SDLGPUBuffer) -> Word32 -> IO ()

foreign import ccall unsafe "SDL_PushGPUVertexUniformData"
  c_sdlPushGPUVertexUniformData :: Ptr SDLGPUCommandBuffer -> Word32 -> Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "SDL_PushGPUFragmentUniformData"
  c_sdlPushGPUFragmentUniformData :: Ptr SDLGPUCommandBuffer -> Word32 -> Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "SDL_DrawGPUPrimitives"
  c_sdlDrawGPUPrimitives :: Ptr SDLGPURenderPass -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "SDL_SetGPUViewport"
  c_sdlSetGPUViewport :: Ptr SDLGPURenderPass -> Ptr GpuViewport -> IO ()

foreign import ccall unsafe "SDL_SetGPUScissor"
  c_sdlSetGPUScissor :: Ptr SDLGPURenderPass -> Ptr SdlRect -> IO ()

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

fontPathRegularRoboto :: FilePath
fontPathRegularRoboto = "../../assets/roboto-flex-source/RobotoFlex-VF.ttf"

fontPathVarRoboto :: FilePath
fontPathVarRoboto = "../../assets/roboto-flex-source/RobotoFlex-VF.ttf"

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

sdlGpuTextureUsageColorTarget :: Word32
sdlGpuTextureUsageColorTarget = #{const SDL_GPU_TEXTUREUSAGE_COLOR_TARGET}

sdlGpuBufferUsageGraphicsStorageRead :: Word32
sdlGpuBufferUsageGraphicsStorageRead = #{const SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ}

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
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  sceneRaw <- lookupEnv "MASDIFF_SDL_SCENE"
  scenePreset <- either die pure (parseScenePreset sceneRaw)
  rasterModeRaw <- lookupEnv "MASDIFF_SDL_GEN_BACKEND"
  rasterMode <- either die pure (parseRasterMode rasterModeRaw)
  strictGpuGeneration <- readBoolEnvDefault "MASDIFF_SDL_GEN_STRICT" False
  debugLog <- readBoolEnvDefault "MASDIFF_SDL_DEBUG" False
  presentHeal <- readBoolEnvDefault "MASDIFF_SDL_PRESENT_HEAL" False
  genShaderRaw <- lookupEnv "MASDIFF_SDL_GEN_SHADER"
  genShaderMode <- either die pure (parseGenFragShaderMode genShaderRaw)
  pipelineProbe <- readBoolEnvDefault "MASDIFF_SDL_PIPELINE_PROBE" False
  requestedGpuBatch <- readBoolEnvDefault "MASDIFF_SDL_GPU_BATCH" False
  let useGpuBatch = requestedGpuBatch && rasterMode == RasterModeGpu
  maxSegsRaw <- readPositiveIntEnvDefault "MASDIFF_SDL_GEN_MAX_SEGS" gpuRasterShaderMaxSegs
  maxPushBytes <- readPositiveIntEnvDefault "MASDIFF_SDL_GEN_MAX_PUSH_BYTES" gpuRasterDefaultMaxPushBytes
  let rasterLimits =
        GpuRasterLimits
          { maxSegs = min gpuRasterShaderMaxSegs maxSegsRaw,
            maxPushBytes = max gpuRasterHeaderBytes maxPushBytes
          }
  putStrLn
    ( "SDL generation backend: "
        <> show rasterMode
        <> if strictGpuGeneration then " (strict)" else ""
    )
  when (rasterMode == RasterModeGpu) $
    putStrLn ("SDL generation batching: " <> if useGpuBatch then "enabled" else "disabled")
  logDbg debugLog ("scene preset: " <> show scenePreset)
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
  let vtx = vertexShader
  let frag = if presentHeal then fragmentShader else fragmentShaderNoHeal
  let gvx = genVertexShader
  let gfg =
        case genShaderMode of
          GenFragFlat -> genFragmentShaderFlat
          GenFragStruct -> genFragmentShader
          GenFragSanity -> genFragmentShaderSanity
  logDbg debugLog ("gen vertex shader: " <> renderShaderInfo gvx)
  logDbg debugLog ("gen fragment shader: " <> renderShaderInfo gfg)
  logDbg debugLog ("generation fragment shader mode: " <> show genShaderMode)
  logDbg debugLog ("presentation heal: " <> show presentHeal)
  logDbg
    debugLog
    ( "generation limits: maxSegs="
        <> show rasterLimits.maxSegs
        <> " maxPushBytes="
        <> show rasterLimits.maxPushBytes
    )
  logDbg debugLog "initializing SDL window + GPU device"
  withSDL $
    withWindow "masdiff SDL3 (Spirdo + GPU MTSDF)" winW winH $ \win ->
      withGpuDevice $ \dev ->
        withClaimedWindow dev win $ do
          logDbg debugLog "creating generation shaders"
          logDbg debugLog "create gen vertex shader (start)"
          withGpuShader dev gvx $ \gvs -> do
            logDbg debugLog "create gen vertex shader (ok)"
            logDbg debugLog "create gen fragment shader (start)"
            withGpuShader dev gfg $ \gfs -> do
              logDbg debugLog "create gen fragment shader (ok)"
              logDbg debugLog "create gen pipeline (start)"
              withGpuPipeline dev sdlGpuTextureFormatRgba8Unorm False gvs gfs $ \gpipe -> do
                logDbg debugLog "create gen pipeline (ok)"
                logDbg debugLog "generation pipeline ready"
                if pipelineProbe
                  then putStrLn "[sdl3] pipeline probe complete"
                  else do
                    gpuStatsRef <- newIORef emptyGpuRasterStats
                    gpuCacheRef <- newIORef []
                    let gpuBatchCtx =
                          GpuBatchCtx
                            { debugLog = debugLog,
                              strictMode = strictGpuGeneration,
                              limits = rasterLimits,
                              statsRef = gpuStatsRef,
                              cacheRef = gpuCacheRef,
                              dev = dev,
                              genPipe = gpipe
                            }
                    let raster =
                          case rasterMode of
                            RasterModeGpu -> gpuRasterIO debugLog strictGpuGeneration rasterLimits gpuStatsRef gpuCacheRef dev gpipe
                            RasterModeCpu -> cpuRasterIO
                    logDbg debugLog "building scene"
                    scene <- buildScene rasterMode useGpuBatch (if rasterMode == RasterModeGpu then Just gpuBatchCtx else Nothing) raster scenePreset
                    when (rasterMode == RasterModeGpu) $ do
                      gpuStats <- readIORef gpuStatsRef
                      putStrLn ("GPU generation stats: " <> renderGpuRasterStats gpuStats)
                    logDbg debugLog "creating presentation shaders"
                    swapFmt <- c_sdlGetGPUSwapchainTextureFormat dev win
                    logDbg debugLog "create present vertex shader (start)"
                    withGpuShader dev vtx $ \vs -> do
                      logDbg debugLog "create present vertex shader (ok)"
                      logDbg debugLog "create present fragment shader (start)"
                      withGpuShader dev frag $ \fs -> do
                        logDbg debugLog "create present fragment shader (ok)"
                        logDbg debugLog "create present pipeline (start)"
                        withGpuPipeline dev swapFmt True vs fs $ \pipe -> do
                          logDbg debugLog "create present pipeline (ok)"
                          withGpuSampler dev $ \smp ->
                            withLineTextures dev scene $ \lineTexs -> do
                              logDbg debugLog "entering render loop"
                              let draws0 = placeScene lineTexs
                              let draws =
                                    if noFit
                                      then draws0
                                      else fitDrawsToWindow winW winH 24 draws0
                              case metaPath of
                                Nothing -> pure ()
                                Just path -> writeDrawMeta path draws
                              runLoop win dev pipe smp draws swapFmt 8.0 capturePath

buildScene :: RasterMode -> Bool -> Maybe GpuBatchCtx -> RasterPreparedIO -> ScenePreset -> IO [LineBuild]
buildScene rasterMode useGpuBatch gpuBatch raster scenePreset = do
  singleEm <- readPositiveDoubleEnvDefault "MASDIFF_SDL_SINGLE_EM" 640.0
  fontPreset <- readFontPresetEnv
  overlapSupport <- readBoolEnvDefault "MASDIFF_SDL_OVLP" True
  dimRaw <- readPositiveIntEnvDefault "MASDIFF_SDL_DIM" 256
  pxRangeRaw <- readPositiveDoubleEnvDefault "MASDIFF_SDL_PXRANGE" 6.0
  dim <- either (die . ("invalid dim: " <>)) pure (mkDim dimRaw)
  pxr <- either (die . ("invalid px range: " <>)) pure (mkPxRange pxRangeRaw)
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
  let regularPath =
        case fontPreset of
          FontPresetInter -> fontPathRegular
          FontPresetRoboto -> fontPathRegularRoboto
      varPath =
        case fontPreset of
          FontPresetInter -> fontPathVar
          FontPresetRoboto -> fontPathVarRoboto
  let regular = FontFile {path = regularPath}
  let varLight =
        VarFontFile
          { path = varPath,
            axes =
              Map.fromList
                [ (AxisTag (T.pack "wght"), AxisVal 300),
                  (AxisTag (T.pack "opsz"), AxisVal 14)
                ]
          }
  let varBold =
        VarFontFile
          { path = varPath,
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
  built <- traverse (buildLine rasterMode useGpuBatch gpuBatch raster cfg atlasCfg) specs
  either (die . ("scene build failed: " <>)) pure (sequence built)

buildLine :: RasterMode -> Bool -> Maybe GpuBatchCtx -> RasterPreparedIO -> GenCfg -> AtlasCfg -> LineSpec -> IO (Either String LineBuild)
buildLine rasterMode useGpuBatch gpuBatch raster cfg atlasCfg spec = do
  let atlasCodes = uniqueGlyphCodes spec.txt
  let glyphList = snd <$> atlasCodes
  case sequence glyphList of
    Left err -> pure (Left err)
    Right glyphs -> do
      putStrLn ("[sdl3] buildLine: txt=\"" <> spec.txt <> "\" glyphs=" <> show (length glyphs) <> " em=" <> show spec.em)
      case (rasterMode, useGpuBatch, gpuBatch) of
        (RasterModeGpu, True, Just gpuCtx) -> buildLineGpu gpuCtx cfg spec glyphs
        _ -> do
          atlasResult <- generateAtlasWithRasterIO 1 raster atlasCfg cfg spec.src glyphs
          case atlasResult of
            Left err -> pure (Left err)
            Right atlas -> assembleAtlasIO atlas
  where
    assembleAtlasIO :: Atlas -> IO (Either String LineBuild)
    assembleAtlasIO atlas =
      case atlas.pages of
        [] -> pure (Left "atlas build produced no pages")
        [page0] -> do
          dumpAtlasMaybe page0.img
          uvInsetRaw <- readNonNegativeDoubleEnvDefault "MASDIFF_SDL_UV_INSET" 0.25
          let uvInset = min 0.5 uvInsetRaw
          pure $ do
            runMap <- runsByEntries uvInset page0.img.w page0.img.h (toEntries atlas.entries)
            atoms <- lineAtoms runMap
            layoutLineCpu page0.img atoms
        _ -> pure (Left "line atlas spilled to multiple pages; increase atlas size for this demo")

    dumpAtlasMaybe :: ImgRGBA8 -> IO ()
    dumpAtlasMaybe img = do
      dumpAtlasPath <- lookupEnv "MASDIFF_SDL_DUMP_LINE_ATLAS"
      case dumpAtlasPath of
        Just p | not (null p) -> do
          _ <- writePngRGBA8File p img
          pure ()
        _ -> pure ()

    toEntries :: [AtlasEntry] -> [(GlyphCode, AtlasRect, Metrics)]
    toEntries entries = [(e.glyph, e.rect, e.metrics) | e <- entries]

    runsByEntries :: Double -> Int -> Int -> [(GlyphCode, AtlasRect, Metrics)] -> Either String (IM.IntMap GlyphRun)
    runsByEntries uvInset0 texWi texHi entries =
      foldM step IM.empty entries
      where
        texW = fromIntegral texWi
        texH = fromIntegral texHi
        step m (gcode, rect0, mtr) = do
          let Metrics {adv = adv0, bounds = (xmin, ymin, xmax, _), scale = mScale, translate = mTranslate} = mtr
              gw = fromIntegral rect0.w
              gh = fromIntegral rect0.h
              fallbackSc = gw / max 1.0e-6 (xmax - xmin)
              sc0 = fromMaybe fallbackSc mScale
              (tx0, ty0) = fromMaybe (negate xmin, negate ymin) mTranslate
          if not (isFiniteD sc0) || sc0 <= 0
            then Left ("invalid glyph scale for codepoint " <> show (unGlyphCode gcode))
            else do
              let cropX0 = 0
                  cropY0 = 0
                  cW = rect0.w
                  cH = rect0.h
                  insetX = if cW > 1 then uvInset0 else 0.0
                  insetY = if cH > 1 then uvInset0 else 0.0
                  ox0 = (sc0 * tx0) - 0.5
                  oy0 = (gh - (sc0 * ty0)) - 0.5
                  desc0 = max 0 ((-ymin) * sc0)
                  uv0 =
                    UvRect
                      { u0 = (fromIntegral (rect0.x + cropX0) + insetX) / texW,
                        v0 = (fromIntegral (rect0.y + cropY0) + insetY) / texH,
                        u1 = (fromIntegral (rect0.x + cropX0 + cW) - insetX) / texW,
                        v1 = (fromIntegral (rect0.y + cropY0 + cH) - insetY) / texH
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
              pure (IM.insert (unGlyphCode gcode) gr m)

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

    buildLineGpu :: GpuBatchCtx -> GenCfg -> LineSpec -> [GlyphCode] -> IO (Either String LineBuild)
    buildLineGpu gpuCtx cfg0 spec0 glyphs = do
      prepared <- prepareGlyphBatchNativeIO spec0.src glyphs
      case sequence prepared of
        Left genErr -> pure (Left ("glyph prepare failed: " <> show genErr))
        Right preparedGlyphs -> do
          let windingCfg = cfg0 {ovlp = False}
          let atlasW = 2048
              atlasH = 2048
              atlasPad = 12
              dimI = unDim cfg0.dim
              cell = dimI + (2 * atlasPad)
              cols = max 1 (atlasW `quot` cell)
              placeOne ix g p =
                let row = ix `quot` cols
                    col = ix `rem` cols
                    x0 = (col * cell) + atlasPad
                    y0 = (row * cell) + atlasPad
                 in
                  ( g,
                    p,
                    AtlasRect {x = x0, y = y0, w = dimI, h = dimI},
                    metricsPrepared cfg0 p,
                    preparedLineSegs cfg0 p,
                    preparedLineSegs windingCfg p
                  )
              placed = zipWith3 placeOne [0 ..] glyphs preparedGlyphs
          case firstOverflow placed atlasW atlasH of
            Just msg -> pure (Left msg)
            Nothing -> do
              atlasResult <- rasterPreparedGpuAtlasTexture gpuCtx cfg0 atlasW atlasH placed
              case atlasResult of
                Left err -> pure (Left err)
                Right atlasTex0 -> do
                  uvInsetRaw <- readNonNegativeDoubleEnvDefault "MASDIFF_SDL_UV_INSET" 0.25
                  let uvInset = min 0.5 uvInsetRaw
                  let entries = [(g, r, mtr) | (g, _, r, mtr, _, _) <- placed]
                  pure $ do
                    runMap <- runsByEntries uvInset atlasW atlasH entries
                    atoms <- lineAtoms runMap
                    layoutLineGpu atlasTex0 atoms

    firstOverflow :: [(GlyphCode, PreparedGlyph, AtlasRect, Metrics, [PreparedLineSeg], [PreparedLineSeg])] -> Int -> Int -> Maybe String
    firstOverflow placed atlasW atlasH =
      case [rect0 | (_, _, rect0, _, _, _) <- placed, rect0.x + rect0.w > atlasW || rect0.y + rect0.h > atlasH] of
        [] -> Nothing
        _ -> Just ("line atlas overflow: increase atlas size for text \"" <> spec.txt <> "\"")

cpuRasterIO :: RasterPreparedIO
cpuRasterIO cfg prepared = pure (rasterPreparedCpu cfg prepared)

emptyGpuRasterStats :: GpuRasterStats
emptyGpuRasterStats = GpuRasterStats {ok = 0, cacheHit = 0, badFrame = 0, segOverflow = 0, gpuErr = 0}

gpuRasterFallbacks :: GpuRasterStats -> Int
gpuRasterFallbacks stats = stats.badFrame + stats.segOverflow + stats.gpuErr

renderGpuRasterStats :: GpuRasterStats -> String
renderGpuRasterStats stats =
  "ok="
    <> show stats.ok
    <> " cache-hit="
    <> show stats.cacheHit
    <> " fallback="
    <> show (gpuRasterFallbacks stats)
    <> " (bad-frame="
    <> show stats.badFrame
    <> ", seg-overflow="
    <> show stats.segOverflow
    <> ", gpu-err="
    <> show stats.gpuErr
    <> ")"

renderShaderInfo :: GpuShader -> String
renderShaderInfo shader =
  let (ns, nst, nsb, nu) = resourceCounts shader
   in "stage="
        <> show shader.stg
        <> " samplers="
        <> show ns
        <> " storageTextures="
        <> show nst
        <> " storageBuffers="
        <> show nsb
        <> " uniforms="
        <> show nu
        <> " binds="
        <> show (fmap renderBinding shader.binds)
  where
    renderBinding b =
      "("
        <> show b.biGroup
        <> ","
        <> show b.biBinding
        <> ","
        <> show b.biKind
        <> ")"

gpuRasterIO :: Bool -> Bool -> GpuRasterLimits -> IORef GpuRasterStats -> IORef [(GpuRasterCacheKey, GenOut)] -> Ptr SDLGPUDevice -> Ptr SDLGPUGraphicsPipeline -> RasterPreparedIO
gpuRasterIO debugLog strictMode limits statsRef cacheRef dev genPipe cfg prepared = do
  let fallback = pure (rasterPreparedCpu cfg prepared)
      failOrFallback err bump = do
        logDbg debugLog ("gpuRaster fallback: " <> show err)
        modifyIORef' statsRef bump
        if strictMode
          then pure (Left err)
          else fallback
  case (metrics.scale, metrics.translate) of
    (Just scale0, Just (tx0, ty0)) -> do
      let selectorSegs0 = preparedLineSegs cfg prepared
          windingCfg = cfg {ovlp = False}
          windingSegs0 = preparedLineSegs windingCfg prepared
          selectorSegCount = length selectorSegs0
          windingSegCount = length windingSegs0
          selectorSegBytes = gpuRasterBytesForSegments selectorSegCount
          windingSegBytes = gpuRasterBytesForSegments windingSegCount
          cacheKey =
            GpuRasterCacheKey
              { dim = unDim cfg.dim,
                pxr = unPxRange cfg.pxr,
                sc = scale0,
                tx = tx0,
                ty = ty0,
                selectorSegs = selectorSegs0,
                windingSegs = windingSegs0
              }
      logDbg
        debugLog
        ( "gpuRaster: selector-segs="
            <> show selectorSegCount
            <> " winding-segs="
            <> show windingSegCount
            <> " selector-bytes="
            <> show selectorSegBytes
            <> " winding-bytes="
            <> show windingSegBytes
            <> " dim="
            <> show (unDim cfg.dim)
        )
      cache <- readIORef cacheRef
      case find (\(k, _) -> k == cacheKey) cache of
        Just (_, cachedOut) -> do
          logDbg debugLog "gpuRaster: cache-hit"
          modifyIORef' statsRef (\stats -> stats {cacheHit = stats.cacheHit + 1})
          pure (Right cachedOut)
        Nothing ->
          if null selectorSegs0
            || null windingSegs0
            || selectorSegCount > limits.maxSegs
            || windingSegCount > limits.maxSegs
            || selectorSegBytes > limits.maxPushBytes
            || windingSegBytes > limits.maxPushBytes
        then
          failOrFallback
            ( ExecFailed
                ( "GPU raster segment budget exceeded (selector-segments="
                    <> show selectorSegCount
                    <> ", winding-segments="
                    <> show windingSegCount
                    <> ", selector-bytes="
                    <> show selectorSegBytes
                    <> ", winding-bytes="
                    <> show windingSegBytes
                    <> ", max-segments="
                    <> show limits.maxSegs
                    <> ", max-bytes="
                    <> show limits.maxPushBytes
                    <> ")."
                )
            )
            (\stats -> stats {segOverflow = stats.segOverflow + 1})
          else do
            rendered <- try (rasterPreparedGpuImage dev genPipe (unDim cfg.dim) scale0 tx0 ty0 (unPxRange cfg.pxr) selectorSegs0 windingSegs0) :: IO (Either SomeException (Either String ImgRGBA8))
            case rendered of
              Left ex ->
                failOrFallback
                  (ExecFailed ("GPU raster pass crashed: " <> displayException ex))
                  (\stats -> stats {gpuErr = stats.gpuErr + 1})
              Right (Left err) ->
                failOrFallback
                  (ExecFailed ("GPU raster pass failed: " <> err))
                  (\stats -> stats {gpuErr = stats.gpuErr + 1})
              Right (Right img0) -> do
                let out =
                      GenOut
                        { img = img0,
                          metrics = metrics
                        }
                logDbg debugLog "gpuRaster: success"
                modifyIORef' statsRef (\stats -> stats {ok = stats.ok + 1})
                modifyIORef' cacheRef (\pairs -> (cacheKey, out) : pairs)
                pure (Right out)
    _ ->
      failOrFallback
        (ExecFailed "GPU raster requires autoframe metrics (scale + translate).")
        (\stats -> stats {badFrame = stats.badFrame + 1})
  where
    metrics = metricsPrepared cfg prepared

gpuRasterShaderMaxSegs :: Int
gpuRasterShaderMaxSegs = 2048

gpuRasterDefaultMaxPushBytes :: Int
-- In buffer-backed mode this is a segment-bytes budget, not push-uniform size.
gpuRasterDefaultMaxPushBytes = gpuRasterBytesForSegments gpuRasterShaderMaxSegs

gpuRasterHeaderBytes :: Int
gpuRasterHeaderBytes = 48

gpuRasterSegStrideBytes :: Int
gpuRasterSegStrideBytes = 32

gpuRasterBytesForSegments :: Int -> Int
gpuRasterBytesForSegments n = n * gpuRasterSegStrideBytes

rasterPreparedGpuImage ::
  Ptr SDLGPUDevice ->
  Ptr SDLGPUGraphicsPipeline ->
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  [PreparedLineSeg] ->
  [PreparedLineSeg] ->
  IO (Either String ImgRGBA8)
rasterPreparedGpuImage dev genPipe dim scale0 tx0 ty0 pxr0 selectorSegs0 windingSegs0 = do
  let usage = sdlGpuTextureUsageSampler .|. sdlGpuTextureUsageColorTarget
      clear = FColor 0 0 0 0
  bracket
    (withTextureCreateInfoUsage sdlGpuTextureFormatRgba8Unorm usage dim dim (\ci -> requirePtr "SDL_CreateGPUTexture(gen)" (c_sdlCreateGPUTexture dev ci)))
    (c_sdlReleaseGPUTexture dev)
    (\tex -> do
       cmd <- requirePtr "SDL_AcquireGPUCommandBuffer(gen)" (c_sdlAcquireGPUCommandBuffer dev)
       withUploadedSegBuffer dev cmd selectorSegs0 $ \selectorSegBuf ->
         withUploadedSegBuffer dev cmd windingSegs0 $ \windingSegBuf -> do
           withColorTargetInfo tex clear $ \ctInfo -> do
             rp <- requirePtr "SDL_BeginGPURenderPass(gen)" (c_sdlBeginGPURenderPass cmd ctInfo 1 nullPtr)
             c_sdlBindGPUGraphicsPipeline rp genPipe
             withArray [selectorSegBuf, windingSegBuf] $ \bufPtr ->
               c_sdlBindGPUFragmentStorageBuffers rp 0 bufPtr 2
             withGpuRasterUniform dim dim scale0 tx0 ty0 pxr0 (length selectorSegs0) (length windingSegs0) (requiresNonZeroWinding windingSegs0) $ \uPtr uSize ->
               c_sdlPushGPUFragmentUniformData cmd 0 uPtr (fromIntegral uSize)
             c_sdlDrawGPUPrimitives rp 6 1 0 0
             c_sdlEndGPURenderPass rp
       bytes <- downloadSwapTexture dev cmd tex dim dim
       pure (mkImgRGBA8 dim dim bytes)
    )

rasterPreparedGpuAtlasTexture ::
  GpuBatchCtx ->
  GenCfg ->
  Int ->
  Int ->
  [(GlyphCode, PreparedGlyph, AtlasRect, Metrics, [PreparedLineSeg], [PreparedLineSeg])] ->
  IO (Either String (Ptr SDLGPUTexture))
rasterPreparedGpuAtlasTexture gpuCtx cfg atlasW atlasH glyphs = do
  let usage = sdlGpuTextureUsageSampler .|. sdlGpuTextureUsageColorTarget
      clear = FColor 0 0 0 0
  case sequence (fmap mkDraw glyphs) of
    Left err -> do
      modifyIORef' gpuCtx.statsRef (\stats -> stats {badFrame = stats.badFrame + 1})
      pure (Left err)
    Right draws -> do
      logDbg gpuCtx.debugLog ("gpuRasterBatch: draws=" <> show (length draws) <> " atlas=" <> show atlasW <> "x" <> show atlasH)
      tex <- withTextureCreateInfoUsage sdlGpuTextureFormatRgba8Unorm usage atlasW atlasH (\ci -> requirePtr "SDL_CreateGPUTexture(gen-atlas)" (c_sdlCreateGPUTexture gpuCtx.dev ci))
      res <- try $ do
        cmd <- requirePtr "SDL_AcquireGPUCommandBuffer(gen-atlas)" (c_sdlAcquireGPUCommandBuffer gpuCtx.dev)
        withColorTargetInfo tex clear $ \ctInfo -> do
          rp <- requirePtr "SDL_BeginGPURenderPass(gen-atlas)" (c_sdlBeginGPURenderPass cmd ctInfo 1 nullPtr)
          c_sdlBindGPUGraphicsPipeline rp gpuCtx.genPipe
          forM_ draws $ \d -> do
            with d.viewport $ \vpPtr -> c_sdlSetGPUViewport rp vpPtr
            with d.scissor $ \scPtr -> c_sdlSetGPUScissor rp scPtr
            withUploadedSegBuffer gpuCtx.dev cmd d.selectorSegs $ \selectorSegBuf ->
              withUploadedSegBuffer gpuCtx.dev cmd d.windingSegs $ \windingSegBuf -> do
                withArray [selectorSegBuf, windingSegBuf] $ \bufPtr ->
                  c_sdlBindGPUFragmentStorageBuffers rp 0 bufPtr 2
                withGpuRasterUniform d.dimX d.dimY d.scale d.tx d.ty (unPxRange cfg.pxr) (length d.selectorSegs) (length d.windingSegs) (requiresNonZeroWinding d.windingSegs) $ \uPtr uSize ->
                  c_sdlPushGPUFragmentUniformData cmd 0 uPtr (fromIntegral uSize)
                c_sdlDrawGPUPrimitives rp 6 1 0 0
          c_sdlEndGPURenderPass rp
        requireTrue "SDL_SubmitGPUCommandBuffer(gen-atlas)" (c_sdlSubmitGPUCommandBuffer cmd)
      case res of
        Left (ex :: SomeException) -> do
          c_sdlReleaseGPUTexture gpuCtx.dev tex
          pure (Left ("GPU atlas raster failed: " <> displayException ex))
        Right () -> do
          logDbg gpuCtx.debugLog "gpuRasterBatch: success"
          modifyIORef' gpuCtx.statsRef (\stats -> stats {ok = stats.ok + length draws})
          pure (Right tex)
  where
    dimI = unDim cfg.dim

    mkDraw (_, _, rect0, metrics0, selectorSegs0, windingSegs0) =
      case (metrics0.scale, metrics0.translate) of
        (Just scale0, Just (tx0, ty0)) ->
          let selectorSegCount = length selectorSegs0
              windingSegCount = length windingSegs0
              selectorSegBytes = gpuRasterBytesForSegments selectorSegCount
              windingSegBytes = gpuRasterBytesForSegments windingSegCount
           in if null selectorSegs0
                || null windingSegs0
                || selectorSegCount > gpuCtx.limits.maxSegs
                || windingSegCount > gpuCtx.limits.maxSegs
                || selectorSegBytes > gpuCtx.limits.maxPushBytes
                || windingSegBytes > gpuCtx.limits.maxPushBytes
                then
                  Left
                    ( "GPU atlas raster segment budget exceeded (selector-segments="
                        <> show selectorSegCount
                        <> ", winding-segments="
                        <> show windingSegCount
                        <> ", selector-bytes="
                        <> show selectorSegBytes
                        <> ", winding-bytes="
                        <> show windingSegBytes
                        <> ")."
                    )
                else
                  Right
                    GpuAtlasDraw
                      { viewport =
                          GpuViewport
                            { x = fromIntegral rect0.x,
                              y = fromIntegral rect0.y,
                              w = fromIntegral rect0.w,
                              h = fromIntegral rect0.h,
                              minDepth = 0,
                              maxDepth = 1
                            },
                        scissor =
                          SdlRect
                            { x = fromIntegral rect0.x,
                              y = fromIntegral rect0.y,
                              w = fromIntegral rect0.w,
                              h = fromIntegral rect0.h
                            },
                        dimX = dimI,
                        dimY = dimI,
                        scale = scale0,
                        tx = tx0,
                        ty = ty0,
                        selectorSegs = selectorSegs0,
                        windingSegs = windingSegs0
                      }
        _ -> Left "GPU atlas raster requires autoframe metrics (scale + translate)."


withGpuRasterUniform ::
  Int ->
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  Int ->
  Int ->
  Bool ->
  (Ptr () -> Int -> IO a) ->
  IO a
withGpuRasterUniform dimX dimY scale0 tx0 ty0 pxRange selectorSegCount windingSegCount useNonZeroWinding action =
  allocaBytes totalBytes $ \ptr -> do
    let pokeF off v = pokeByteOff ptr off (realToFrac v :: CFloat)
    fillBytes ptr 0 totalBytes
    pokeF 0 (fromIntegral dimX)
    pokeF 4 (fromIntegral dimY)
    pokeF 8 scale0
    pokeF 12 tx0
    pokeF 16 ty0
    pokeF 20 pxRange
    pokeF 24 (fromIntegral selectorSegCount)
    pokeF 28 (fromIntegral windingSegCount)
    pokeF 32 (if useNonZeroWinding then (1 :: Double) else 0)
    action (castPtr ptr) totalBytes
  where
    totalBytes = gpuRasterHeaderBytes

withUploadedSegBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUCommandBuffer -> [PreparedLineSeg] -> (Ptr SDLGPUBuffer -> IO a) -> IO a
withUploadedSegBuffer dev cmd segs action = do
  let segBytes = gpuRasterBytesForSegments (length segs)
  bracket
    (withBufferCreateInfo sdlGpuBufferUsageGraphicsStorageRead segBytes (\ci -> requirePtr "SDL_CreateGPUBuffer(segs)" (c_sdlCreateGPUBuffer dev ci)))
    (c_sdlReleaseGPUBuffer dev)
    (\segBuf ->
       bracket
         (withTransferBufferCreateInfo sdlGpuTransferUsageUpload segBytes (\ci -> requirePtr "SDL_CreateGPUTransferBuffer(seg-upload)" (c_sdlCreateGPUTransferBuffer dev ci)))
         (c_sdlReleaseGPUTransferBuffer dev)
         (\tb -> do
            mapped <- c_sdlMapGPUTransferBuffer dev tb 0
            when (mapped == nullPtr) (dieSdl "SDL_MapGPUTransferBuffer(seg-upload)")
            writeSegBytes mapped segs
            c_sdlUnmapGPUTransferBuffer dev tb
            cp <- requirePtr "SDL_BeginGPUCopyPass(seg-upload)" (c_sdlBeginGPUCopyPass cmd)
            withTransferBufferLocation tb 0 $ \srcLoc ->
              withBufferRegion segBuf 0 segBytes $ \dstRegion ->
                c_sdlUploadToGPUBuffer cp srcLoc dstRegion 0
            c_sdlEndGPUCopyPass cp
            action segBuf
         )
    )

writeSegBytes :: Ptr () -> [PreparedLineSeg] -> IO ()
writeSegBytes base segs =
  forM_ (zip [0 ..] segs) $ \(ix, seg) -> do
    let off = ix * gpuRasterSegStrideBytes
        pokeF rel v = pokeByteOff base (off + rel) (realToFrac v :: CFloat)
    pokeF 0 seg.x0
    pokeF 4 seg.y0
    pokeF 8 seg.x1
    pokeF 12 seg.y1
    pokeF 16 (fromIntegral seg.col :: Float)
    pokeF 20 (fromIntegral seg.caps :: Float)

uniqueGlyphCodes :: String -> [(Char, Either String GlyphCode)]
uniqueGlyphCodes txt =
  [ (ch, mkGlyphCode (ord ch))
    | ch <- nub txt,
      ch /= ' '
  ]

layoutLineCore :: [LineAtom] -> Either String ([PlacedGlyph], Double, Double)
layoutLineCore atoms =
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
                ([], penEnd, 1)
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
              Right (shifted, fromIntegral (maxX - minX + (2 * padI)), fromIntegral (maxY - minY + (2 * padI)))

layoutLineCpu :: ImgRGBA8 -> [LineAtom] -> Either String LineBuild
layoutLineCpu atlas0 atoms = do
  (placed, outW, outH) <- layoutLineCore atoms
  Right
    LineBuild
      { atlasImg = Just atlas0,
        atlasTex = Nothing,
        glyphs = placed,
        w = outW,
        h = outH
      }

layoutLineGpu :: Ptr SDLGPUTexture -> [LineAtom] -> Either String LineBuild
layoutLineGpu tex0 atoms = do
  (placed, outW, outH) <- layoutLineCore atoms
  Right
    LineBuild
      { atlasImg = Nothing,
        atlasTex = Just tex0,
        glyphs = placed,
        w = outW,
        h = outH
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
    mk line =
      case line.atlasTex of
        Just tex -> pure (tex, line)
        Nothing ->
          case line.atlasImg of
            Just atlas0 -> do
              tex <- uploadTexture dev atlas0
              pure (tex, line)
            Nothing -> die "line has no atlas image or atlas texture"
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
  Bool ->
  Ptr SDLGPUShader ->
  Ptr SDLGPUShader ->
  (Ptr SDLGPUGraphicsPipeline -> IO a) ->
  IO a
withGpuPipeline dev swapFmt enableBlend vs fs action =
  bracket create destroy action
  where
    create =
      withPipelineCreateInfo swapFmt enableBlend vs fs $ \ci ->
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
  Bool ->
  Ptr SDLGPUShader ->
  Ptr SDLGPUShader ->
  (Ptr () -> IO a) ->
  IO a
withPipelineCreateInfo swapFmt enableBlend vs fs action =
  withZeroStruct #{size SDL_GPUColorTargetDescription} $ \ctDesc -> do
    pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, format} swapFmt
    if enableBlend
      then do
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.src_color_blendfactor} sdlGpuBlendFactorSrcAlpha
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.dst_color_blendfactor} sdlGpuBlendFactorOneMinusSrcAlpha
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.color_blend_op} sdlGpuBlendOpAdd
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.src_alpha_blendfactor} sdlGpuBlendFactorOne
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.dst_alpha_blendfactor} sdlGpuBlendFactorOneMinusSrcAlpha
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.alpha_blend_op} sdlGpuBlendOpAdd
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.enable_blend} (1 :: CBool)
      else do
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.src_color_blendfactor} sdlGpuBlendFactorOne
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.dst_color_blendfactor} sdlGpuBlendFactorOne
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.color_blend_op} sdlGpuBlendOpAdd
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.src_alpha_blendfactor} sdlGpuBlendFactorOne
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.dst_alpha_blendfactor} sdlGpuBlendFactorOne
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.alpha_blend_op} sdlGpuBlendOpAdd
        pokeByteOff ctDesc #{offset SDL_GPUColorTargetDescription, blend_state.enable_blend} (0 :: CBool)
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
  withTextureCreateInfoUsage sdlGpuTextureFormatRgba8Unorm sdlGpuTextureUsageSampler w h action

withTextureCreateInfoUsage :: CInt -> Word32 -> Int -> Int -> (Ptr () -> IO a) -> IO a
withTextureCreateInfoUsage fmt usage w h action =
  withZeroStruct #{size SDL_GPUTextureCreateInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, type} sdlGpuTextureType2d
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, format} fmt
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, usage} usage
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

withBufferCreateInfo :: Word32 -> Int -> (Ptr () -> IO a) -> IO a
withBufferCreateInfo usage bytes action =
  withZeroStruct #{size SDL_GPUBufferCreateInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUBufferCreateInfo, usage} usage
    pokeByteOff ptr #{offset SDL_GPUBufferCreateInfo, size} (fromIntegral bytes :: Word32)
    pokeByteOff ptr #{offset SDL_GPUBufferCreateInfo, props} (0 :: Word32)
    action ptr

withTextureTransferInfo :: Ptr SDLGPUTransferBuffer -> Int -> Int -> (Ptr () -> IO a) -> IO a
withTextureTransferInfo tb w h action =
  withZeroStruct #{size SDL_GPUTextureTransferInfo} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, transfer_buffer} tb
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, offset} (0 :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, pixels_per_row} (fromIntegral w :: Word32)
    pokeByteOff ptr #{offset SDL_GPUTextureTransferInfo, rows_per_layer} (fromIntegral h :: Word32)
    action ptr

withTransferBufferLocation :: Ptr SDLGPUTransferBuffer -> Int -> (Ptr () -> IO a) -> IO a
withTransferBufferLocation tb off action =
  withZeroStruct #{size SDL_GPUTransferBufferLocation} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUTransferBufferLocation, transfer_buffer} tb
    pokeByteOff ptr #{offset SDL_GPUTransferBufferLocation, offset} (fromIntegral off :: Word32)
    action ptr

withBufferRegion :: Ptr SDLGPUBuffer -> Int -> Int -> (Ptr () -> IO a) -> IO a
withBufferRegion buf off bytes action =
  withZeroStruct #{size SDL_GPUBufferRegion} $ \ptr -> do
    pokeByteOff ptr #{offset SDL_GPUBufferRegion, buffer} buf
    pokeByteOff ptr #{offset SDL_GPUBufferRegion, offset} (fromIntegral off :: Word32)
    pokeByteOff ptr #{offset SDL_GPUBufferRegion, size} (fromIntegral bytes :: Word32)
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

parseRasterMode :: Maybe String -> Either String RasterMode
parseRasterMode raw =
  case fmap (fmap toLower) raw of
    Nothing -> Right RasterModeGpu
    Just "" -> Right RasterModeGpu
    Just "gpu" -> Right RasterModeGpu
    Just "cpu" -> Right RasterModeCpu
    Just other ->
      Left
        ( "Unknown MASDIFF_SDL_GEN_BACKEND value: "
            <> other
            <> " (expected: gpu or cpu)"
        )

parseGenFragShaderMode :: Maybe String -> Either String GenFragShaderMode
parseGenFragShaderMode raw =
  case fmap (fmap toLower) raw of
    Nothing -> Right GenFragFlat
    Just "" -> Right GenFragFlat
    Just "flat" -> Right GenFragFlat
    Just "struct" -> Right GenFragStruct
    Just "sanity" -> Right GenFragSanity
    Just other ->
      Left
        ( "Unknown MASDIFF_SDL_GEN_SHADER value: "
            <> other
            <> " (expected: flat, struct, or sanity)"
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

readNonNegativeDoubleEnvDefault :: String -> Double -> IO Double
readNonNegativeDoubleEnvDefault key defVal = do
  raw <- lookupEnv key
  case raw of
    Nothing -> pure defVal
    Just "" -> pure defVal
    Just txt ->
      case reads txt of
        [(x, "")] | isFiniteD x && x >= 0 -> pure x
        _ -> die ("invalid " <> key <> " value: " <> txt <> " (expected finite >= 0)")

readPositiveIntEnvDefault :: String -> Int -> IO Int
readPositiveIntEnvDefault key defVal = do
  raw <- lookupEnv key
  case raw of
    Nothing -> pure defVal
    Just "" -> pure defVal
    Just txt ->
      case reads txt of
        [(x, "")] | x > 0 -> pure x
        _ -> die ("invalid " <> key <> " value: " <> txt <> " (expected integer > 0)")

data FontPreset
  = FontPresetInter
  | FontPresetRoboto
  deriving stock (Eq, Show)

readFontPresetEnv :: IO FontPreset
readFontPresetEnv = do
  raw <- lookupEnv "MASDIFF_SDL_FONT"
  case fmap (fmap toLower) raw of
    Nothing -> pure FontPresetRoboto
    Just "" -> pure FontPresetRoboto
    Just "roboto" -> pure FontPresetRoboto
    Just "roboto-flex" -> pure FontPresetRoboto
    Just "inter" -> pure FontPresetInter
    Just bad ->
      die
        ( "invalid MASDIFF_SDL_FONT value: "
            <> bad
            <> " (expected: roboto|roboto-flex|inter)"
        )

logDbg :: Bool -> String -> IO ()
logDbg enabled msg =
  when enabled (putStrLn ("[sdl3] " <> msg))

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

genVertexShader :: GpuShader
genVertexShader =
  mkGpuShader
    [weslShader|
struct VsOut {
  @builtin(position) pos: vec4<f32>,
  @location(0) uv: vec2<f32>,
};

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
  let ndc = vec2<f32>((c.x * 2.0) - 1.0, 1.0 - (c.y * 2.0));
  var out: VsOut;
  out.pos = vec4<f32>(ndc, 0.0, 1.0);
  out.uv = c;
  return out;
}
	|]

genFragmentShaderSanity :: GpuShader
genFragmentShaderSanity =
  mkGpuShader
    [weslShader|
@fragment
fn main() -> @location(0) vec4<f32> {
  return vec4<f32>(0.5, 0.5, 0.5, 1.0);
}
|]

genFragmentShaderFlat :: GpuShader
genFragmentShaderFlat =
  mkGpuShader
    [weslShader|
struct FsU {
  meta0: vec4<f32>, // dimX, dimY, scale, tx
  meta1: vec4<f32>, // ty, pxRange, selectorSegCount, windingSegCount
  meta2: vec4<f32>, // useNonZeroWinding, pad, pad, pad
};

struct Seg {
  p0p1: vec4<f32>,
  meta: vec4<f32>, // meta.x=color mask [0..7], meta.y=endpoint cap bits (1=start, 2=end).
};

struct SegBuf {
  data: array<Seg>,
};

@group(3) @binding(0) var<uniform> u: FsU;
@group(2) @binding(0) var<storage, read> segBufSelector: SegBuf;
@group(2) @binding(1) var<storage, read> segBufWinding: SegBuf;

const EPS: f32 = 1.0e-5;
const EPS_SQ: f32 = EPS * EPS;

fn segDistancePseudo(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>, capBits: u32) -> f32 {
  let v = p1 - p0;
  let vv = max(dot(v, v), EPS_SQ);
  let t = dot(p - p0, v) / vv;
  let cross = abs((v.x * (p.y - p0.y)) - (v.y * (p.x - p0.x)));
  let lineDist = cross / sqrt(vv);
  if (t < 0.0 && (capBits & 1u) == 0u) {
    return lineDist;
  }
  if (t > 1.0 && (capBits & 2u) == 0u) {
    return lineDist;
  }
  let tc = clamp(t, 0.0, 1.0);
  let q = p0 + (tc * v);
  return length(p - q);
}

fn segDistanceClamped(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>) -> f32 {
  let v = p1 - p0;
  let vv = max(dot(v, v), EPS_SQ);
  let tc = clamp(dot(p - p0, v) / vv, 0.0, 1.0);
  let q = p0 + (tc * v);
  return length(p - q);
}

fn windingStepParity(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>) -> u32 {
  let crosses = (p0.y > p.y) != (p1.y > p.y);
  if (!crosses) {
    return 0u;
  }
  let dy = p1.y - p0.y;
  if (abs(dy) <= EPS) {
    return 0u;
  }
  let xInt = p0.x + ((p.y - p0.y) * (p1.x - p0.x) / dy);
  if (xInt > p.x) {
    return 1u;
  }
  return 0u;
}

fn windingStepNonZero(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>) -> i32 {
  let isLeft = ((p1.x - p0.x) * (p.y - p0.y)) - ((p.x - p0.x) * (p1.y - p0.y));
  if (p0.y <= p.y) {
    if (p1.y > p.y && isLeft > EPS) {
      return 1;
    }
  } else {
    if (p1.y <= p.y && isLeft < -EPS) {
      return -1;
    }
  }
  return 0;
}

@fragment
fn main(@location(0) uv: vec2<f32>) -> @location(0) vec4<f32> {
  let dimX = max(1.0, u.meta0.x);
  let dimY = max(1.0, u.meta0.y);
  let scale = max(1.0e-6, u.meta0.z);
  let tx = u.meta0.w;
  let ty = u.meta1.x;
  let pxRange = max(1.0, u.meta1.y);
  let selectorSegCount = u32(u.meta1.z + 0.5);
  let windingSegCount = u32(u.meta1.w + 0.5);
  let useNonZero = u.meta2.x > 0.5;

  let px = vec2<f32>(uv.x * dimX, uv.y * dimY);
  let glyph = vec2<f32>(
    (px.x / scale) - tx,
    ((dimY - px.y) / scale) - ty
  );

  var dA = 1.0e12;
  var dR = 1.0e12;
  var dG = 1.0e12;
  var dB = 1.0e12;
  var windingParity: u32 = 0u;
  var windingNonZero: i32 = 0;

  for (var i: u32 = 0u; i < selectorSegCount; i = i + 1u) {
    let seg = segBufSelector.data[i];
    let p = seg.p0p1;
    let p0 = p.xy;
    let p1 = p.zw;
    let col = u32(seg.meta.x + 0.5);
    let caps = u32(seg.meta.y + 0.5);
    let dAEdge = segDistanceClamped(glyph, p0, p1);
    let dRgbEdge = max(segDistancePseudo(glyph, p0, p1, caps), dAEdge);
    if (dAEdge < dA) {
      dA = dAEdge;
    }
    if ((col & 1u) != 0u) {
      dR = min(dR, dRgbEdge);
    }
    if ((col & 2u) != 0u) {
      dG = min(dG, dRgbEdge);
    }
    if ((col & 4u) != 0u) {
      dB = min(dB, dRgbEdge);
    }
  }

  for (var i: u32 = 0u; i < windingSegCount; i = i + 1u) {
    let seg = segBufWinding.data[i];
    let p0 = seg.p0p1.xy;
    let p1 = seg.p0p1.zw;
    windingParity = windingParity ^ windingStepParity(glyph, p0, p1);
    windingNonZero = windingNonZero + windingStepNonZero(glyph, p0, p1);
  }

  if (dR > 1.0e11) {
    dR = dA;
  }
  if (dG > 1.0e11) {
    dG = dA;
  }
  if (dB > 1.0e11) {
    dB = dA;
  }

  let insideParity = windingParity != 0u;
  let insideNonZero = windingNonZero != 0;
  let inside = select(insideParity, insideNonZero, useNonZero);
  let sign = select(-1.0, 1.0, inside);
  let r = clamp(0.5 + ((scale * sign * dR) / pxRange), 0.0, 1.0);
  let g = clamp(0.5 + ((scale * sign * dG) / pxRange), 0.0, 1.0);
  let b = clamp(0.5 + ((scale * sign * dB) / pxRange), 0.0, 1.0);
  let a = clamp(0.5 + ((scale * sign * dA) / pxRange), 0.0, 1.0);
  return vec4<f32>(r, g, b, a);
}
|]

genFragmentShader :: GpuShader
genFragmentShader =
  mkGpuShader
    [weslShader|
const MAX_SEGS: u32 = 120u;

struct Seg {
  p0p1: vec4<f32>,
  meta: vec4<f32>, // meta.x=color mask [0..7], meta.y=endpoint cap bits (1=start, 2=end).
};

struct FsU {
  meta0: vec4<f32>, // dimX, dimY, scale, tx
  meta1: vec4<f32>, // ty, pxRange, segCount, pad
  segs: array<Seg, MAX_SEGS>,
};

@group(3) @binding(0) var<uniform> u: FsU;

const EPS: f32 = 1.0e-5;
const EPS_SQ: f32 = EPS * EPS;

fn segDistancePseudo(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>, capBits: u32) -> f32 {
  let v = p1 - p0;
  let vv = max(dot(v, v), EPS_SQ);
  let t = dot(p - p0, v) / vv;
  let cross = abs((v.x * (p.y - p0.y)) - (v.y * (p.x - p0.x)));
  let lineDist = cross / sqrt(vv);
  if (t < 0.0 && (capBits & 1u) == 0u) {
    return lineDist;
  }
  if (t > 1.0 && (capBits & 2u) == 0u) {
    return lineDist;
  }
  let tc = clamp(t, 0.0, 1.0);
  let q = p0 + (tc * v);
  return length(p - q);
}

fn segDistanceClamped(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>) -> f32 {
  let v = p1 - p0;
  let vv = max(dot(v, v), EPS_SQ);
  let tc = clamp(dot(p - p0, v) / vv, 0.0, 1.0);
  let q = p0 + (tc * v);
  return length(p - q);
}

fn windingStepParity(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>) -> u32 {
  let crosses = (p0.y > p.y) != (p1.y > p.y);
  if (!crosses) {
    return 0u;
  }
  let dy = p1.y - p0.y;
  if (abs(dy) <= EPS) {
    return 0u;
  }
  let xInt = p0.x + ((p.y - p0.y) * (p1.x - p0.x) / dy);
  if (xInt > p.x) {
    return 1u;
  }
  return 0u;
}

fn windingStepNonZero(p: vec2<f32>, p0: vec2<f32>, p1: vec2<f32>) -> i32 {
  let isLeft = ((p1.x - p0.x) * (p.y - p0.y)) - ((p.x - p0.x) * (p1.y - p0.y));
  if (p0.y <= p.y) {
    if (p1.y > p.y && isLeft > EPS) {
      return 1;
    }
  } else {
    if (p1.y <= p.y && isLeft < -EPS) {
      return -1;
    }
  }
  return 0;
}

@fragment
fn main(@location(0) uv: vec2<f32>) -> @location(0) vec4<f32> {
  let dimX = max(1.0, u.meta0.x);
  let dimY = max(1.0, u.meta0.y);
  let scale = max(1.0e-6, u.meta0.z);
  let tx = u.meta0.w;
  let ty = u.meta1.x;
  let pxRange = max(1.0, u.meta1.y);
  let segCount = min(MAX_SEGS, u32(u.meta1.z + 0.5));

  let px = vec2<f32>(uv.x * dimX, uv.y * dimY);
  let glyph = vec2<f32>(
    (px.x / scale) - tx,
    ((dimY - px.y) / scale) - ty
  );

  var dA = 1.0e12;
  var dR = 1.0e12;
  var dG = 1.0e12;
  var dB = 1.0e12;
  var windingParity: u32 = 0u;
  var windingNonZero: i32 = 0;

  for (var i: u32 = 0u; i < segCount; i = i + 1u) {
    let seg = u.segs[i];
    let p0 = seg.p0p1.xy;
    let p1 = seg.p0p1.zw;
    let col = u32(seg.meta.x + 0.5);
    let caps = u32(seg.meta.y + 0.5);
    let dAEdge = segDistanceClamped(glyph, p0, p1);
    let dRgbEdge = max(segDistancePseudo(glyph, p0, p1, caps), dAEdge);
    if (dAEdge < dA) {
      dA = dAEdge;
    }
    if ((col & 1u) != 0u) {
      dR = min(dR, dRgbEdge);
    }
    if ((col & 2u) != 0u) {
      dG = min(dG, dRgbEdge);
    }
    if ((col & 4u) != 0u) {
      dB = min(dB, dRgbEdge);
    }
    windingParity = windingParity ^ windingStepParity(glyph, p0, p1);
    windingNonZero = windingNonZero + windingStepNonZero(glyph, p0, p1);
  }

  if (dR > 1.0e11) {
    dR = dA;
  }
  if (dG > 1.0e11) {
    dG = dA;
  }
  if (dB > 1.0e11) {
    dB = dA;
  }

  let useNonZero = u.meta1.w > 0.5;
  let insideParity = windingParity != 0u;
  let insideNonZero = windingNonZero != 0;
  let inside = select(insideParity, insideNonZero, useNonZero);
  let sign = select(-1.0, 1.0, inside);
  let r = clamp(0.5 + ((scale * sign * dR) / pxRange), 0.0, 1.0);
  let g = clamp(0.5 + ((scale * sign * dG) / pxRange), 0.0, 1.0);
  let b = clamp(0.5 + ((scale * sign * dB) / pxRange), 0.0, 1.0);
  let a = clamp(0.5 + ((scale * sign * dA) / pxRange), 0.0, 1.0);
  return vec4<f32>(r, g, b, a);
}
|]

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
  let sample = textureSample(tx, smp, uvCenter);
  let sd = median3(sample.r, sample.g, sample.b);
  let screenPxDistance = screenPxRange(uvCenter, pxRange) * (sd - 0.5);
  let opacity = clamp(screenPxDistance + 0.5, 0.0, 1.0);
  let fg = vec3<f32>(0.92, 0.94, 0.98);
  return vec4<f32>(fg, opacity);
}
|]

fragmentShaderNoHeal :: GpuShader
fragmentShaderNoHeal =
  fragmentShader
