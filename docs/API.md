# Exposed Library API

This file documents the modules listed under `exposed-modules` in `masdiff.cabal`.

## Module `Font`

Harness/font fixture descriptors used by CLI tools and tests.

### Exported types

```haskell
data FontKind = Static | Variable
data FontSource = StaticFont FilePath | VariableFont FilePath [(String, String)]
data FontCase = FontCase
  { fontCaseId :: String
  , fontCaseKind :: FontKind
  , fontCaseSource :: FontSource
  }
```

### Exported values

```haskell
fontFilePath :: FontCase -> FilePath
fontInputArgs :: FontCase -> [String]
fontInputLabel :: FontCase -> String
interHarnessFontCases :: [FontCase]
interOracleFontCases :: [FontCase]
interHarnessGlyphs :: [Char]
```

## Module `MSDF.Types`

Core domain types and smart constructors used across generation, comparison, and rendering.

### Exported type aliases / newtypes / ADTs

```haskell
type AxisMap = Map.Map AxisTag AxisVal

newtype AxisTag = AxisTag { tag :: Text }
newtype AxisVal = AxisVal { value :: Double }

data Mode = Mtsdf

newtype Dim
newtype PxRange
newtype GlyphCode

data FontSrc
  = FontFile { path :: FilePath }
  | VarFontFile { path :: FilePath, axes :: AxisMap }

data GenCfg = GenCfg
  { mode :: Mode
  , dim :: Dim
  , pxr :: PxRange
  , seed :: Int
  , autoframe :: Bool
  , ovlp :: Bool
  }

data Metrics = Metrics
  { adv :: Double
  , bounds :: (Double, Double, Double, Double)
  , scale :: Maybe Double
  , translate :: Maybe (Double, Double)
  , range :: Maybe (Double, Double)
  }

data ImgRGBA8 = ImgRGBA8
  { w :: Int
  , h :: Int
  , px :: ByteString
  }

data GenOut = GenOut
  { img :: ImgRGBA8
  , metrics :: Metrics
  }

data GenErr
  = InvalidCfg String
  | MissingInput String
  | Unsupported String
  | ExecFailed String
  | ParseFailed String
```

`Dim`, `PxRange`, and `GlyphCode` are intentionally exported without constructors.
Use `mkDim`, `mkPxRange`, and `mkGlyphCode` to preserve invariants.

### Exported functions

```haskell
mkDim :: Int -> Either String Dim
unDim :: Dim -> Int

mkPxRange :: Double -> Either String PxRange
unPxRange :: PxRange -> Double

mkGlyphCode :: Int -> Either String GlyphCode
unGlyphCode :: GlyphCode -> Int
showGlyphCodeHex :: GlyphCode -> String

mkImgRGBA8 :: Int -> Int -> ByteString -> Either String ImgRGBA8
```

## Module `MSDF.Generate`

Top-level runtime API for glyph generation, with backend selection.

### Exported types

```haskell
data BackendMode = BackendNative | BackendProcess

data RuntimeCfg = RuntimeCfg
  { backend :: BackendMode
  , msdfgenBin :: FilePath
  }
```

### Exported functions

```haskell
defaultRuntimeCfg :: RuntimeCfg
parseBackendModeEnv :: Maybe String -> Either String BackendMode
generateGlyphIO :: RuntimeCfg -> GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
generateGlyphBatchIO :: RuntimeCfg -> Int -> GenCfg -> FontSrc -> [GlyphCode] -> IO [Either GenErr GenOut]
renderMetrics :: Metrics -> String
```

## Module `MSDF.Native`

Direct native backend entrypoint.

### Exported function

```haskell
generateGlyphNativeIO :: GenCfg -> FontSrc -> GlyphCode -> IO (Either GenErr GenOut)
```

## Module `MSDF.Encode`

PNG and msdfgen RGBA encode/decode helpers.

### Exported functions

```haskell
encodePngRGBA8 :: ImgRGBA8 -> ByteString
decodePngRGBA8 :: ByteString -> Either String ImgRGBA8
readPngRGBA8File :: FilePath -> IO (Either String ImgRGBA8)
writePngRGBA8File :: FilePath -> ImgRGBA8 -> IO ()

encodeMsdfgenRgba :: ImgRGBA8 -> ByteString
decodeMsdfgenRgba :: ByteString -> Either String ImgRGBA8
writeMsdfgenRgbaFile :: FilePath -> ImgRGBA8 -> IO ()
```

## Module `MSDF.Compare`

Image diff stats and strict gating for parity/validation checks.

### Exported types

```haskell
data DiffStats = DiffStats
  { pxCount :: Int
  , chCount :: Int
  , maxAbs :: Int
  , maxCh :: (Int, Int, Int, Int)
  , p99Abs :: Int
  , meanAbs :: Double
  , mismatch :: Int
  }

data DiffGate = DiffGate
  { maxChLimit :: Int
  , p99Limit :: Int
  , meanLimit :: Double
  }
```

### Exported functions

```haskell
strictGate :: DiffGate
passesGate :: DiffGate -> DiffStats -> Bool
diffRGBA8 :: ImgRGBA8 -> ImgRGBA8 -> Either String DiffStats
```

## Module `MSDF.Manifest`

Manifest model + parser for fixture corpora.

### Exported types

```haskell
data ManifestMeta = ManifestMeta
  { dim :: Dim
  , pxr :: PxRange
  , seed :: Int
  }

data ManifestRow = ManifestRow
  { fontCase :: String
  , glyph :: GlyphCode
  , glyphHex :: String
  , outputPng :: FilePath
  , src :: FontSrc
  }

data Manifest = Manifest
  { meta :: ManifestMeta
  , rows :: [ManifestRow]
  }
```

### Exported functions

```haskell
loadManifest :: FilePath -> IO (Either String Manifest)
manifestCfg :: ManifestMeta -> GenCfg
```

## Module `MSDF.TextRender`

CPU-side MTSDF shading and composition utilities for preview rendering.

### Exported types

```haskell
data ScreenPxRange
  = AutoPxRange Double
  | FixedPxRange Double

data ShaderCfg = ShaderCfg
  { spr :: ScreenPxRange
  , alphaFallback :: Bool
  , fallbackThreshold :: Double
  , ssaa :: Int
  }
```

### Exported functions

```haskell
mkShaderCfg :: ScreenPxRange -> Bool -> Double -> Either String ShaderCfg
shadeMtsdfImg :: ShaderCfg -> ImgRGBA8 -> Either String ImgRGBA8
shadeMtsdfImgTo :: ShaderCfg -> Int -> Int -> ImgRGBA8 -> Either String ImgRGBA8
resampleBilinear :: Int -> Int -> ImgRGBA8 -> Either String ImgRGBA8
solidImg :: Int -> Int -> (Word8, Word8, Word8, Word8) -> Either String ImgRGBA8
hcatWithGap :: Int -> [ImgRGBA8] -> Either String ImgRGBA8
addBorder :: Int -> ImgRGBA8 -> Either String ImgRGBA8
```
