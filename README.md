# ma(t)sdiff (cringe vibes)

ok so this is a cozy pure Haskell MSDF/MTSDF generator for TrueType fonts with terminal-gremlin energy. it parses `glyf/loca/cmap` outlines and produces per‑glyph MSDF (RGB) or MTSDF (RGBA, alpha = true SDF) bitmaps plus metrics and kerning (legacy `kern` and GPOS pair adjustments). yes, it’s real.

## Features (cringe but real)

- Pure Haskell pipeline (no external CLI/tools), comfy and deterministic (i promise).
- TrueType outline parsing (simple + composite glyphs).
- MSDF raster generation per glyph (packed RGB bytes).
- MTSDF output mode (RGBA, alpha channel = true signed distance).
- Kerning support via `kern` and GPOS pair adjustment (lookup type 2).
- Vertical metrics via `vhea/vmtx` when present.
- Variable font support (fvar/avar/gvar + HVAR/VVAR/MVAR for metrics).
- Optional atlas packing with per-glyph UV placement.

## Usage (pls)

```haskell
import MSDF.Generated (generateMSDF, generateMSDFOrThrow, generateMTSDFOrThrow)
import MSDF.TTF.Parser (ParseError(..))

main :: IO ()
main = do
  result <- generateMSDF "path/to/font.ttf"
  case result of
    Left err -> putStrLn (err.context ++ ": " ++ err.message)
    Right atlas -> print (atlas.fontName)

-- Or throw on parse errors:
main = do
  atlas <- generateMSDFOrThrow "path/to/font.ttf"
  print (atlas.fontName)

-- MTSDF (RGBA, alpha = true SDF):
main = do
  atlas <- generateMTSDFOrThrow "path/to/font.ttf"
  print (atlas.fontName)
```

### Custom config

```haskell
import MSDF.Generated (generateMSDFWithConfig)
import MSDF.MSDF (defaultMSDFConfig, GlyphSet(..))
import qualified MSDF.MSDF as MSDF
import MSDF.Types (BitmapFormat(..))
import MSDF.TTF.Parser (ParseError(..))

main :: IO ()
main = do
  let cfg = defaultMSDFConfig
        { MSDF.pixelSize = 24
        , MSDF.glyphSet = GlyphSetCodepoints [65,66,67]
        , MSDF.parallelism = 64 -- optional: chunk size for parallel rendering
        , MSDF.variations = [("wght", 700)] -- optional: variable font axes
        , MSDF.packAtlas = True
        , MSDF.atlasPadding = 1
        , MSDF.outputFormat = BitmapMSDF -- or BitmapMTSDF
        }
  result <- generateMSDFWithConfig cfg "path/to/font.ttf"
  case result of
    Left err -> putStrLn (err.context ++ ": " ++ err.message)
    Right atlas -> print (length (atlas.glyphs))
```

Variable font example (Inter Variable, weight 400 vs 700):

```haskell
let cfgRegular = defaultMSDFConfig { MSDF.variations = [("wght", 400)] }
let cfgBold = defaultMSDFConfig { MSDF.variations = [("wght", 700)] }
atlasRegular <- generateMSDFWithConfig cfgRegular "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
atlasBold <- generateMSDFWithConfig cfgBold "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
```

## Tests

```sh
cabal test msdf-tests
```

## API overview (quick peek)

See `docs/api.md` for a concise overview of the public modules, data ordering
invariants, and determinism guarantees.
See `docs/render_guide.md` for a concrete MSDF rendering reference (GLSL/WGSL).
See `docs/reference_renderer.md` for a minimal rendering sample.
See `docs/versioning.md` for versioning and changelog policy.

## Coordinate conventions (very explicit, pls read)

masdiff uses **font-style, y-up** math for glyph metrics and quads:

- `GlyphMSDF.bbox`, `advance`, `bearingX/bearingY` are in pixel space with **+Y up**.
- `glyphQuad` returns a quad in **y-up** coordinates for a given baseline `(penX, penY)`.
- The MSDF bitmap row `y = 0` corresponds to the **lowest** glyph-space Y.
- `GlyphPlacement.x/y` and `glyphUV` use a **bottom-left** UV origin (`v` increases upward).

If your renderer expects **top-left UVs** (D3D/Metal/WGPU-style), flip V or use
`glyphUVTopLeft`. If your screen space is **y-down**, reflect the quad around the
baseline or use `glyphQuadYDown`. SDL_gpu uses **top-left** texture coordinates,
but masdiff’s atlas data is bottom-up, so the SDL example uses `glyphUV`
directly. (If you flip the atlas on upload, use `glyphUVTopLeft` instead.)

Practical recipes:

**OpenGL-style (y-up, UV bottom-left)**

```text
(x0, y0, x1, y1) = glyphQuad(glyph, (penX, penY))
(u0, v0, u1, v1) = glyphUV(glyph)
drawQuad((x0, y0, x1, y1), (u0, v0, u1, v1))
```

**WGPU/D3D/Metal-style (y-down, UV top-left)**

```text
(x0, y0, x1, y1) = glyphQuadYDown(glyph, (penX, penY))
(u0, v0, u1, v1) = glyphUVTopLeft(glyph)
drawQuad((x0, y0, x1, y1), (u0, v0, u1, v1))
```

**SDL_gpu (y-up, UV bottom-left)**

```text
(x0, y0, x1, y1) = glyphQuad(glyph, (penX, penY))
(u0, v0, u1, v1) = glyphUV(glyph)
drawQuad((x0, y0, x1, y1), (u0, v0, u1, v1))
```

Sampler guidance is available in `MSDF.Render.msdfSamplerHints` and coordinate
metadata is exposed via `atlasOrigin`, `uvOrigin`, and `glyphQuadSpace`.

## Pseudocode: rendering MSDF/MTSDF glyphs (pls dont mess it up)

This is a high-level sketch of how to use the generated MSDF/MTSDF data to render text.
It assumes you have a shader that decodes MSDF (or MTSDF alpha) and outputs alpha.
Helper utilities are available in `MSDF.Render` (`glyphQuad`, `glyphUV`, `pixelRange`).
If you render the same glyphs at multiple sizes, use `MSDF.MSDF.prepareGlyphCache`
and `renderGlyphMSDFCached` to reuse parsed outlines.

```text
atlas = generateMSDFOrThrow("path/to/font.ttf")

-- Build a glyph texture for each glyph (or use packed atlas).
if atlas.atlas != None:
  -- atlas.atlas.format is BitmapMSDF (RGB) or BitmapMTSDF (RGBA)
  uploadTexture("atlas", atlas.atlas.width, atlas.atlas.height, atlas.atlas.pixels)
else:
  for glyph in atlas.glyphs:
    if glyph.bitmap.width == 0:
      continue
    uploadTexture(glyph.index, glyph.bitmap.width, glyph.bitmap.height, glyph.bitmap.pixels)

-- Layout text.
penX = 0
penY = baseline
for codepoint in text:
  glyphIndex = lookupCodepoint(atlas, codepoint)
  if glyphIndex == None:
    continue
  glyph = atlas.glyphs[glyphIndex]

  -- Apply kerning (if you have a previous glyph).
  if prevGlyphIndex != None:
    penX += lookupKerning(atlas, prevGlyphIndex, glyphIndex)

  -- Compute quad in screen space.
  x0 = penX + glyph.bbox.xMin - glyph.bitmap.offsetX
  y0 = penY - glyph.bbox.yMax - glyph.bitmap.offsetY
  x1 = x0 + glyph.bitmap.width
  y1 = y0 + glyph.bitmap.height

  -- Draw quad textured with the glyph's MSDF bitmap.
  if glyph.placement != None:
    uv = (glyph.placement.u0, glyph.placement.v0, glyph.placement.u1, glyph.placement.v1)
    texture = "atlas"
  else:
    uv = (0,0,1,1)
    texture = glyph.index
  drawQuad(texture = texture, position = (x0, y0, x1, y1), uv = uv)

  -- Advance pen.
  penX += glyph.advance
prevGlyphIndex = glyphIndex
```

MSDF shader idea (pseudocode):

```text
sample = texture(msdfTex, uv).rgb
sd = median(sample.r, sample.g, sample.b) - 0.5
alpha = clamp(sd * pxRange + 0.5, 0, 1)
output = vec4(textColor.rgb, textColor.a * alpha)
```

### Shader expectations (example, yes im serious)

When rendering MSDF glyphs, the shader is expected to:

- Sample the MSDF texture in **linear color space** (disable sRGB sampling for the MSDF).
- Use **bilinear filtering** (nearest gives jagged edges).
- Compute a pixel-range factor based on glyph scale and the MSDF `range` used at generation time.
- Use the **median** of RGB channels to get the signed distance.
- Convert distance to alpha with a smooth range to avoid hard edges.

If you generated **MTSDF** (`BitmapMTSDF`), the alpha channel already stores the
true signed distance. In that case, use `sample.a` instead of the RGB median.

Example GLSL-like fragment shader:

```glsl
uniform sampler2D msdfTex;
uniform vec4 textColor;
uniform float pxRange;  // screen-space pixel range, e.g. (atlasRange * scale)

float median(float r, float g, float b) {
  return max(min(r, g), min(max(r, g), b));
}

void main() {
  vec3 sample = texture(msdfTex, vUV).rgb;
  float sd = median(sample.r, sample.g, sample.b) - 0.5;
  float alpha = clamp(sd * pxRange + 0.5, 0.0, 1.0);
  outColor = vec4(textColor.rgb, textColor.a * alpha);
}
```

MTSDF variation (alpha channel is the true SDF):

```glsl
vec4 sample = texture(msdfTex, vUV);
float sd = sample.a - 0.5;
float alpha = clamp(sd * pxRange + 0.5, 0.0, 1.0);
outColor = vec4(textColor.rgb, textColor.a * alpha);
```

Notes:

- `pxRange` depends on how the glyph is scaled on screen. If MSDF was generated with `range = 4` and the glyph is scaled by `S`, a common approximation is `pxRange = 4 * S`.
- Use **premultiplied alpha** if your pipeline expects it.

Example WESL fragment shader:

```wesl
@group(2) @binding(0) var msdfTex : texture_2d<f32>;
@group(2) @binding(1) var msdfSampler : sampler;
@group(3) @binding(0) var<uniform> uTextColor : vec4<f32>;
@group(3) @binding(1) var<uniform> uPxRange : f32;

fn median(r: f32, g: f32, b: f32) -> f32 {
  return max(min(r, g), min(max(r, g), b));
}

@fragment
fn fs_main(@location(0) vUV: vec2<f32>) -> @location(0) vec4<f32> {
  let sample = textureSample(msdfTex, msdfSampler, vUV).rgb;
  let sd = median(sample.r, sample.g, sample.b) - 0.5;
  let dist = sd * uPxRange;
  let w = fwidth(dist);
  let alpha = smoothstep(-w, w, dist);
  return vec4<f32>(uTextColor.rgb, uTextColor.a * alpha);
}
```

Note: SDL_gpu (SPIR-V) expects fragment textures in set 2 and uniform buffers in
set 3, which is why the WESL example uses `@group(2)`/`@group(3)`.

## SDL 3.4 + WESL (SDL_gpu) example (yes, actually works)

Short and specific path:

- Generate a packed atlas in Haskell.
- Convert RGB → RGBA8 (SDL_gpu doesn’t accept RGB8).
- Build **y-up** quads + **bottom-left** UVs (inset by half a texel to avoid bleed).
- Compile WESL → SPIR-V and pass the bytes to `SDL_CreateGPUShader`.

Haskell side (atlas + vertices in clip space):

```haskell
import qualified Data.Array.Unboxed as UA
import Data.Array ((!))
import Data.Word (Word8)

import MSDF.Generated (generateMSDFWithConfig)
import qualified MSDF.MSDF as MSDF
import MSDF.MSDF (GlyphSet(..))
import MSDF.Render (glyphQuad, glyphUV, pixelRangeForAtlas)
import MSDF.Types (GlyphMSDF(..), lookupCodepoint)

rgbToRgbaList :: UA.UArray Int Word8 -> [Word8]
rgbToRgbaList arr = go (UA.elems arr)
  where
    go (r:g:b:rest) = r:g:b:255:go rest
    go [] = []
    go _ = error "rgbToRgbaList: bad RGB length"

toClip :: (Double, Double) -> (Double, Double) -> (Float, Float)
toClip (screenW, screenH) (x, y) =
  ( realToFrac (x / (screenW * 0.5) - 1)
  , realToFrac (y / (screenH * 0.5) - 1)
  )

insetUV :: (Double, Double) -> (Double, Double, Double, Double) -> (Double, Double, Double, Double)
insetUV (du, dv) (u0, v0, u1, v1) =
  let insetU = du * 0.5
      insetV = dv * 0.5
  in (u0 + insetU, v0 + insetV, u1 - insetU, v1 - insetV)

quadVerts :: (Double, Double) -> (Double, Double) -> (Double, Double) -> GlyphMSDF -> [Float]
quadVerts screen pen texel glyph =
  let (x0, y0, x1, y1) = glyphQuad glyph pen
      (u0, v0, u1, v1) = insetUV texel (glyphUV glyph)
      (cx0, cy0) = toClip screen (x0, y0)
      (cx1, cy1) = toClip screen (x1, y1)
  in [ cx0, cy0, realToFrac u0, realToFrac v0
     , cx1, cy0, realToFrac u1, realToFrac v0
     , cx1, cy1, realToFrac u1, realToFrac v1
     , cx0, cy0, realToFrac u0, realToFrac v0
     , cx1, cy1, realToFrac u1, realToFrac v1
     , cx0, cy1, realToFrac u0, realToFrac v1
     ]

main :: IO ()
main = do
  let cfg = MSDF.defaultMSDFConfig
        { MSDF.pixelSize = 48
        , MSDF.range = 8
        , MSDF.atlasPadding = 12
        , MSDF.glyphSet = GlyphSetCodepoints [fromEnum 'H']
        , MSDF.packAtlas = True
        }
  Right atlas <- generateMSDFWithConfig cfg "assets/Inter/Inter-VariableFont_opsz,wght.ttf"
  let Just img = atlas.atlas
      rgbaBytes = rgbToRgbaList img.pixels
      pxRange = pixelRangeForAtlas atlas 32
      screen = (1280, 720)
      pen = (32, 720 - 96)
      texel = (1 / fromIntegral img.width, 1 / fromIntegral img.height)
      Just gi = lookupCodepoint atlas (fromEnum 'H')
      glyph = atlas.glyphs ! gi
      verts = quadVerts screen pen texel glyph
  -- rgbaBytes -> upload to SDL_gpu texture (RGBA8)
  -- verts -> upload to SDL_gpu vertex buffer (x,y,u,v)
  -- pxRange -> fragment uniform
  print (length rgbaBytes, length verts, pxRange)
```

SDL_gpu WESL (vertex + fragment, single uniform block):

```wesl
struct VSIn {
  @location(0) pos : vec2<f32>;
  @location(1) uv : vec2<f32>;
};

struct VSOut {
  @builtin(position) pos : vec4<f32>;
  @location(0) uv : vec2<f32>;
};

@vertex
fn vs_main(input: VSIn) -> VSOut {
  var out: VSOut;
  out.pos = vec4<f32>(input.pos, 0.0, 1.0);
  out.uv = input.uv;
  return out;
}

struct FragUniforms {
  textColor : vec4<f32>;
  params : vec4<f32>; // params.x = pxRange, params.y = 0 (MSDF) / 1 (MTSDF)
};

@group(2) @binding(0) var msdfTex : texture_2d<f32>;
@group(2) @binding(1) var msdfSampler : sampler;
@group(3) @binding(0) var<uniform> uFrag : FragUniforms;

fn median(r: f32, g: f32, b: f32) -> f32 {
  return max(min(r, g), min(max(r, g), b));
}

@fragment
fn fs_main(@location(0) vUV: vec2<f32>) -> @location(0) vec4<f32> {
  let sample = textureSample(msdfTex, msdfSampler, vUV);
  var sd = median(sample.r, sample.g, sample.b) - 0.5;
  if (uFrag.params.y > 0.5) {
    sd = sample.a - 0.5;
  }
  let alpha = clamp(sd * uFrag.params.x + 0.5, 0.0, 1.0);
  return vec4<f32>(uFrag.textColor.rgb, uFrag.textColor.a * alpha);
}
```

SDL_gpu side (C, SDL 3.4):

```c
typedef struct Vertex {
  float x, y;
  float u, v;
} Vertex;

typedef struct FragUniforms {
  float textColor[4];
  float params[4]; /* params.x = pxRange, params.y = 0 (MSDF) / 1 (MTSDF) */
} FragUniforms;

SDL_GPUDevice *gpu = SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV, true, NULL);
SDL_ClaimWindowForGPUDevice(gpu, window);

SDL_GPUShader *vs = SDL_CreateGPUShader(gpu, &(SDL_GPUShaderCreateInfo){
  .code = vs_spv, .code_size = vs_spv_size,
  .entrypoint = "vs_main",
  .format = SDL_GPU_SHADERFORMAT_SPIRV,
  .stage = SDL_GPU_SHADERSTAGE_VERTEX,
  .num_samplers = 0,
  .num_uniform_buffers = 0
});

SDL_GPUShader *fs = SDL_CreateGPUShader(gpu, &(SDL_GPUShaderCreateInfo){
  .code = fs_spv, .code_size = fs_spv_size,
  .entrypoint = "fs_main",
  .format = SDL_GPU_SHADERFORMAT_SPIRV,
  .stage = SDL_GPU_SHADERSTAGE_FRAGMENT,
  .num_samplers = 1,
  .num_uniform_buffers = 1
});

SDL_GPUVertexBufferDescription vb = {
  .slot = 0,
  .pitch = sizeof(Vertex),
  .input_rate = SDL_GPU_VERTEXINPUTRATE_VERTEX
};
SDL_GPUVertexAttribute attrs[2] = {
  { .location = 0, .buffer_slot = 0, .format = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = 0 },
  { .location = 1, .buffer_slot = 0, .format = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = sizeof(float) * 2 }
};

SDL_GPUColorTargetBlendState blend = {
  .src_color_blendfactor = SDL_GPU_BLENDFACTOR_ONE,
  .dst_color_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
  .color_blend_op = SDL_GPU_BLENDOP_ADD,
  .src_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE,
  .dst_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
  .alpha_blend_op = SDL_GPU_BLENDOP_ADD,
  .color_write_mask = SDL_GPU_COLORCOMPONENT_R | SDL_GPU_COLORCOMPONENT_G |
                      SDL_GPU_COLORCOMPONENT_B | SDL_GPU_COLORCOMPONENT_A,
  .enable_blend = true,
  .enable_color_write_mask = true
};

SDL_GPUTextureFormat swapFmt = SDL_GetGPUSwapchainTextureFormat(gpu, window);
SDL_GPUColorTargetDescription colorDesc = { .format = swapFmt, .blend_state = blend };
SDL_GPUGraphicsPipelineCreateInfo pipeInfo = {
  .vertex_shader = vs,
  .fragment_shader = fs,
  .vertex_input_state = { &vb, 1, attrs, 2 },
  .primitive_type = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
  .rasterizer_state = { .fill_mode = SDL_GPU_FILLMODE_FILL, .cull_mode = SDL_GPU_CULLMODE_NONE,
                        .front_face = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE, .enable_depth_clip = true },
  .multisample_state = { .sample_count = SDL_GPU_SAMPLECOUNT_1 },
  .depth_stencil_state = { .enable_depth_test = false, .enable_depth_write = false },
  .target_info = { &colorDesc, 1, SDL_GPU_TEXTUREFORMAT_INVALID, false }
};
SDL_GPUGraphicsPipeline *pipeline = SDL_CreateGPUGraphicsPipeline(gpu, &pipeInfo);

SDL_GPUTexture *atlasTex = SDL_CreateGPUTexture(gpu, &(SDL_GPUTextureCreateInfo){
  .type = SDL_GPU_TEXTURETYPE_2D,
  .format = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
  .usage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
  .width = atlasWidth, .height = atlasHeight,
  .layer_count_or_depth = 1, .num_levels = 1,
  .sample_count = SDL_GPU_SAMPLECOUNT_1
});
SDL_GPUSampler *sampler = SDL_CreateGPUSampler(gpu, &(SDL_GPUSamplerCreateInfo){
  .min_filter = SDL_GPU_FILTER_LINEAR,
  .mag_filter = SDL_GPU_FILTER_LINEAR,
  .mipmap_mode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
  .address_mode_u = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
  .address_mode_v = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
  .address_mode_w = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
});

SDL_GPUTransferBuffer *tbuf = SDL_CreateGPUTransferBuffer(gpu, &(SDL_GPUTransferBufferCreateInfo){
  .usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD, .size = atlasSizeBytes
});
void *mapped = SDL_MapGPUTransferBuffer(gpu, tbuf, false);
SDL_memcpy(mapped, atlasRGBA, atlasSizeBytes);
SDL_UnmapGPUTransferBuffer(gpu, tbuf);

SDL_GPUCommandBuffer *cmd = SDL_AcquireGPUCommandBuffer(gpu);
SDL_GPUCopyPass *copy = SDL_BeginGPUCopyPass(cmd);
SDL_UploadToGPUTexture(copy,
  &(SDL_GPUTextureTransferInfo){ tbuf, 0, atlasWidth, atlasHeight },
  &(SDL_GPUTextureRegion){ atlasTex, 0, 0, 0, 0, 0, atlasWidth, atlasHeight, 1 },
  false);
SDL_EndGPUCopyPass(copy);

SDL_GPUTexture *swapchainTex = NULL;
Uint32 swapW = 0, swapH = 0;
SDL_WaitAndAcquireGPUSwapchainTexture(cmd, window, &swapchainTex, &swapW, &swapH);
SDL_GPUColorTargetInfo colorTarget = {
  .texture = swapchainTex,
  .load_op = SDL_GPU_LOADOP_CLEAR,
  .store_op = SDL_GPU_STOREOP_STORE,
  .clear_color = (SDL_FColor){0, 0, 0, 1}
};
SDL_GPURenderPass *pass = SDL_BeginGPURenderPass(cmd, &colorTarget, 1, NULL);
SDL_BindGPUGraphicsPipeline(pass, pipeline);
SDL_GPUBufferBinding vbind = { .buffer = vertexBuffer, .offset = 0 };
SDL_BindGPUVertexBuffers(pass, 0, &vbind, 1);
SDL_GPUTextureSamplerBinding sbind = { .texture = atlasTex, .sampler = sampler };
SDL_BindGPUFragmentSamplers(pass, 0, &sbind, 1);
FragUniforms fu = { {1, 1, 1, 1}, {pxRange, 0, 0, 0} }; /* set params.y=1 for MTSDF */
SDL_PushGPUFragmentUniformData(cmd, 0, &fu, sizeof(fu));
SDL_DrawGPUPrimitives(pass, vertexCount, 1, 0, 0);
SDL_EndGPURenderPass(pass);
SDL_SubmitGPUCommandBuffer(cmd);
```

Notes:

- Use `SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM` (no RGB8).
- Sample in linear space (avoid sRGB formats).
- `glyphQuad` + `glyphUV` keeps the math consistent with SDL’s NDC (y-up) coords.
- Full runnable example (Haskell generator + SDL renderer) lives in `examples/sdl_gpu_wesl` and uses spirdo to compile WESL → SPIR‑V.
- From repo root, run `just demo` to build + capture a screenshot of the SDL example (see `examples/sdl_gpu_wesl/README.md`). Use `SDL_GPU_DRIVER=vulkan` if you need to force the backend.
- If you see tiny specks at sharp corners, lower `msdfCorrectionThreshold` (e.g. `0.05` → `0.02`), raise `speckleThreshold` (e.g. `1.0`), and increase `range`/`atlasPadding`.

## Limitations (sad but true)

- TrueType outlines only (no CFF).
- Variable fonts support glyf variations and metrics via HVAR/VVAR/MVAR.
  Most other MVAR tags are not implemented.
- GPOS support includes Pair Adjustment (format 1/2), MarkToBase (type 4), and MarkToMark (type 6).
  Other lookups (mark-to-ligature, contextual, device/variation anchors) are not implemented.
- Edge-coloring correction is heuristic and not a full msdfgen parity pass.

ok thats it. happy rendering, dont break it.

## Benchmarks

```sh
cabal run msdf-bench -- --pixel-size 32 --glyphs 256
```

Glyph selection:

- `GlyphSetAll` renders all glyphs.
- `GlyphSetNone` renders only metrics (no bitmaps).
- `GlyphSetCodepoints [...]` renders a subset.

## Assets and licensing

The repository includes the Inter font for test coverage under its own license (OFL). See `assets/Inter/OFL.txt`.

## License

MIT. See `LICENSE`.
