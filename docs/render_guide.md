# Render guide

This guide explains how to render masdiff MSDF output in a typical GPU pipeline.
It assumes you already have an `MSDFAtlas` and know how to draw textured quads.

## Data model

- `GlyphMSDF.bitmap` contains an RGB MSDF texture (one glyph per bitmap).
- `GlyphMSDF.placement` is `Just` when `MSDFConfig.packAtlas` is enabled and
  describes UVs inside the packed atlas.
- `GlyphMSDF.bbox` is in **pixels**, already scaled to `MSDFConfig.pixelSize`.
- `GlyphMSDF.advance` and `GlyphMSDF.bearingX` are also in **pixels**.

Helper utilities in `MSDF.Render`:

- `glyphQuad` computes `(x0, y0, x1, y1)` quad bounds from a pen position.
- `glyphUV` returns `(u0, v0, u1, v1)` based on the atlas placement.
- `glyphQuadYDown` / `glyphUVTopLeft` handle common coordinate flips.
- `pixelRange` converts a font MSDF `range` and screen scale to a shader value.
- `scaleForPixelSize` and `pixelRangeForAtlas` help normalize for a target pixel size.

## Coordinate conventions (explicit)

masdiff uses **font-style, y-up** math for glyph metrics and quads:

- `GlyphMSDF.bbox`, `advance`, and `bearingX/bearingY` are in pixel space with **+Y up**.
- `glyphQuad` returns a quad in **y-up** coordinates for a given baseline `(penX, penY)`.

Bitmap/atlas coordinates are consistent with that y-up convention:

- The MSDF bitmap row `y = 0` corresponds to the **lowest** glyph-space Y.
- `GlyphPlacement.x/y` and `glyphUV` use a **bottom-left** origin (`v` increases upward).

If your renderer expects **top-left UVs** (D3D/Metal/WGPU-style), flip V:

```
let (u0, v0, u1, v1) = glyphUV glyph
let uvTopLeft = (u0, 1 - v1, u1, 1 - v0)
-- or use glyphUVTopLeft
```

If your screen space is **y-down**, reflect the quad around the baseline:

```
let (x0, y0, x1, y1) = glyphQuad glyph (penX, penY)
let quadYDown = (x0, 2*penY - y1, x1, 2*penY - y0)
-- or use glyphQuadYDown
```

These conversions are deterministic; mixing y-up and y-down conventions is the
most common source of “random letters” when sampling a packed atlas.

## Pixel-range factor

If MSDFs were generated with `range = 4`, and you scale glyphs on screen by `S`,
a typical pixel-range value is:

```
pxRange = pixelRange range S
```

If you render at native pixel size (1 font pixel == 1 screen pixel), then
`S = 1.0` and `pxRange = range`.

## Fragment shader logic

MSDF decoding uses the **median** of RGB channels. The signed distance is
centered around `0.5` in texture space.

### GLSL (fragment)

```glsl
uniform sampler2D uMsdfTex;
uniform vec4 uTextColor;
uniform float uPxRange;

float median(float r, float g, float b) {
  return max(min(r, g), min(max(r, g), b));
}

void main() {
  vec3 sample = texture(uMsdfTex, vUV).rgb;
  float sd = median(sample.r, sample.g, sample.b) - 0.5;
  float alpha = clamp(sd * uPxRange + 0.5, 0.0, 1.0);
  fragColor = vec4(uTextColor.rgb, uTextColor.a * alpha);
}
```

### WGSL (fragment)

```wgsl
@group(0) @binding(0) var msdfTex : texture_2d<f32>;
@group(0) @binding(1) var msdfSampler : sampler;
@group(0) @binding(2) var<uniform> uTextColor : vec4<f32>;
@group(0) @binding(3) var<uniform> uPxRange : f32;

fn median(r: f32, g: f32, b: f32) -> f32 {
  return max(min(r, g), min(max(r, g), b));
}

@fragment
fn fs_main(@location(0) vUV: vec2<f32>) -> @location(0) vec4<f32> {
  let sample = textureSample(msdfTex, msdfSampler, vUV).rgb;
  let sd = median(sample.r, sample.g, sample.b) - 0.5;
  let alpha = clamp(sd * uPxRange + 0.5, 0.0, 1.0);
  return vec4<f32>(uTextColor.rgb, uTextColor.a * alpha);
}
```

### WESL (fragment)

```
@group(0) @binding(0) var msdfTex : texture_2d<f32>;
@group(0) @binding(1) var msdfSampler : sampler;
@group(0) @binding(2) var<uniform> uTextColor : vec4<f32>;
@group(0) @binding(3) var<uniform> uPxRange : f32;

fn median(r: f32, g: f32, b: f32) -> f32 {
  return max(min(r, g), min(max(r, g), b));
}

@fragment
fn fs_main(@location(0) vUV: vec2<f32>) -> @location(0) vec4<f32> {
  let sample = textureSample(msdfTex, msdfSampler, vUV).rgb;
  let sd = median(sample.r, sample.g, sample.b) - 0.5;
  let alpha = clamp(sd * uPxRange + 0.5, 0.0, 1.0);
  return vec4<f32>(uTextColor.rgb, uTextColor.a * alpha);
}
```

## Rendering notes

- Sample MSDF textures in **linear color space** (disable sRGB sampling).
- Use **bilinear filtering** for MSDF textures.
- Premultiplied alpha is recommended if your pipeline expects it.
- To avoid artifacts, keep MSDF `range` between 2 and 8 pixels.
