# Reference renderer (minimal)

This is a minimal rendering sketch showing how to feed `MSDFAtlas` data into a
GPU pipeline. It uses a single quad per glyph and the MSDF shader from the
render guide.

## CPU-side layout (pseudocode)

```text
penX = 0
penY = baseline
prev = None

for codepoint in text:
  glyphIndex = lookupCodepoint(atlas, codepoint)
  if glyphIndex == None:
    continue
  glyph = atlas.glyphs[glyphIndex]

  if prev != None:
    penX += lookupKerning(atlas, prev, glyphIndex)

  quad = glyphQuad(glyph, (penX, penY))
  uv = glyphUV(glyph)  -- bottom-left UV origin
  -- If your renderer uses top-left UVs, use glyphUVTopLeft instead.

  pushInstance(quad, uv, glyphIndex)
  penX += glyph.advance
  prev = glyphIndex
```

## Minimal WGSL pair

```wgsl
struct VertexIn {
  @location(0) pos : vec2<f32>;
  @location(1) uv : vec2<f32>;
};

struct VertexOut {
  @builtin(position) position : vec4<f32>;
  @location(0) vUV : vec2<f32>;
};

@vertex
fn vs_main(in: VertexIn) -> VertexOut {
  var out: VertexOut;
  out.position = vec4<f32>(in.pos, 0.0, 1.0);
  out.vUV = in.uv;
  return out;
}

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
  let sd = 0.5 - median(sample.r, sample.g, sample.b);
  let dist = sd * uPxRange;
  let w = fwidth(dist);
  let alpha = smoothstep(-w, w, dist);
  return vec4<f32>(uTextColor.rgb, uTextColor.a * alpha);
}
```

Notes:

- Use linear sampling and disable sRGB for MSDF textures.
- `uPxRange` should be computed using `pixelRange` or `pixelRangeForAtlas`.
- Use atlas UVs (`GlyphPlacement`) when `MSDFConfig.packAtlas` is enabled.
