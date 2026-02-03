struct FragUniforms {
  textColor: vec4<f32>;
  params: vec4<f32>; // params.x = pxRange, params.y = 0 (MSDF) / 1 (MTSDF), params.z = debug (alpha-only)
  atlasSize: vec2<f32>;
  screenSize: vec2<f32>;
};

// SDL GPU (SPIR-V) expects fragment textures in set=2 and uniforms in set=3.
@group(2) @binding(0) var msdfTex: texture_2d<f32>;
@group(2) @binding(1) var msdfSampler: sampler;
@group(3) @binding(0) var<uniform> uFrag: FragUniforms;

fn median(r: f32, g: f32, b: f32) -> f32 {
  return max(min(r, g), min(max(r, g), b));
}

struct FSIn {
  @location(0) uv: vec2<f32>;
  @builtin(position) pos: vec4<f32>;
};

fn debugChannel(sample: vec4<f32>, mode: f32, isMtsdf: bool, pos: vec2<f32>, screen: vec2<f32>) -> f32 {
  if (mode < 1.5) {
    if (isMtsdf) {
      return sample.a;
    }
    return median(sample.r, sample.g, sample.b);
  }
  if (mode < 2.5) {
    return sample.r;
  }
  if (mode < 3.5) {
    return sample.g;
  }
  if (mode < 4.5) {
    return sample.b;
  }
  if (mode < 5.5) {
    return median(sample.r, sample.g, sample.b);
  }
  if (mode < 6.5) {
    let half = screen * 0.5;
    let left = pos.x < half.x;
    let top = pos.y < half.y;
    if (top && left) {
      return sample.r;
    }
    if (top && !left) {
      return sample.g;
    }
    if (!top && left) {
      return sample.b;
    }
    if (isMtsdf) {
      return sample.a;
    }
    return median(sample.r, sample.g, sample.b);
  }
  if (isMtsdf) {
    return step(0.5, sample.a);
  }
  return step(0.5, median(sample.r, sample.g, sample.b));
}

@fragment
fn fs_main(input: FSIn) -> @location(0) vec4<f32> {
  let sample = textureSample(msdfTex, msdfSampler, input.uv);
  let isMtsdf = uFrag.params.y > 0.5;
  if (uFrag.params.z > 0.5) {
    let a = debugChannel(sample, uFrag.params.z, isMtsdf, input.pos.xy, uFrag.screenSize);
    return vec4<f32>(a, a, a, 1.0);
  }
  var sd = median(sample.r, sample.g, sample.b) - 0.5;
  if (isMtsdf) {
    sd = sample.a - 0.5;
  }
  let texelFwidth = fwidth(input.uv) * uFrag.atlasSize;
  let screenPxRange = uFrag.params.x * max(texelFwidth.x, texelFwidth.y);
  let dist = sd * screenPxRange;
  let w = fwidth(dist);
  let alpha = smoothstep(-w, w, dist);
  return vec4<f32>(uFrag.textColor.rgb, uFrag.textColor.a * alpha);
}
