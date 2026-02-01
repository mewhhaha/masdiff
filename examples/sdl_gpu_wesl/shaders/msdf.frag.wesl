struct FragUniforms {
  textColor: vec4<f32>;
  params: vec4<f32>; // params.x = pxRange, params.y = 0 (MSDF) / 1 (MTSDF), params.z = debug (alpha-only)
};

// SDL GPU (SPIR-V) expects fragment textures in set=2 and uniforms in set=3.
@group(2) @binding(0) var msdfTex: texture_2d<f32>;
@group(2) @binding(1) var msdfSampler: sampler;
@group(3) @binding(0) var<uniform> uFrag: FragUniforms;

fn median(r: f32, g: f32, b: f32) -> f32 {
  return max(min(r, g), min(max(r, g), b));
}

@fragment
fn fs_main(@location(0) vUV: vec2<f32>) -> @location(0) vec4<f32> {
  let sample = textureSample(msdfTex, msdfSampler, vUV);
  if (uFrag.params.z > 0.5) {
    var a = median(sample.r, sample.g, sample.b);
    if (uFrag.params.y > 0.5) {
      a = sample.a;
    }
    return vec4<f32>(a, a, a, 1.0);
  }
  var sd = median(sample.r, sample.g, sample.b) - 0.5;
  if (uFrag.params.y > 0.5) {
    sd = sample.a - 0.5;
  }
  let dist = sd * uFrag.params.x;
  let w = fwidth(dist);
  let alpha = smoothstep(-w, w, dist);
  return vec4<f32>(uFrag.textColor.rgb, uFrag.textColor.a * alpha);
}
