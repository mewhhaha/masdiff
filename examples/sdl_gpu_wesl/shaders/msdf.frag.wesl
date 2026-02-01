struct FragUniforms {
  textColor: vec4<f32>;
  params: vec4<f32>; // params.x = pxRange
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
  let sample = textureSample(msdfTex, msdfSampler, vUV).rgb;
  let sd = 0.5 - median(sample.r, sample.g, sample.b);
  let dist = sd * uFrag.params.x;
  let w = fwidth(dist);
  let alpha = smoothstep(-w, w, dist);
  return vec4<f32>(uFrag.textColor.rgb, uFrag.textColor.a * alpha);
}
