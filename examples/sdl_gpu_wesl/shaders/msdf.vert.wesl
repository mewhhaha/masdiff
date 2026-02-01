struct VSIn {
  @location(0) pos: vec2<f32>;
  @location(1) uv: vec2<f32>;
};

struct VSOut {
  @builtin(position) pos: vec4<f32>;
  @location(0) uv: vec2<f32>;
};

@vertex
fn vs_main(input: VSIn) -> VSOut {
  var out: VSOut;
  out.pos = vec4<f32>(input.pos, 0.0, 1.0);
  out.uv = input.uv;
  return out;
}
