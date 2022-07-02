[[group(0), binding(0)]]
var vram: texture_2d<f32>;
[[group(0), binding(1)]]
var vram_sampler: sampler;

[[stage(fragment)]]
fn main([[location(0)]] pos: vec2<f32>) -> [[location(0)]] vec4<f32> {
    return vec4<f32>(textureSample(vram, vram_sampler, pos));
}
