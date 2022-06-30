[[group(0), binding(0)]]
var vram: texture_2d<f32>;
[[group(0), binding(1)]]
var vram_sampler: sampler;

[[stage(fragment)]]
fn main(
    [[location(1)]] id: u32,
    [[location(2)]] pos: vec2<u32>,
) -> [[location(0)]] vec4<f32> {
    switch id {
        // VRAM.
        case 0 {
            return vec4<f32>(
                textureSample(vram, vram_sampler, vec2<f32>(pos)).rgb,
                // Ignore the alpha channel.
                1.0,
            );
        }
        // Void.
        default {
            return vec4<f32>(1.0, 1.0, 1.0, 1.0);
        }
    }
}
