[[group(0), binding(0)]]
var pre_vram: texture_2d<f32>;
[[group(0), binding(1)]]
var pre_vram_sampler: sampler;

[[stage(fragment)]]
fn main(
    [[builtin(position)]] pos: vec4<f32>,
    [[location(1)]] id: u32,
) -> [[location(0)]] vec4<f32> {
    switch(id) {
        // VRAM.
        case 1: {
            return vec4<f32>(
                textureSample(pre_vram, pre_vram_sampler, pos.xy).rgb,
                // Ignore the alpha channel.
                1.0,
            );
        }
        // Void.
        default: {
            return vec4<f32>(1.0, 1.0, 1.0, 1.0);
        }
    }
}
