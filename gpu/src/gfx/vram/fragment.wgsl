[[group(0), binding(0)]]
var pre_vram: texture_2d<f32>;
[[group(0), binding(1)]]
var pre_vram_sampler: sampler;

[[stage(fragment)]]
fn main(
    [[location(0)]] pos: vec2<f32>,
    [[location(1)]] sample_strat: u32,
    [[location(2)]] color: u32,
) -> [[location(0)]] vec4<f32> {
    switch(sample_strat) {
        // VRAM.
        case 1: {
            return vec4<f32>(
                textureSample(pre_vram, pre_vram_sampler, pos).rgb,
                // Ignore the alpha channel.
                1.0,
            );
        }
        // Constant.
        default: {
            return vec4<f32>(
                unpack4x8unorm(color).rgb,
                // Ignore the alpha channel.
                1.0,
            );
        }
    }
}
