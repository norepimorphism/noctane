[[group(0), binding(0)]]
var tex_vram: texture_2d<f32>;
[[group(0), binding(1)]]
var tex_vram_sampler: sampler;

[[stage(fragment)]]
fn main(
    [[location(0)]] pos: vec2<f32>,
    [[location(1)]] sample_strat: u32,
    [[location(2)]] color: vec4<f32>,
) -> [[location(0)]] vec4<f32> {
    switch(sample_strat) {
        // Blended texture.
        case 1: {
            return mix(
                textureSample(tex_vram, tex_vram_sampler, pos),
                color,
                0.5,
            );
        }
        // Raw texture.
        case 2: {
            return textureSample(tex_vram, tex_vram_sampler, pos);
        }
        // Constant.
        default: {
            return color;
        }
    }
}
