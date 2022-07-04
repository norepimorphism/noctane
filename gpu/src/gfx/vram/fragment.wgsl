[[group(0), binding(0)]]
var tex_vram: texture_2d<f32>;
[[group(0), binding(1)]]
var tex_vram_sampler: sampler;

[[stage(fragment)]]
fn main(
    [[builtin(position)]] pos: vec4<f32>,
    [[location(0)]] tex_pos: vec2<f32>,
    [[location(1)]] sample_strat: u32,
    [[location(2)]] color: vec4<f32>,
) -> [[location(0)]] vec4<f32> {
    if (sample_strat == 0u) {
        // Constant.
        return color;
    } else {
        let sample = textureSample(tex_vram, tex_vram_sampler, tex_pos);
        if (all(sample.rgb == vec3<f32>(0.0, 0.0, 0.0))) {
            // 'Black' is rendered as transparent.
            discard;
        }

        switch(sample_strat) {
            // Blended texture.
            case 1: {
                return mix(
                    2.0 * sample,
                    2.0 * color,
                    0.5,
                );
            }
            // Raw texture.
            default: {
                return sample;
            }
        }
    }
}
