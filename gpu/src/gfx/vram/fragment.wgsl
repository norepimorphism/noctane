[[group(0), binding(0)]]
var tex_vram: texture_2d<f32>;
[[group(0), binding(1)]]
var tex_vram_sampler: sampler;

[[stage(fragment)]]
fn main(
    [[location(0), interpolate(flat)]]
    tex_left: f32,
    [[location(1), interpolate(flat)]]
    flags: u32,
    [[location(2), interpolate(perspective, center)]]
    color: vec4<f32>,
    [[location(3), interpolate(perspective, center)]]
    tex_pos: vec2<f32>,
    [[location(4), interpolate(flat)]]
    pal_pos: vec2<f32>,
) -> [[location(0)]] vec4<f32> {
    switch (flags & 1u) {
        // Constant.
        case 0u: {
            return color;
        }
        // Texture.
        default: {
            var sample: vec4<f32>;

            // Palette.
            if (((flags >> 1u) & 1u) == 1u) {
                var idx: u32;

                switch ((flags >> 3u) & 1u) {
                    // 4-bit.
                    case 0: {
                        let x_pixel_offset = u32(1024.0 * (tex_pos.x - tex_left));
                        sample = textureSample(
                            tex_vram,
                            tex_vram_sampler,
                            vec2<f32>(
                                tex_pos.x + ((tex_pos.x - tex_left) / 4.0),
                                tex_pos.y,
                            )
                        );
                        // TODO
                        idx = 0u;
                    }
                    // 8-bit.
                    default: {
                        // TODO
                        idx = 0u;
                    }
                }

                // sample = textureSample(
                //     tex_vram,
                //     tex_vram_sampler,
                //     vec2<f32>(
                //         pal_pos.x + (f32(idx) / 1024.0),
                //         pal_pos.y,
                //     ),
                // )
            } else {
                sample = textureSample(tex_vram, tex_vram_sampler, tex_pos);

                if (all(sample.rgb == vec3<f32>(0.0, 0.0, 0.0))) {
                    // 'Black' is rendered as transparent.
                    discard;
                }
            }

            // Blending.
            if (((flags >> 2u) & 1u) == 1u) {
                sample = mix(
                    2.0 * sample,
                    2.0 * color,
                    0.5,
                );
            }

            return sample;
        }
    }
}
