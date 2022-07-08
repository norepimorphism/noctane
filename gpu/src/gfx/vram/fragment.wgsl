[[group(0), binding(0)]]
var tex_vram: texture_2d<u32>;

fn calc_palette_index(
    tex_pos: vec2<i32>,
    tex_page_x: i32,
    color_depth: i32,
) -> u32 {
    let tex_page_offset = tex_pos.x - tex_page_x;
    let tex_page_idx = (color_depth * tex_page_offset) / 16;
    let load = textureLoad(tex_vram, vec2<i32>(tex_page_x + tex_page_idx, tex_pos.y), 0);
    let packed = (load.b << 10u) | (load.g << 5u) | load.r;
    let shifted = packed >> u32(color_depth * (tex_page_offset & ((16 / color_depth) - 1)));
    let masked = shifted & ((1u << u32(color_depth)) - 1u);
    return masked;
}

[[stage(fragment)]]
fn main(
    [[location(0), interpolate(flat)]]
    flags: u32,
    [[location(1), interpolate(perspective, center)]]
    color: vec4<f32>,
    [[location(2), interpolate(perspective, center)]]
    tex_pos: vec2<f32>,
    [[location(3), interpolate(flat)]]
    pal_pos: vec2<f32>,
    [[location(4), interpolate(flat)]]
    tex_page_x: u32,
) -> [[location(0)]] vec4<u32> {
    let color = vec4<u32>(255.0 * color);

    switch (flags & 1u) {
        // Constant.
        case 0u: {
            return color;
        }
        // Texture.
        default: {
            var tex_pos = vec2<i32>(tex_pos);
            let pal_pos = vec2<i32>(pal_pos);

            var sample: vec4<u32>;

            // Palette.
            if (((flags >> 1u) & 1u) == 1u) {
                let idx = calc_palette_index(
                    tex_pos,
                    i32(tex_page_x),
                    i32(4u * (1u + ((flags >> 3u) & 1u))),
                );
                tex_pos = vec2<i32>(pal_pos.x + i32(idx), pal_pos.y);
            }

            sample = textureLoad(tex_vram, tex_pos, 0);
            if (all(sample.rgb == vec3<u32>(0u, 0u, 0u))) {
                // 'Black' is rendered as transparent.
                discard;
            }

            // Blending.
            if (((flags >> 2u) & 1u) == 1u) {
                var rgb = color.rgb * sample.rgb;
                sample = vec4<u32>(
                    255u & (rgb.r >> 4u),
                    255u & (rgb.g >> 4u),
                    255u & (rgb.b >> 4u),
                    sample.a
                );
            }

            return sample;
        }
    }
}
