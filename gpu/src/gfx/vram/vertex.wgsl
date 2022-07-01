struct Output {
    [[builtin(position)]] pos: vec4<f32>;
    [[
        location(0),
        // `pre_vram_pos` is the texture coordinate sent to the fragment shader for pre-VRAM
        // fragments. Despite the vertex shader only being called three times---once for each
        // vertex---and `pre_vram_pos` therefore being calculated only three times, its value is
        // interpolated for the fragment shader such that a unique value exists for each pixel.
        interpolate(perspective, center),
    ]]
    pre_vram_pos: vec2<f32>;
    [[
        location(1),
        // The texture ID of the invoking vertex is used for all fragments within this triangle.
        interpolate(flat),
    ]]
    tex_id: u32;
};

fn make_tex_coord(coord: u32, max: f32) -> f32 {
    return f32(coord) / max;
}

[[stage(vertex)]]
fn main(
    // X range: [0, 1024).
    // Y range: [0, 512).
    [[location(0)]] pos: vec2<u32>,
    [[location(1)]] tex_id: u32,
) -> Output {
    var out: Output;

    out.tex_id = tex_id;
    out.pre_vram_pos = vec2<f32>(
        // [0, 1024) -> [0.0, 1.0].
        make_tex_coord(pos.x, 1023.0),
        // [0, 512) -> [0.0, 1.0].
        make_tex_coord(pos.y, 511.0),
    );
    out.pos = vec4<f32>(
        // [0.0, 1.0] -> [-1.0, 1.0].
        (2.0 * out.pre_vram_pos.xy) - 1.0,
        0.0,
        1.0,
    );

    return out;
}
