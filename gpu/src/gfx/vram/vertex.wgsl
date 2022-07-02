struct Output {
    [[builtin(position)]]
    pos: vec4<f32>;
    [[location(0), interpolate(perspective, center)]]
    tex_pos: vec2<f32>;
    [[location(1), interpolate(flat)]]
    sample_strat: u32;
    [[location(2), interpolate(perspective, center)]]
    color: vec4<f32>;
};

fn make_tex_coord(coord: u32, max: f32) -> f32 {
    return f32(coord) / max;
}

fn make_ndc(coord: u32, max: f32) -> f32 {
    return (2.0 * make_tex_coord(coord, max)) - 1.0;
}

[[stage(vertex)]]
fn main(
    // X range: [0, 1024).
    // Y range: [0, 512).
    [[location(0)]] pos: vec2<u32>,
    [[location(1)]] tex_pos: vec2<u32>,
    [[location(2)]] sample_strat: u32,
    [[location(3)]] color: u32,
) -> Output {
    var out: Output;
    out.color = unpack4x8unorm(color);
    out.sample_strat = sample_strat;
    out.tex_pos = vec2<f32>(
        make_tex_coord(tex_pos.x, 1023.0),
        make_tex_coord(tex_pos.y, 511.0),
    );
    out.pos = vec4<f32>(
        // [0, 1024) -> [-1.0, 1.0].
        make_ndc(pos.x, 1023.0),
        // [0, 512) -> [-1.0, 1.0].
        make_ndc(pos.y, 511.0) * -1.0,
        0.0,
        1.0,
    );
    return out;
}
