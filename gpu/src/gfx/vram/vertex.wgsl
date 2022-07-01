struct Output {
    [[builtin(position)]] pos: vec4<f32>;
    [[
        location(1),
        // The sampling strategy of the invoking vertex is used for all fragments within this
        // triangle.
        interpolate(flat),
    ]]
    sample_strat: u32;
    [[
        location(2),
        // The color of the invoking vertex is used for all fragments within this triangle.
        interpolate(flat),
    ]]
    color: u32;
};

fn make_ndc(coord: u32, max: f32) -> f32 {
    return (2.0 * (f32(coord) / max)) - 1.0;
}

[[stage(vertex)]]
fn main(
    // X range: [0, 1024).
    // Y range: [0, 512).
    [[location(0)]] pos: vec2<u32>,
    [[location(1)]] sample_strat: u32,
    [[location(2)]] color: u32,
) -> Output {
    var out: Output;
    out.color = color;
    out.sample_strat = sample_strat;
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
