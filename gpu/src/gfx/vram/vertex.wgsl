struct Output {
    [[builtin(position)]] pos: vec4<f32>;
    [[location(1)]] id: u32;
};

fn scale_coord(coord: u32, max: f32) -> f32 {
    return ((f32(coord) / max) * 2.0) - 1.0;
}

[[stage(vertex)]]
fn main(
    [[location(0)]] pos: vec2<u32>,
    [[location(1)]] id: u32,
) -> Output {
    var out: Output;
    out.pos = vec4<f32>(
        scale_coord(pos.x, 1024.0),
        scale_coord(pos.y, 512.0),
        0.0,
        1.0,
    );
    out.id = id;

    return out;
}
