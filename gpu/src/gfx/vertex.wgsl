fn scale_coord(coord: i32) -> f32 {
    return f32(coord + 1024) / 2048.0;
}

[[stage(vertex)]]
fn main(
    [[location(0)]] pos: vec2<i32>,
) -> [[builtin(position)]] vec4<f32> {
    return vec4<f32>(
        scale_coord(pos.x),
        scale_coord(pos.y),
        0.0,
        1.0,
    );
}
