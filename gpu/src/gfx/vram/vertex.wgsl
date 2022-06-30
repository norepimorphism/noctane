fn scale_coord(coord: u32, max: f32) -> f32 {
    return ((f32(coord) / max) * 2.0) - 1.0;
}

[[stage(vertex)]]
fn main([[location(0)]] pos: vec2<u32>) -> [[builtin(position)]] vec4<f32> {
    return vec4<f32>(
        scale_coord(pos.x, 1024.0),
        scale_coord(pos.y, 512.0),
        0.0,
        1.0,
    );
}
