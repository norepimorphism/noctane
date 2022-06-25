[[stage(vertex)]]
fn main(
    [[builtin(vertex_index)]] index: u32,
) -> [[builtin(position)]] vec4<f32> {
    // TODO
    let x = f32(1 - i32(index)) * 0.5;
    let y = f32((i32(index & 1u) * 2) - 1) * 0.5;
    return vec4<f32>(x, y, 0.0, 1.0);
}
