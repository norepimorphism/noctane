struct Output {
    @builtin(position)
    pos: vec4<f32>,
    @location(0) @interpolate(perspective, center)
    vram_pos: vec2<f32>
}

@vertex
fn main(@builtin(vertex_index) index: u32) -> Output {
    // We need to generate a fullscreen quad with one triangle (I guess it's not really a quad at
    // that point...). Specifically, we'll construct a right triangle such that the hypotenuse is
    // bisected by the bottom-right corner of the screen.
    var out: Output;
    // First, we convert the indices to texture coordinates:
    //
    // [0 (00)] -> (0.0, 0.0)
    // [1 (01)] -> (2.0, 0.0)
    // [2 (10)] -> (0.0, 2.0)
    //
    // TODO: As of 7/22/21, the WGSL working draft lists `vecN<f32>(u32, u32)` as a valid
    // conversion. If that lands in `naga`, remove the inner `f32` conversions here.
    out.vram_pos = vec2<f32>(f32((index << 1u) & 2u), f32(index & 2u));
    // Then, we transform those into NDCs.
    //
    // [0] -> (-1.0, +1.0) -- Top-left
    // [1] -> (+3.0, +1.0) -- Top-right
    // [2] -> (-1.0, -3.0) -- Bottom-left
    out.pos = vec4<f32>(
        ( 2.0 * out.vram_pos.x) - 1.0,
        (-2.0 * out.vram_pos.y) + 1.0,
        0.0,
        1.0,
    );

    return out;
}
