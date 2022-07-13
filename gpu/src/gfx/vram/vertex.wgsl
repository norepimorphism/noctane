struct Output {
    @builtin(position)
    pos: vec4<f32>,
    @location(0) @interpolate(flat)
    flags: u32,
    @location(1) @interpolate(perspective, center)
    color: vec4<f32>,
    @location(2) @interpolate(perspective, center)
    tex_pos: vec2<f32>,
    @location(3) @interpolate(flat)
    pal_pos: vec2<u32>,
    @location(4) @interpolate(flat)
    tex_page_x: u32
}

fn make_tex_coord(coord: u32, max: f32) -> f32 {
    return f32(coord) / max;
}

fn make_tex_pos(pos: vec2<u32>, bounds: vec2<f32>) -> vec2<f32> {
    return vec2<f32>(
        make_tex_coord(pos.x, bounds.x),
        make_tex_coord(pos.y, bounds.y),
    );
}

fn make_ndc(coord: u32, max: f32) -> f32 {
    return (2.0 * make_tex_coord(coord, max)) - 1.0;
}

@vertex
fn main(
    @location(0) pos: vec2<u32>,
    @location(1) packed: vec2<u32>,
    @location(2) color: u32,
    @location(3) tex_pos: vec2<u32>,
    @location(4) pal_pos: vec2<u32>,
) -> Output {
    let vram_bounds = vec2<f32>(1024.0, 512.0);

    var out: Output;
    out.pos = vec4<f32>(
        // [0, 1024) -> [-1.0, 1.0].
        make_ndc(pos.x, vram_bounds.x),
        // [0, 512)  -> [-1.0, 1.0].
        make_ndc(pos.y, vram_bounds.y) * -1.0,
        0.0,
        1.0,
    );
    out.flags = packed.x;
    // out.color = unpack4x8unorm(color);
    out.color = vec4<f32>(0.0, 0.0, 0.0, 1.0);
    out.tex_pos = vec2<f32>(tex_pos);
    out.pal_pos = pal_pos;
    out.tex_page_x = packed.y;
    return out;
}
