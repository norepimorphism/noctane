@group(0) @binding(0)
var vram: texture_2d<u32>;

@fragment
fn main(@location(0) pos: vec2<f32>) -> @location(0) vec4<f32> {
    return vec4<f32>(textureLoad(vram, vec2<i32>(vec2<f32>(1024.0 * pos.x, 512.0 * pos.y)), 0)) / 255.0;
}
