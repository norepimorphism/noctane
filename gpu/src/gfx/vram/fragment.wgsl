[[stage(fragment)]]
fn main(
    [[location(1)]] sample_strat: u32,
    [[location(2)]] color: u32,
) -> [[location(0)]] vec4<f32> {
    switch(sample_strat) {
        // Constant.
        default: {
            return vec4<f32>(
                unpack4x8unorm(color).rgb,
                // Ignore the alpha channel.
                1.0,
            );
        }
    }
}
