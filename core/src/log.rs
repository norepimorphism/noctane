#[cfg(not(target_arch = "wasm32"))]
pub fn init() {
    use tracing_subscriber::EnvFilter;

    tracing_subscriber::fmt()
        // Set the environment variable `RUST_LOG` to one of `TRACE`, `DEBUG`, `INFO`, `WARN`, or
        // `ERROR`. Reading from the environment saves us from writing additional code to parse
        // verbosity flags.
        .with_env_filter({
            // TODO: It would be awesome if these directives could be parsed at compile-time.
            [
                "gfx_backend_vulkan=warn",
                "naga=warn",
                "wgpu_core=warn",
                "wgpu_hal=warn",
            ]
            .iter()
            .map(|it| {
                it.parse()
                    .expect("Failed to parse directive '{}': {}")
            })
            .fold(
                EnvFilter::from_default_env(),
                |filter, it| filter.add_directive(it),
            )
        })
        .with_ansi(true)
        .with_level(true)
        // The target is mostly just noise, I think.
        .with_target(false)
        // Timestamps are mostly noise as well.
        .without_time()
        .init();
}

#[cfg(target_arch = "wasm32")]
pub fn init() {
    console_error_panic_hook::set_once();
    let config = tracing_wasm::WASMLayerConfigBuilder::new()
        .set_max_level(tracing::Level::INFO)
        .build();
    tracing_wasm::set_as_global_default_with_config(config);
}
