// #![windows_subsystem = "windows"]

fn main() {
    let (writer, _guard) = tracing_appender::non_blocking(std::io::stdout());
    tracing_subscriber::fmt().with_max_level(tracing::Level::DEBUG).with_writer(writer).init();

    noctane_ui::run();
}
