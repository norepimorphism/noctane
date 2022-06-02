// SPDX-License-Identifier: MPL-2.0

use std::io;

use anyhow::Context as _;

impl<'b> Sink<'b> {
    pub fn new(
        boing: &'b boing::Ui,
        inner: crossbeam_channel::Receiver<Vec<u8>>,
    ) -> anyhow::Result<Self> {
        let entry = boing
            .create_non_wrapping_multiline_text_entry()
            .context("Failed to create log text entry")?;
        entry.set_read_only(true);

        Ok(Self { entry, inner })
    }
}

pub struct Sink<'b> {
    entry: &'b boing::MultilineTextEntry<'b>,
    inner: crossbeam_channel::Receiver<Vec<u8>>,
}

impl<'b> Sink<'b> {
    /// # Safety
    ///
    /// You must ensure that the source to this sink has only written valid UTF-8 characters.
    pub unsafe fn try_refresh(&self) -> anyhow::Result<()> {
        if let Ok(buf) = self.inner.try_recv() {
            let text = std::str::from_utf8_unchecked(&buf);
            print!("{}", text);
            let _ = self.entry.push_text(text);
        }

        Ok(())
    }

    pub fn entry(&'b self) -> &'b boing::MultilineTextEntry<'b> {
        self.entry
    }
}

impl Source {
    pub fn new(inner: crossbeam_channel::Sender<Vec<u8>>) -> Self {
        Self { inner }
    }
}

pub struct Source {
    inner: crossbeam_channel::Sender<Vec<u8>>,
}

impl io::Write for Source {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.send(buf.to_vec()).map_err(|_| {
            io::Error::new(io::ErrorKind::NotConnected, "Log sink was disconnected")
        })?;

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        // Writing occurs immediately, so flushing is a no-op.
        Ok(())
    }
}

pub fn setup(source: Source) -> tracing_appender::non_blocking::WorkerGuard {
    use tracing_subscriber::EnvFilter;

    // These are kinda noisy. If your terminal is spewing with `wgpu` messages, this is the place to
    // go.
    static DIRECTIVES: &[&str] = &[
        "gfx_backend_vulkan=debug",
        "naga=info",
        "wgpu_core=info",
        "wgpu_hal=info",
    ];

    // TODO: This sucks, big time.
    let source = Box::leak(Box::new(source));

    let (writer, guard) = tracing_appender::non_blocking(source);
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_level(false)
        .without_time()
        .with_writer(writer)
        .with_env_filter({
            // TODO: It would be awesome if these directives could be parsed at compile-time.
            DIRECTIVES
                .iter()
                .map(|it| {
                    it.parse()
                        .expect("Failed to parse directive '{}': {}")
                })
                .fold(EnvFilter::default(), |filter, it| filter.add_directive(it))
                .add_directive(tracing::Level::DEBUG.into())
        })
        .init();

    guard
}
