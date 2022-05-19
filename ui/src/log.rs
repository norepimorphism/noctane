use std::io;

use anyhow::Context as _;

impl<'b> Sink<'b> {
    pub fn new(
        boing: &'b boing::Ui,
        inner: crossbeam_channel::Receiver<Vec<u8>>,
    ) -> anyhow::Result<Self> {
        let entry = boing.create_non_wrapping_multiline_text_entry()
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
        self.inner.send(buf.to_vec())
            .map_err(|_| {
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
    // TODO: This sucks, big time.
    let source = Box::leak(Box::new(source));

    let (writer, guard) = tracing_appender::non_blocking(source);
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .with_ansi(false)
        .with_level(false)
        .without_time()
        .with_writer(writer)
        .init();

    guard
}
