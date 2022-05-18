mod about;
mod log;
mod main;
mod prefs;

use anyhow::Context as _;

impl<'b> Windows<'b> {
    pub fn new(boing: &'b boing::Ui, log_entry: &'b boing::MultilineTextEntry<'b>) -> Self {
        Self {
            about: Window::new(boing, about::DESCRIPTOR, log_entry),
            log: Window::new(boing, log::DESCRIPTOR, log_entry),
            main: Window::new(boing, main::DESCRIPTOR, log_entry),
            prefs: Window::new(boing, prefs::DESCRIPTOR, log_entry),
        }
    }
}

struct Descriptor {
    title: &'static str,
    size: (u16, u16),
    is_main: bool,
    setup: fn(&boing::Ui, &mut boing::Window, &boing::MultilineTextEntry),
}

impl<'b> Window<'b> {
    fn new(boing: &'b boing::Ui, desc: Descriptor, log_entry: &'b boing::MultilineTextEntry<'b>) -> Self {
        Self {
            boing,
            desc,
            log_entry,
            inner: None,
        }
    }
}

pub struct Window<'b> {
    boing: &'b boing::Ui,
    desc: Descriptor,
    log_entry: &'b boing::MultilineTextEntry<'b>,
    inner: Option<&'b mut boing::Window<'b>>,
}

pub struct Windows<'b> {
    pub about: Window<'b>,
    pub log: Window<'b>,
    pub main: Window<'b>,
    pub prefs: Window<'b>,
}

impl Window<'_> {
    pub fn show(&mut self) -> anyhow::Result<()> {
        if let Some(ref window) = self.inner {
            window.show();

            Ok(())
        } else {
            let desc = &self.desc;
            let window = self.boing.create_window(
                desc.title,
                desc.size.0,
                desc.size.1,
                desc.is_main,
                desc.is_main,
            )
            .with_context(|| format!("Failed to show \"{}\" window", desc.title))?;

            (desc.setup)(self.boing, window, self.log_entry);
            window.show();

            self.inner = Some(window);

            Ok(())
        }
    }
}
