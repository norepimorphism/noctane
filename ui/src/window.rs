mod about;
mod log;
mod main;
mod prefs;

use anyhow::Context as _;

impl<'b> Windows<'b> {
    pub fn new(boing: &'b boing::Ui) -> anyhow::Result<Self> {
        Ok(Self {
            about: about::Window::new(boing)?,
            log: log::Window::new(boing)?,
            main: main::Window::new(boing)?,
            prefs: prefs::Window::new(boing)?,
        })
    }
}

pub struct Windows<'b> {
    pub about: about::Window<'b>,
    pub log: log::Window<'b>,
    pub main: main::Window<'b>,
    pub prefs: prefs::Window<'b>,
}

struct Descriptor {
    title: &'static str,
    size: (u16, u16),
    is_main: bool,
}

impl Descriptor {
    fn create_window<'b>(
        &self,
        boing: &'b boing::Ui,
    ) -> anyhow::Result<&'b mut boing::Window<'b>> {
        boing.create_window(
            self.title,
            self.size.0,
            self.size.1,
            self.is_main,
            self.is_main,
        )
        .with_context(|| format!("Failed to create \"{}\" window", self.title))
    }
}

macro_rules! impl_deref {
    ($ty:ident) => {
        impl<'b> std::ops::Deref for $ty<'b> {
            type Target = boing::Window<'b>;

            fn deref(&self) -> &Self::Target {
                self.inner
            }
        }

        impl std::ops::DerefMut for $ty<'_> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.inner
            }
        }
    };
}

use impl_deref;
