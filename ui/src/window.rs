mod about;
mod log;
mod main;
mod prefs;

use anyhow::Context as _;

impl<'b> Windows<'b> {
    pub fn new(boing: &'b boing::Ui) -> Self {
        Self {
            about: Window::new(
                boing,
                Descriptor {
                    title: "About Noctane",
                    size: (256, 144),
                    is_main: false,
                }
                .create_window(boing),
            ),
            log: Window::new(
                boing,
                Descriptor {
                    title: "Log",
                    size: (480, 360),
                    is_main: false,
                }
                .create_window(boing),
            ),
            main: Window::new(
                boing,
                Descriptor {
                    title: "Noctane",
                    size: (256, 144),
                    is_main: true,
                }
                .create_window(boing),
            ),
            prefs: Window::new(
                boing,
                Descriptor {
                    title: "Preferences",
                    size: (480, 360),
                    is_main: false,
                }
                .create_window(boing),
            ),
        }
    }
}

pub struct Windows<'b> {
    pub about: Window<'b>,
    pub log: Window<'b>,
    pub main: Window<'b>,
    pub prefs: Window<'b>,
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
