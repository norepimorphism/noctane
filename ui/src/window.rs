// SPDX-License-Identifier: MPL-2.0

mod about;
pub mod game;
mod log;
mod main;
mod prefs;

use anyhow::Context as _;

impl<'b> Windows<'b> {
    pub fn new(boing: &'b boing::Ui) -> anyhow::Result<Self> {
        Ok(Self {
            about: about::Window::new(
                boing,
                Descriptor {
                    title: "About Noctane",
                    size: (256, 144),
                    is_main: false,
                }
                .create_window(boing)?,
            )?,
            game: game::Window::new(640, 480),
            log: log::Window::new(
                Descriptor {
                    title: "Log",
                    size: (480, 360),
                    is_main: false,
                }
                .create_window(boing)?,
            )?,
            main: main::Window::new(
                boing,
                Descriptor {
                    title: "Noctane",
                    size: (256, 144),
                    is_main: true,
                }
                .create_window(boing)?,
            )?,
            prefs: prefs::Window::new(
                boing,
                Descriptor {
                    title: "Preferences",
                    size: (480, 360),
                    is_main: false,
                }
                .create_window(boing)?,
            )?,
        })
    }
}

pub struct Windows<'b> {
    pub about: about::Window<'b>,
    pub game: game::Window,
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
    fn create_window<'b>(&self, boing: &'b boing::Ui) -> anyhow::Result<&'b mut boing::Window<'b>> {
        boing
            .create_window(
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
                self.0
            }
        }

        impl std::ops::DerefMut for $ty<'_> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.0
            }
        }
    };
}

use impl_deref;
