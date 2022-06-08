// SPDX-License-Identifier: MPL-2.0

pub mod cmd;

impl Gpu {
    pub fn new() -> Self {
        Self(())
    }
}

#[derive(Debug, Default)]
pub struct Gpu(());
