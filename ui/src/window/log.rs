// SPDX-License-Identifier: MPL-2.0

impl<'b> Window<'b> {
    pub fn new(window: &'b mut boing::Window<'b>) -> anyhow::Result<Self> {
        window.set_resizeable(true);
        window.set_margined(true);

        Ok(Self(window))
    }
}

pub struct Window<'b>(&'b mut boing::Window<'b>);

super::impl_deref!(Window);
