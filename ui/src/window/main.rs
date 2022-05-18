impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui, window: &'b mut boing::Window<'b>) -> anyhow::Result<Self> {
        window.set_resizeable(false);
        window.set_margined(true);

        Ok(Self(window))
    }
}

pub struct Window<'b>(&'b mut boing::Window<'b>);

super::impl_deref!(Window);
