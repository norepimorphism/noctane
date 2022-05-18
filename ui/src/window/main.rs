use super::Descriptor;

impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui) -> anyhow::Result<Self> {
        const DESCRIPTOR: Descriptor = Descriptor {
            title: "Noctane",
            size: (256, 144),
            is_main: true,
        };

        let window = DESCRIPTOR.create_window(boing)?;
        window.set_resizeable(false);
        window.set_margined(true);

        Ok(Self {
            inner: window,
        })
    }
}

pub struct Window<'b> {
    inner: &'b mut boing::Window<'b>,
}

super::impl_deref!(Window);
