use super::Descriptor;

impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui) -> anyhow::Result<Self> {
        const DESCRIPTOR: Descriptor = Descriptor {
            title: "Log",
            size: (480, 360),
            is_main: false,
        };

        let window = DESCRIPTOR.create_window(boing)?;
        window.set_resizeable(true);
        window.set_margined(true);

        // let writer = Writer::new(boing);
        // window.set_child(writer.entry);

        Ok(Self {
            inner: window,
        })
    }
}

pub struct Window<'b> {
    inner: &'b mut boing::Window<'b>,
}

super::impl_deref!(Window);
