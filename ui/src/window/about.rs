use rust_embed::RustEmbed;

impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui, window: &'b mut boing::Window<'b>) -> anyhow::Result<Self> {
        let y_axis = boing.create_vertical_axis().unwrap();
        y_axis.set_padded(true);
        y_axis.push_new_child(boing.create_horizontal_separator().unwrap(), false);
        y_axis.push_new_child(boing.create_label("Noctane").unwrap(), false);
        y_axis.push_new_child(create_version_label(boing), false);

        window.set_resizeable(false);
        window.set_margined(true);
        window.set_child(y_axis);

        Ok(Self(window))
    }
}

fn create_logo(boing: &boing::Ui) -> &mut boing::Image {
    #[derive(RustEmbed)]
    #[folder = "images"]
    struct Images;

    let image = boing.create_image(94.0, 22.0).unwrap();
    image.push(
        &mut Images::get("noctane.bin").unwrap().data.into_owned(),
        94,
        22,
        4,
    );

    image
}

fn create_version_label(boing: &boing::Ui) -> &mut boing::Label {
    let version = env!("CARGO_PKG_VERSION");

    boing.create_label(format!("Version {}", version)).unwrap()
}

pub struct Window<'b>(&'b mut boing::Window<'b>);

super::impl_deref!(Window);
