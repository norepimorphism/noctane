use super::Descriptor;

impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui) -> anyhow::Result<Self> {
        const DESCRIPTOR: Descriptor = Descriptor {
            title: "About Noctane",
            size: (256, 144),
            is_main: false,
        };

        let window = DESCRIPTOR.create_window(boing)?;
        window.set_resizeable(false);
        window.set_margined(true);

        let y_axis = boing.create_vertical_axis().unwrap();
        y_axis.set_padded(true);
        y_axis.push_new_child(boing.create_horizontal_separator().unwrap(), false);
        y_axis.push_new_child(boing.create_label("Noctane").unwrap(), false);
        y_axis.push_new_child(create_version_label(boing), false);

        window.set_child(y_axis);

        Ok(Self {
            inner: window,
        })
    }
}

pub struct Window<'b> {
    inner: &'b mut boing::Window<'b>,
}

super::impl_deref!(Window);

// fn create_logo(boing: &boing::Ui) -> &mut boing::Image {
//     #[derive(RustEmbed)]
//     #[folder = "images"]
//     struct Images;

//     let png = Images::get("noctane.png").unwrap().data.into_owned();
//     let mut reader = png::Decoder::new(png.as_slice()).read_info().unwrap();

//     let (color_type, bit_depth) = reader.output_color_type();
//     assert_eq!(png::ColorType::Rgba, color_type);
//     assert_eq!(png::BitDepth::Eight, bit_depth);

//     let mut buf = Vec::new();
//     buf.resize(reader.output_buffer_size(), 0);

//     let buf = Box::leak(buf.into_boxed_slice());
//     reader.next_frame(buf).unwrap();

//     let image = boing.create_image(94.0, 22.0).unwrap();

//     image
// }

fn create_version_label(boing: &boing::Ui) -> &mut boing::Label {
    let version = env!("CARGO_PKG_VERSION");

    boing.create_label(format!("Version {}", version)).unwrap()
}
