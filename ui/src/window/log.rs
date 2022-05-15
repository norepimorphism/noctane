use super::Descriptor;

pub(super) const DESCRIPTOR: Descriptor = Descriptor {
    title: "Log",
    size: (480, 360),
    setup,
};

fn setup(boing: &boing::Ui, window: &mut boing::Window) {
    window.set_resizeable(true);
    window.set_margined(true);
    window.set_child(create_log(boing));
}

fn create_log(boing: &boing::Ui) -> &mut boing::TextEntry {
    let entry = boing.create_text_entry().unwrap();
    entry.set_read_only(true);
    entry.set_text("Test test test");

    entry
}
