use super::Descriptor;

pub(super) const DESCRIPTOR: Descriptor = Descriptor {
    title: "Log",
    size: (480, 360),
    is_main: false,
    setup,
};

fn setup(boing: &boing::Ui, window: &mut boing::Window, log_entry: &boing::MultilineTextEntry) {
    window.set_resizeable(true);
    window.set_margined(true);
    window.set_child(log_entry);
}
