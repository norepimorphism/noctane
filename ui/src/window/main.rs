use super::Descriptor;

pub(super) const DESCRIPTOR: Descriptor = Descriptor {
    title: "Noctane",
    size: (256, 144),
    is_main: true,
    setup,
};

fn setup(_: &boing::Ui, window: &mut boing::Window, _: &boing::MultilineTextEntry) {
    window.set_resizeable(false);
}
