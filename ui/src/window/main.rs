use super::Descriptor;

pub(super) const DESCRIPTOR: Descriptor = Descriptor {
    title: "Noctane",
    size: (256, 144),
    setup,
};

fn setup(boing: &boing::Ui, window: &mut boing::Window) {
    window.set_resizeable(false);
}
