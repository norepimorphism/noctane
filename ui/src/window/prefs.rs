impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui, window: &'b mut boing::Window<'b>) -> anyhow::Result<Self> {
        window.set_resizeable(false);
        window.set_margined(true);
        window.set_child(create_tab(boing));

        Ok(Self(window))
    }
}

fn create_tab(boing: &boing::Ui) -> &mut boing::Tab {
    let tab = boing.create_tab().unwrap();
    let cpu_page = tab.push_new_page("CPU", create_cpu_page(boing)).unwrap();
    tab.set_page_margined(cpu_page, true);

    tab
}

fn create_cpu_page(boing: &boing::Ui) -> &mut boing::Group {
    let group = boing.create_group("Instruction Decoding").unwrap();
    group.set_margined(true);

    let decode_options = boing.create_radio_buttons().unwrap();
    let interpreter_option = decode_options.push_new_item("Interpreter").unwrap();
    decode_options.set_selected_item(interpreter_option);

    group.set_child(decode_options);

    group
}

pub struct Window<'b>(&'b mut boing::Window<'b>);

super::impl_deref!(Window);
