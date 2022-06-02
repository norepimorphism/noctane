// SPDX-License-Identifier: MPL-2.0

impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui, window: &'b mut boing::Window<'b>) -> anyhow::Result<Self> {
        let ok_button = boing.create_pushbutton("OK").unwrap();
        ok_button.on_clicked(|_| {
            window.hide();
        });

        let y_axis = boing.create_vertical_axis().unwrap();
        y_axis.set_padded(true);
        y_axis.push_new_child(create_tab(boing), false);
        y_axis.push_new_child(ok_button, false);

        window.set_resizeable(false);
        window.set_margined(true);
        window.set_child(y_axis);

        Ok(Self(window))
    }
}

fn create_tab(boing: &boing::Ui) -> &mut boing::Tab {
    let tab = boing.create_tab().unwrap();
    let cpu_page = tab
        .push_new_page("CPU", create_cpu_page(boing))
        .unwrap();
    tab.set_page_margined(cpu_page, true);

    tab
}

fn create_cpu_page(boing: &boing::Ui) -> &mut boing::Axis {
    let y_axis = boing.create_vertical_axis().unwrap();
    y_axis.set_padded(true);
    y_axis.push_new_child(create_instr_decode_group(boing), false);
    y_axis.push_new_child(create_bios_group(boing), false);

    y_axis
}

fn create_instr_decode_group(boing: &boing::Ui) -> &mut boing::Group {
    let options = boing.create_radio_buttons().unwrap();
    let interpreter_option = options.push_new_item("Interpreter").unwrap();
    options.set_selected_item(interpreter_option);

    let group = boing
        .create_group("Instruction Decoding")
        .unwrap();
    group.set_margined(true);
    group.set_child(options);

    group
}

fn create_bios_group(boing: &boing::Ui) -> &mut boing::Group {
    let path_field = boing.create_text_entry().unwrap();
    path_field.set_read_only(true);

    let path_button = boing.create_pushbutton("Select").unwrap();
    path_button.on_clicked(|_| {
        if let Ok(Some(path)) = native_dialog::FileDialog::new().show_open_single_file() {
            path_field
                .set_text(format!("{}", path.display()))
                .unwrap();
        }
    });

    let path_axis = boing.create_horizontal_axis().unwrap();
    path_axis.set_padded(true);
    path_axis.push_new_child(path_button, false);
    path_axis.push_new_child(path_field, true);

    let group = boing.create_group("BIOS").unwrap();
    group.set_margined(true);
    group.set_child(path_axis);

    group
}

pub struct Window<'b>(&'b mut boing::Window<'b>);

super::impl_deref!(Window);
