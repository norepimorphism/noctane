impl<'b> Window<'b> {
    pub fn new(boing: &'b boing::Ui, window: &'b mut boing::Window<'b>) -> anyhow::Result<Self> {
        let x_axis = boing.create_horizontal_axis().unwrap();
        x_axis.set_padded(true);
        add_buttons(boing, x_axis);

        let y_axis = boing.create_vertical_axis().unwrap();
        y_axis.set_padded(true);
        y_axis.push_new_child(x_axis, false);

        let group = boing.create_group("Control Panel").unwrap();
        group.set_margined(true);
        group.set_child(y_axis);

        window.set_resizeable(false);
        window.set_margined(true);
        window.set_child(group);

        Ok(Self(window))
    }
}

fn add_buttons(boing: &boing::Ui, axis: &boing::Axis) {
    let pp_button = boing.create_pushbutton("Play").unwrap();
    let step_button = boing.create_pushbutton("Step").unwrap();

    pp_button.on_clicked(|_| {
        step_button.disable();
    });

    axis.push_new_child(pp_button, true);
    axis.push_new_child(step_button, false);
}

pub struct Window<'b>(&'b mut boing::Window<'b>);

super::impl_deref!(Window);
