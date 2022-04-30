pub fn setup_item(ui: &mut boing::Ui, menu: &mut boing::Menu) -> Result<(), boing::Error> {
    let mut item = menu.append_about_item()?;
    item.on_clicked(
        |ui| {
            // TODO: Don't unwrap.
            show_window(ui).unwrap();
        },
        ui,
    );

    Ok(())
}

fn show_window(ui: &mut boing::Ui) -> Result<(), boing::Error> {
    let mut window = ui.create_window(
        "About Noctane",
        480,
        360,
        false,
        false,
    )?;

    window.on_closing(|_| { 1 }, ());

    window.show();

    Ok(())
}
