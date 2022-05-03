// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub fn setup_item(ui: &boing::Ui, menu: &mut boing::Menu) -> Result<(), boing::Error> {
    let item = menu.append_about_item()?;
    item.on_clicked(
        ui,
        |_| {
            // TODO: Don't unwrap.
            show_window(ui).unwrap();
        },
    );

    Ok(())
}

fn show_window(ui: &boing::Ui) -> Result<(), boing::Error> {
    let window = ui.create_window(
        "About Noctane",
        480,
        360,
        false,
        false,
    )?;
    window.show();

    Ok(())
}
