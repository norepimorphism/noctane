// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod menu_bar;

pub fn run() {
    boing::Ui::run(|ui| {
        menu_bar::setup(ui).unwrap();

        let mut window = ui.create_window(
            "Noctane",
            256,
            144,
            true,
            true,
        )
        .unwrap();
        window.show();
    })
    .unwrap();
}
