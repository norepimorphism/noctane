// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod menu_bar;
mod window;

use i18n_embed::{
    DesktopLanguageRequester,
    fluent::{FluentLanguageLoader, fluent_language_loader},
    I18nEmbedError,
};
use rust_embed::RustEmbed;
use window::Windows;

pub fn run() {
    let lang_loader = setup_i18n().unwrap();
    let boing = boing::Ui::new().unwrap();
    let mut windows = Windows::new(&boing);

    menu_bar::setup(&lang_loader, &boing, &mut windows);
    windows.main().show().unwrap();
    boing.run();
}

fn setup_i18n() -> Result<FluentLanguageLoader, I18nEmbedError> {
    #[derive(RustEmbed)]
    #[folder = "i18n"]
    struct Localizations;

    let lang_loader = fluent_language_loader!();
    i18n_embed::select(
        &lang_loader,
        &Localizations,
        &DesktopLanguageRequester::requested_languages(),
    )?;

    Ok(lang_loader)
}
