// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod log;
mod menu_bar;
mod window;

use anyhow::Context as _;
use i18n_embed::{
    DesktopLanguageRequester,
    fluent::{FluentLanguageLoader, fluent_language_loader},
};
use rust_embed::RustEmbed;
use window::Windows;

use menu_bar::MenuBar;

pub fn run() -> Result<(), anyhow::Error> {
    let boing = boing::Ui::new()
        .context("Failed to initialize GUI")?;
    let windows = Windows::new(&boing)
        .context("Failed to create windows")?;

    let (log_source, log_sink) = crossbeam_channel::unbounded();
    let log_sink = log::Sink::new(&boing, log_sink)
        .context("Failed to initialize logging")?;

    // Logging ceases when this guard dies.
    let _log_guard = log::setup(log::Source::new(log_source));

    // After this point, logging is functional.

    let i18n = create_i18n()?;
    tracing::debug!("Hello!");

    // TODO: Uncomment me!
    // MenuBar::setup(&i18n, &boing, &mut windows)?;

    windows.main.show();
    windows.log.show();
    windows.log.set_child(log_sink.entry());

    loop {
        let should_continue = boing.step();
        if !should_continue {
            break;
        }

        log_sink.try_refresh().context("Failed to refresh log sink")?;
    }

    Ok(())
}

fn create_i18n() -> anyhow::Result<FluentLanguageLoader> {
    #[derive(RustEmbed)]
    #[folder = "i18n"]
    struct Localizations;

    let lang_loader = fluent_language_loader!();
    i18n_embed::select(
        &lang_loader,
        &Localizations,
        &DesktopLanguageRequester::requested_languages(),
    )
    .context("Failed to load language i18n file")?;

    Ok(lang_loader)
}
