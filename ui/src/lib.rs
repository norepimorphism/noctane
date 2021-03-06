// SPDX-License-Identifier: MPL-2.0
mod log;
mod menu_bar;
mod window;

use std::cell::Cell;

use anyhow::Context as _;
use i18n_embed::{
    fluent::{fluent_language_loader, FluentLanguageLoader},
    DesktopLanguageRequester,
};
use menu_bar::MenuBar;
use rust_embed::RustEmbed;
use window::Windows;

pub fn run() -> Result<(), anyhow::Error> {
    let mut noctane = noctane::Core::default();
    let boing = boing::Ui::new().context("Failed to initialize GUI")?;

    let (log_source, log_sink) = crossbeam_channel::unbounded();
    let log_sink = log::Sink::new(&boing, log_sink).context("Failed to initialize logging")?;

    // Logging ceases when this guard dies.
    let _log_guard = log::setup(log::Source::new(log_source));

    let i18n = create_i18n().context("Failed to setup i18n")?;
    let mut menu_bar = MenuBar::new(&i18n, &boing).context("Failed to create menu bar")?;

    let mut windows = Windows::new(&boing).context("Failed to create windows")?;
    windows.log.set_child(log_sink.entry());

    let should_quit = Cell::new(false);
    menu_bar.file.open_iso.setup(&mut noctane);
    menu_bar.file.quit.setup(&should_quit);
    menu_bar.edit.prefs.setup(&windows.prefs);
    menu_bar.view.log.setup(&windows.log);
    menu_bar.help.about.setup(&windows.about);

    windows.main.show();
    if cfg!(debug_assertions) {
        windows.log.show();
    }

    loop {
        if should_quit.get() {
            break;
        }

        let should_continue = boing.step();
        if !should_continue {
            break;
        }

        if windows.game.is_active() {
            windows.game.update();
            // windows.game.gfx_mut().render();
        }

        // SAFETY: *tracing* only writes ACSII logs, so the input to `log_sink` *should* be valid
        // UTF-8.
        unsafe {
            log_sink
                .try_refresh()
                .context("Failed to refresh log sink")?
        };
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
