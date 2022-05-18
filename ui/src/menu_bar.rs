// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::Context as _;

use crate::window::Windows;
use i18n_embed::fluent::FluentLanguageLoader;

impl MenuBar<'_, '_, '_> {
    pub fn setup<'b>(
        i18n: &FluentLanguageLoader,
        boing: &'b boing::Ui,
        windows: &mut Windows<'b>,
    ) -> anyhow::Result<()> {
        let mut this = MenuBar { i18n, boing, windows };
        this.setup_file_menu()?;
        this.setup_edit_menu()?;
        this.setup_view_menu()?;
        this.setup_help_menu()?;

        Ok(())
    }
}

pub struct MenuBar<'l, 'b, 'w> {
    boing: &'b boing::Ui,
    windows: &'w mut Windows<'b>,
    i18n: &'l FluentLanguageLoader,
}

impl MenuBar<'_, '_, '_> {
    fn fl(&self, id: &str) -> String {
        self.i18n.get(id)
    }

    fn setup_file_menu(&mut self) -> anyhow::Result<()> {
        let menu = self.boing.create_menu(self.fl("menu_file_title"))
            .context("Failed to create \"File\" menu")?;
        self.setup_open_iso_item(menu)?;
        menu.push_separator();
        self.setup_quit_item(menu)?;

        Ok(())
    }

    fn setup_open_iso_item(&mut self, menu: &boing::Menu) -> anyhow::Result<()> {
        let item = menu.push_new_item(self.fl("menu-item_open-iso_title"))
            .context("Failed to append \"Open ISO...\" menu item")?;
        item.on_clicked(|_| {
            if let Ok(Some(iso_path)) = native_dialog::FileDialog::new()
                .add_filter("CD-ROM Image", &["iso"])
                .show_open_single_file()
            {
                let iso = std::fs::File::open(iso_path).unwrap();
                noctane_cdrom::FileSystem::from_reader(iso).unwrap();
            }
        });

        Ok(())
    }

    fn setup_quit_item(&mut self, menu: &boing::Menu) -> anyhow::Result<()> {
        let item = menu.push_new_item(self.fl("menu-item_quit_title"))
            .context("Failed to append \"Quit\" menu item")?;
        item.on_clicked(|_| {
            // TODO
        });

        Ok(())
    }


    fn setup_edit_menu(&mut self) -> anyhow::Result<()> {
        let menu = self.boing.create_menu(self.fl("menu_edit_title"))
            .context("Failed to create \"Edit\" menu")?;
        self.setup_prefs_item(menu)?;

        Ok(())
    }

    fn setup_prefs_item(&mut self, menu: &boing::Menu) -> anyhow::Result<()> {
        let item = menu.push_new_item(self.fl("menu-item_prefs_title"))
            .context("Failed to append \"Preferences...\" menu item")?;
        item.on_clicked(|_| {
            let _ = self.windows.prefs.show();
        });

        Ok(())
    }

    fn setup_view_menu(&mut self) -> anyhow::Result<()> {
        let menu = self.boing.create_menu(self.fl("menu_view_title"))
            .context("Failed to create \"View\" menu")?;
        self.setup_log_item(menu)?;

        Ok(())
    }

    fn setup_log_item(&mut self, menu: &boing::Menu) -> anyhow::Result<()> {
        let item = menu.push_new_item(self.fl("menu-item_log_title"))
            .context("Failed to append \"Log\" menu item")?;
        item.on_clicked(|_| {
            let _ = self.windows.log.show();
        });

        Ok(())
    }

    fn setup_help_menu(&mut self) -> anyhow::Result<()> {
        let menu = self.boing.create_menu(self.fl("menu_help_title"))
            .context("Failed to create \"Help\" menu")?;
        self.setup_website_item(menu)?;
        menu.push_separator();
        self.setup_about_item(menu)?;

        Ok(())
    }

    fn setup_website_item(&mut self, menu: &boing::Menu) -> anyhow::Result<()> {
        static REPO_URL: &str = "https://github.com/norepimorphism/noctane";

        let item = menu.push_new_item(self.fl("menu-item_website_title"))
            .context("Failed to append \"Website\" menu item")?;
        item.on_clicked(|_| {
            let _ = open::that(REPO_URL);
        });

        Ok(())
    }

    fn setup_about_item(&mut self, menu: &boing::Menu) -> anyhow::Result<()> {
        let item = menu.push_new_item(self.fl("menu-item_about_title"))
            .context("Failed to append \"About\" menu item")?;
        item.on_clicked(|_| {
            let _ = self.windows.about.show();
        });

        Ok(())
    }
}
