// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::Context as _;

use crate::window::Windows;
use i18n_embed::fluent::FluentLanguageLoader;

pub struct MenuBar<'b> {
    pub file: FileMenu<'b>,
    pub edit: EditMenu<'b>,
    pub view: ViewMenu<'b>,
    pub help: HelpMenu<'b>,
}

pub struct FileMenu<'b> {
    pub open_iso: &'b mut boing::MenuItem<'b>,
    pub quit: &'b mut boing::MenuItem<'b>,
}

pub struct EditMenu<'b> {
    pub prefs: &'b mut boing::MenuItem<'b>,
}

pub struct ViewMenu<'b> {
    pub log: &'b mut boing::MenuItem<'b>,
}

pub struct HelpMenu<'b> {
    pub about: &'b mut boing::MenuItem<'b>,
}

impl MenuBar<'_, '_, '_> {
    pub fn new<'b>(
        i18n: &FluentLanguageLoader,
        boing: &'b boing::Ui,
    ) -> anyhow::Result<Self> {
        let mut ctx = Context { i18n, boing };

        Ok(MenuBar {
            file: ctx.create_file_menu()?,
            edit: ctx.create_edit_menu()?,
            view: ctx.create_view_menu()?,
            help: ctx.create_help_menu()?,
        })
    }
}

struct Context<'l, 'b> {
    i18n: &'l FluentLanguageLoader,
    boing: &'b boing::Ui,
}

impl Context<'_, '_> {
    fn fl(&self, id: &str) -> String {
        self.i18n.get(id)
    }

    fn create_file_menu(&mut self) -> anyhow::Result<FileMenu> {
        let menu = self.boing.create_menu(self.fl("menu_file_title"))
            .context("Failed to create \"File\" menu")?;
        let open_iso = self.create_open_iso_item(menu)?;
        menu.push_separator();
        let quit = self.setup_quit_item(menu)?;

        Ok(Self { open_iso, quit })
    }

    fn create_open_iso_item(&mut self, menu: &boing::Menu) -> anyhow::Result<&mut boing::MenuItem> {
        menu.push_new_item(self.fl("menu-item_open-iso_title"))
            .context("Failed to append \"Open ISO...\" menu item")
    }

    fn create_quit_item(&mut self, menu: &boing::Menu) -> anyhow::Result<&mut boing::MenuItem> {
        menu.push_new_item(self.fl("menu-item_quit_title"))
            .context("Failed to append \"Quit\" menu item")
    }


    fn create_edit_menu(&mut self) -> anyhow::Result<EditMenu> {
        let menu = self.boing.create_menu(self.fl("menu_edit_title"))
            .context("Failed to create \"Edit\" menu")?;
        let prefs = self.create_prefs_item(menu)?;

        Ok(Self { prefs })
    }

    fn create_prefs_item(&mut self, menu: &boing::Menu) -> anyhow::Result<&mut boing::MenuItem> {
        menu.push_new_item(self.fl("menu-item_prefs_title"))
            .context("Failed to append \"Preferences...\" menu item")
    }

    fn create_view_menu(&mut self) -> anyhow::Result<ViewMenu> {
        let menu = self.boing.create_menu(self.fl("menu_view_title"))
            .context("Failed to create \"View\" menu")?;
        let log = self.create_log_item(menu)?;

        Ok(Self { log })
    }

    fn create_log_item(&mut self, menu: &boing::Menu) -> anyhow::Result<&mut boing::MenuItem> {
        menu.push_new_item(self.fl("menu-item_log_title"))
            .context("Failed to append \"Log\" menu item")
    }

    fn create_help_menu(&mut self) -> anyhow::Result<HelpMenu> {
        let menu = self.boing.create_menu(self.fl("menu_help_title"))
            .context("Failed to create \"Help\" menu")?;
        self.setup_website_item(menu)?;
        menu.push_separator();
        let about = self.create_about_item(menu)?;

        Ok(Self { about })
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

    fn create_about_item(&mut self, menu: &boing::Menu) -> anyhow::Result<&mut boing::MenuItem> {
        menu.push_new_item(self.fl("menu-item_about_title"))
            .context("Failed to append \"About\" menu item")
    }
}
