// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{cell::Cell, fs::File};

use anyhow::Context as _;

use i18n_embed::fluent::FluentLanguageLoader;

impl<'b> MenuBar<'b> {
    pub fn new<'l>(
        i18n: &'l FluentLanguageLoader,
        boing: &'b boing::Ui,
    ) -> anyhow::Result<Self> {
        let mut ctx = Context::<'l, 'b> { i18n, boing };

        Ok(Self {
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

impl<'l, 'b> Context<'l, 'b> {
    fn fl(&self, id: &str) -> String {
        self.i18n.get(id)
    }

    fn create_file_menu(&mut self) -> anyhow::Result<FileMenu<'b>> {
        let menu = self.boing.create_menu(self.fl("menu_file_title"))
            .context("Failed to create \"File\" menu")?;
        let open_iso = self.create_open_iso_item(menu)?;
        menu.push_separator();
        let quit = self.create_quit_item(menu)?;

        Ok(FileMenu { open_iso, quit })
    }

    fn create_open_iso_item(&mut self, menu: &boing::Menu<'b>) -> anyhow::Result<OpenIsoItem<'b>> {
        menu.push_new_item(self.fl("menu-item_open-iso_title"))
            .context("Failed to append \"Open ISO...\" menu item")
            .map(OpenIsoItem)
    }

    fn create_quit_item(&mut self, menu: &boing::Menu<'b>) -> anyhow::Result<QuitItem<'b>> {
        menu.push_new_item(self.fl("menu-item_quit_title"))
            .context("Failed to append \"Quit\" menu item")
            .map(QuitItem)
    }

    fn create_edit_menu(&mut self) -> anyhow::Result<EditMenu<'b>> {
        let menu = self.boing.create_menu(self.fl("menu_edit_title"))
            .context("Failed to create \"Edit\" menu")?;
        let prefs = self.create_prefs_item(menu)?;

        Ok(EditMenu { prefs })
    }

    fn create_prefs_item(&mut self, menu: &boing::Menu<'b>) -> anyhow::Result<PrefsItem<'b>> {
        menu.push_new_item(self.fl("menu-item_prefs_title"))
            .context("Failed to append \"Preferences...\" menu item")
            .map(PrefsItem)
    }

    fn create_view_menu(&mut self) -> anyhow::Result<ViewMenu<'b>> {
        let menu = self.boing.create_menu(self.fl("menu_view_title"))
            .context("Failed to create \"View\" menu")?;
        let log = self.create_log_item(menu)?;

        Ok(ViewMenu { log })
    }

    fn create_log_item(&mut self, menu: &boing::Menu<'b>) -> anyhow::Result<LogItem<'b>> {
        menu.push_new_item(self.fl("menu-item_log_title"))
            .context("Failed to append \"Log\" menu item")
            .map(LogItem)
    }

    fn create_help_menu(&mut self) -> anyhow::Result<HelpMenu<'b>> {
        let menu = self.boing.create_menu(self.fl("menu_help_title"))
            .context("Failed to create \"Help\" menu")?;
        self.setup_website_item(menu)?;
        menu.push_separator();
        let about = self.create_about_item(menu)?;

        Ok(HelpMenu { about })
    }

    fn setup_website_item(&mut self, menu: &boing::Menu<'b>) -> anyhow::Result<()> {
        static REPO_URL: &str = "https://github.com/norepimorphism/noctane";

        let item = menu.push_new_item(self.fl("menu-item_website_title"))
            .context("Failed to append \"Website\" menu item")?;
        item.on_clicked(|_| {
            let _ = open::that(REPO_URL);
        });

        Ok(())
    }

    fn create_about_item(&mut self, menu: &boing::Menu<'b>) -> anyhow::Result<AboutItem<'b>> {
        menu.push_new_item(self.fl("menu-item_about_title"))
            .context("Failed to append \"About\" menu item")
            .map(AboutItem)
    }
}

pub struct MenuBar<'b> {
    pub file: FileMenu<'b>,
    pub edit: EditMenu<'b>,
    pub view: ViewMenu<'b>,
    pub help: HelpMenu<'b>,
}

pub struct FileMenu<'b> {
    pub open_iso: OpenIsoItem<'b>,
    pub quit: QuitItem<'b>,
}

pub struct OpenIsoItem<'b>(&'b mut boing::MenuItem<'b>);

impl<'b> OpenIsoItem<'b> {
   pub fn setup(&'b mut self, noctane: &'b mut noctane::Core) {
        self.0.on_clicked(|_| {
            if let Ok(Some(iso_path)) = native_dialog::FileDialog::new()
                .add_filter("CD-ROM Image", &["iso"])
                .show_open_single_file()
            {
                let iso = File::open(iso_path).unwrap();
                let fs = noctane_cdrom::FileSystem::from_reader(iso).unwrap();
                for (i, dir) in fs.root_dirs.into_iter().enumerate() {
                    for (path, file) in recurse_cdrom_directory(format!("{}:", i), dir) {
                        tracing::debug!("{}\n  {}", path, file.meta.timestamp);
                    }
                }

                let mut cpu = noctane.cpu();
                let reg = cpu.reg_mut();
                reg.set_gpr(1, 9);
                reg.set_gpr(2, 10);

                let instr = noctane_cpu::Instr::Add(noctane_cpu::instr::r::Instr {
                    rs: 1,
                    rt: 2,
                    rd: 3,
                    shamt: 0,
                    funct: 0,
                });
                cpu.execute_instr(instr);
                for _ in 0..10 {
                    cpu.execute_next_instr();
                }
                tracing::debug!("{}", noctane.cpu().reg());
            }
        });
    }
}

fn recurse_cdrom_directory(
    current_path: String,
    dir: noctane_cdrom::Directory,
) -> Vec<(String, noctane_cdrom::File)> {
    let mut files = Vec::new();

    for entry in dir.entries {
        match entry {
            noctane_cdrom::Entry::Directory(dir) => {
                files.append(&mut recurse_cdrom_directory(
                    format!("{}/{}", current_path, dir.meta.name),
                    dir,
                ));
            }
            noctane_cdrom::Entry::File(file) => {
                files.push((format!("{}/{}", current_path, file.meta.name), file));
            }
        }
    }

    files
}

pub struct QuitItem<'b>(&'b mut boing::MenuItem<'b>);

impl<'b> QuitItem<'b> {
    pub fn setup(&mut self, should_quit: &'b Cell<bool>) {
        self.0.on_clicked(|_| {
            should_quit.set(true);
        });
    }
}

pub struct EditMenu<'b> {
    pub prefs: PrefsItem<'b>,
}

pub struct PrefsItem<'b>(&'b mut boing::MenuItem<'b>);

impl<'b> PrefsItem<'b> {
    pub fn setup(&mut self, prefs_window: &'b boing::Window<'b>) {
        self.0.on_clicked(|_| {
            let _ = prefs_window.show();
        });
    }
}

pub struct ViewMenu<'b> {
    pub log: LogItem<'b>,
}

pub struct LogItem<'b>(&'b mut boing::MenuItem<'b>);

impl<'b> LogItem<'b> {
    pub fn setup(&mut self, log_window: &'b boing::Window<'b>) {
        self.0.on_clicked(|_| {
            let _ = log_window.show();
        });
    }
}

pub struct HelpMenu<'b> {
    pub about: AboutItem<'b>,
}

pub struct AboutItem<'b>(&'b mut boing::MenuItem<'b>);

impl<'b> AboutItem<'b> {
    pub fn setup(&mut self, about_window: &'b boing::Window<'b>) {
        self.0.on_clicked(|_| {
            let _ = about_window.show();
        });
    }
}
