// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::window::Windows;
use i18n_embed::fluent::FluentLanguageLoader;

pub fn setup<'b>(
    lang_loader: &FluentLanguageLoader,
    boing: &'b boing::Ui,
    windows: &mut Windows<'b>,
) {
    let mut ctx = Context { lang_loader, boing, windows };
    file_menu::setup(&mut ctx);
    edit_menu::setup(&mut ctx);
    view_menu::setup(&mut ctx);
    help_menu::setup(&mut ctx);
}

struct Context<'l, 'b, 'w> {
    pub lang_loader: &'l FluentLanguageLoader,
    pub boing: &'b boing::Ui,
    pub windows: &'w mut Windows<'b>,
}

mod file_menu {
    use i18n_embed_fl::fl;

    use super::Context;

    pub(super) fn setup(ctx: &mut Context) {
        let menu = ctx.boing.create_menu(fl!(ctx.lang_loader, "menu_file_title")).unwrap();
        setup_open_iso(ctx, &menu);
        menu.push_separator();
        setup_quit(ctx, &menu);
    }

    fn setup_open_iso(ctx: &mut Context, menu: &boing::Menu) {
        let item = menu.push_new_item(fl!(ctx.lang_loader, "menu-item_open-iso_title")).unwrap();
        item.on_clicked(|_| {
            let iso_path = native_dialog::FileDialog::new()
                .add_filter("CD-ROM Image", &["iso"])
                .show_open_single_file()
                .unwrap();

            if let Some(iso_path) = iso_path {
                let iso = std::fs::File::open(iso_path).unwrap();
                noctane_cdrom_drive::FileSystem::from_reader(iso).unwrap();
            }
        });
    }

    fn setup_quit(ctx: &mut Context, menu: &boing::Menu) {
        let item = menu.push_new_item(fl!(ctx.lang_loader, "menu-item_quit_title")).unwrap();
        item.on_clicked(|_| {
            // TODO
        });
    }
}

mod edit_menu {
    use i18n_embed_fl::fl;

    use super::Context;

    pub(super) fn setup(ctx: &mut Context) {
        let menu = ctx.boing.create_menu(fl!(ctx.lang_loader, "menu_edit_title")).unwrap();
        setup_prefs(ctx, &menu);
    }

    fn setup_prefs(ctx: &mut Context, menu: &boing::Menu) {
        let item = menu.push_new_item(fl!(ctx.lang_loader, "menu-item_prefs_title")).unwrap();
        item.on_clicked(|_| ctx.windows.prefs().show().unwrap());
    }
}

mod view_menu {
    use i18n_embed_fl::fl;

    use super::Context;

    pub(super) fn setup(ctx: &mut Context) {
        let menu = ctx.boing.create_menu(fl!(ctx.lang_loader, "menu_view_title")).unwrap();
        setup_log(ctx, &menu);
    }

    fn setup_log(ctx: &mut Context, menu: &boing::Menu) {
        let item = menu.push_new_item(fl!(ctx.lang_loader, "menu-item_log_title")).unwrap();
        item.on_clicked(|_| ctx.windows.log().show().unwrap());
    }
}

mod help_menu {
    use i18n_embed_fl::fl;

    use super::Context;

    pub(super) fn setup(ctx: &mut Context) {
        let menu = ctx.boing.create_menu(fl!(ctx.lang_loader, "menu_help_title")).unwrap();
        setup_website(ctx, &menu);
        menu.push_separator();
        setup_about(ctx, &menu);
    }

    fn setup_website(ctx: &mut Context, menu: &boing::Menu) {
        static REPO_URL: &str = "https://github.com/norepimorphism/noctane";

        let item = menu.push_new_item(fl!(ctx.lang_loader, "menu-item_website_title")).unwrap();
        item.on_clicked(|_| open::that(REPO_URL).unwrap());
    }

    fn setup_about(ctx: &mut Context, menu: &boing::Menu) {
        let item = menu.push_new_item(fl!(ctx.lang_loader, "menu-item_about_title")).unwrap();
        item.on_clicked(|_| ctx.windows.about().show().unwrap());
    }
}
