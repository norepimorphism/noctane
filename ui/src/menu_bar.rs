mod about;

use boing::Ui;

pub fn setup(ui: &mut Ui) -> Result<(), boing::Error> {
    setup_file_menu(ui)?;
    setup_edit_menu(ui)?;
    setup_view_menu(ui)?;
    setup_help_menu(ui)?;

    Ok(())
}

fn setup_file_menu(ui: &mut Ui) -> Result<(), boing::Error> {
    let mut menu = ui.create_menu("File")?;
    menu.append_item("Open ISO...")?;
    menu.append_quit_item()?;

    Ok(())
}

fn setup_edit_menu(ui: &mut Ui) -> Result<(), boing::Error> {
    let mut menu = ui.create_menu("Edit")?;
    menu.append_preferences_item()?;

    Ok(())
}

fn setup_view_menu(ui: &mut Ui) -> Result<(), boing::Error> {
    let mut menu = ui.create_menu("View")?;

    Ok(())
}

fn setup_help_menu(ui: &mut Ui) -> Result<(), boing::Error> {
    let mut menu = ui.create_menu("Help")?;
    setup_website_item(ui, &mut menu)?;
    about::setup_item(ui, &mut menu)?;

    Ok(())
}

fn setup_website_item(ui: &mut Ui, menu: &mut boing::Menu) -> Result<(), boing::Error> {
    static REPO_URL: &str = "https://github.com/norepimorphism/noctane";

    let mut item = menu.append_item("Website")?;
    item.on_clicked(
        |_| {
            open::that(REPO_URL).unwrap();
        },
        (),
    );

    Ok(())
}
