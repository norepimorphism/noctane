mod about;
mod log;
mod main;
mod prefs;

impl<'b> Windows<'b> {
    pub fn new(boing: &'b boing::Ui) -> Self {
        Self {
            about: Window::new(boing, about::DESCRIPTOR, false),
            log: Window::new(boing, log::DESCRIPTOR, false),
            main: Window::new(boing, main::DESCRIPTOR, true),
            prefs: Window::new(boing, prefs::DESCRIPTOR, false),
        }
    }
}

struct Descriptor {
    title: &'static str,
    size: (u16, u16),
    setup: fn(&boing::Ui, &mut boing::Window),
}

impl<'b> Window<'b> {
    fn new(boing: &'b boing::Ui, desc: Descriptor, is_main: bool) -> Self {
        Self {
            boing,
            desc,
            is_main,
            inner: None,
        }
    }
}

pub struct Window<'b> {
    boing: &'b boing::Ui,
    desc: Descriptor,
    is_main: bool,
    inner: Option<&'b mut boing::Window<'b>>,
}

pub struct Windows<'b> {
    about: Window<'b>,
    log: Window<'b>,
    main: Window<'b>,
    prefs: Window<'b>,
}

impl<'b> Windows<'b> {
    pub fn about(&mut self) -> &mut Window<'b> {
        &mut self.about
    }

    pub fn log(&mut self) -> &mut Window<'b> {
        &mut self.log
    }

    pub fn main(&mut self) -> &mut Window<'b> {
        &mut self.main
    }

    pub fn prefs(&mut self) -> &mut Window<'b> {
        &mut self.prefs
    }
}

impl Window<'_> {
    pub fn show(&mut self) -> Result<(), boing::Error> {
        if let Some(ref window) = self.inner {
            window.show();

            Ok(())
        } else {
            let desc = &self.desc;
            let window = self.boing.create_window(
                desc.title,
                desc.size.0,
                desc.size.1,
                self.is_main,
                self.is_main,
            )?;
            (self.desc.setup)(self.boing, window);
            window.show();

            self.inner = Some(window);

            Ok(())
        }
    }
}
