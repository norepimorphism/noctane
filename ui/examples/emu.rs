
fn main() {
    use libui_ng_sys::*;

    let mut init_options = uiInitOptions { Size: 0 };

    unsafe {
        uiInit(std::ptr::addr_of_mut!(init_options));
    }

    let main_win = unsafe {
        uiNewWindow(
            std::ffi::CString::new("noctane")
                .unwrap()
                .as_bytes_with_nul()
                .as_ptr()
                .cast(),
            500,
            500,
            1,
        )
    };

    unsafe {
        uiControlShow(main_win.cast());
        uiMain();
        uiUninit();
    }
}
