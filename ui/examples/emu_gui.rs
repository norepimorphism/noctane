#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
#![feature(backtrace)]

use std::{backtrace::{Backtrace, BacktraceStatus}, fmt::Write as _, process::ExitCode};

fn main() -> ExitCode {
    if let Err(e) = noctane_ui::run() {
        let mut text = String::new();

        let _ = writeln!(text, "{}", e);

        let causes = e
            .chain()
            // Skip the primary error that we already printed.
            .skip(1)
            .collect::<Vec<&dyn std::error::Error>>();

        if !causes.is_empty() {
            let _ = writeln!(text);
            let _ = writeln!(text, "Caused by (most recent first):");
            for (i, cause) in causes.iter().enumerate() {
                let _ = writeln!(text, "  {}. {}", (i + 1), cause);
            }
        }

        let trace = Backtrace::capture();
        if matches!(trace.status(), BacktraceStatus::Captured) {
            let _ = writeln!(text);
            let _ = writeln!(text, "OS backtrace:");
            let _ = writeln!(text, "{}", trace);
        }

        let _ = native_dialog::MessageDialog::new()
            .set_title("Fatal Error")
            .set_type(native_dialog::MessageType::Error)
            .set_text(text.as_str())
            .show_alert();

        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
