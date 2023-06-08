cfg_if::cfg_if! {
    if #[cfg(all(not(feature = "print-std-out"), not(feature = "single")))] {
        fn main() -> Result<(), Box<dyn std::error::Error>> {
            dwarf::tui::start_tui()
        }
    } else if #[cfg(not(feature = "print-std-out"))] {
        fn main() {
           let error_style = ansi_term::Colour::Red;
           let alert_style = ansi_term::Colour::Yellow;

            println!("{}: The debugger requires the feature flag: `{}`.",
                error_style.paint("error"),
                alert_style.paint("multi"));
        }
    } else if #[cfg(not(feature = "single"))] {
        fn main() {
           let error_style = ansi_term::Colour::Red;
           let alert_style = ansi_term::Colour::Yellow;

            println!("{}: The debugger won't function properly with feature flag: `{}`.",
                error_style.paint("error"),
                alert_style.paint("print-std-out"));
        }
    } else {
        fn main() {
           let error_style = ansi_term::Colour::Red;
           let alert_style = ansi_term::Colour::Yellow;

            println!("{}: The debugger won't function properly with feature flag: `{}`, and also requires `{}`.",
                error_style.paint("error"),
                alert_style.paint("print-std-out"),
                alert_style.paint("multi"));
        }
    }
}
