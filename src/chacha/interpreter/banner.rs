use ansi_term::{
    ANSIString, ANSIStrings,
    Colour::{self, RGB},
};

#[allow(dead_code)]
pub fn banner() -> String {
    let strings: &[ANSIString<'static>] = &[
        RGB(255, 184, 0).paint(
            r"
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣴⣄⠀⢠⣾⣆⠀⣠⣶⡀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⣼⣿⣦⣼⣿⣿⣷⣿⣿⣿⣶⣿⣿⣧⣤⣾⣿⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣷⣶⣿⣿⣿⣿⣿⣿⣿⡏⠀⢘⣿⣿⣿⣿⣿⣿⣿⣶⣾⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣶⣶⣶⣿⣿⣿⣿⡿⠟⠋⠉⠁⢹⣿⣶⣾⣿⣥⣭⣽⠛⠿⣿⣿⣿⣿⣧⣶⣶⡆⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢀⣀⣀⣿⣿⣿⣿⡿⠟⠁⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⣿⠀⠀⠈⠙⢿⣿⣿⣿⣿⣀⣀⣀⠀⠀⠀⠀
",
        ),
        RGB(255, 156, 0).paint(
            r"⠀⠀⠀⠘⣿⣿⣿⣿⣿⠏⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⡏⠀⠀⠀⠀
⠀⢠⣤⣤⣿⣿⣿⡿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⢻⣿⣿⣿⣦⣤⣄⠀⠀
⠀⠈⢻⣿⣿⠿⢿⣧⠀⠀⠀⠀⠀⠀⢀⣀⣀⣀⡀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⣰⣿⠿⢿⣿⡿⠁⠀⠀
⢠⣴⣾⣿⣇⠀⣨⣿⡇⠀⠀⠀⣠⣾⣿⣿⣿⣿⣿⣷⣄⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⢰⣿⣇⠀⣨⣿⣷⣦⣤⠀
",
        ),
        RGB(255, 128, 0).paint(
            r"⠈⠻⣿⣿⣿⡿⠟⠋⠁⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠉⠛⢿⣿⣿⣿⡟⠁⠀
⣤⣶⣿⣿⣿⡇⠀⠀⠀⠀⢰⣿⣿⣿⣿⣿⠟⠛⠛⠻⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣷⣦⡀
",
        ),
        // Colour::Yellow.italic().paint("ChaCha:\n"),
        RGB(255, 128, 0).paint(
            r"⠉⠻⣿⣿⣿⡇⠀⠀⠀⠀⣿⣿⣿⣿⣿⡏⠀⠀⠀⠀⠘⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⡟⠋⠀
",
        ),
        // Colour::Green.italic().paint("a dwarf language REPL.\n"),
        RGB(255, 128, 0).paint(
            r"⢠⣾⣿⣿⣿⣧⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣷⣄⠀
",
        ),
        RGB(255, 102, 0).paint(
            r"⠈⠙⢻⣿⣿⣿⡄⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⡟⠛⠉⠀
⠀⢠⣾⣿⣿⣿⣷⡀⠀⠀⣿⣿⣿⣿⣿⣇⠀⠀⠀⠀⢠⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣄⠀⠀
⠀⠈⠉⠉⣿⣿⣿⣿⣄⠀⠸⣿⣿⣿⣿⣿⣦⣀⣀⣴⣿⣿⣿⣿⣿⣿⣀⣀⡀⠀⠀⠀⢀⣾⣿⣿⣿⡋⠉⠁⠀⠀
⠀⠀⠀⢰⣿⣿⣿⣿⣿⣶⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣶⣶⣿⣿⣿⣿⣿⣧⠀⠀⠀⠀
",
        ),
        RGB(255, 67, 0).paint(
            r"⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⡁⠀⢹⣿⣿⣿⣿⣿⣿⡿⠋⣿⣿⣿⣿⣿⣿⣿⣟⠀⠈⣿⣿⣿⣿⡀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠟⠛⠛⣿⣿⣶⣿⣿⣿⣿⣿⣉⣉⣀⣀⣉⣉⣉⣭⣽⣿⣿⣿⣷⣾⣿⡟⠛⠻⠃⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠿⠛⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠛⠻⢿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⠿⠋⠹⣿⣿⠿⢿⣿⣿⠿⣿⣿⡟⠙⠻⡿⠀⠀⠀      ",
        ),
        Colour::Yellow.italic().paint("ChaCha:\n"),
        RGB(255, 67, 0).paint(r"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⠁⠀⠈⠿⠃⠀⠈⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"),
        Colour::Green.paint("a dwarf language interpreter\n\n"),
    ];

    format!("{}", ANSIStrings(strings))
}

pub fn banner2() -> String {
    let strings: &[ANSIString<'static>] = &[
        RGB(255, 184, 0).paint(
            r"
⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣀⠀⣿⣦⣠⣿⣦⣠⣿⡀⢀⣤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⣿⣶⣿⣿⣿⣿⣿⣿⠀⢹⣿⣿⣿⣿⣿⣶⣾⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠸⣿⣿⣿⣿⠟⠉⠀⠀⠀⣿⣿⣿⣿⣿⣿⠉⠛⣿⣿⣿⣿⡿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 156, 0).paint(
            r"⠀⠀⠈⣿⣿⣿⣿⠉⠀⠀⠀⠀⠀⠀⣿⠿⣿⣿⣿⣿⠀⠀⠀⠈⢿⣿⣿⣿⠏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣶⠛⢿⣿⣦⠀⠀⠀
⠀⢶⣿⣿⣿⣟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⠀⠀⠀⠀⠀⢙⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⡟⠀⠀⠉⠉⠀⠀⠀
⣠⣶⣿⣅⠀⣿⡆⠀⠀⢠⣾⣿⣿⣿⣿⣄⣿⣿⣿⣿⠀⠀⠀⠀⠀⣿⡀⢀⣿⣶⣤⠰⣶⣶⣶⣶⠶⠀⠰⣶⣶⣶⡶⠀⠀⠶⣶⣶⡶⠀⣠⣶⠖⠲⣶⣶⡀⠀⠀⢠⣤⣶⣶⣶⢀⣶⣿⣦⣶⣶⣿⣿⣿⣶⣶⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 128, 0).paint(
            r"⠀⣽⣿⣿⠉⠀⠀⠀⣼⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠈⣿⣿⣿⡀⠀⠀⢻⣿⣿⡄⠀⠀⠀⣿⣿⣷⠀⠀⠀⣿⠋⠀⠘⣿⡿⠀⠀⢹⣿⣿⠀⠀⠀⠀⣿⣿⣿⠉⠙⠿⠋⠀⠀⣿⣿⡟⠀⠀⠀⠀⠀⠀⠀
⠉⢻⣿⣿⠀⠀⠀⢰⣿⣿⣿⡟⠀⠀⠀⠘⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⠋⠀⠀⠀⢿⣿⣿⡀⠀⣿⠉⣿⣿⣧⠀⣼⠋⠀⠀⠀⣠⣶⡾⠋⢹⣿⣿⠀⠀⠀⠀⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⡟⠀⠀⠀⠀⠀⠀⠀
⠺⣿⣿⣿⣆⠀⠀⢸⣿⣿⣿⡇⠀⠀⠀⠀⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⠿⠀⠀⠀⠀⣿⣿⣿⣾⠁⠀⠘⣿⣿⣶⠏⠀⠀⠀⣿⣿⣿⠀⠀⢸⣿⣿⠀⠀⠀⠀⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 102, 0).paint(
            r"⠀⣴⣿⣿⣿⣄⠀⠘⣿⣿⣿⣷⠀⠀⠀⢠⣿⣿⣿⣿⠀⠀⠀⠀⠀⢀⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠿⠿⠁⠀⠀⠀⠻⠿⠟⠀⠀⠀⠀⠈⠻⠿⠿⠋⠈⠿⠿⠟⠉⠐⠚⠛⠛⠛⠓⠒⠀⠀⠒⠛⠛⠛⠛⠓⠒⠀⠀⠀⠀⠀
⠀⠀⢀⣿⣿⣿⣷⣀⣿⣿⣿⣿⣿⣶⣾⣿⣿⣿⣿⣿⣶⣶⣄⣀⣴⣿⣿⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢠⣿⣿⣿⠀⣹⣿⣿⣿⣿⡿⠋⣿⣿⣿⣿⣿⣿⠀⢹⣿⣿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 67, 0).paint(
            r"⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣶⣶⣶⣶⣶⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠻⠛⠉⣿⡿⠻⣿⡿⠻⣿⠋⠙⠿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
",
        ),
        // Colour::Yellow.italic().paint("ChaCha:\n"),
        // RGB(255, 67, 0).paint(r"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⠁⠀⠈⠿⠃⠀⠈⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"),
        // Colour::Green.paint("a dwarf language interpreter\n\n"),
    ];

    format!("{}", ANSIStrings(strings))
}
