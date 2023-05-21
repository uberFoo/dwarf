use std::{
    io,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use ansi_to_tui::{IntoSpans, IntoText};
use chacha::{
    dwarf::{inter_statement, parse_line},
    initialize_interpreter,
    interpreter::{
        banner, eval_statement, initialize_interpreter_paths, start_main, start_vm, Context,
    },
    lu_dog::DwarfSourceFile,
    // merlin::{ErrorExpressionProxy, ExpressionProxy},
    // merlin::{
    //     AnchorProxy, BisectionProxy, EdgeProxy, GlyphProxy, LineProxy, LineSegmentPointProxy,
    //     LineSegmentProxy, PointProxy, RelationshipNameProxy, RelationshipPhraseProxy, XBoxProxy,
    // },
    start_repl,
    ChaChaError,
};
use clap::Parser;
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers, MouseEvent,
    },
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use log::{Level, LevelFilter, Metadata, Record, SetLoggerError};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, BorderType, Borders, List, ListItem, Paragraph, Wrap},
    Frame, Terminal,
};
use syntect::{
    easy::HighlightLines,
    highlighting::{Style as SyntectStyle, ThemeSet},
    parsing::SyntaxSet,
    util::{as_24_bit_terminal_escaped, LinesWithEndings},
};
use tui_input::{backend::crossterm::EventHandler, Input};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget, TuiWidgetState};
use tui_textarea::{CursorMove, TextArea};

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Args {
    /// Compiled Dwarf Source File
    ///
    /// Path to the compiled source file to load.
    object_file: PathBuf,

    /// List available themess
    #[arg(long, short, action)]
    list_themes: bool,

    /// Selecet theme
    #[arg(long, short, default_value = "InspiredGitHub")]
    theme: String,
}

// struct ConsoleLogger<'a>(Arc<RwLock<Console<'a>>>);

// impl log::Log for ConsoleLogger<'_> {
//     fn enabled(&self, metadata: &Metadata) -> bool {
//         metadata.level() <= Level::Info
//     }

//     fn log(&self, record: &Record) {
//         if self.enabled(record.metadata()) {
//             let mut writer = self.0.write().unwrap();
//             writer.output.extend(
//                 format!("{} - {}", record.level(), record.args())
//                     .into_text()
//                     .unwrap(),
//             );
//         }
//     }

//     fn flush(&self) {}
// }

#[derive(Clone, Copy, Eq, PartialEq)]
enum Window {
    Console,
    Source,
}

struct App<'a> {
    console: Console<'a>,
    source: TextArea<'a>,
    stdout: Text<'a>,
    window: Window,
    hover: Window,
    theme: String,
    mouse_position: (u16, u16),
}

impl App<'_> {
    fn new(theme: String, object_file: &PathBuf) -> App<'static> {
        let console = Console::new(object_file);

        let lines = console
            .source
            .split('\n')
            .map(|f| f.into())
            .collect::<Vec<String>>();

        let source = TextArea::new(lines);

        App {
            console,
            source,
            stdout: Text::default(),
            window: Window::Console,
            hover: Window::Console,
            theme,
            mouse_position: (0, 0),
        }
    }

    fn scroll_up(&mut self, e: MouseEvent) {
        match self.hover {
            Window::Console => self.console.scroll_up(),
            Window::Source => self.source.input(e),
        };
    }

    fn scroll_down(&mut self, e: MouseEvent) {
        match self.hover {
            Window::Console => self.console.scroll_down(),
            Window::Source => self.source.input(e),
        };
    }

    fn select_window(&mut self) {
        self.window = self.hover;
    }
}

struct Console<'a> {
    /// Current value of the input box
    input: Input,
    /// History of recorded messages
    output: Text<'a>,
    pub interpreter: Context,
    scroll: i16,
    source: String,
}

impl Console<'_> {
    fn new(object_file: &PathBuf) -> Console<'static> {
        let output = banner().into_text().unwrap();

        let interpreter = initialize_interpreter_paths(object_file).unwrap();

        Console {
            input: Input::new(interpreter.prompt().into()),
            output,
            source: interpreter.source(),
            interpreter,
            scroll: 0,
        }
    }

    fn scroll_up(&mut self) -> bool {
        self.scroll -= 1;
        true
    }

    fn scroll_down(&mut self) -> bool {
        self.scroll += 1;
        true
    }
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> io::Result<()> {
    loop {
        terminal.draw(|f| draw_frame(f, &mut app))?;

        if let Event::Key(key) = event::read()? {
            match key.code {
                KeyCode::Char('c') | KeyCode::Char('d')
                    if key.modifiers == KeyModifiers::CONTROL =>
                {
                    return Ok(());
                }
                KeyCode::Enter => {
                    match app.window {
                        Window::Console => {
                            app.console.scroll = 0;
                            app.console
                                .output
                                .extend(app.console.input.value().into_text().unwrap());
                            let value = &app.console.input.value()[14..];

                            if let Some((stmt, _span)) = parse_line(value) {
                                let lu_dog = app.console.interpreter.lu_dog_heel();
                                let block = app.console.interpreter.block();
                                let sarzak = app.console.interpreter.sarzak_heel();
                                let models = app.console.interpreter.models();

                                let stmt = {
                                    let mut writer = lu_dog.write().unwrap();
                                    match inter_statement(
                                        &Arc::new(RwLock::new(stmt)),
                                        &DwarfSourceFile::new(value.to_owned(), &mut writer),
                                        &block,
                                        &mut writer,
                                        &models.read().unwrap(),
                                        &sarzak.read().unwrap(),
                                    ) {
                                        Ok(stmt) => stmt.0,
                                        Err(e) => {
                                            println!("{}", e);
                                            continue;
                                        }
                                    }
                                };

                                // 🚧 This needs fixing too.
                                match eval_statement(stmt, &mut app.console.interpreter) {
                                    Ok((value, ty)) => {
                                        let value = format!("{}", value.read().unwrap());
                                        // println!("\n'{}'", value);
                                        // app.console.output.extend(value.into_text().unwrap());
                                        // print!("\n'{}'", result_style.paint(value));

                                        // let ty = PrintableValueType(&ty, &context);
                                        // let ty = format!("{}", ty);
                                        // println!("\t  ──➤  {}", type_style.paint(ty));
                                    }
                                    Err(e) => {
                                        if let ChaChaError::TypeMismatch {
                                            expected: _,
                                            got: _,
                                            span,
                                        } = &e
                                        {
                                            // This belongs in the library, I think?
                                            // I'll just do it here and see how it feels
                                            // later.
                                            let mut iter = app.source.lines().iter();
                                            let mut next = iter.next().unwrap();
                                            let mut len = next.len() + 1;
                                            let mut n = next.len();
                                            let mut m = 0;
                                            // app.console.output.extend(
                                            //     format!("({m},{n}), ({}): {}", next.len(), next)
                                            //         .into_text()
                                            //         .unwrap(),
                                            // );
                                            while n < span.start {
                                                next = iter.next().unwrap();
                                                m += 1;
                                                len = next.len() + 1; // `\n`
                                                n += len;
                                                // app.console.output.extend(
                                                //     format!(
                                                //         "({m},{n}), ({}): {}",
                                                //         next.len(),
                                                //         next
                                                //     )
                                                //     .into_text()
                                                //     .unwrap(),
                                                // );
                                                log::debug!(target: "Bar", "({m},{n},{len}), ({}): {}", next.len(), next);
                                                // dbg!(&next, &n);
                                            }
                                            // dbg!(&span, &m, &n, &next.len());
                                            // let cursor = app.source.cursor();
                                            // let mut scroll = scroll.saturating_add_signed(app.console.scroll);
                                            // m -= cursor.0;
                                            // dbg!(&m);
                                            // app.console.output.extend(
                                            //     format!("({},{})", m, n).into_text().unwrap(),
                                            // );
                                            log::debug!(target: "Foo", "({},{})", m, n - span.start);
                                            log::debug!("{:?}", app.source.cursor());
                                            log::debug!("span: ({},{})", span.start, span.end);
                                            log::debug!("{}", (len + span.start - n) as u16);
                                            // app.console.output.extend(
                                            //     format!("({},{})", cursor.0, cursor.1)
                                            //         .into_text()
                                            //         .unwrap(),
                                            // );

                                            // app.source.scroll((m as i16, (n - span.start) as i16));
                                            // app.source.scroll((m as i16, cursor.1 as i16));
                                            app.source.move_cursor(CursorMove::Jump(
                                                m as u16,
                                                (len - (n - span.start)) as u16,
                                            ));
                                            // app.console.output.extend(
                                            //     format!("({},{})", cursor.0, cursor.1)
                                            //         .into_text()
                                            //         .unwrap(),
                                            // );
                                        }
                                        app.console
                                            .output
                                            .extend(e.to_string().into_text().unwrap());
                                        if let ChaChaError::Return { value: _, ty: _ } = e {
                                            println!("👋 Bye bye!");
                                            // break;
                                        }
                                    }
                                }
                            } else {
                                app.console.output.extend(
                                    format!("Something went wrong: {}", value)
                                        .into_text()
                                        .unwrap(),
                                );
                                // println!("{}", error_style.paint("WTF?"));
                            }

                            // This is lame.
                            app.console.input = Input::new(app.console.interpreter.prompt().into());
                        }
                        Window::Source => {
                            app.source.input(Event::Key(key));
                        }
                    }
                }
                _ => {
                    match app.window {
                        Window::Console => {
                            app.console.input.handle_event(&Event::Key(key));
                        }
                        Window::Source => {
                            app.source.input(Event::Key(key));
                        }
                    };
                }
            }
        } else if let Event::Mouse(e) = event::read()? {
            if e.kind == event::MouseEventKind::ScrollUp {
                app.scroll_up(e);
            } else if e.kind == event::MouseEventKind::ScrollDown {
                app.scroll_down(e);
            } else if e.kind == event::MouseEventKind::Moved {
                app.mouse_position = (e.row, e.column);
            } else if e.kind == event::MouseEventKind::Down(event::MouseButton::Left) {
                app.select_window();
            }
        }
    }
}

const MIN_COLUMN_WIDTH: u16 = 99;

fn draw_frame<B: Backend>(f: &mut Frame<B>, app: &mut App) {
    let dwarf = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Percentage(60),
                Constraint::Percentage(40),
                // Constraint::Min(MIN_COLUMN_WIDTH),
            ]
            .as_ref(),
        )
        .split(f.size());
    let con_out = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(60), Constraint::Percentage(40)].as_ref())
        .split(dwarf[1]);
    let console = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Min(1)].as_ref())
        .split(con_out[0]);
    let console = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints(
            [
                Constraint::Min(1),
                Constraint::Length(1),
                Constraint::Length(1),
            ]
            .as_ref(),
        )
        .split(console[0]);
    let stdout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Min(1)].as_ref())
        .split(con_out[1]);

    let (row, col) = app.mouse_position;
    if row >= dwarf[0].top() && row <= dwarf[0].bottom() {
        if col >= dwarf[0].left() && col <= dwarf[0].right() {
            app.hover = Window::Source;
        }
    } else if row >= console[0].top() && row <= console[0].bottom() {
        if col >= console[0].left() && col <= console[0].right() {
            app.hover = Window::Console;
        }
    }

    let source = &mut app.source;
    source.set_line_number_style(Style::default().fg(Color::DarkGray));
    let block = Block::default()
        .borders(Borders::ALL)
        .style(Style::default().fg(Color::Gray))
        .border_style(Style::default().fg(Color::Gray))
        // .border_type(BorderType::Thick)
        .title("Source: fib.tao");
    let block = match app.window {
        Window::Console => block,
        Window::Source => block
            .border_type(BorderType::Thick)
            .style(Style::default().fg(Color::Yellow))
            .border_style(Style::default().fg(Color::Yellow)),
    };
    source.set_block(block);
    f.render_widget(source.widget(), dwarf[0]);
    // f.render_widget(source.syntax_widget(&app.theme), dwarf[0]);

    // let tui_sm = TuiLoggerSmartWidget::default()
    //     .style_error(Style::default().fg(Color::Red))
    //     .style_debug(Style::default().fg(Color::Green))
    //     .style_warn(Style::default().fg(Color::Yellow))
    //     .style_trace(Style::default().fg(Color::Magenta))
    //     .style_info(Style::default().fg(Color::Cyan))
    //     .output_separator(':')
    //     .output_timestamp(Some("%H:%M:%S".to_string()))
    //     .output_level(Some(TuiLoggerLevelOutput::Abbreviated))
    //     .output_target(true)
    //     .output_file(true)
    //     .output_line(true)
    //     .state(&mut app.states[sel]);
    // f.render_widget(tui_sm, stdout[1]);
    let filter_state = TuiWidgetState::new()
        .set_default_display_level(log::LevelFilter::Off)
        .set_level_for_target("New event", log::LevelFilter::Debug)
        .set_level_for_target("info", log::LevelFilter::Info);
    let tui_w: TuiLoggerWidget = TuiLoggerWidget::default()
        .block(
            Block::default()
                .title("Logging Output")
                .border_style(Style::default().fg(Color::White).bg(Color::Black))
                .borders(Borders::ALL),
        )
        .output_separator('|')
        .style_error(Style::default().fg(Color::Red))
        .style_debug(Style::default().fg(Color::Green))
        .style_warn(Style::default().fg(Color::Yellow))
        .style_trace(Style::default().fg(Color::Magenta))
        .style_info(Style::default().fg(Color::Cyan))
        // .output_separator(':')
        // .output_timestamp(Some("%F %H:%M:%S%.3f".to_string()))
        .output_level(Some(TuiLoggerLevelOutput::Long))
        .output_target(false)
        .output_file(false)
        .output_line(false)
        .style(Style::default().fg(Color::White).bg(Color::Black));
    // .state(&filter_state);
    f.render_widget(tui_w, stdout[0]);

    // let block = Paragraph::new(app.stdout.clone())
    //     .block(Block::default().borders(Borders::ALL).title("Stdout"))
    //     .style(Style::default().fg(Color::Gray));
    // f.render_widget(block, stdout[0]);
    // .border_style(Style::default().fg(Color::Gray));

    let block = Block::default()
        .title("Console")
        .borders(Borders::ALL)
        .style(Style::default().fg(Color::Gray))
        .border_style(Style::default().fg(Color::Gray));
    let block = match app.window {
        Window::Source => block,
        Window::Console => block
            .border_type(BorderType::Thick)
            .style(Style::default().fg(Color::Yellow))
            .border_style(Style::default().fg(Color::Yellow)),
    };
    f.render_widget(block, con_out[0]);

    let height = console[0].height;
    let lines = app.console.output.lines.len() as u16;
    let scroll = if height >= lines {
        app.console.scroll = 0;
        0
    } else {
        let scroll = lines - height;
        let mut scroll = scroll.saturating_add_signed(app.console.scroll);

        // dbg!(&scroll, &app.output.lines.len(), &app.scroll);

        if scroll == 0 {
            app.console.scroll += 1;
            scroll = 1;
        }

        if scroll + height >= lines {
            app.console.scroll -= 1;
            scroll -= 1;
        }
        scroll
    };

    // dbg!(&scroll, &app.scroll);

    let messages = Paragraph::new(app.console.output.clone())
        .wrap(Wrap { trim: false })
        .scroll((scroll, 0));

    f.render_widget(messages, console[0]);

    let width = console[0].width.max(3) - 3; // keep 2 for borders and 1 for cursor

    let scroll = app.console.input.visual_scroll(width as usize);
    let input = Paragraph::new(app.console.input.value().into_text().unwrap())
        .style(Style::default())
        .scroll((0, scroll as u16));
    // .block(Block::default().borders(Borders::ALL).title("Input"));
    f.render_widget(input, console[2]);

    if app.window == Window::Console {
        f.set_cursor(
            // Put cursor past the end of the input text
            console[2].x + ((app.console.input.visual_cursor()).max(scroll) - scroll - 7) as u16,
            // Move one line down, from the border to the input line
            console[2].y,
        )
        // } else {
        // let cursor = app.source.cursor();
        // f.set_cursor(cursor.0 as u16, cursor.1 as u16)
        // app.source.show_cursor(f);
        // app.source
        //     .set_cursor_style(Style::default().add_modifier(Modifier::REVERSED));
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    if args.list_themes {
        let ts = ThemeSet::load_defaults();

        println!("Embedded themes:");

        for t in ts.themes.keys() {
            eprintln!("- {}", t);
        }

        return Ok(());
    }

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    crossterm::execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Set max_log_level to Trace
    tui_logger::init_logger(log::LevelFilter::Trace).unwrap();

    // Set default level for unknown targets to Trace
    tui_logger::set_default_level(log::LevelFilter::Trace);

    // create app and run it
    let app = App::new(args.theme, &args.object_file);
    let res = run_app(&mut terminal, app);

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err)
    }

    Ok(())
}
