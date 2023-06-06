use tracy_client::Client;

cfg_if::cfg_if! {
if #[cfg(not(feature = "single"))] {
use std::{
    io::{self, stdout},
    panic::{self, PanicInfo},
    path::PathBuf,
    sync::Arc,
    thread,
    time::Duration,
};

use ansi_to_tui::IntoText;
use dwarf::{
    interpreter::{
        banner2, initialize_interpreter_paths, start_repl2, DebuggerControl, DebuggerStatus,
        MemoryUpdateMessage,
    },
    ref_read, ChaChaError,
};
use clap::Parser;
use crossbeam::channel::{Receiver, RecvTimeoutError, Sender};
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers, MouseEvent,
    },
    execute,
    style::Print,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use log;
use parking_lot::{Condvar, Mutex, RwLock as ParkingLotRwLock};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::Text,
    widgets::{Block, BorderType, Borders, Paragraph, Wrap},
    Frame, Terminal,
};
use syntect::highlighting::ThemeSet;
use tui_input::{backend::crossterm::EventHandler, Input};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget};
use tui_textarea::{CursorMove, TextArea};
use tui_tree_widget::{Tree, TreeItem, TreeState};

macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        name.strip_suffix("::f").unwrap()
    }};
}

macro_rules! debug {
    ($($arg:tt)*) => {
        log::debug!("{} -- {}", function!(), format_args!($($arg)*));
    };
}

macro_rules! error {
    ($($arg:tt)*) => {
        log::error!("{} -- {}", function!(), format_args!($($arg)*));
    };
}

type SharedRef<T> = Arc<ParkingLotRwLock<T>>;

pub struct StatefulTree<'a> {
    pub state: TreeState,
    pub items: Vec<TreeItem<'a>>,
}

impl<'a> StatefulTree<'a> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            state: TreeState::default(),
            items: Vec::new(),
        }
    }

    pub fn with_items(items: Vec<TreeItem<'a>>) -> Self {
        Self {
            state: TreeState::default(),
            items,
        }
    }

    pub fn first(&mut self) {
        self.state.select_first();
    }

    pub fn last(&mut self) {
        self.state.select_last(&self.items);
    }

    pub fn down(&mut self) {
        self.state.key_down(&self.items);
    }

    pub fn up(&mut self) {
        self.state.key_up(&self.items);
    }

    pub fn left(&mut self) {
        self.state.key_left();
    }

    pub fn right(&mut self) {
        self.state.key_right();
    }

    pub fn toggle(&mut self) {
        self.state.toggle_selected();
    }
}

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

#[derive(Clone, Copy, Eq, PartialEq)]
enum Window {
    Console,
    Source,
    Stack,
    Logger,
}

struct App<'a> {
    console: Console<'a>,
    source: TextArea<'a>,
    stack: StatefulTree<'a>,
    frame_number: usize,
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

        let items = vec![TreeItem::new("0:", vec![])];

        App {
            console,
            source,
            stack: StatefulTree::with_items(items),
            frame_number: 0,
            window: Window::Console,
            hover: Window::Console,
            theme,
            mouse_position: (0, 0),
        }
    }

    fn scroll_up(&mut self, e: MouseEvent) {
        match self.hover {
            Window::Console => self.console.scroll_up(),
            Window::Logger => true,
            Window::Source => self.source.input(e),
            Window::Stack => true,
        };
    }

    fn scroll_down(&mut self, e: MouseEvent) {
        match self.hover {
            Window::Console => self.console.scroll_down(),
            Window::Logger => true,
            Window::Source => self.source.input(e),
            Window::Stack => true,
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
    repl_send: Sender<DebuggerControl>,
    repl_recv: Receiver<DebuggerStatus>,
    memory_recv: Receiver<MemoryUpdateMessage>,
    scroll: i16,
    source: String,
    prompt: String,
}

impl Console<'_> {
    fn new(object_file: &PathBuf) -> Console<'static> {
        let output = banner2().into_text().unwrap();

        let interpreter = initialize_interpreter_paths(object_file).unwrap();
        let prompt = interpreter.prompt().into();
        let memory_recv = interpreter.register_memory_updates();
        let source = interpreter.source();

        let (repl_send, repl_recv) = start_repl2(interpreter);

        Console {
            input: Input::default(),
            output,
            repl_send,
            repl_recv,
            memory_recv,
            scroll: 0,
            source,
            prompt,
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

    fn step_into(&mut self) {
        self.repl_send
            .send(DebuggerControl::StepInto)
            .expect("Failed to send step-into command");
    }

    fn run(&mut self) {
        self.repl_send
            .send(DebuggerControl::Run)
            .expect("Failed to send run command");
    }

    fn execute(&mut self, message: &str) {
        self.repl_send
            .send(DebuggerControl::ExecuteInput(message.into()))
            .unwrap();
    }

    fn get_memory_recv(&self) -> Receiver<MemoryUpdateMessage> {
        self.memory_recv.clone()
    }

    fn get_repl_recv(&self) -> Receiver<DebuggerStatus> {
        self.repl_recv.clone()
    }
}

fn eval_console<'a>(app: &mut App) -> Result<(), ChaChaError> {
    let input = app.console.input.value().to_owned();
    app.console.input.reset();

    app.console.scroll = 0;
    app.console.output.extend(input.into_text().unwrap());
    app.console.execute(&input);

    Ok(())
}

fn repl_updater(
    app: SharedRef<App>,
    pair: Arc<(Mutex<bool>, Condvar)>,
    halt: Arc<Mutex<bool>>,
) -> io::Result<()> {
    let (lock, cvar) = &*pair;
    let receiver = app.read().console.get_repl_recv();

    loop {
        let msg = match receiver.recv_timeout(Duration::from_millis(10)) {
            Ok(msg) => msg,
            Err(RecvTimeoutError::Timeout) => {
                if *halt.lock() {
                    // We maybe want to ping the condvar here?
                    debug!("stopping");
                    break;
                }
                continue;
            }
            Err(_) => {
                // Nobody is there to send any longer.
                error!("Repl updater thread died.");
                break;
            }
        };

        match msg {
            DebuggerStatus::Error(e) => {
                debug!("Error: {}", e);
                app.write().console.output.extend(e.into_text().unwrap());
            }
            DebuggerStatus::Paused(span) => {
                let source = &mut app.write().source;

                debug!("Paused: {:?}", span);
                // This belongs in the library, I think?
                // I'll just do it here and see how it feels
                // later.
                let mut iter = source.lines().iter();
                let mut next = iter.next().unwrap();
                let mut len = next.len() + 1;
                let mut n = next.len();
                let mut m = 0;

                while n < span.start {
                    next = iter.next().unwrap();
                    m += 1;
                    len = next.len() + 1; // `\n`
                    n += len;
                    debug!("({m},{n},{len}), ({}): {}", next.len(), next);
                }

                debug!("({},{})", m, n - span.start);
                debug!("{:?}", source.cursor());
                debug!("span: ({},{})", span.start, span.end);
                debug!("{}", (len + span.start - n) as u16);

                // let midpoint = span.start + (span.end - span.start) / 2;

                source.move_cursor(CursorMove::Jump(m as u16, (len - (n - span.start)) as u16));
                // source.move_cursor(CursorMove::Jump(m as u16, (len - (n - midpoint)) as u16));
            }
            DebuggerStatus::Running => {
                debug!("Running");
            }
            DebuggerStatus::StdOut(value) => {
                debug!("StdOut: {:?}", value);
                app.write()
                    .console
                    .output
                    .extend(value.into_text().unwrap());
            }
            DebuggerStatus::Stopped(value, ty) => {
                debug!("Stopped: {:?}", value);
            }
        }

        // Do this after we'ne updated our bits.
        {
            *lock.lock() = true;
            cvar.notify_all();
        }
    }

    Ok(())
}

fn stack_updater(
    app: SharedRef<App>,
    pair: Arc<(Mutex<bool>, Condvar)>,
    halt: Arc<Mutex<bool>>,
) -> io::Result<()> {
    let (lock, cvar) = &*pair;
    let receiver = app.read().console.get_memory_recv();

    loop {
        let msg = match receiver.recv_timeout(Duration::from_millis(10)) {
            Ok(msg) => msg,
            Err(RecvTimeoutError::Timeout) => {
                if *halt.lock() {
                    // We maybe want to ping the condvar here?
                    debug!("stopping");
                    break;
                }
                continue;
            }
            Err(_) => {
                // Nobody is there to send any longer.
                error!("Stack updater thread died.");
                break;
            }
        };

        match msg {
            MemoryUpdateMessage::AddGlobal(cell) => {}
            MemoryUpdateMessage::AddMeta(cell) => {}
            MemoryUpdateMessage::AddLocal(cell) => {
                let frame_number = app.read().frame_number.clone();
                let items = &mut app.write().stack.items[frame_number];
                // let items = items.child_mut(frame_number).expect("fucker");
                items.add_child(TreeItem::new_leaf(format!(
                    "{}: {}",
                    cell.0,
                    ref_read!(cell.1)
                )));
            }
            MemoryUpdateMessage::PushFrame => {
                let frame_number = app.read().frame_number + 1;
                app.write()
                    .stack
                    .items
                    .push(TreeItem::new(format!("{frame_number}:"), Vec::new()));
                app.write().frame_number = frame_number;
            }
            MemoryUpdateMessage::PopFrame => {
                let mut app = app.write();
                app.frame_number -= 1;
                app.stack.items.pop();
            }
        };

        // Do this after we've updated our bits.
        {
            *lock.lock() = true;
            cvar.notify_all();
        }
    }

    Ok(())
}

fn run_app(
    app: SharedRef<App>,
    pair: Arc<(Mutex<bool>, Condvar)>,
    halt: Arc<Mutex<bool>>,
) -> io::Result<()> {
    let (lock, cvar) = &*pair;

    // Let the UI run.
    *lock.lock() = true;
    cvar.notify_all();

    loop {
        // debug!("waiting for event");

        // 🚧 This causes deadlock... :-( I'll put it back once I sort it out.
        // if let Ok(poll) = event::poll(Duration::from_millis(10)) {
        //     if poll {
        let event = event::read()?;

        // debug!("read event: {:?}", event);

        if let Event::Key(key) = event {
            let mut app = app.write();
            match key.code {
                KeyCode::F(5) => app.console.run(),
                KeyCode::F(11) => app.console.step_into(),
                // KeyCode::Char(' ') => app.stack.toggle(),
                KeyCode::Left => app.stack.left(),
                KeyCode::Right => app.stack.right(),
                KeyCode::Down => app.stack.down(),
                KeyCode::Up => app.stack.up(),
                KeyCode::Home => app.stack.first(),
                KeyCode::End => app.stack.last(),
                // Bail out.
                KeyCode::Char('c') | KeyCode::Char('d')
                    if key.modifiers == KeyModifiers::CONTROL =>
                {
                    *halt.lock() = true;

                    *lock.lock() = true;
                    cvar.notify_all();

                    return Ok(());
                }
                KeyCode::Enter => match app.window {
                    Window::Console => {
                        eval_console(&mut app).unwrap();
                        // eval_console(&mut app.console, &mut app.source).context(BadJuJuSnafu {
                        //     message: "no clue".to_string(),
                        // })?;
                    }
                    Window::Logger => {}
                    Window::Source => {
                        app.source.input(Event::Key(key));
                    }
                    Window::Stack => {
                        app.stack.toggle();
                    }
                },
                _ => {
                    match app.window {
                        Window::Console => {
                            app.console.input.handle_event(&Event::Key(key));
                        }
                        Window::Logger => {}
                        Window::Source => {
                            app.source.input(Event::Key(key));
                        }
                        Window::Stack => {}
                    };
                }
            }
        } else if let Event::Mouse(e) = event {
            let mut app = app.write();
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
        // } // 🚧
        // Redraw after an event, or every 10ms.
        {
            *lock.lock() = true;
            cvar.notify_all();
        }
        // } // 🚧
    }

    Ok(())
}

const MIN_SRC_COLUMN_WIDTH: u16 = 99;
const MIN_STD_OUT_COLUMN_WIDTH: u16 = 80;

fn draw_frame<B: Backend>(f: &mut Frame<B>, app: &SharedRef<App>) {
    // This is the initial vertical split.
    let dwarf = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(60), Constraint::Percentage(40)].as_ref())
        .split(f.size());
    // This is the top half of the screen.
    let source_vars = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [
                Constraint::Min(MIN_SRC_COLUMN_WIDTH),
                Constraint::Percentage(33),
            ]
            .as_ref(),
        )
        .split(dwarf[0]);
    // This is the bottom half of the screen.
    let con_out = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [
                Constraint::Min(MIN_STD_OUT_COLUMN_WIDTH),
                Constraint::Percentage(40),
            ]
            .as_ref(),
        )
        .split(dwarf[1]);
    // The console -- bottom left.
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
        .split(con_out[0]);
    // The logger is the bottom right.
    let logger = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Min(1)].as_ref())
        .split(con_out[1]);

    // Figure out where the mouse pointer is located.
    let (row, col) = app.read().mouse_position;
    for widget in [
        (con_out[0], Window::Console),
        (con_out[1], Window::Logger),
        (source_vars[0], Window::Source),
        (source_vars[1], Window::Stack),
    ] {
        if row >= widget.0.top() && row <= widget.0.bottom() {
            if col >= widget.0.left() && col <= widget.0.right() {
                app.write().hover = widget.1;
            }
        }
    }

    let block = Block::default()
        .borders(Borders::ALL)
        .style(Style::default().fg(Color::Gray))
        .border_style(Style::default().fg(Color::Gray))
        // .border_type(BorderType::Thick)
        .title("Source: fib.tao");
    let block = match app.read().window {
        Window::Console => block,
        Window::Logger => block,
        Window::Source => block
            .border_type(BorderType::Thick)
            .style(Style::default().fg(Color::Yellow))
            .border_style(Style::default().fg(Color::Yellow)),
        Window::Stack => block,
    };

    {
        let source = &mut app.write().source;
        source.set_line_number_style(Style::default().fg(Color::DarkGray));
        source.set_block(block);
        f.render_widget(source.widget(), source_vars[0]);
    }
    // f.render_widget(source.syntax_widget(&app.theme), dwarf[0]);

    let block = Block::default()
        .borders(Borders::ALL)
        .style(Style::default().fg(Color::Gray))
        .border_style(Style::default().fg(Color::Gray))
        // .border_type(BorderType::Thick)
        .title("Logging Output");
    let block = match app.read().window {
        Window::Console => block,
        Window::Logger => block
            .border_type(BorderType::Thick)
            .style(Style::default().fg(Color::Yellow))
            .border_style(Style::default().fg(Color::Yellow)),
        Window::Source => block,
        Window::Stack => block,
    };

    let tui_logger: TuiLoggerWidget = TuiLoggerWidget::default()
        .block(
            block, // Block::default()
                  //     .title("Logging Output")
                  //     .border_style(Style::default().fg(Color::White).bg(Color::Black))
                  //     .borders(Borders::ALL),
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
        .output_line(false);
    // .style(Style::default().fg(Color::White).bg(Color::Black));
    // .state(&filter_state);
    f.render_widget(tui_logger, logger[0]);

    let block = Block::default()
        .title("Stack Frames")
        .borders(Borders::ALL)
        .style(Style::default().fg(Color::Gray))
        .border_style(Style::default().fg(Color::Gray));
    let block = match app.read().window {
        Window::Console => block,
        Window::Logger => block,
        Window::Source => block,
        Window::Stack => block
            .border_type(BorderType::Thick)
            .style(Style::default().fg(Color::Yellow))
            .border_style(Style::default().fg(Color::Yellow)),
    };

    let items = app.read().stack.items.clone();
    let items = Tree::new(items)
        .block(block)
        .highlight_style(
            Style::default()
                .fg(Color::Black)
                .bg(Color::LightGreen)
                .add_modifier(Modifier::BOLD),
        )
        .highlight_symbol(">> ");
    f.render_stateful_widget(items, source_vars[1], &mut app.write().stack.state);

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
    let block = match app.read().window {
        Window::Console => block
            .border_type(BorderType::Thick)
            .style(Style::default().fg(Color::Yellow))
            .border_style(Style::default().fg(Color::Yellow)),
        Window::Logger => block,
        Window::Source => block,
        Window::Stack => block,
    };
    f.render_widget(block, con_out[0]);

    let height = console[0].height;
    let width = console[0].width as usize;
    let lines = app
        .read()
        .console
        .output
        .lines
        .iter()
        .map(|line| {
            line.spans
                .iter()
                .map(|span| span.content.chars().count())
                .sum::<usize>()
                / width
                + 1
        })
        .sum::<usize>() as u16;
    let scroll = if height >= lines {
        app.write().console.scroll = 0;
        0
    } else {
        let offset = lines - height;
        let scroll = app.read().console.scroll;
        let mut offset = offset.saturating_add_signed(scroll);

        // This is the top of the buffer.
        if offset == 0 {
            // Interestingly, doing this incrementally keeps the screen from
            // jumping about erratically.
            app.write().console.scroll += 1;
            offset = 1;
        }

        if offset + height > lines {
            // Some here -- no surprise. However, we need to be incremental in
            // the update of scroll, otherwise the screen jumps.
            app.write().console.scroll -= 1;
            offset -= 1;
        }

        offset
    };

    // dbg!(&scroll, &app.scroll);

    let std_out = Paragraph::new(app.read().console.output.clone())
        // 🚧  see above
        .wrap(Wrap { trim: false })
        .scroll((scroll, 0));

    f.render_widget(std_out, console[0]);

    let width = console[0].width.max(3) - 3; // keep 2 for borders and 1 for cursor

    let scroll = app.read().console.input.visual_scroll(width as usize);
    let input = Paragraph::new(app.read().console.input.value().into_text().unwrap())
        .style(Style::default())
        .scroll((0, scroll as u16));
    // .block(Block::default().borders(Borders::ALL).title("Input"));

    let line = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Min("道:>".len() as u16), Constraint::Min(1)].as_ref())
        .split(console[2]);

    // f.render_widget(input, console[2]);
    let prompt =
        Paragraph::new(app.read().console.prompt.into_text().unwrap()).style(Style::default());
    f.render_widget(prompt, line[0]);
    f.render_widget(input, line[1]);

    if app.read().window == Window::Console {
        f.set_cursor(
            // Put cursor past the end of the input text
            line[1].x + (app.read().console.input.visual_cursor()).max(scroll) as u16,
            // Move one line down, from the border to the input line
            line[1].y,
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
            println!("- {}", t);
        }

        return Ok(());
    }

    let _client = Client::start();

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    crossterm::execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Set panic hook
    panic::set_hook(Box::new(panic_hook));

    // Set max_log_level to Trace
    tui_logger::init_logger(log::LevelFilter::Debug).unwrap();

    // Set default level for unknown targets to Trace
    tui_logger::set_default_level(log::LevelFilter::Debug);

    // // Early initialization of the logger
    // let drain = tui_logger::Drain::new();
    // // instead of tui_logger::init_logger, we use `env_logger`
    // env_logger::Builder::default()
    //     .format(move |buf, record| {
    //         dbg!(&record);
    //         // patch the env-logger entry through our drain to the tui-logger
    //         Ok(drain.log(record))
    //     })
    //     .init(); // make this the global logger

    let halt = Arc::new(Mutex::new(false));
    let halt2 = halt.clone();
    let halt3 = halt.clone();
    let halt4 = halt.clone();

    let pair = Arc::new((Mutex::new(true), Condvar::new()));
    let pair2 = Arc::clone(&pair);
    let pair3 = Arc::clone(&pair);
    let pair4 = Arc::clone(&pair);

    let app = Arc::new(ParkingLotRwLock::new(App::new(
        args.theme,
        &args.object_file,
    )));
    let app2 = app.clone();
    let app3 = app.clone();
    let app4 = app.clone();

    // Inside of our lock, spawn a new thread, and then wait for it to start.
    // thread::spawn(move || {
    // let (lock, cvar) = &*pair2;
    // create app and run it
    // let res = run_app(app2, pair2);
    // let mut started = lock.lock().unwrap();
    // *started = true;
    // We notify the condvar that the value has changed.
    // cvar.notify_one();
    // });

    let updater = thread::spawn(move || stack_updater(app3, pair3, halt3));

    let repl = thread::spawn(move || repl_updater(app4, pair4, halt4));

    let renderer = thread::spawn(move || {
        // Wait for the thread to start up.
        let (lock, cvar) = &*pair;
        loop {
            let mut started = lock.lock();
            if !*started {
                // dbg!("waiting");
                cvar.wait(&mut started);
            }

            *started = false;
            // dbg!(["notified"]);
            terminal.draw(|f| draw_frame(f, &app)).unwrap();

            if *halt.lock() {
                break;
            }
        }

        // restore terminal
        disable_raw_mode().unwrap();
        execute!(
            terminal.backend_mut(),
            LeaveAlternateScreen,
            DisableMouseCapture
        )
        .unwrap();
        terminal.show_cursor().unwrap();
    });

    let res = run_app(app2, pair2, halt2);

    if let Err(err) = res {
        println!("{:?}", err)
    }

    updater.join().unwrap()?;
    renderer.join().unwrap();
    repl.join().unwrap()?;

    Ok(())
}

/// A panic hook to properly restore the terminal in the case of a panic.
/// Based on [spotify-tui's implementation](https://github.com/Rigellute/spotify-tui/blob/master/src/main.rs).
pub fn panic_hook(panic_info: &PanicInfo<'_>) {
    let mut stdout = stdout();

    let msg = match panic_info.payload().downcast_ref::<&'static str>() {
        Some(s) => *s,
        None => match panic_info.payload().downcast_ref::<String>() {
            Some(s) => &s[..],
            None => "Box<Any>",
        },
    };

    let stacktrace: String = format!("{:?}", backtrace::Backtrace::new());

    disable_raw_mode().unwrap();
    execute!(stdout, DisableMouseCapture, LeaveAlternateScreen).unwrap();

    // Print stack trace.  Must be done after!
    execute!(
        stdout,
        Print(format!(
            "thread '<unnamed>' panicked at '{}', {}\n\r{}",
            msg,
            panic_info.location().unwrap(),
            stacktrace
        )),
    )
    .unwrap();
}
} else{
    fn main() {
       let error_style = ansi_term::Colour::Red;
       let alert_style = ansi_term::Colour::Yellow;

        println!("{}: The debugger requires the feature flag: `{}`.",
            error_style.paint("error"),
            alert_style.paint("std-rwlock"));
    }
}
}
