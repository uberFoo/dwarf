use ansi_term::Colour;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use log;
use rustc_hash::FxHashMap as HashMap;

use crate::dwarf::{
    Attribute, AttributeMap, BlockType, DwarfFloat, EnumField, Expression as DwarfExpression,
    Generics, InnerAttribute, InnerItem, Item, Pattern, Spanned, Statement, Token, Type,
};

use super::{error::DwarfError, DwarfInteger};

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
    ($msg:literal, $($arg:expr),*) => {
        $(
            log::trace!(
                target: "parser",
                "{}: {} --> {:?}\n  --> {}:{}:{}",
                Colour::Green.dimmed().italic().paint(function!()),
                Colour::Yellow.underline().paint($msg),
                $arg,
                file!(),
                line!(),
                column!()
            );
        )*
    };
    ($arg:literal) => {
        log::trace!(
            target: "parser",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            $arg,
            file!(),
            line!(),
            column!())
    };
    ($arg:expr) => {
        log::trace!(
            target: "parser",
            "{}: {:?}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            $arg,
            file!(),
            line!(),
            column!())
    };
}

macro_rules! error {
    ($msg:literal, $($arg:expr),*) => {
        $(
            log::debug!(
                target: "parser",
                "{}: {} --> {:?}\n  --> {}:{}:{}",
                Colour::Green.dimmed().italic().paint(function!()),
                Colour::Red.underline().paint($msg),
                $arg,
                file!(),
                line!(),
                column!()
            );
        )*
    };
    ($arg:literal) => {
        log::debug!(
            target: "parser",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            Colour::Red.underline().paint($arg),
            file!(),
            line!(),
            column!())
    };
    ($arg:expr) => {
        log::debug!(
            target: "parser",
            "{}: {:?}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            Colour::Ref.underline().paint($arg),
            file!(),
            line!(),
            column!())
    };
}

type Result<T, E = Box<Simple<String>>> = std::result::Result<T, E>;
type Expression = (Spanned<DwarfExpression>, (u8, u8));

// These are the binding strengths of the operators used by the parser.
// The idea comes from
// [this article](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
// on Pratt parsing.
// The first value in the tuple is the binding power of the operator on its left,
// and the second value is the binding power on its right. Assign is the only
// one that's inverted, and that seems wrong. Shouldn't '.' (for method calls)
// also associate right to left?
const PATH: (u8, u8) = (100, 100);
const METHOD: (u8, u8) = (90, 91);
const FIELD: (u8, u8) = (80, 81);
const FUNC_CALL: (u8, u8) = (70, 71);
const UNARY: (u8, u8) = (60, 61);
const AS_OP: (u8, u8) = (55, 56);
const MUL_DIV: (u8, u8) = (52, 53);
const ADD_SUB: (u8, u8) = (50, 51);
const COMP: (u8, u8) = (30, 31);
const BOOL: (u8, u8) = (25, 26);
const RANGE: (u8, u8) = (20, 21);
const ASSIGN: (u8, u8) = (11, 0);
// Literal, closure, break, return, etc.
const LITERAL: (u8, u8) = (0, 1);
const BLOCK: (u8, u8) = (0, 0);
const STRUCT: (u8, u8) = (0, 0);
const ENTER: u8 = 0;

fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    // A parser for ints
    let int = text::int(10).map(Token::Integer);

    // Float parser
    let float = text::int(10)
        .chain::<char, _, _>(just('.').chain::<char, _, _>(text::digits(10)))
        .collect::<String>()
        .map(Token::Float);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::String);

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "as" => Token::As,
        "asm" => Token::Asm,
        "async" => Token::Async,
        "await" => Token::Await,
        "bool" => Token::Type(Type::Boolean),
        "debugger" => Token::Debugger,
        "else" => Token::Else,
        "enum" => Token::Enum,
        "false" => Token::Bool(false),
        "float" => Token::Type(Type::Float),
        "fn" => Token::Fn,
        "for" => Token::For,
        "if" => Token::If,
        "impl" => Token::Impl,
        "int" => Token::Type(Type::Integer),
        "in" => Token::In,
        "let" => Token::Let,
        "match" => Token::Match,
        "mod" => Token::Mod,
        "print" => Token::Print,
        "return" => Token::Return,
        "string" => Token::Type(Type::String),
        "struct" => Token::Struct,
        "true" => Token::Bool(true),
        "use" => Token::Use,
        _ => Token::Ident(ident),
    });

    // A parser for punctuation (delimiters, semicolons, etc.)
    let punct = one_of("#=-()[]{}:;,.|&<>+*/!").map(Token::Punct);

    // A single token can be one of the above
    let token = float
        .or(int)
        .or(str_)
        // .or(dagger)
        // .or(double_colon)
        // .or(op)
        .or(punct)
        // .or(object)
        // .or(some)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();
    // let doc_comment = just('#').padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        // .padded_by(doc_comment.repeated())
        .padded()
        .repeated()
}

#[derive(Debug)]
struct DwarfParser {
    tokens: Vec<Spanned<Token>>,
    current: usize,
    errors: Vec<Simple<String>>,
    async_block: u8,
}

impl DwarfParser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
            async_block: 0,
        }
    }

    /// Parse a program
    ///
    /// A program is a list of items
    ///
    /// program -> item*
    fn parse_program(&mut self) -> (Vec<Item>, Vec<Simple<String>>) {
        debug!("enter");

        let mut result = Vec::new();

        while !self.at_end() {
            if let Some(item) = self.parse_item() {
                debug!("item", item);
                result.push(item);
            } else {
                let tok = if let Some(tok) = self.peek() {
                    tok
                } else {
                    self.previous().unwrap()
                };

                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [
                        Some("use".to_owned()),
                        Some("impl".to_owned()),
                        Some("struct".to_owned()),
                        Some("Fn".to_owned()),
                    ],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected item");

                self.errors.push(err);

                error!("parse_program: resynchronize looking for '}'");
                while !self.at_end() && self.match_tokens(&[Token::Punct('}')]).is_none() {
                    self.advance();
                }
                error!("parse_program: resynchronized");
            }
        }

        debug!("exit", result);

        (result, self.errors.clone())
    }

    fn parse_pattern(&mut self) -> Result<Option<Spanned<Pattern>>> {
        let mut path_span = 0..0;
        let mut path = Vec::new();
        while let Some(ident) = self.parse_ident() {
            if path_span.start == 0 {
                path_span.start = ident.1.start;
            }
            path_span.end = ident.1.end;

            debug!("ident", ident);
            path.push(Type::UserType(ident.clone(), vec![]));

            match self.match_tokens(&[Token::Punct(':'), Token::Punct('('), Token::Punct('{')]) {
                Some((Token::Punct(':'), span)) => match self.match_tokens(&[Token::Punct(':')]) {
                    // No need to do anything, we just matched the second colon. The loop
                    // will iterate, and find the next element of the path.
                    Some(_) => {
                        let generics = self.parse_generics()?;
                        if let Some(generics) = generics {
                            path.pop();
                            path.push(Type::UserType(ident.clone(), generics.0));

                            if self.match_tokens(&[Token::Punct(':')]).is_none() {
                                let err = Simple::expected_input_found(
                                    span,
                                    [Some(":".to_owned())],
                                    Some(format!("{:?}", self.peek().unwrap())),
                                );
                                error!("exit", err);
                                return Err(Box::new(err));
                            }
                            if self.match_tokens(&[Token::Punct(':')]).is_none() {
                                let err = Simple::expected_input_found(
                                    span,
                                    [Some(":".to_owned())],
                                    Some(format!("{:?}", self.peek().unwrap())),
                                );
                                error!("exit", err);
                                return Err(Box::new(err));
                            }
                        }
                    }
                    None => {
                        let err = Simple::expected_input_found(
                            span,
                            [Some(":".to_owned())],
                            Some(format!("{:?}", self.peek().unwrap())),
                        );
                        error!("exit", err);
                        return Err(Box::new(err));
                    }
                },
                Some((Token::Punct('('), span)) => {
                    let mut args_span = 0..0;
                    let mut args = Vec::new();
                    while let Some(pat) = self.parse_pattern()? {
                        if args_span.start == 0 {
                            args_span.start = pat.1.start;
                        }
                        args_span.end = pat.1.end;
                        debug!("arg", pat);
                        args.push(pat);
                        let _ = self.match_tokens(&[Token::Punct(',')]);
                    }
                    match self.match_tokens(&[Token::Punct(')')]) {
                        Some((Token::Punct(')'), _)) => {
                            let path = Pattern::PathPattern((path, path_span));
                            debug!("path", path);
                            return Ok(Some((
                                Pattern::TupleStruct(Box::new(path), (args, args_span)),
                                span,
                            )));
                        }
                        Some((tok, span2)) => {
                            let err = Simple::unclosed_delimiter(
                                span,
                                "(".to_owned(),
                                span2,
                                ")".to_owned(),
                                Some(tok.to_string()),
                            );
                            debug!("exit no ')'");
                            return Err(Box::new(err));
                        }
                        None => {}
                    }
                }
                Some((Token::Punct('{'), _span)) => {}
                Some((tok, span)) => {
                    let err = Simple::expected_input_found(
                        span,
                        [
                            Some("(".to_owned()),
                            Some("{".to_owned()),
                            Some(":".to_owned()),
                        ],
                        Some(format!("{:?}", tok)),
                    );
                    error!("exit", err);
                    return Err(Box::new(err));
                }
                None => {}
            }
        }

        if path.is_empty() {
            Ok(None)
        } else if path.len() == 1 {
            let ty = if let Type::UserType(ty, _) = path[0].clone() {
                ty
            } else {
                unreachable!()
            };
            Ok(Some((Pattern::Identifier(ty), path_span)))
        } else {
            Ok(Some((
                Pattern::PathPattern((path, path_span.clone())),
                path_span,
            )))
        }
    }

    /// Parse an if expression
    ///
    /// if --> IF expression { BLOCK } (ELSE { BLOCK })?
    fn parse_if_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit");
            return Ok(None);
        };

        if self.match_tokens(&[Token::If]).is_none() {
            debug!("exit");
            return Ok(None);
        }

        let pattern = if self.match_tokens(&[Token::Let]).is_some() {
            let pattern = self.parse_pattern()?;
            if self.match_tokens(&[Token::Punct('=')]).is_none() {
                let tok = self.peek().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("'='".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected equals");
                error!("exit", err);
                return Err(Box::new(err));
            }
            pattern
        } else {
            None
        };

        debug!("getting conditional");
        let conditional = if let Some(cond) = self.parse_expression(LITERAL.1)? {
            debug!("conditional", cond);
            cond
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            debug!("exit");
            return Err(Box::new(err));
        };

        debug!("getting true block");
        let true_block = if let Some(expr) = self.parse_block_expression()? {
            debug!("true block", expr);
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            debug!("exit");
            return Err(Box::new(err));
        };

        let false_block = if self.match_tokens(&[Token::Else]).is_some() {
            debug!("getting false block");
            let false_block = if let Some(expr) = self.parse_block_expression()? {
                debug!("false block", expr);
                expr
            } else {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("<expression>".to_owned())],
                    Some(token.0.to_string()),
                );
                error!("no false block");
                return Err(Box::new(err));
            };

            debug!("exit getting false block");

            Some(Box::new(false_block.0))
        } else {
            None
        };

        debug!("exit match");

        if let Some(pattern) = pattern {
            Ok(Some((
                (
                    DwarfExpression::Match(
                        Box::new(conditional.clone().0),
                        vec![((pattern.0, true_block.0 .0), true_block.0 .1)],
                    ),
                    start..self.previous().unwrap().1.end,
                ),
                BLOCK,
            )))
        } else {
            Ok(Some((
                (
                    DwarfExpression::If(
                        Box::new(conditional.0),
                        Box::new(true_block.0),
                        false_block,
                    ),
                    start..self.previous().unwrap().1.end,
                ),
                BLOCK,
            )))
        }
    }

    fn parse_inner_attribute(&mut self) -> Option<InnerAttribute> {
        if let Some(Attribute { name, value }) = self.parse_attribute_key_value() {
            debug!("parsing inner attribute^2");

            let mut values = HashMap::default();
            values
                .entry(name.0)
                .or_insert_with(Vec::new)
                .push((name.1, value));

            if self.match_tokens(&[Token::Punct(',')]).is_none() {
                debug!("no comma");
            } else {
                while let Some(Attribute { name, value }) = self.parse_attribute_key_value() {
                    debug!("parsed inner attribute^2", name, value);
                    values
                        .entry(name.0)
                        .or_insert_with(Vec::new)
                        .push((name.1, value));
                    if self.match_tokens(&[Token::Punct(',')]).is_none() {
                        debug!("no comma");
                        break;
                    }
                }
            }

            debug!("parsed inner attribute^2");
            Some(InnerAttribute::Attribute(values))
        } else {
            match self.match_tokens(&[Token::Punct('='), Token::Punct('(')]) {
                Some((Token::Punct('='), _)) => {
                    debug!("getting value for key");
                    let value = if let Ok(Some(value)) = self.parse_expression(ENTER) {
                        debug!("value", value);
                        value
                    } else {
                        let token = self.previous().unwrap();
                        let err = Simple::expected_input_found(
                            token.1.clone(),
                            [Some("<expression>".to_owned())],
                            Some(token.0.to_string()),
                        );
                        error!("error", err);
                        self.errors.push(err);
                        return None;
                    };
                    debug!("exit getting value");
                    Some(InnerAttribute::Expression(value.0))
                }
                Some((Token::Punct('('), _)) => {
                    debug!("parsing inner attribute");
                    let inner = self.parse_inner_attribute();
                    if self.match_tokens(&[Token::Punct(')')]).is_none() {
                        let token = self.previous().unwrap();
                        let err = Simple::expected_input_found(
                            token.1.clone(),
                            [Some(")".to_owned())],
                            Some(token.0.to_string()),
                        );
                        error!("error", err);
                        self.errors.push(err);
                        return None;
                    }
                    debug!("done parsing inner attribute", inner);
                    inner
                }
                None => Some(InnerAttribute::None),
                Some(_) => unreachable!(),
            }
        }
    }

    fn parse_attribute_key_value(&mut self) -> Option<Attribute> {
        let name = if let Some(name) = self.parse_ident() {
            debug!("name", name);
            name
        } else {
            // let token = self.previous().unwrap();
            // let err = Simple::expected_input_found(
            //     token.1.clone(),
            //     [Some("<identifier>".to_owned())],
            //     Some(token.0.to_string()),
            // );
            // error!("error", err);
            // self.errors.push(err);
            return None;
        };

        debug!("parsing attribute value");
        let value = self.parse_inner_attribute()?;

        Some(Attribute { name, value })
    }

    fn parse_attribute(&mut self) -> Option<Attribute> {
        debug!("enter");

        if self.match_tokens(&[Token::Punct('#')]).is_none() {
            debug!("exit");
            return None;
        }

        let start = if let Some(tok) = self.match_tokens(&[Token::Punct('[')]) {
            tok.1.start
        } else {
            debug!("exit");
            return None;
        };

        let attribute = self.parse_attribute_key_value()?;

        if self.match_tokens(&[Token::Punct(']')]).is_none() {
            let token = if let Some(token) = self.peek() {
                token
            } else {
                self.previous().unwrap()
            };
            let err = Simple::unclosed_delimiter(
                start..token.1.end,
                "[".to_owned(),
                token.1.clone(),
                "]".to_owned(),
                Some(token.0.to_string()),
            );
            error!("error", err);
            self.errors.push(err);
            return None;
        }

        debug!("exit");

        Some(attribute)
    }

    /// Parse an Item
    ///
    /// This should probably just return an error...
    ///  item -> Struct | ImplBlock | Import | Function
    fn parse_item(&mut self) -> Option<Item> {
        debug!("enter");

        let mut attributes = HashMap::default();
        while let Some(Attribute { name, value }) = self.parse_attribute() {
            debug!("attribute", name, value);
            attributes
                .entry(name.0)
                .or_insert_with(Vec::new)
                .push((name.1, value));
        }

        // Try to parse a struct
        match self.parse_struct() {
            Ok(Some(item)) => {
                debug!("struct", item);
                return Some(Item { item, attributes });
            }
            Ok(None) => {}
            Err(err) => {
                error!("error", err);
                self.errors.push(*err);
            }
        }

        match self.parse_enum() {
            Ok(Some(item)) => {
                debug!("enum", item);
                return Some(Item { item, attributes });
            }
            Ok(None) => {}
            Err(err) => {
                error!("error", err);
                self.errors.push(*err);
            }
        }

        if let Some(item) = self.parse_impl_block() {
            debug!("impl", item);
            return Some(Item { item, attributes });
        }

        if let Some(item) = self.parse_use() {
            debug!("import", item);
            return Some(Item { item, attributes });
        }

        if let Some(item) = self.parse_module() {
            debug!("import", item);
            return Some(Item { item, attributes });
        }

        // Try to parse a function
        match self.parse_function() {
            Ok(Some(item)) => {
                debug!("function", item);
                return Some(Item { item, attributes });
            }
            Ok(None) => {}
            Err(err) => {
                self.errors.push(*err);
            }
        }

        debug!("exit None");

        None
    }

    /// Parse a path
    ///
    /// path -> IDENTIFIER (:: IDENTIFIER)*
    fn parse_path(&mut self) -> Option<Spanned<Vec<Spanned<String>>>> {
        debug!("enter");

        let mut path = Vec::new();
        while let Some(ident) = self.parse_ident() {
            path.push(ident);
            if self.match_tokens(&[Token::Punct(':')]).is_some()
                && self.match_tokens(&[Token::Punct(':')]).is_none()
            {
                debug!("exit snarf", path);
                break;
            }
        }

        if path.is_empty() {
            return None;
        }

        debug!("exit", path);
        let span = path[0].1.start..path[path.len() - 1].1.end;
        Some((path, span))
    }

    /// Parse a mod
    ///
    /// mod -> mod IDENTIFIER ;
    fn parse_module(&mut self) -> Option<Spanned<InnerItem>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit");
            return None;
        };

        if self.match_tokens(&[Token::Mod]).is_none() {
            debug!("exit no token");
            return None;
        }

        let name = if let Some(ident) = self.parse_ident() {
            debug!("path", ident);
            ident
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("<path -> IDENTIFIER (:: IDENTIFIER)*>".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected path");
            self.errors.push(err);
            error!("exit no path");
            return None;
        };

        if self.match_tokens(&[Token::Punct(';')]).is_none() {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("';'".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected ;");
            self.errors.push(err);
            error!("exit no semicolon");
            return None;
        }

        debug!("exit");

        Some((
            InnerItem::Module(name),
            start..self.previous().unwrap().1.end,
        ))
    }

    /// Parse a Use
    ///
    /// use -> use IDENTIFIER (:: IDENTIFIER)* ( AS IDENTIFIER )? ;
    fn parse_use(&mut self) -> Option<Spanned<InnerItem>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit");
            return None;
        };

        if self.match_tokens(&[Token::Use]).is_none() {
            debug!("exit no token");
            return None;
        }

        let name = if let Some(ident) = self.parse_path() {
            debug!("path", ident);
            ident
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("<path -> IDENTIFIER (:: IDENTIFIER)*>".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected path");
            self.errors.push(err);
            error!("exit no path");
            return None;
        };

        let alias = if self.match_tokens(&[Token::As]).is_some() {
            if let Some(ident) = self.parse_ident() {
                debug!("alias", ident);
                Some(ident)
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("identifier".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected identifier");
                self.errors.push(err);
                error!("exit no alias");
                return None;
            }
        } else {
            None
        };

        if self.match_tokens(&[Token::Punct(';')]).is_none() {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("';'".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected ;");
            self.errors.push(err);
            error!("exit no semicolon");
            return None;
        }

        debug!("exit");

        Some((
            InnerItem::Import(name, alias),
            start..self.previous().unwrap().1.end,
        ))
    }

    /// Parse an impl block
    ///
    /// impl_block -> impl IDENTIFIER  { impl_block_body }
    fn parse_impl_block(&mut self) -> Option<Spanned<InnerItem>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return None;
        };

        if self.match_tokens(&[Token::Impl]).is_none() {
            debug!("exit no impl");
            return None;
        }

        let name = if let Some(ident) = self.parse_ident() {
            if let Ok(Some(_g)) = self.parse_generics() {
                // let g = generic_to_string(&g);
                // (format!("{}{}", ident.0, g.0), ident.1.start..g.1.end)
                ident
            } else {
                ident
            }
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("identifier".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected identifier");
            self.errors.push(err);
            error!("exit no ident");
            return None;
        };

        let open_brace = if let Some(tok) = self.match_tokens(&[Token::Punct('{')]) {
            tok.1.start
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("'{'".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected '{'");
            self.errors.push(err);
            error!("exit no `{`");
            return None;
        };

        let mut body = Vec::new();
        let mut end = false;

        while !self.at_end() {
            if self.match_tokens(&[Token::Punct('}')]).is_some() {
                end = true;
                break;
            }

            if let Some(item) = self.parse_item() {
                debug!("item", item);
                body.push(item);
            } else {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::unclosed_delimiter(
                    open_brace..token.1.end,
                    "{".to_owned(),
                    token.1.clone(),
                    "}".to_owned(),
                    Some(token.0.to_string()),
                );
                let err = err.with_label("expected '}'");
                self.errors.push(err);
                error!("exit no `}`");
                return None;
            }
        }

        // We got here because we reached the end of the input
        if !end {
            let token = self.previous().unwrap();
            let err = Simple::unclosed_delimiter(
                open_brace..token.1.end,
                "{".to_owned(),
                token.1.clone(),
                "}".to_owned(),
                Some(token.0.to_string()),
            );

            debug!("exit: no '}'");
            self.errors.push(err);

            return None;
        }

        debug!("exit ", (&name, &body));

        Some((
            InnerItem::Implementation(name, body),
            start..self.previous().unwrap().1.end,
        ))
    }

    /// Parse a Let Statement
    ///
    /// let_statement -> let IDENTIFIER(:TYPE)? = expression
    fn parse_let_statement(&mut self) -> Result<Option<Spanned<Statement>>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Let]).is_none() {
            debug!("exit no let");
            return Ok(None);
        }

        let name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("identifier".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected identifier");
            error!("exit", err);
            return Err(Box::new(err));
        };

        let type_ = if self.match_tokens(&[Token::Punct(':')]).is_some() {
            if let Some(ty) = self.parse_type()? {
                Some(ty)
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [
                        Some("'bool'".to_owned()),
                        Some("'float'".to_owned()),
                        Some("'int'".to_owned()),
                        Some("'string'".to_owned()),
                        Some("'Uuid'".to_owned()),
                        Some("'Option<T>'".to_owned()),
                        Some("'[T]'".to_owned()),
                        Some("'()'".to_owned()),
                    ],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected type");
                error!("exit", err);
                return Err(Box::new(err));
            }
        } else {
            None
        };

        if self.match_tokens(&[Token::Punct('=')]).is_none() {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("'='".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected equals");
            error!("exit", err);
            return Err(Box::new(err));
        }

        let value = if let Some(expr) = self.parse_expression(ENTER)? {
            expr
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("expression".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected expression");
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");

        Ok(Some((
            Statement::Let(name, type_, value.0),
            start..self.previous().unwrap().1.end,
        )))
    }

    /// Parse an assignment expression
    ///
    /// assignment = expression = expression
    fn parse_assignment_expression(
        &mut self,
        left: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > ASSIGN.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('=')]).is_none() {
            debug!("exit no =");
            return Ok(None);
        }

        debug!("getting right");
        let right = if let Some(expr) = self.parse_expression(ASSIGN.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Assignment(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.end,
            ),
            ASSIGN,
        )))
    }

    /// Parse an as operator
    ///
    /// as = expression as type
    fn parse_as_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > AS_OP.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::As]).is_none() {
            return Ok(None);
        }

        let right = if let Some(ty) = self.parse_type()? {
            ty
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit no expression");
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::As(Box::new(left.0.to_owned()), right),
                start..self.previous().unwrap().1.start,
            ),
            AS_OP,
        )))
    }

    /// Parse an addition operator
    ///
    /// addition = expression + expression
    fn parse_addition_operator(
        &mut self,
        left: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > ADD_SUB.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('+')]).is_none() {
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(ADD_SUB.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit no expression");
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Addition(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            ADD_SUB,
        )))
    }

    /// Parse a multiplication operator
    ///
    /// multiplication = expression * expression
    fn parse_multiplication_operator(
        &mut self,
        left: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > MUL_DIV.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('*')]).is_none() {
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(MUL_DIV.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit no expression");
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Multiplication(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            MUL_DIV,
        )))
    }

    /// Parse a division operator
    ///
    /// division = expression / expression
    fn parse_division_operator(
        &mut self,
        left: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > MUL_DIV.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('/')]).is_none() {
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(MUL_DIV.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit no expression");
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Division(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            MUL_DIV,
        )))
    }

    /// Parse a range expression
    ///
    /// range = expression .. expression
    fn parse_range(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > RANGE.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if !self.check(&Token::Punct('.')) {
            debug!("exit no ..");
            return Ok(None);
        }

        if !self.check2(&Token::Punct('.')) {
            debug!("exit no ..");
            return Ok(None);
        }

        self.advance();
        self.advance();

        let right = if let Some(expr) = self.parse_expression(RANGE.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit no expression");
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Range(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.end,
            ),
            RANGE,
        )))
    }

    /// Parse an subtraction operator
    ///
    /// subtraction = expression - expression
    fn parse_subtraction_operator(
        &mut self,
        left: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > ADD_SUB.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('-')]).is_none() {
            debug!("exit no '-'");
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(ADD_SUB.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit no expression");
            return Err(Box::new(err));
        };

        debug!("exit");

        Ok(Some((
            (
                DwarfExpression::Subtraction(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            ADD_SUB,
        )))
    }

    /// Parse a greater than or equal to operator
    ///
    /// lte -> expression >= expression
    fn parse_gte_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > COMP.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if !self.check(&Token::Punct('>')) {
            debug!("exit no ''");
            return Ok(None);
        }

        if !self.check2(&Token::Punct('=')) {
            debug!("exit no '='");
            return Ok(None);
        }

        self.advance();
        self.advance();

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::GreaterThanOrEqual(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            COMP,
        )))
    }

    /// Parse a less than or equal to operator
    ///
    /// lte -> expression <= expression
    fn parse_lte_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > COMP.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if !self.check(&Token::Punct('<')) {
            debug!("exit no '<'");
            return Ok(None);
        }

        if !self.check2(&Token::Punct('=')) {
            debug!("exit no '='");
            return Ok(None);
        }

        self.advance();
        self.advance();

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::LessThanOrEqual(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            COMP,
        )))
    }

    /// Parse an equals operator
    ///
    /// eq -> expression == expression
    fn parse_eq_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > COMP.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if !self.check(&Token::Punct('=')) {
            debug!("exit no '='");
            return Ok(None);
        }

        if !self.check2(&Token::Punct('=')) {
            debug!("exit no '='");
            return Ok(None);
        }

        self.advance();
        self.advance();

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::Equals(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            COMP,
        )))
    }

    /// Parse a not equals operator
    ///
    /// neq -> expression != expression
    fn parse_neq_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > COMP.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if !self.check(&Token::Punct('!')) {
            debug!("exit no '!'");
            return Ok(None);
        }

        if !self.check2(&Token::Punct('=')) {
            debug!("exit no '='");
            return Ok(None);
        }

        self.advance();
        self.advance();

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::NotEquals(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            COMP,
        )))
    }

    /// Parse an and operator
    ///
    /// and -> expression && expression
    fn parse_and_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > BOOL.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if !self.check(&Token::Punct('&')) {
            debug!("exit no '&'");
            return Ok(None);
        }

        if !self.check2(&Token::Punct('&')) {
            debug!("exit no '&'");
            return Ok(None);
        }

        self.advance();
        self.advance();

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::And(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            BOOL,
        )))
    }

    /// Parse an or operator
    ///
    /// or -> expression || expression
    fn parse_or_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > BOOL.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if !self.check(&Token::Punct('|')) {
            debug!("exit no '|'");
            return Ok(None);
        }

        if !self.check2(&Token::Punct('|')) {
            debug!("exit no '|'");
            return Ok(None);
        }

        self.advance();
        self.advance();

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::Or(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            BOOL,
        )))
    }

    /// Parse a greater-than operator
    ///
    /// gt -> expression > expression
    fn parse_gt_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > COMP.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('>')]).is_none() {
            debug!("exit no '>'");
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::GreaterThan(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            COMP,
        )))
    }

    /// Parse a less-than operator
    ///
    /// lt -> expression > expression
    fn parse_lt_operator(&mut self, left: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > COMP.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no token");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('<')]).is_none() {
            debug!("exit no '<'");
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit");
        Ok(Some((
            (
                DwarfExpression::LessThan(Box::new(left.0.to_owned()), Box::new(right.0)),
                start..self.previous().unwrap().1.start,
            ),
            COMP,
        )))
    }

    /// Parse an expression that does not have a block.
    ///
    /// This is where the Pratt stuff comes in. The main point to make about this is
    /// that in this function we are calling parsers with some left-hand-side expression.
    /// So all the parsers being called here deal with binding powers.
    fn parse_expression_without_block(&mut self, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        let lhs = if let Some(mut lhs) = self.parse_simple_expression()? {
            debug!("simple expression", lhs);
            // power = lhs.1 .1;
            loop {
                let rhs = if let Some(expression) = self.parse_path_in_expression(&lhs, power)? {
                    debug!("path in expression", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_as_operator(&lhs, power)? {
                    // let rhs = if let Some(expression) = self.parse_as_operator(&lhs, power)? {
                    debug!("as operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_await(&lhs, power)? {
                    debug!("await expression", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_addition_operator(&lhs, power)? {
                    debug!("addition operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_subtraction_operator(&lhs, power)? {
                    debug!("subtraction operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_multiplication_operator(&lhs, power)? {
                    debug!("multiplication operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_division_operator(&lhs, power)? {
                    debug!("division operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_lte_operator(&lhs, power)? {
                    debug!("lte operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_gte_operator(&lhs, power)? {
                    debug!("gte operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_lt_operator(&lhs, power)? {
                    debug!("less-than operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_gt_operator(&lhs, power)? {
                    debug!("greater-than operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_eq_operator(&lhs, power)? {
                    debug!("equal operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_neq_operator(&lhs, power)? {
                    debug!("not equal operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_and_operator(&lhs, power)? {
                    debug!("and operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_or_operator(&lhs, power)? {
                    debug!("or operator", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_assignment_expression(&lhs, power)? {
                    debug!("assignment expression", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_index_expression(&lhs, power)? {
                    debug!("index expression", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_struct_expression(&lhs, power)? {
                    debug!("struct expression", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_function_call(&lhs, power)? {
                    debug!("function call", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_range(&lhs, power)? {
                    debug!("range", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_method_call(&lhs, power)? {
                    debug!("method call", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_static_method_call(&lhs, power)? {
                    debug!("static method call", expression);
                    Some(expression)
                } else if let Some(expression) = self.parse_unit_enum_expression(&lhs, power)? {
                    debug!("enum expression plain", expression);
                    Some(expression)
                } else {
                    debug!("exit no operator", lhs);
                    None
                };

                if let Some(rhs) = rhs {
                    if rhs.1 .0 < power {
                        debug!("exit loop power", power);
                        debug!("exit loop rhs", rhs);
                        break Some(rhs);
                    } else {
                        debug!("continue loop power", power);
                        debug!("continue loop", rhs);
                        lhs = rhs;
                    }
                } else {
                    debug!("exit loop no rhs", lhs);
                    break Some(lhs);
                }
            }
        } else {
            None
        };

        // This is an ugly hack. I don't even imagine the alternative.
        // We just can't be returning a PathInExpression an an Expression.
        let lhs = if let Some(((DwarfExpression::PathInExpression(mut path), span), _)) = lhs {
            let method_name = path.pop().unwrap();
            let field_name = if let Type::UserType(name, _generics) = method_name {
                name
            } else {
                unreachable!()
            };

            Some((
                (
                    DwarfExpression::UnitEnum(
                        Box::new((DwarfExpression::PathInExpression(path), span.to_owned())),
                        field_name,
                    ),
                    span,
                ),
                LITERAL,
            ))
        } else {
            lhs
        };

        debug!("exit", lhs);
        Ok(lhs)
    }

    /// The parsers called from here don't need to worry about binding.
    fn parse_simple_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        // parse a boolean literal
        if let Some(expression) = self.parse_boolean_literal() {
            debug!("boolean literal", expression);
            return Ok(Some(expression));
        }

        // parse a float literal
        if let Some(expression) = self.parse_float_literal() {
            debug!("float literal", expression);
            return Ok(Some(expression));
        }

        // parse an integer literal
        if let Some(expression) = self.parse_integer_literal() {
            debug!("integer literal", expression);
            return Ok(Some(expression));
        }

        // parse a string literal
        if let Some(expression) = self.parse_string_literal() {
            debug!("string literal", expression);
            return Ok(Some(expression));
        }

        // parse a list literal
        if let Some(expression) = self.parse_list_literal()? {
            debug!("list literal", expression);
            return Ok(Some(expression));
        }

        // parse an empty literal
        if let Some(expression) = self.parse_empty_literal() {
            debug!("empty literal", expression);
            return Ok(Some(expression));
        }

        // parse a group expression
        if let Some(expression) = self.parse_group_expression()? {
            debug!("group expression", expression);
            return Ok(Some(expression));
        }

        // parse a negation operator
        if let Some(expression) = self.parse_negation_operator()? {
            debug!("negation operator", expression);
            return Ok(Some(expression));
        }

        // parse a bang operator
        if let Some(expression) = self.parse_bang_operator()? {
            debug!("boolean not operator", expression);
            return Ok(Some(expression));
        }

        // parse a local variable
        if let Some(expression) = self.parse_local_variable() {
            debug!("local variable", expression);
            return Ok(Some(expression));
        }

        // parse a print expression
        if let Some(expression) = self.parse_print_expression()? {
            debug!("print expression", expression);
            return Ok(Some(expression));
        }

        // parse an asm! expression
        if let Some(expression) = self.parse_asm_expression()? {
            debug!("asm! expression", expression);
            return Ok(Some(expression));
        }

        // parse a debugger expression
        if let Some(expression) = self.parse_debugger_expression()? {
            debug!("debugger expression");
            return Ok(Some(expression));
        }

        // parse a return expression
        if let Some(expression) = self.parse_return_expression()? {
            debug!("return expression", expression);
            return Ok(Some(expression));
        }

        Ok(None)
    }

    fn parse_expression(&mut self, power: u8) -> Result<Option<Expression>> {
        if let Some(expression) = self.parse_expression_without_block(power)? {
            debug!("expression without block", expression);
            return Ok(Some(expression));
        }

        if let Some(expression) = self.parse_expression_with_block()? {
            debug!("expression with block", expression);
            return Ok(Some(expression));
        };

        debug!("exit None");
        Ok(None)
    }

    /// Parse an expression with a block
    ///
    /// expression -> block | for | if | lambda | match
    fn parse_expression_with_block(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        // parse a lambda expression
        if let Some(expression) = self.parse_lambda_expression()? {
            debug!("lambda expression", expression);
            return Ok(Some(expression));
        }

        // parse a block expression
        if let Some(expression) = self.parse_block_expression()? {
            debug!("block expression", expression);
            if let Some(expression) = self.parse_await(&expression, BLOCK.0)? {
                return Ok(Some(expression));
            } else {
                return Ok(Some(expression));
            }
        }

        // parse a for loop expression
        if let Some(expression) = self.parse_for_loop_expression()? {
            debug!("for loop expression", expression);
            return Ok(Some(expression));
        }

        // parse an if expression
        if let Some(expression) = self.parse_if_expression()? {
            debug!("if expression", expression);
            return Ok(Some(expression));
        }

        // parse a match expression
        if let Some(expression) = self.parse_match_expression()? {
            debug!("match expression", expression);
            return Ok(Some(expression));
        }

        debug!("None");
        Ok(None)
    }

    /// Parse a Boolean Literal
    ///
    /// boolean_literal -> true | false
    fn parse_boolean_literal(&mut self) -> Option<Expression> {
        debug!("enter parse_boolean_literal");

        let token = self.peek()?.clone();

        if let (Token::Bool(bool), span) = token {
            self.advance();
            Some(((DwarfExpression::BooleanLiteral(bool), span), LITERAL))
        } else {
            None
        }
    }

    /// Parse await expression
    ///
    /// await -> <expression>.await
    fn parse_await(&mut self, name: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > FIELD.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = name.0 .1.start;

        if !self.check(&Token::Punct('.'))
        //     || self.check(&Token::Punct('.')) && self.check2(&Token::Punct('.'))
        {
            return Ok(None);
        }

        self.advance();

        let (_, span) = if let Some(token) = self.match_tokens(&[Token::Await]) {
            token
        } else {
            self.retreat();
            return Ok(None);
        };

        let end = span.end;

        if self.async_block == 0 {
            let err = Simple::expected_input_found(
                start..end,
                [Some(";".to_owned())],
                Some("await".to_owned()),
            )
            .with_label("await is only allowed in async blocks");
            error!("exit", err);
            return Err(Box::new(err));
        }

        debug!("exit ok");

        Ok(Some((
            (DwarfExpression::Await(Box::new(name.0.clone())), start..end),
            FIELD,
        )))
    }

    /// Parse field access
    ///
    /// field _access -> expression ('.' expression)+
    fn parse_field_access(&mut self, name: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > FIELD.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = name.0 .1.start;

        // if !self.check(&Token::Punct('.'))
        //     || self.check(&Token::Punct('.')) && self.check2(&Token::Punct('.'))
        // {
        //     return Ok(None);
        // }

        // self.advance();

        let token = if let Some(token) = self.peek() {
            token.clone()
        } else {
            let err = Simple::expected_input_found(
                self.previous().unwrap().1.clone(),
                [Some("<IDENT>".to_owned())],
                None,
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        let (ident, span) = if let (Token::Ident(ident), span) = token {
            self.advance();
            (ident, span)
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<IDENT>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        let end = span.end;

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::FieldAccess(Box::new(name.0.clone()), (ident, span)),
                start..end,
            ),
            FIELD,
        )))
    }

    /// Parse a Float Literal
    ///
    /// float_literal -> FLOAT
    fn parse_float_literal(&mut self) -> Option<Expression> {
        debug!("enter parse_float_literal");

        let token = self.peek()?.clone();

        if let (Token::Float(float), span) = token {
            if let Ok(float) = float.parse::<DwarfFloat>() {
                self.advance();
                Some(((DwarfExpression::FloatLiteral(float), span), LITERAL))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Parse a negation operator
    ///
    /// negation -> '-' expression
    fn parse_negation_operator(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no tok");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('-')]).is_none() {
            debug!("exit no for");
            return Ok(None);
        }

        debug!("getting expression");

        debug!("getting collection");

        let operand = if let Some(expr) = self.parse_expression(UNARY.1)? {
            debug!("minus operand", expr);
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Negation(Box::new(operand.0)),
                start..self.previous().unwrap().1.end,
            ),
            UNARY,
        )))
    }

    /// Parse a boolean not operator
    ///
    /// not -> '!' expression
    fn parse_bang_operator(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no tok");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('!')]).is_none() {
            debug!("exit no !");
            return Ok(None);
        }

        debug!("getting expression");

        debug!("getting collection");

        let operand = if let Some(expr) = self.parse_expression(UNARY.1)? {
            debug!("minus operand", expr);
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Bang(Box::new(operand.0)),
                start..self.previous().unwrap().1.end,
            ),
            UNARY,
        )))
    }

    /// Parse a for loop expression
    ///
    /// for_loop --> FOR expression IN expression expression
    fn parse_for_loop_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no tok");
            return Ok(None);
        };

        if self.match_tokens(&[Token::For]).is_none() {
            debug!("exit no for");
            return Ok(None);
        }

        debug!("getting iterator");
        let iterator = if let Some(ident) = self.parse_ident() {
            debug!("iterator", ident);
            ident
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("IDENT".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        if self.match_tokens(&[Token::In]).is_none() {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("in".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        }

        debug!("getting collection");
        // This may be a hack, but I need the variable we parse to bind closely with
        // the in, and not with something else. Like the `{` we are expecting.
        let collection = if let Some(expr) = self.parse_expression(ENTER + 1)? {
            debug!("parse_for_loop_expression collection", expr);
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("getting body");
        let body = if let Some(expr) = self.parse_block_expression()? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::For(iterator, Box::new(collection.0), Box::new(body.0)),
                start..self.previous().unwrap().1.end,
            ),
            BLOCK,
        )))
    }

    /// Parse a function call
    ///
    /// function_call -> expression '(' expression,* ')'
    fn parse_function_call(&mut self, name: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > FUNC_CALL.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = name.0 .1.start;

        let name = if let name @ (DwarfExpression::LocalVariable(..), _) = &name.0 {
            name.clone()
        } else {
            debug!("exit not a local variable");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('(')]).is_none() {
            return Ok(None);
        }

        let mut arguments = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
            if let Some(expr) = self.parse_expression(ENTER)? {
                arguments.push(expr.0);
                let _ = self.match_tokens(&[Token::Punct(',')]);
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("expression".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected expression");
                error!("exit", err);
                return Err(Box::new(err));
            }
        }

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::FunctionCall(Box::new(name), arguments),
                start..self.previous().unwrap().1.end,
            ),
            FUNC_CALL,
        )))
    }

    /// Parse a group expression
    ///
    /// group -> '(' expression,* ')'
    fn parse_group_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit parse_list_expression");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('(')]).is_none() {
            return Ok(None);
        }

        let expr = if let Some(expr) = self.parse_expression(ENTER)? {
            expr
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("expression".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected expression");
            error!("exit", err);
            return Err(Box::new(err));
        };

        if self.match_tokens(&[Token::Punct(')')]).is_none() {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("]".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected expression");
            error!("exit", err);
            return Err(Box::new(err));
        }

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Group(Box::new(expr.0)),
                start..self.previous().unwrap().1.end,
            ),
            FUNC_CALL,
        )))
    }

    /// Parse a method call
    ///
    /// method_call -> `struct`.ident '(' expression,* ')'
    fn parse_method_call(&mut self, name: &Expression, power: u8) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > METHOD.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = name.0 .1.start;

        if self.match_tokens(&[Token::Punct('.')]).is_none() {
            return Ok(None);
        }

        if self.peek_ident().is_some() && !self.check2(&Token::Punct('(')) {
            return self.parse_field_access(name, power);
        }

        let method_name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<ident>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        if self.match_tokens(&[Token::Punct('(')]).is_none() {
            return Ok(None);
        }

        let mut arguments = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
            if let Some(expr) = self.parse_expression(ENTER)? {
                arguments.push(expr.0);
                let _ = self.match_tokens(&[Token::Punct(',')]);
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("expression".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected expression");
                error!("exit", err);
                return Err(Box::new(err));
            }
        }

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::MethodCall(Box::new(name.0.clone()), method_name, arguments),
                start..self.previous().unwrap().1.end,
            ),
            METHOD,
        )))
    }

    /// Parse index expression
    ///
    /// index_expression -> expression '[' expression,* ']'
    fn parse_index_expression(
        &mut self,
        name: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > FUNC_CALL.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = name.0 .1.start;

        if self.match_tokens(&[Token::Punct('[')]).is_none() {
            return Ok(None);
        }

        let expr = if let Some(expr) = self.parse_expression(ENTER)? {
            expr
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("expression".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected expression");
            error!("exit", err);
            return Err(Box::new(err));
        };

        if self.match_tokens(&[Token::Punct(']')]).is_none() {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("]".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected expression");
            error!("exit", err);
            return Err(Box::new(err));
        }

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Index(Box::new(name.0.clone()), Box::new(expr.0)),
                start..self.previous().unwrap().1.end,
            ),
            FUNC_CALL,
        )))
    }

    /// Parse an Integer Literal
    ///
    /// integer_literal -> INTEGER
    fn parse_integer_literal(&mut self) -> Option<Expression> {
        debug!("enter parse_integer_literal");

        let token = self.peek()?.clone();

        if let (Token::Integer(int), span) = token {
            if let Ok(int) = int.parse::<DwarfInteger>() {
                self.advance();
                Some(((DwarfExpression::IntegerLiteral(int), span), LITERAL))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Parse a list expression
    ///
    /// Each element in the expression should be the same type, and I think that
    /// needs to be checked in the compiler, and not the parser, since the parser
    /// doesn't really grok types.
    ///
    /// list -> '[' expression (, expression)* ']'
    fn parse_list_literal(&mut self) -> Result<Option<Expression>> {
        debug!("enter parse_list_expression");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit parse_list_expression");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('[')]).is_none() {
            debug!("exit parse_list_expression no token");
            return Ok(None);
        }

        let mut elements = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct(']')]).is_none() {
            if let Some(expr) = self.parse_expression(LITERAL.1)? {
                elements.push(expr.0);
                let _ = self.match_tokens(&[Token::Punct(',')]);
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("expression".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected expression");
                return Err(Box::new(err));
            }
        }

        debug!("exit parse_list_expression");

        Ok(Some((
            (
                DwarfExpression::List(elements),
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
    }

    /// Parse a local variable
    ///
    /// var -> IDENTIFIER
    fn parse_local_variable(&mut self) -> Option<Expression> {
        debug!("enter parse_local_variable");

        let token = self.peek()?.clone();

        if let (Token::Ident(ident), span) = token {
            self.advance();
            debug!("exit parse_local_variable", ident);
            Some(((DwarfExpression::LocalVariable(ident), span), LITERAL))
        } else {
            None
        }
    }

    /// Parse an empty expression
    ///
    /// () -> Empty
    fn parse_empty_literal(&mut self) -> Option<Expression> {
        debug!("enter parse_empty_expression");

        if !self.check(&Token::Punct('(')) || !self.check2(&Token::Punct(')')) {
            return None;
        }

        let start = self.match_tokens(&[Token::Punct('(')]).unwrap().1.start;
        let end = self.match_tokens(&[Token::Punct(')')]).unwrap().1.end;
        let span = start..end;
        Some(((DwarfExpression::Empty, span), LITERAL))
    }

    /// Parse an asm! expression
    ///
    /// asm_expression -> asm! '(' expression (, expression)* ')'
    fn parse_asm_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if !self.check(&Token::Asm) {
            return Ok(None);
        }
        if !self.check2(&Token::Punct('!')) {
            return Ok(None);
        }
        self.advance();
        self.advance();

        if self.match_tokens(&[Token::Punct('(')]).is_none() {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("(".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        }

        let mut arguments = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
            if let Some(expr) = self.parse_expression(LITERAL.1)? {
                arguments.push(expr.0);

                let _ = self.match_tokens(&[Token::Punct(',')]);
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("expression".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected expression");
                error!("exit", err);
                return Err(Box::new(err));
            }
        }

        debug!("exit");

        Ok(Some((
            (
                DwarfExpression::Asm(arguments),
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
    }

    /// Parse a print expression
    ///
    /// This really should just be another function call, and I sort it out
    /// at name resolution time.
    ///
    /// I'm not so sure that I still agree with this. I think you need a built-in
    /// output. I should have an input too.
    ///
    /// print_expression -> PRINT( expr )
    fn parse_print_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter parse_print_expression");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if self.match_tokens(&[Token::Print]).is_none() {
            return Ok(None);
        }

        if self.match_tokens(&[Token::Punct('(')]).is_none() {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("(".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        }

        let expression = if let Some(expr) = self.parse_expression(BLOCK.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        };

        let _ = self.match_tokens(&[Token::Punct(',')]);

        if self.match_tokens(&[Token::Punct(')')]).is_none() {
            let token = self.previous().unwrap();
            // 🚧 use the unclosed_delimiter constructor
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some(")".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        }

        debug!("exit parse_print_expression");

        Ok(Some((
            (
                DwarfExpression::Print(Box::new(expression.0)),
                start..self.previous().unwrap().1.end,
            ),
            BLOCK,
        )))
    }

    /// Parse a debugger expression
    ///
    /// debugger_expression -> DEBUGGER
    fn parse_debugger_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if self.match_tokens(&[Token::Debugger]).is_none() {
            return Ok(None);
        }

        debug!("exit");

        Ok(Some((
            (
                DwarfExpression::Debug,
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
    }

    /// Parse a Statement
    ///
    /// This returns `Result<Option<T>>`. Result because we may err, and Option
    /// because we may not err, and yet there may still not be a statement.
    ///
    ///  statement -> ; | Item | LetStatement | ExpressionStatement
    fn parse_statement(&mut self) -> Result<Option<Spanned<Statement>>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit tok");
            return Ok(None);
        };

        //
        // FIrst we can take care of the trivial case.
        if self.match_tokens(&[Token::Punct(';')]).is_some() {
            debug!("empty statement");
            // 🚧 We need to implement our own error type so that we can
            //     report warnings and they show up in yellow.
            self.errors.push(Simple::custom(
                self.previous().unwrap().1.clone(),
                "unnecessary `;`".to_owned(),
            ));
            return Ok(Some((
                Statement::Empty,
                start..self.previous().unwrap().1.end,
            )));
        }

        //
        // A slightly less trivial case: parse an item.
        if let Some(item) = self.parse_item() {
            debug!("item", item);
            return Ok(Some((
                Statement::Item(item),
                start..self.previous().unwrap().1.end,
            )));
        }

        //
        // Parse a block expression. It may or may not be followed by a semi-
        // colon.
        if let Some(expr) = self.parse_expression_with_block()? {
            debug!("block expression", expr);
            // The `;` is optional, so we take a peek, and if it's there we
            // snag it. Maybe print a warning?
            if self.match_tokens(&[Token::Punct(';')]).is_some() {
                return Ok(Some((
                    Statement::Expression(expr.0),
                    start..self.previous().unwrap().1.end,
                )));
            } else {
                debug!("result statement", expr);
                return Ok(Some((
                    Statement::Result(expr.0),
                    start..self.previous().unwrap().1.end,
                )));
            }
        }

        //
        // Parse an expression that is not a block expression. It _must_ be
        // followed by a semicolon, _unless_ it's the last statement in a block.
        // And how the fuck do we figure that out? We could look for a closing
        // brace. I feel like we've tried that. But maybe not in this context.
        if let Some(expr) = self.parse_expression_without_block(ENTER)? {
            debug!("expression", expr);
            if self.match_tokens(&[Token::Punct(';')]).is_some() {
                debug!("expression statement", expr);
                return Ok(Some((
                    Statement::Expression(expr.0),
                    start..self.previous().unwrap().1.end,
                )));
            } else if self.check(&Token::Punct('}')) {
                return Ok(Some((
                    Statement::Result(expr.0),
                    start..self.previous().unwrap().1.end,
                )));
            } else {
                let tok = if let Some(tok) = self.peek() {
                    tok
                } else {
                    self.previous().unwrap()
                };
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("'}'".to_owned()), Some("';'".to_owned())],
                    Some(tok.0.to_string()),
                );

                error!("exit error", err);
                return Err(Box::new(err));
            }
        }

        //
        // This one is easy-peasy. Parse a let statement.
        if let Some(statement) = self.parse_let_statement()? {
            if self.match_tokens(&[Token::Punct(';')]).is_some() {
                debug!("parse_statement: let statement", statement);
                return Ok(Some(statement));
            } else {
                let tok = if let Some(tok) = self.peek() {
                    tok
                } else {
                    self.previous().unwrap()
                };
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("';'".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected ;");
                error!("exit error", err);
                return Err(Box::new(err));
            }
        }

        debug!("exit None");

        Ok(None)
    }

    fn parse_path_in_expression(
        &mut self,
        path: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > PATH.0 {
            debug!("I'll not be denied!!!", power);
            return Ok(None);
        }

        let start = path.0 .1.start;
        // let mut end = path.0 .1.end;

        if self.match_tokens(&[Token::Punct(':')]).is_none() {
            // return Ok(Some((
            // (DwarfExpression::PathInExpression(path), start..end),
            // PATH,
            // )))
            return Ok(None);
        }

        if self.match_tokens(&[Token::Punct(':')]).is_none() {
            debug!("exit no other colon");
            return Ok(None);
        }

        let mut path = if let (DwarfExpression::LocalVariable(name), span) = &path.0 {
            vec![Type::UserType((name.to_owned(), span.to_owned()), vec![])]
        } else if let (DwarfExpression::PathInExpression(path), _) = &path.0 {
            path.to_owned()
        } else {
            let err = Simple::expected_input_found(
                path.0 .1.clone(),
                [Some("<local_variable>".to_owned())],
                Some(format!("{:?}", path.0)),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        if let Some(ident) = self.parse_ident() {
            if let Some(g) = self.parse_generics()? {
                let new_type = Type::UserType(ident, g.0);
                path.push(new_type);

                debug!("exit ok");

                Ok(Some((
                    (
                        DwarfExpression::PathInExpression(path),
                        start..self.previous().unwrap().1.end,
                    ),
                    PATH,
                )))
            } else {
                path.push(Type::UserType(ident, vec![]));

                debug!("exit ok");

                Ok(Some((
                    (
                        DwarfExpression::PathInExpression(path),
                        start..self.previous().unwrap().1.end,
                    ),
                    PATH,
                )))
            }
        } else if let Some(g) = self.parse_generics()? {
            let mut last_type = path.pop().unwrap();
            if let Type::UserType((_, _), ref mut generics) = &mut last_type {
                *generics = g.0;
            } else {
                unreachable!()
            }
            path.push(last_type);

            debug!("exit ok");

            Ok(Some((
                (
                    DwarfExpression::PathInExpression(path),
                    start..self.previous().unwrap().1.end,
                ),
                PATH,
            )))
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<ident>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        }
    }

    /// Parse a static method call
    ///
    /// This is a bit goofy. Rust calls locals and the static method syntax a
    /// path. I guess the discriminate all the bits in the compiler when they
    /// are doing something with the AST. I don't think that I need that sort
    /// of flexibility.
    ///
    /// Anyway, the type part of the static method call has already been parsed
    /// as a local variable. If the next two tokens (yes, I'm passing that as two
    /// tokens and not one. I may pay for that later.) are ':' and ':' then I
    /// am going to change the type of the passed in thing.
    ///
    /// static_method_call -> Type '::' IDENTIFIER '(' expression,* ')'
    fn parse_static_method_call(
        &mut self,
        path: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > METHOD.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = path.0 .1.start;

        let mut path = if let (DwarfExpression::PathInExpression(path), _) = &path.0 {
            path.to_owned()
        } else {
            debug!("exit not a path");
            return Ok(None);
        };

        let method_name = path.pop().unwrap();
        let method_name = if let Type::UserType(name, _generics) = method_name {
            name
        } else {
            unreachable!()
        };

        if !self.check(&Token::Punct('(')) {
            debug!("exit no paren");
            return Ok(None);
        }

        self.advance();

        let mut arguments = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
            if let Some(expr) = self.parse_expression(ENTER)? {
                arguments.push(expr.0);

                let _ = self.match_tokens(&[Token::Punct(',')]);
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("expression".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected expression");
                error!("exit", err);
                return Err(Box::new(err));
            }
        }

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::StaticMethodCall(
                    Box::new(DwarfExpression::PathInExpression(path)),
                    method_name,
                    arguments,
                ),
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
    }

    /// Parse a struct expression
    ///
    /// struct_expression -> EXPRESSION '{' (IDENTIFIER ':' expression,)* '}'
    fn parse_struct_expression(
        &mut self,
        path: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter");

        if power > STRUCT.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no tok");
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('{')]).is_none() {
            debug!("exit no {");
            return Ok(None);
        }

        let mut fields = Vec::new();
        let mut end = false;

        while !self.at_end() {
            if self.match_tokens(&[Token::Punct('}')]).is_some() {
                end = true;
                break;
            }

            let field_name = if let Some(ident) = self.parse_ident() {
                ident
            } else {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("<ident>".to_owned())],
                    Some(token.0.to_string()),
                );
                error!("exit", err);
                return Err(Box::new(err));
            };

            if self.match_tokens(&[Token::Punct(':')]).is_none() {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some(":".to_owned())],
                    Some(token.0.to_string()),
                );
                error!("exit", err);
                return Err(Box::new(err));
            }

            let expression = if let Some(expr) = self.parse_expression(ENTER)? {
                expr
            } else {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("<expression>".to_owned())],
                    Some(token.0.to_string()),
                );
                error!("exit", err);
                return Err(Box::new(err));
            };

            fields.push((field_name, expression.0));

            if self.match_tokens(&[Token::Punct(',')]).is_none() {
                if self.match_tokens(&[Token::Punct('}')]).is_none() {
                    break;
                } else {
                    end = true;
                    break;
                }
            }
        }

        if !end {
            let token = self.previous().unwrap();
            let err = Simple::unclosed_delimiter(
                start..token.1.end,
                "{".to_owned(),
                token.1.clone(),
                "}".to_owned(),
                Some(token.0.to_string()),
            );

            debug!("exit: no '}'");
            return Err(Box::new(err));
        }

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Struct(Box::new(path.0.to_owned()), fields),
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
    }

    /// Parse a unit enum expression
    ///
    /// unit_enum_expression -> PATH_IN_EXPRESSION
    fn parse_unit_enum_expression(
        &mut self,
        path: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter");

        if power > STRUCT.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = path.0 .1.start;

        // let path = if let Some(path) = self.parse_path_in_expression(path, power)? {
        //     path
        // } else {
        //     debug!("exit no path");
        //     return Ok(None);
        // };

        let (mut path, span) = if let (DwarfExpression::PathInExpression(path), span) = &path.0 {
            (path.to_owned(), span)
        } else {
            debug!("exit not a path");
            return Ok(None);
        };

        let method_name = path.pop().unwrap();
        let field_name = if let Type::UserType(name, _generics) = method_name {
            name
        } else {
            unreachable!()
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::UnitEnum(
                    Box::new((DwarfExpression::PathInExpression(path), span.to_owned())),
                    field_name,
                ),
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
    }

    /// Parse a return expression
    ///
    /// return -> RETURN expression
    fn parse_return_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let mut span = match self.match_tokens(&[Token::Return]) {
            Some((_, span)) => span,
            None => {
                debug!("exit no return");
                return Ok(None);
            }
        };

        let (expression, expr_span) = if let Some(expr) = self.parse_expression(ENTER)? {
            let span = expr.0 .1.clone();
            (Some(Box::new(expr.0)), Some(span))
        } else {
            (None, None)
        };

        if let Some(expr_span) = expr_span {
            span.end = expr_span.end;
        }

        Ok(Some(((DwarfExpression::Return(expression), span), LITERAL)))
    }

    /// Parse a String Literal
    ///
    /// string_literal -> STRING
    fn parse_string_literal(&mut self) -> Option<Expression> {
        debug!("enter parse_string_literal");

        let token = self.peek()?.clone();

        if let (Token::String(string), span) = token {
            self.advance();
            Some(((DwarfExpression::StringLiteral(string), span), LITERAL))
        } else {
            None
        }
    }

    fn parse_block_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter parse_block_expression");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("parse_block_expression: no tokens");
            return Ok(None);
        };

        let start_block = self.match_tokens(&[Token::Punct('{'), Token::Async]);
        if start_block.is_none() {
            debug!("exit parse_block_expression: no '{' or async");

            return Ok(None);
        }
        let block_ty = if let Some((Token::Async, _)) = start_block {
            // Make sure that we parse on open curly if we have parsed `async`.
            if self.match_tokens(&[Token::Punct('{')]).is_none() {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("'{'".to_owned())],
                    Some(token.0.to_string()),
                );
                debug!("exit parse_block_expression: no '{'");
                return Err(Box::new(err));
            } else {
                debug!("parse_block_expression: async on");
                self.async_block += 1;
                BlockType::Async
            }
        } else {
            BlockType::Sync
        };

        let mut statements = Vec::new();
        let mut end = false;

        while !self.at_end() {
            if self.match_tokens(&[Token::Punct('}')]).is_some() {
                end = true;
                break;
            }

            match self.parse_statement() {
                Ok(Some(statement)) => {
                    debug!("parse_block_expression: statement", statement);
                    statements.push(statement);
                }
                Ok(None) => {
                    error!("parse_block_expression: no statement");
                    let token = if let Some(token) = self.peek() {
                        token
                    } else {
                        self.previous().unwrap()
                    };
                    let err = Simple::expected_input_found(
                        token.1.clone(),
                        [Some(
                            "<statement -> [(LET | EXPRESSION); | EXPRESSION]>".to_owned(),
                        )],
                        Some(token.0.to_string()),
                    );

                    debug!("parse_block_expression: no statement");
                    return Err(Box::new(err));
                }

                Err(error) => {
                    debug!("parse_block_expression: error", error);
                    return Err(error);
                }
            }
        }

        // We got here because we reached the end of the input
        if !end {
            let token = self.previous().unwrap();
            let err = Simple::unclosed_delimiter(
                start..token.1.end,
                "{".to_owned(),
                token.1.clone(),
                "}".to_owned(),
                Some(token.0.to_string()),
            );

            debug!("exit: no '}'");
            return Err(Box::new(err));
        }

        debug!("exit parse_block_expression");

        match block_ty {
            BlockType::Async => {
                debug!("parse_block_expression: async off");
                self.async_block -= 1;
            }
            BlockType::Sync => {}
        };

        Ok(Some((
            (
                DwarfExpression::Block(block_ty, statements, vec![], vec![]),
                start..self.previous().unwrap().1.end,
            ),
            PATH,
        )))
    }

    fn parse_function(&mut self) -> Result<Option<Spanned<InnerItem>>> {
        debug!("enter parse_function");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit parse_function: no token");
            return Ok(None);
        };

        let start_func = self.match_tokens(&[Token::Fn, Token::Async]);
        if start_func.is_none() {
            debug!("exit parse_function: no fn or async");
            return Ok(None);
        }
        let func_ty = if let Some((Token::Async, _)) = start_func {
            if self.match_tokens(&[Token::Fn]).is_none() {
                self.retreat();
                debug!("exit parse_function: no fn");
                return Ok(None);
            } else {
                debug!("parse_block_expression: async on");
                self.async_block += 1;
                BlockType::Async
            }
        } else {
            BlockType::Sync
        };

        let name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            let token = self.peek().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("identifier".to_owned())],
                Some(token.0.to_string()),
            );
            debug!("exit parse_function: no ident");
            return Err(Box::new(err));
        };

        let _ = self.parse_generics();

        if self.match_tokens(&[Token::Punct('(')]).is_none() {
            let token = self.peek().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("'('".to_owned())],
                Some(token.0.to_string()),
            );
            debug!("exit parse_function: no '('");
            return Err(Box::new(err));
        }

        let mut params = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
            match self.parse_param() {
                Ok(Some(param)) => {
                    params.push(param);
                    let _ = self.match_tokens(&[Token::Punct(',')]);
                }
                Ok(None) => {
                    let token = if let Some(token) = self.peek() {
                        token
                    } else {
                        self.previous().unwrap()
                    };
                    let err = Simple::expected_input_found(
                        token.1.clone(),
                        [Some("<param -> IDENTIFIER: TYPE>".to_owned())],
                        Some(token.0.to_string()),
                    );
                    self.errors.push(err);

                    error!("no param");
                    error!("resynchronize looking for ')'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
                        self.advance();
                    }
                    error!("resynchronized");
                }
                Err(error) => {
                    self.errors.push(*error);

                    error!("resynchronize looking for ')'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
                        self.advance();
                    }
                    error!("resynchronized");
                }
            }
        }

        let return_type = if self.match_tokens(&[Token::Punct('-')]).is_some() {
            if self.match_tokens(&[Token::Punct('>')]).is_none() {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("'>'".to_owned())],
                    Some(token.0.to_string()),
                );
                debug!("exit: got '-', but no '>'");
                return Err(Box::new(err));
            }

            match self.parse_type() {
                Ok(Some(ty)) => ty,
                Ok(None) => {
                    let start = self.previous().unwrap().1.end;
                    let end = self.peek().unwrap().1.start;
                    debug!("exit parse_function: no type");
                    return Err(Box::new(Simple::custom(start..end, "missing type")));
                }
                Err(error) => {
                    self.errors.push(*error);

                    error!("parse_function: resynchronize looking for '{'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct('{')]).is_none() {
                        self.advance();
                    }
                    error!("parse_function: resynchronized");

                    let start = self.previous().unwrap().1.end;
                    let end = self.peek().unwrap().1.start;
                    (Type::Empty, start..end)
                }
            }
        } else {
            let start = self.previous().unwrap().1.end;
            let end = self.previous().unwrap().1.start;
            (Type::Empty, start..end)
        };

        let (body, end) = if self.match_tokens(&[Token::Punct(';')]).is_none() {
            if let Some(body) = self.parse_block_expression()? {
                let end = body.0 .1.end;
                (Some(body.0), end)
            } else {
                let prev = self.previous().unwrap();
                let start = prev.1.start;
                let end = prev.1.end;
                let err = Simple::custom(
                    start..end,
                    "missing body. If this is meant to be a declaration, add a ';'",
                );
                debug!("exit parse_function: no body");
                return Err(Box::new(err));
            }
        } else {
            (None, return_type.1.end)
        };

        debug!("exit parse_function");

        match func_ty {
            BlockType::Async => {
                debug!("parse_function: async off");
                self.async_block -= 1;
            }
            BlockType::Sync => {}
        };

        Ok(Some((
            InnerItem::Function(func_ty, name, params, return_type, body),
            start..end,
        )))
    }

    /// Parse a lambda expression
    ///
    /// lambda --> |expression,*| (-> type)? { BLOCK }
    fn parse_lambda_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit: no token");
            return Ok(None);
        };

        let start_func = self.match_tokens(&[Token::Punct('|'), Token::Async]);
        if start_func.is_none() {
            debug!("exit parse_function: no `|` or async");
            return Ok(None);
        }
        let func_ty = if let Some((Token::Async, _)) = start_func {
            if self.match_tokens(&[Token::Punct('|')]).is_none() {
                self.retreat();
                debug!("exit parse_function: no `|`");
                return Ok(None);
            } else {
                debug!("parse_block_expression: async on");
                self.async_block += 1;
                BlockType::Async
            }
        } else {
            BlockType::Sync
        };

        let mut params = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct('|')]).is_none() {
            match self.parse_param() {
                Ok(Some(param)) => {
                    params.push(param);
                    let _ = self.match_tokens(&[Token::Punct(',')]);
                }
                Ok(None) => {
                    error!("no param");
                    error!("resynchronize looking for '|'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct('|')]).is_none() {
                        self.advance();
                    }
                    error!("resynchronized");
                }
                Err(error) => {
                    self.errors.push(*error);

                    error!("resynchronize looking for '|'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct('|')]).is_none() {
                        self.advance();
                    }
                    error!("resynchronized");
                }
            }
        }

        let return_type = if self.match_tokens(&[Token::Punct('-')]).is_some() {
            if self.match_tokens(&[Token::Punct('>')]).is_none() {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("'>'".to_owned())],
                    Some(token.0.to_string()),
                );
                error!("exit: got '-', but no '>'");
                return Err(Box::new(err));
            }

            match self.parse_type() {
                Ok(Some(ty)) => ty,
                Ok(None) => {
                    let span = self.previous().unwrap();
                    let start = span.1.start;
                    let end = span.1.end;
                    error!("exit: no type");
                    return Err(Box::new(Simple::custom(start..end, "missing type")));
                }
                Err(error) => {
                    self.errors.push(*error);

                    error!("resynchronize looking for '|'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct('|')]).is_none() {
                        self.advance();
                    }
                    error!("resynchronized");

                    let span = self.previous().unwrap();
                    let start = span.1.start;
                    let end = span.1.end;
                    (Type::Empty, start..end)
                }
            }
        } else {
            let span = self.previous().unwrap();
            let start = span.1.start;
            let end = span.1.end;

            (Type::Empty, start..end)
        };

        let body = if let Some(body) = self.parse_block_expression()? {
            body
        } else {
            let prev = self.previous().unwrap();
            let start = prev.1.start;
            let end = prev.1.end;
            let err = Simple::custom(start..end, "missing body");
            debug!("exit no body");
            return Err(Box::new(err));
        };

        let body = if let Some(expression) = self.parse_await(&body, BLOCK.0)? {
            expression
        } else {
            body
        };

        let end = body.0 .1.end;

        match func_ty {
            BlockType::Async => {
                debug!("parse_function: async off");
                self.async_block -= 1;
            }
            BlockType::Sync => {}
        };

        debug!("exit");

        Ok(Some((
            (
                DwarfExpression::Lambda(func_ty, params, return_type, Box::new(body.0)),
                start..end,
            ),
            LITERAL,
        )))
    }

    /// Parse a parameter
    ///
    /// param -> ident : type
    fn parse_param(&mut self) -> Result<Option<(Spanned<String>, Spanned<Type>)>> {
        debug!("enter");

        let name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            return Ok(None);
        };

        let ty = if name.0 != "self" {
            if self.match_tokens(&[Token::Punct(':')]).is_none() {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("':'".to_owned())],
                    Some(token.0.to_string()),
                );
                debug!("exit: no ':'");

                return Err(Box::new(err));
            }

            if let Some(ty) = self.parse_type()? {
                ty
            } else {
                let start = self.previous().unwrap().1.end;
                let end = self.peek().unwrap().1.start;
                let err = Simple::custom(start..end, "missing type");
                debug!("exit: no type");

                return Err(Box::new(err));
            }
        } else {
            (Type::Self_, name.1.clone())
        };

        debug!("exit: ", (&name, &ty));

        Ok(Some((name, ty)))
    }

    /// Parse a Type
    ///
    /// type -> boolean | empty | float | integer | option | string | UDT | uuid
    fn parse_type(&mut self) -> Result<Option<Spanned<Type>>> {
        debug!("enter parse_type");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        // if let Some(ident) = self.parse_path() {
        //     dbg!("path", ident);
        // }

        // Match a boolean
        if self.match_tokens(&[Token::Type(Type::Boolean)]).is_some() {
            debug!("exit parse_type: boolean");
            return Ok(Some((
                Type::Boolean,
                start
                    ..self
                        .peek()
                        .map_or(self.previous().unwrap().1.end, |t| t.1.end),
            )));
        }

        // Match empty
        if self.match_tokens(&[Token::Punct('(')]).is_some() {
            if self.match_tokens(&[Token::Punct(')')]).is_none() {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("')'".to_owned())],
                    Some(token.0.to_string()),
                );
                return Err(Box::new(err));
            }

            debug!("exit parse_type: empty");
            return Ok(Some((
                Type::Empty,
                start
                    ..self
                        .peek()
                        .map_or(self.previous().unwrap().1.end, |t| t.1.end),
            )));
        }

        // Match a float
        if self.match_tokens(&[Token::Type(Type::Float)]).is_some() {
            debug!("exit parse_type: float");
            return Ok(Some((
                Type::Float,
                start
                    ..self
                        .peek()
                        .map_or(self.previous().unwrap().1.end, |t| t.1.end),
            )));
        }

        // Match an integer
        if self.match_tokens(&[Token::Type(Type::Integer)]).is_some() {
            debug!("exit parse_type: integer");
            return Ok(Some((
                Type::Integer,
                start
                    ..self
                        .peek()
                        .map_or(self.previous().unwrap().1.end, |t| t.1.end),
            )));
        }

        // Match a list
        if self.match_tokens(&[Token::Punct('[')]).is_some() {
            let ty = if let Some(ty) = self.parse_type()? {
                ty
            } else {
                let start = self.previous().unwrap().1.end;
                let end = self
                    .peek()
                    .map_or(self.previous().unwrap().1.end, |t| t.1.end);

                let err = Simple::custom(start..end, "missing type");
                return Err(Box::new(err));
            };

            if self.match_tokens(&[Token::Punct(']')]).is_none() {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("']'".to_owned())],
                    Some(token.0.to_string()),
                );
                return Err(Box::new(err));
            }

            debug!("exit parse_type: list");
            return Ok(Some((
                Type::List(Box::new(ty)),
                start
                    ..self
                        .peek()
                        .map_or(self.previous().unwrap().1.end, |t| t.1.end),
            )));
        }

        // Match a fn
        if self.match_tokens(&[Token::Fn]).is_some() {
            if self.match_tokens(&[Token::Punct('(')]).is_none() {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("'('".to_owned())],
                    Some(token.0.to_string()),
                );
                debug!("exit parse_function: no '('");
                return Err(Box::new(err));
            }

            let mut params = Vec::new();

            while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
                match self.parse_type() {
                    Ok(Some(param)) => {
                        params.push(param);
                        let _ = self.match_tokens(&[Token::Punct(',')]);
                    }
                    Ok(None) => {
                        error!("no type");
                        error!("resynchronize looking for ')'");
                        while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
                            self.advance();
                        }
                        error!("resynchronized");
                    }
                    Err(error) => {
                        self.errors.push(*error);

                        error!("resynchronize looking for ')'");
                        while !self.at_end() && self.match_tokens(&[Token::Punct(')')]).is_none() {
                            self.advance();
                        }
                        error!("resynchronized");
                    }
                }
            }

            let return_type = if self.match_tokens(&[Token::Punct('-')]).is_some() {
                if self.match_tokens(&[Token::Punct('>')]).is_none() {
                    let token = self.previous().unwrap();
                    // 🚧 use the unclosed_delimiter constructor
                    let err = Simple::expected_input_found(
                        token.1.clone(),
                        [Some("'>'".to_owned())],
                        Some(token.0.to_string()),
                    );
                    debug!("exit: got '-', but no '>'");
                    return Err(Box::new(err));
                }

                match self.parse_type() {
                    Ok(Some(ty)) => ty,
                    Ok(None) => {
                        let start = self.previous().unwrap().1.end;
                        let end = self
                            .peek()
                            .map_or(self.previous().unwrap().1.end, |t| t.1.start);

                        debug!("exit parse_function: no type");
                        return Err(Box::new(Simple::custom(start..end, "missing type")));
                    }
                    Err(error) => {
                        self.errors.push(*error);

                        error!("parse_function: resynchronize looking for '{'");
                        while !self.at_end() && self.match_tokens(&[Token::Punct('{')]).is_none() {
                            self.advance();
                        }
                        error!("parse_function: resynchronized");

                        let start = self.previous().unwrap().1.end;
                        let end = self
                            .peek()
                            .map_or(self.previous().unwrap().1.end, |t| t.1.start);

                        (Type::Empty, start..end)
                    }
                }
            } else {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("'-> <type>'".to_owned())],
                    Some(token.0.to_string()),
                );
                debug!("exit: got {token}, and needed -> <type>");
                return Err(Box::new(err));
            };
            debug!("exit parse_type: fn");
            return Ok(Some((
                Type::Fn(params, Box::new(return_type)),
                start
                    ..self
                        .peek()
                        .map_or(self.previous().unwrap().1.end, |t| t.1.end),
            )));
        }

        // Match Self
        if self.match_tokens(&[Token::Self_]).is_some() {
            debug!("exit parse_type: self");
            return Ok(Some((Type::Self_, start..self.peek().unwrap().1.end)));
        }

        // Match String
        if self.match_tokens(&[Token::Type(Type::String)]).is_some() {
            debug!("exit parse_type: string");
            return Ok(Some((Type::String, start..self.peek().unwrap().1.end)));
        }

        // Match Uuid
        if self.match_tokens(&[Token::Uuid]).is_some() {
            debug!("exit parse_type: uuid");
            return Ok(Some((Type::Uuid, start..self.peek().unwrap().1.end)));
        }

        // Match User Defined Type
        if let Some(ident) = self.parse_ident() {
            let inner = if let Ok(Some(generics)) = self.parse_generics() {
                generics.0
            } else {
                vec![]
            };
            debug!("exit parse_type: user defined", ident);
            return Ok(Some((
                Type::UserType(ident, inner),
                start..self.peek().unwrap().1.start,
            )));
        }

        Ok(None)
    }

    /// Parse a generic declaration
    ///
    /// Basically the `<T>` in `fn foo<T>()`
    fn parse_generics(&mut self) -> Result<Option<Generics>> {
        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if self.match_tokens(&[Token::Punct('<')]).is_none() {
            return Ok(None);
        }

        let mut generics = Vec::new();

        while !self.at_end() && self.match_tokens(&[Token::Punct('>')]).is_none() {
            match self.parse_type() {
                Ok(Some((Type::UserType(generic, _), span))) => {
                    generics.push((Type::Generic(generic), span));
                    let _ = self.match_tokens(&[Token::Punct(',')]);
                }
                Ok(Some(a)) => {
                    generics.push(a);
                    // generics.push((Type::Generic((format!("{}", a.0), a.1.clone())), a.1));
                    let _ = self.match_tokens(&[Token::Punct(',')]);
                }
                Ok(None) => {
                    error!("no type");
                    error!("resynchronize looking for '>'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct('>')]).is_none() {
                        self.advance();
                    }
                    error!("resynchronized");
                }
                Err(error) => {
                    self.errors.push(*error);

                    error!("resynchronize looking for '>'");
                    while !self.at_end() && self.match_tokens(&[Token::Punct('>')]).is_none() {
                        self.advance();
                    }
                    error!("resynchronized");
                }
            }
        }

        debug!("exit parse_generic");
        Ok(Some((generics, start..self.peek().unwrap().1.end)))
    }

    /// Parse a Struct
    ///
    /// struct -> struct IDENT { struct_fields? }
    /// struct_fields -> struct_field (, struct_field)? ,?
    /// struct_field -> IDENT : TYPE
    fn parse_struct(&mut self) -> Result<Option<Spanned<InnerItem>>> {
        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if self.match_tokens(&[Token::Struct]).is_none() {
            return Ok(None);
        }

        let name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("identifier".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected identifier");
            return Err(Box::new(err));
        };

        let generics = if let Some(generics) = self.parse_generics()? {
            generics
        } else {
            (vec![], 0..0)
        };

        let fields = if self.match_tokens(&[Token::Punct('{')]).is_some() {
            let mut fields = Vec::new();
            let mut end = false;

            while !self.at_end() {
                if self.match_tokens(&[Token::Punct('}')]).is_some() {
                    end = true;
                    break;
                }

                match self.parse_struct_field() {
                    Ok(field) => {
                        fields.push(field);
                    }
                    Err(err) => {
                        return Err(err);
                    }
                }
                self.match_tokens(&[Token::Punct(',')]);
            }

            // We got here because we reached the end of the input
            if !end {
                let token = self.previous().unwrap();
                let err = Simple::unclosed_delimiter(
                    start..token.1.end,
                    "{".to_owned(),
                    token.1.clone(),
                    "}".to_owned(),
                    Some(token.0.to_string()),
                );

                debug!("exit: no '}'");
                return Err(Box::new(err));
            }

            fields
        } else {
            if self.match_tokens(&[Token::Punct(';')]).is_none() {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("';'".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected ;");
                error!("exit no semicolon");

                return Err(Box::new(err));
            }

            vec![]
        };

        // 🚧 This isn't right, but maybe it's good enough.
        let end = if let Some(tok) = self.peek() {
            tok.1.end
        } else {
            self.previous().unwrap().1.end
        };

        Ok(Some((
            InnerItem::Struct(name, fields, generics),
            start..end,
        )))
    }

    /// Parse a match
    ///
    /// match -> match scrutinee { match_arms }
    /// scrutinee -> expression
    /// match_arms -> (match_arm => (expression_without_block, | expression_with_block,?))*
    ///            -> match_arm => expression,?
    /// match_arm -> pattern
    fn parse_match_expression(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if self.match_tokens(&[Token::Match]).is_none() {
            debug!("exit no match");
            return Ok(None);
        }

        debug!("getting scrutinee");
        let scrutinee = if let Some(cond) = self.parse_expression(LITERAL.1)? {
            debug!("scrutinee", cond);
            cond
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression>".to_owned())],
                Some(token.0.to_string()),
            );
            debug!("exit");
            return Err(Box::new(err));
        };

        if self.match_tokens(&[Token::Punct('{')]).is_none() {
            debug!("parse_match_expression: no opening brace");
            return Ok(None);
        }

        let mut match_arms = Vec::new();
        let mut end = false;

        while !self.at_end() {
            if self.match_tokens(&[Token::Punct('}')]).is_some() {
                end = true;
                break;
            }

            // 🚧 Should this be one token? I feel like it probably should be.
            // I think I need to rewrite the lexer to make this happen.
            if let Some(pattern) = self.parse_pattern()? {
                if self.match_tokens(&[Token::Punct('=')]).is_none() {
                    let token = self.previous().unwrap();
                    let err = Simple::expected_input_found(
                        token.1.clone(),
                        [Some("'='".to_owned())],
                        Some(token.0.to_string()),
                    );
                    debug!("exit");
                    return Err(Box::new(err));
                }
                if self.match_tokens(&[Token::Punct('>')]).is_none() {
                    let token = self.previous().unwrap();
                    let err = Simple::expected_input_found(
                        token.1.clone(),
                        [Some("'>'".to_owned())],
                        Some(token.0.to_string()),
                    );
                    debug!("exit");
                    return Err(Box::new(err));
                }

                let expr = if let Some(expr) = self.parse_expression_without_block(LITERAL.1)? {
                    if self.match_tokens(&[Token::Punct(',')]).is_none() {
                        let token = self.previous().unwrap();
                        let err = Simple::expected_input_found(
                            token.1.clone(),
                            [Some("','".to_owned())],
                            Some(token.0.to_string()),
                        );
                        debug!("exit");
                        return Err(Box::new(err));
                    }

                    expr
                } else if let Some(expr) = self.parse_expression_with_block()? {
                    expr
                } else {
                    let token = self.previous().unwrap();
                    let err = Simple::expected_input_found(
                        token.1.clone(),
                        [Some("<expression>".to_owned())],
                        Some(token.0.to_string()),
                    );
                    debug!("exit");
                    return Err(Box::new(err));
                };

                match_arms.push(((pattern.0, expr.0 .0), pattern.1.start..expr.0 .1.end));
            } else {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("<pattern>".to_owned())],
                    Some(token.0.to_string()),
                );
                debug!("exit");
                return Err(Box::new(err));
            }

            // self.match_(&[Token::Punct(',')]);
        }

        // We got here because we reached the end of the input
        if !end {
            let token = self.previous().unwrap();
            let err = Simple::unclosed_delimiter(
                start..token.1.end,
                "{".to_owned(),
                token.1.clone(),
                "}".to_owned(),
                Some(token.0.to_string()),
            );

            debug!("exit: no '}'");
            return Err(Box::new(err));
        }

        // 🚧 This isn't right, but maybe it's good enough.
        let end = if let Some(tok) = self.peek() {
            tok.1.end
        } else {
            self.previous().unwrap().1.end
        };

        Ok(Some((
            (
                DwarfExpression::Match(Box::new(scrutinee.0), match_arms),
                start..end,
            ),
            BLOCK,
        )))
    }

    /// Parse an Enum
    ///
    /// enum -> enum IDENT { enum_fields? }
    /// enum_fields -> enum_field (, enum_field)? ,?
    /// enum_field -> IDENT (tuple | struct)?
    /// tuple -> (IDENT)
    /// struct -> { struct_fields? }
    fn parse_enum(&mut self) -> Result<Option<Spanned<InnerItem>>> {
        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if self.match_tokens(&[Token::Enum]).is_none() {
            return Ok(None);
        }

        let name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("identifier".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected identifier");
            return Err(Box::new(err));
        };

        let generics = if let Some(generics) = self.parse_generics()? {
            generics
        } else {
            (vec![], 0..0)
        };

        if self.match_tokens(&[Token::Punct('{')]).is_none() {
            let tok = self.previous().unwrap();
            return Err(Box::new(Simple::expected_input_found(
                tok.1.clone(),
                [Some("'{".to_owned())],
                Some(tok.0.to_string()),
            )));
        }

        let mut fields = Vec::new();
        let mut end = false;

        while !self.at_end() {
            if self.match_tokens(&[Token::Punct('}')]).is_some() {
                end = true;
                break;
            }

            let field_name = if let Some(ident) = self.parse_ident() {
                ident
            } else {
                let tok = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("field name".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected identifier");
                return Err(Box::new(err));
            };

            let field = match self.match_tokens(&[Token::Punct('{'), Token::Punct('(')]) {
                Some((&Token::Punct('('), _)) => {
                    let ty = if let Some(ty) = self.parse_type()? {
                        ty
                    } else {
                        let tok = self.previous().unwrap();
                        let err = Simple::expected_input_found(
                            tok.1.clone(),
                            [
                                Some("'bool'".to_owned()),
                                Some("'float'".to_owned()),
                                Some("'int'".to_owned()),
                                Some("'string'".to_owned()),
                                Some("'Uuid'".to_owned()),
                                Some("'Option<T>'".to_owned()),
                                Some("'[T]'".to_owned()),
                            ],
                            Some(tok.0.to_string()),
                        );
                        let err = err.with_label("expected type");
                        return Err(Box::new(err));
                    };
                    self.match_tokens(&[Token::Punct(')')]);

                    Some(EnumField::Tuple(ty))
                }
                Some((Token::Punct('{'), _)) => {
                    let mut struct_fields = Vec::new();
                    while !self.check(&Token::Punct('}')) {
                        match self.parse_struct_field() {
                            Ok(field) => {
                                struct_fields.push(field);
                            }
                            Err(err) => {
                                return Err(err);
                            }
                        }

                        self.match_tokens(&[Token::Punct(',')]);
                    }
                    self.match_tokens(&[Token::Punct('}')]);

                    Some(EnumField::Struct(struct_fields))
                }
                None => None,
                _ => unreachable!(),
            };

            self.match_tokens(&[Token::Punct(',')]);

            fields.push((field_name, field));
        }

        // We got here because we reached the end of the input
        if !end {
            let token = self.previous().unwrap();
            let err = Simple::unclosed_delimiter(
                start..token.1.end,
                "{".to_owned(),
                token.1.clone(),
                "}".to_owned(),
                Some(token.0.to_string()),
            );

            debug!("exit: no '}'");
            return Err(Box::new(err));
        }

        // 🚧 This isn't right, but maybe it's good enough.
        let end = if let Some(tok) = self.peek() {
            tok.1.end
        } else {
            self.previous().unwrap().1.end
        };

        Ok(Some((InnerItem::Enum(name, fields, generics), start..end)))
    }

    /// Parse an identifier
    ///
    /// ident -> IDENT
    fn parse_ident(&mut self) -> Option<Spanned<String>> {
        debug!("enter");

        // We need to clone this here so that we can mutably borrow for self.advance.
        let next = self.peek()?.clone();

        if let (Token::Ident(ident), span) = next {
            self.advance();
            debug!("exit", ident);
            Some((ident, span))
        } else {
            None
        }
    }

    /// Peek an identifier
    ///
    /// ident -> IDENT
    fn peek_ident(&mut self) -> Option<bool> {
        debug!("enter");

        let next = self.peek()?.clone();

        if let (Token::Ident(ident), _span) = next {
            debug!("exit", ident);
            Some(true)
        } else {
            Some(false)
        }
    }

    /// Parse a struct field
    ///
    /// field -> IDENT : TYPE
    fn parse_struct_field(&mut self) -> Result<(Spanned<String>, Spanned<Type>, AttributeMap)> {
        debug!("enter parse_struct_field");

        let mut attributes = HashMap::default();
        while let Some(Attribute { name, value }) = self.parse_attribute() {
            debug!("attribute", name, value);
            attributes
                .entry(name.0)
                .or_insert_with(Vec::new)
                .push((name.1, value));
        }

        let name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("identifier".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected identifier");
            return Err(Box::new(err));
        };

        if self.match_tokens(&[Token::Punct(':')]).is_none() {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [Some("':'".to_owned())],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected colon");
            return Err(Box::new(err));
        }

        let ty = if let Some(ty) = self.parse_type()? {
            ty
        } else {
            let tok = self.previous().unwrap();
            let err = Simple::expected_input_found(
                tok.1.clone(),
                [
                    Some("'bool'".to_owned()),
                    Some("'float'".to_owned()),
                    Some("'int'".to_owned()),
                    Some("'string'".to_owned()),
                    Some("'Uuid'".to_owned()),
                    Some("'[T]'".to_owned()),
                ],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected type");
            return Err(Box::new(err));
        };

        debug!("exit parse_struct_field: ", (&name, &ty));

        Ok((name, ty, attributes))
    }

    fn match_tokens<'a>(&mut self, tokens: &'a [Token]) -> Option<Spanned<&'a Token>> {
        for tok in tokens {
            if self.check(tok) {
                let span = self.advance().unwrap().1.clone();
                debug!("matched: ", tok);
                return Some((tok, span));
            }
        }

        None
    }

    fn check(&mut self, tok: &Token) -> bool {
        debug!("check", tok);
        if self.at_end() {
            return false;
        }

        self.peek().unwrap().0 == *tok
    }

    fn check2(&mut self, tok: &Token) -> bool {
        if self.at_end() {
            return false;
        }

        debug!("check2", tok);
        if let Some(next) = self.next() {
            next.0 == *tok
        } else {
            false
        }
    }

    fn advance(&mut self) -> Option<&Spanned<Token>> {
        if !self.at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn retreat(&mut self) -> Option<&Spanned<Token>> {
        if !self.at_beginning() {
            self.current -= 1;
        }

        self.tokens.get(self.current)
    }

    fn at_beginning(&self) -> bool {
        self.current == 0
    }

    fn at_end(&self) -> bool {
        self.peek().is_none()
    }

    fn peek(&self) -> Option<&Spanned<Token>> {
        let current = self.tokens.get(self.current);
        debug!("Peking", current);

        current
    }

    fn next(&self) -> Option<&Spanned<Token>> {
        if self.current + 1 >= self.tokens.len() {
            return None;
        }

        self.tokens.get(self.current + 1)
    }

    fn previous(&self) -> Option<&Spanned<Token>> {
        if self.current == 0 {
            return None;
        }

        self.tokens.get(self.current - 1)
    }
}

/// Interpreter Entry Point
///
/// Parses a single line of input as a statement.
pub fn parse_line(src: &str) -> Result<Option<Spanned<Statement>>, String> {
    let (tokens, errs) = lexer().parse_recovery_verbose(src);

    let mut parser = DwarfParser::new(tokens.unwrap());
    let ast = parser.parse_statement();

    let ast = if let Err(e) = ast {
        parser.errors.push(*e);
        None
    } else {
        ast.unwrap()
    };

    if !errs.is_empty() || !parser.errors.is_empty() {
        Err(report_errors(errs, parser.errors, "REPL", src))
    } else {
        // Ok(ParsedLine::Statement(ast))
        Ok(ast)
    }
}

// This will return as much of the parsed ast as possible, even when hitting an
// error, which explains the return type.
pub fn parse_dwarf(name: &str, src: &str) -> Result<Vec<Item>, DwarfError> {
    let (tokens, errs) = lexer().parse_recovery_verbose(src);

    let mut parser = DwarfParser::new(tokens.unwrap());
    let (ast, parse_errs) = parser.parse_program();

    debug!("parse_dwarf: {:#?}", ast);

    if !errs.is_empty() || !parse_errs.is_empty() {
        let error = report_errors(errs, parse_errs, name, src);
        eprintln!("{}", error);
        Err(DwarfError::Parse { error, ast })
    } else {
        Ok(ast)
    }
}

fn report_errors(
    errs: Vec<Simple<char>>,
    parse_errs: Vec<Simple<String>>,
    name: &str,
    src: &str,
) -> String {
    let mut result = Vec::new();

    errs.into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|s| s)))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, name, e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new((name, span.clone()))
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((name, e.span()))
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new((name, e.span()))
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new((name, e.span()))
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report
                .finish()
                .write((name, Source::from(&src)), &mut result)
                .unwrap();
        });

    String::from_utf8_lossy(&result).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_statement() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            ;
        "#;

        let ast = parse_line(src);

        assert!(ast.is_err());
        // assert_eq!(ast, Ok(Some((Statement::Empty, (13..14)))));
    }

    #[test]
    fn test_empty_value() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn main() {
                assert_eq((), ());
            }
        "#;

        let ast = parse_line(src);

        assert!(ast.is_ok());
        // assert_eq!(ast, Ok(Some((Statement::Empty, (13..14)))));
    }

    #[test]
    fn test_lexer() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            // This is a comment
            type Foo {
                bar: int,
                baz: str,
            }
            enum Bar {}
            enum Baz
        "#;

        let (tokens, errs) = lexer().parse_recovery(src);

        assert!(tokens.is_some());
        assert!(errs.is_empty());
    }

    #[test]
    fn test_func_decl() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            struct Bar {}

            impl Bar {
                fn new() -> Self;
            }

            fn something(a: int, b: string) -> bool;
        "#;

        let ast = parse_dwarf("test_func_decl", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_struct() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            struct Foo {
                bar: Option<int>,
                baz: string,
                uber: Uuid
            }

            struct Bar {}
        "#;

        let ast = parse_dwarf("test_struct", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_struct_attrs() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            #[proxy(name = "foo", model = "bar", baz(uber = "baz", bar, baz = "foo"))]
            struct Foo {
                bar: Option<int>,
                baz: string,
                uber: Uuid
            }

            #[proxy = "bar"]
            #[foo]
            struct Bar {}
        "#;

        let ast = parse_dwarf("test_struct_attrs", src);

        assert!(ast.is_ok());
        let ast = ast.unwrap();
        let foo = &ast[0];
        let attrs = &foo.attributes;

        if let (_, InnerAttribute::Attribute(attrs)) = &attrs.get("proxy").unwrap()[0] {
            if let (
                _,
                InnerAttribute::Expression((crate::dwarf::Expression::StringLiteral(name), _)),
            ) = &attrs.get("name").unwrap()[0]
            {
                assert_eq!(name, "foo");
            } else {
                panic!("Expected name attribute");
            }
            if let (
                _,
                InnerAttribute::Expression((crate::dwarf::Expression::StringLiteral(model), _)),
            ) = &attrs.get("model").unwrap()[0]
            {
                assert_eq!(model, "bar");
            } else {
                panic!("Expected model attribute");
            }
            if let (_, InnerAttribute::Attribute(attrs)) = &attrs.get("baz").unwrap()[0] {
                if let (
                    _,
                    InnerAttribute::Expression((crate::dwarf::Expression::StringLiteral(uber), _)),
                ) = &attrs.get("uber").unwrap()[0]
                {
                    assert_eq!(uber, "baz");
                } else {
                    panic!("Expected uber attribute");
                }
                if let (
                    _,
                    InnerAttribute::Expression((crate::dwarf::Expression::StringLiteral(baz), _)),
                ) = &attrs.get("baz").unwrap()[0]
                {
                    assert_eq!(baz, "foo");
                } else {
                    panic!("Expected baz attribute");
                }
                if let (_, InnerAttribute::None) = &attrs.get("bar").unwrap()[0] {
                } else {
                    panic!("Expected bar attribute");
                }
            }
        }
    }

    #[test]
    fn test_func_attrs() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            #[proxy(name = "foo", model = "bar")]
            fn foo(a: string, b: int, d: Option<Foo>) -> Option<bool> {
                None
            }

            #[proxy = "bar"]
            #[foo]
            fn bar() -> Option<string> {
                Some("Hello, World!")
            }
        "#;

        let ast = parse_dwarf("test_func_attrs", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_import() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            use foo;
            use foo::bar::baz::Baz;
            use foo::bar::Xyzzy as Plugh;
        "#;

        let ast = parse_dwarf("test_import", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn xyzzy() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo(a: string, b: int, d: Option<Foo>) -> Option<bool> {
                None
            }

            fn bar() -> Option<string> {
                Some("Hello, World!")
            }

            fn xyzzy() -> Bar {
                foo(a, b, c.d(), Foo::new(), 42, 3.14, "Hello, World!", true);
                let a = Foo::new();
                let b = a.b();
                a.collect();
                a.b.c.e;
                a.b.c.d();
                Bar {
                    foo: 42,
                    bar: 3.14,
                    baz: "Hello, World!",
                    uber: Foo::new(),
                    foo: true
                }
            }

            fn yzzyx() -> Self {
                Foo::bar(a, 42, 3.14, true, "Hello, World!", Foo::new(), a.b.c, d.e.f.g(4, 8));
                Uuid::new();
                Self {
                    foo: 42,
                    bar: 3.14,
                    test: func(),
                    baz: "Hello, World!",
                    uber: Foo::new(),
                    foo: true
                }
            }
        "#;

        let ast = parse_dwarf("xyzzy", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_impl() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            impl Foo {
                fn new() -> Self {
                    let empty = Self {a: 42,
                        b: 3.14, c: "Hello, World!"};

                    Uuid::new();
                    Self {
                        bar: 42,
                        pi: 3.14,
                        baz: "Hello, World!",
                        test: func(),
                        uber: Uuid::new(),
                        foo: true
                    }
                }

                fn foo() -> Foo {
                    let empty = Foo {};
                    Foo {
                        bar: 42,
                        pi: 3.14,
                        baz: "Hello, World!",
                        uber: Uuid::new(),
                        foo: false,
                    }
                }

                fn bar(baz: Bar, id: Uuid) -> int {
                    let a = 1;
                    let b = true;
                    let c = "Hello, World!";
                    let d = 3.14;
                    true
                }

                fn baz() -> Bar {
                    let id = id_func(a, foo(), 42, 3.14, "Hello, World!", true);
                    let a = id.name;
                    let bar = id.func();
                    let id = Uuid::new();
                    let hello = Bar::new(42, 3.14, "Hello, World!", true, bar, id, func());
                }
            }
        "#;

        let ast = parse_dwarf("test_impl", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_item_fn() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> int {
                let a = 1;
                let b = true;
                let c = "Hello, World!";
                let d = 3.14;
                true
            }

            fn beano() -> bool {
                let a: int = 1;
                let b: bool = true;
                let c: string = "Hello, World!";
                let d: float = 3.14;
                true
            }

            fn bar() -> () {
                print("Hello World");
            }
        "#;

        let ast = parse_dwarf("test_item_fn", src);

        assert!(ast.is_ok());
        assert!(ast.unwrap().len() == 3);
    }

    // #[test]
    // fn test_reference() {
    //     let _ = env_logger::builder().is_test(true).try_init();

    //     let src = r#"
    //         fn foo() -> () {
    //             let a = &b;
    //         }

    //         fn fun() -> () {}

    //         fn bar(a: &int) -> [&Bar] {}

    //         impl Bar {
    //             fn baz(&self) -> () {}
    //         }
    //     "#;

    //     let ast = parse_dwarf(src);

    //     assert!(ast.is_ok());
    // }

    #[test]
    fn test_list() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> [int] {
                let a = [1, 2, 3];
            }
        "#;

        let ast = parse_dwarf("test_list", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_option() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> [int] {
                let a = 123;
                let b = Some (42);
                let a = None;
                call(a, b, None, Some(42));
            }
        "#;

        let ast = parse_dwarf("test_option", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_for() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> () {
                for a in b {
                    print(a);
                }

                for a in [1, 2, 3] {
                    print(a);
                }
            }
        "#;

        let ast = parse_dwarf("test_for", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_field_access() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = "a.id;";

        let ast = parse_line(src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_return() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> int {
                return 42;
            }

            fn bar() -> int {
                return { 42 };
            }
        "#;

        let ast = parse_dwarf("test_return", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_fib() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn fib(n: int) -> int {
                if n <= 1 {
                    return n;
                }

                return fib(n - 1) + fib(n - 2);
            }
        "#;

        let ast = parse_dwarf("test_fib", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_simpler_expr() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            1 + 1;
        "#;

        let ast = parse_line(src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_simple_expr() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            1 + 1 <= 2;
        "#;

        let ast = parse_line(src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_struct_expression() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
        fn foo() -> Foo {
            Foo {
                bar: 42,
                pi: 3.14,
                baz: "Hello, World!",
                test: func(),
                uber: Uuid::new(),
                foo: true
            }
        }
        "#;

        let ast = parse_dwarf("test_struct_expression", src);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_assignment() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> () {
                a = 1;
                b = true;
                c = "Hello, World!";
                d = 3.14;
                e = Some(42);
            }
        "#;

        let ast = parse_dwarf("test_assignment", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_index_expression() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> () {
                a[1];
                b[1][2];
                c[1][2][3];
            }
        "#;

        let ast = parse_dwarf("test_index_expression", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_trailing_commas() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> () {
                a = [1, 2, 3,];
                call(a,);
                print("Hello, World!",);
            }
        "#;

        let ast = parse_dwarf("test_trailing_commas", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_uuid() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo(id: Uuid) -> () {
                a = Uuid::new();
                b = Uuid::new_v4();
                c = Uuid::parse_str("936DA01F9ABD4d9d80C702AF85C822A8").unwrap();
            }
        "#;

        let ast = parse_dwarf("test_uuid", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_method_call() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> () {
                let a = Complex::new();
                let b = Complex::new();
                b.re;
                a.add(b);
            }
        "#;

        let ast = parse_dwarf("test_method_call", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_asm() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn foo() -> () {
                let foo: string = "nop";

                asm!(
                    "nop",
                    "nop",
                    foo
                );
            }
        "#;

        let ast = parse_dwarf("test_asm", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_lambda() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn main() {
                test(|| {
                    print("Hello, World!");
                });

                test(|a: int| {
                    print(a);
                });

                test(|a: int, b: int| -> int {
                    a + b
                });
            }
        "#;

        let ast = parse_dwarf("test_asm", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_enum() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            enum Foo {
                Bar,
                Baz,
                Qux
            }
            enum Bar {
                Foo(Foo),
                Bar{a: int, b: int},
                Baz(int),
            }

            fn main() {
                let foo = Foo::Bar;
                let bar = Foo::Baz;
                let baz = Foo::Qux;

                let a = Bar::Foo(Foo::Bar);
                let b = Bar::Bar{a: 1, b: 2};
                let c = Bar::Baz(42);
            }
        "#;

        let ast = parse_dwarf("test_enum", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_struct_enum() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            enum Foo {
                Bar,
                Baz,
                Qux
            }

            struct Bar {
                a: Foo
            }

            fn main() {
                let a = Bar { a: Foo::Bar };
                a.a = Foo::Baz;
                b = Foo::Qux;
            }
        "#;

        let ast = parse_dwarf("test_plain_enum", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_generic_decls() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            struct Foo<I, C, E> {
                a: I,
                b: C,
                c: E,
            }

            enum Option<T> {
                Some(T),
                None,
            }

            fn foo<T>(a: T) -> T {
                a
            }

            fn foo(a: int) -> Bar<()> {
                a
            }

            fn foo() {
                let a = Option::<int>::None;
            }
        "#;

        let ast = parse_dwarf("test_generic_decls", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_if_let() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn main() {
                // Identifier Pattern
                let a = 20;
                // let a::b::c = "hello";

                // Path Pattern
                if let None = foo {}
                if let Option::None = foo {}

                // Tuple Struct Pattern
                if let Some(a) = bar {}
                if let Option::Some(a) = bar {}
                if let FuBar(a, b) = foo {}
                if let Foo::Bar::FuBar(a, b) = foo {}
            }
        "#;

        let ast = parse_dwarf("test_if_let", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_struct_field_assignment() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            struct A {
                b: B
            }

            struct B {
                b: int
            }

            fn main() {
                let a = A { b: B { b: 42 } };
                a.b.b = 69;
            }
        "#;

        let ast = parse_dwarf("test_struct_field_assignment", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_async_block() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            fn main() {
                async {
                    print("Hello, World!");
                };
            }
        "#;

        let ast = parse_dwarf("test_async_block", src);
        assert!(ast.is_ok());
    }

    #[test]
    fn parse_await() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            async fn main() {
                let bar = foo().await.bar;

                async {}.await;

                let foo = || async {}.await;
            }
        "#;

        let ast = parse_dwarf("test_await", src);
        assert!(ast.is_ok());
    }
}
