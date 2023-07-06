use ansi_term::Colour;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use log;

use crate::dwarf::{
    DwarfFloat, Expression as DwarfExpression, Item, Spanned, Statement, Token, Type,
};

use super::{DwarfError, DwarfInteger};

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
const ASSIGN: (u8, u8) = (11, 10);
// Literal, closure, break, return, etc.
const LITERAL: (u8, u8) = (0, 1);
const BLOCK: (u8, u8) = (0, 0);
const STRUCT: (u8, u8) = (0, 0);
const ENTER: u8 = 0;

type Expression = (Spanned<DwarfExpression>, (u8, u8));

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
        "bool" => Token::Type(Type::Boolean),
        "debugger" => Token::Debugger,
        "else" => Token::Else,
        "false" => Token::Bool(false),
        "float" => Token::Type(Type::Float),
        "fn" => Token::Fn,
        "for" => Token::For,
        // "global" => Token::Global,
        "if" => Token::If,
        "impl" => Token::Impl,
        "int" => Token::Type(Type::Integer),
        "in" => Token::In,
        "let" => Token::Let,
        "None" => Token::None,
        "print" => Token::Print,
        // "Self" => Token::Self_,
        // "self" => Token::SmallSelf,
        "return" => Token::Return,
        "Some" => Token::Some,
        "string" => Token::Type(Type::String),
        "struct" => Token::Struct,
        "true" => Token::Bool(true),
        "use" => Token::Import,
        // "Uuid" => Token::Uuid,
        _ => Token::Ident(ident),
    });

    // A parser for punctuation (delimiters, semicolons, etc.)
    let punct = one_of("=-()[]{}:;,.|&<>+*/!").map(Token::Punct);

    let option = just("Option").map(|_| Token::Option);

    // A single token can be one of the above
    let token = float
        .or(int)
        .or(str_)
        // .or(dagger)
        // .or(double_colon)
        // .or(op)
        .or(punct)
        .or(option)
        // .or(object)
        // .or(some)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();
    let doc_comment = just('#').padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded_by(doc_comment.repeated())
        .padded()
        .repeated()
}

#[derive(Debug)]
struct DwarfParser {
    tokens: Vec<Spanned<Token>>,
    current: usize,
    errors: Vec<Simple<String>>,
}

impl DwarfParser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    /// Parse a program
    ///
    /// A program is a list of items
    ///
    /// program -> item*
    fn parse_program(&mut self) -> (Vec<Spanned<Item>>, Vec<Simple<String>>) {
        debug!("enter");

        let mut result = Vec::new();

        while !self.at_end() {
            debug!("parse_item");
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
                while !self.at_end() && !self.match_(&[Token::Punct('}')]) {
                    self.advance();
                }
                error!("parse_program: resynchronized");
            }
        }

        debug!("exit", result);

        (result, self.errors.clone())
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

        if !self.match_(&[Token::If]) {
            debug!("exit");
            return Ok(None);
        }

        debug!("getting conditional");
        let conditional = if let Some(cond) = self.parse_expression(LITERAL.1)? {
            debug!("conditional", cond);
            cond
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
                Some(token.0.to_string()),
            );
            debug!("exit");
            return Err(Box::new(err));
        };

        let false_block = if self.match_(&[Token::Else]) {
            debug!("getting false block");
            let false_block = if let Some(expr) = self.parse_block_expression()? {
                debug!("false block", expr);
                expr
            } else {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("<expression -> there's a lot of them...>".to_owned())],
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

        Ok(Some((
            (
                DwarfExpression::If(Box::new(conditional.0), Box::new(true_block.0), false_block),
                start..self.previous().unwrap().1.end,
            ),
            BLOCK,
        )))
    }

    /// Parse an Item
    ///
    /// This should probably just return an error...
    ///  item -> Struct | ImplBlock | Import | Function
    fn parse_item(&mut self) -> Option<Spanned<Item>> {
        debug!("enter");

        // Try to parse a struct
        match self.parse_struct() {
            Ok(Some(item)) => {
                debug!("struct", item);
                return Some(item);
            }
            Ok(None) => {}
            Err(err) => {
                error!("error", err);
                self.errors.push(*err);
            }
        }

        if let Some(item) = self.parse_impl_block() {
            debug!("impl", item);
            return Some(item);
        }

        if let Some(item) = self.parse_import() {
            debug!("import", item);
            return Some(item);
        }

        // Try to parse a function
        match self.parse_function() {
            Ok(Some(func)) => {
                debug!("function", func);
                return Some(func);
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
            if !self.match_(&[Token::Punct(':')]) || !self.match_(&[Token::Punct(':')]) {
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

    /// Parse an Import
    ///
    /// import -> USE IDENTIFIER (:: IDENTIFIER)* ( AS IDENTIFIER )? ;
    fn parse_import(&mut self) -> Option<Spanned<Item>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit");
            return None;
        };

        if !self.match_(&[Token::Import]) {
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

        let alias = if self.match_(&[Token::As]) {
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

        if !self.match_(&[Token::Punct(';')]) {
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
            Item::Import(name, alias),
            start..self.previous().unwrap().1.end,
        ))
    }

    /// Parse an impl block
    ///
    /// impl_block -> impl IDENTIFIER  { impl_block_body }
    fn parse_impl_block(&mut self) -> Option<Spanned<Item>> {
        debug!("enter");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return None;
        };

        if !self.match_(&[Token::Impl]) {
            debug!("exit no impl");
            return None;
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
            self.errors.push(err);
            error!("exit no ident");
            return None;
        };

        if !self.match_(&[Token::Punct('{')]) {
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
        }

        let mut body = Vec::new();
        while !self.match_(&[Token::Punct('}')]) {
            if let Some(item) = self.parse_item() {
                debug!("item", item);
                body.push(item);
            } else {
                let tok = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    tok.1.clone(),
                    [Some("'}'".to_owned())],
                    Some(tok.0.to_string()),
                );
                let err = err.with_label("expected '}'");
                self.errors.push(err);
                error!("exit no `}`");
                return None;
            }
        }

        debug!("exit ", (&name, &body));

        Some((
            Item::Implementation(name, body),
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

        if !self.match_(&[Token::Let]) {
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

        let type_ = if self.match_(&[Token::Punct(':')]) {
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

        if !self.match_(&[Token::Punct('=')]) {
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

        if !self.match_(&[Token::Punct('=')]) {
            debug!("exit no =");
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(ASSIGN.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::As]) {
            return Ok(None);
        }

        let right = if let Some(ty) = self.parse_type()? {
            ty
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('+')]) {
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(ADD_SUB.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('*')]) {
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(MUL_DIV.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('/')]) {
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(MUL_DIV.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('-')]) {
            debug!("exit no '-'");
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(ADD_SUB.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('>')]) {
            debug!("exit no '>'");
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('<')]) {
            debug!("exit no '<'");
            return Ok(None);
        }

        let right = if let Some(expr) = self.parse_expression(COMP.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                let rhs = if let Some(expression) = self.parse_as_operator(&lhs, power)? {
                    debug!("as operator", expression);
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
                    break Some(lhs);
                }
            }
        } else {
            None
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

        // parse a none literal
        if let Some(expression) = self.parse_none_literal() {
            debug!("none literal", expression);
            return Ok(Some(expression));
        }

        // parse a some literal
        if let Some(expression) = self.parse_some_literal()? {
            debug!("some literal", expression);
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

        // parse a negation operator
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
    /// expression -> assignment | block | Error | field_access |
    ///               for | function_call | if |
    ///               list | method_call |print |
    ///               static_method_call | struct
    fn parse_expression_with_block(&mut self) -> Result<Option<Expression>> {
        debug!("enter");

        // parse a block expression
        if let Some(expression) = self.parse_block_expression()? {
            debug!("block expression", expression);
            return Ok(Some(expression));
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

        // parse a lambda expression
        if let Some(expression) = self.parse_lambda_expression()? {
            debug!("lambda expression", expression);
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

        // let expr = if let Some(expr) = self.parse_expression(FIELD.1)? {
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

        if !self.match_(&[Token::Punct('-')]) {
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('!')]) {
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::For]) {
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

        if !self.match_(&[Token::In]) {
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
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

        if !self.match_(&[Token::Punct('(')]) {
            return Ok(None);
        }

        let mut arguments = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct(')')]) {
            if let Some(expr) = self.parse_expression(ENTER)? {
                arguments.push(expr.0);
                if self.peek().unwrap().0 == Token::Punct(',') {
                    self.advance();
                }
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
                DwarfExpression::FunctionCall(Box::new(name.0.clone()), arguments),
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

        if !self.match_(&[Token::Punct('(')]) {
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

        if !self.match_(&[Token::Punct(')')]) {
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

        if !self.match_(&[Token::Punct('.')]) {
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

        if !self.match_(&[Token::Punct('(')]) {
            return Ok(None);
        }

        let mut arguments = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct(')')]) {
            if let Some(expr) = self.parse_expression(ENTER)? {
                arguments.push(expr.0);
                if self.peek().unwrap().0 == Token::Punct(',') {
                    self.advance();
                }
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

        if !self.match_(&[Token::Punct('[')]) {
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

        if !self.match_(&[Token::Punct(']')]) {
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

        if !self.match_(&[Token::Punct('[')]) {
            debug!("exit parse_list_expression no token");
            return Ok(None);
        }

        let mut elements = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct(']')]) {
            if let Some(expr) = self.parse_expression(LITERAL.1)? {
                elements.push(expr.0);
                if self.peek().unwrap().0 == Token::Punct(',') {
                    self.advance();
                }
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

    /// Parse a None expression
    ///
    /// none -> NONE
    fn parse_none_literal(&mut self) -> Option<Expression> {
        debug!("enter parse_none_expression");

        let token = self.peek()?.clone();

        if let (Token::None, span) = token {
            self.advance();
            Some(((DwarfExpression::None, span), LITERAL))
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

        let start = self.next().unwrap().1.start;
        let end = self.next().unwrap().1.end;
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

        if !self.match_(&[Token::Punct('(')]) {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("(".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        }

        let mut arguments = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct(')')]) {
            if let Some(expr) = self.parse_expression(LITERAL.1)? {
                arguments.push(expr.0);

                if self.peek().unwrap().0 == Token::Punct(',') {
                    self.advance();
                }
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

        if !self.match_(&[Token::Print]) {
            return Ok(None);
        }

        if !self.match_(&[Token::Punct('(')]) {
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
                [Some("<expression -> there's a lot of them...>".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        };

        if self.peek().unwrap().0 == Token::Punct(',') {
            self.advance();
        }

        if !self.match_(&[Token::Punct(')')]) {
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

        if !self.match_(&[Token::Debugger]) {
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
        if self.match_(&[Token::Punct(';')]) {
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
            self.match_(&[Token::Punct(';')]);

            debug!("result statement", expr);
            return Ok(Some((
                Statement::Result(expr.0),
                start..self.previous().unwrap().1.end,
            )));
        }

        //
        // Parse an expression that is not a block expression. It _must_ be
        // followed by a semicolon, _unless_ it's the last statement in a block.
        // And how the fuck do we figure that out? We could look for a closing
        // brace. I feel like we've tried that. But maybe not in this context.
        if let Some(expr) = self.parse_expression_without_block(ENTER)? {
            debug!("expression", expr);
            if self.match_(&[Token::Punct(';')]) {
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
            if self.match_(&[Token::Punct(';')]) {
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
        name: &Expression,
        power: u8,
    ) -> Result<Option<Expression>> {
        debug!("enter", power);

        if power > METHOD.0 {
            debug!("exit no power", power);
            return Ok(None);
        }

        let start = name.0 .1.start;

        if !self.match_(&[Token::Punct(':')]) {
            debug!("exit no colon");
            return Ok(None);
        }

        if !self.match_(&[Token::Punct(':')]) {
            debug!("exit no other colon");
            return Ok(None);
        }

        let name = if let (DwarfExpression::LocalVariable(name), span) = &name.0 {
            Type::UserType((name.to_owned(), span.to_owned()))
        } else {
            let err = Simple::expected_input_found(
                name.0 .1.clone(),
                [Some("<local_variable>".to_owned())],
                Some(format!("{:?}", name.0)),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

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

        if !self.match_(&[Token::Punct('(')]) {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("(".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        }

        let mut arguments = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct(')')]) {
            if let Some(expr) = self.parse_expression(ENTER)? {
                arguments.push(expr.0);

                if self.peek().unwrap().0 == Token::Punct(',') {
                    self.advance();
                }
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
                DwarfExpression::StaticMethodCall(name, method_name, arguments),
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
        name: &Expression,
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

        if !self.match_(&[Token::Punct('{')]) {
            debug!("exit no open brace");
            return Ok(None);
        }

        let mut fields = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct('}')]) {
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

            if !self.match_(&[Token::Punct(':')]) {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("".to_owned())],
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
                    [Some("<expression -> there's a lot of them...>".to_owned())],
                    Some(token.0.to_string()),
                );
                error!("exit", err);
                return Err(Box::new(err));
            };

            fields.push((field_name, expression.0));

            if self.peek().unwrap().0 == Token::Punct(',') {
                self.advance();
            }
        }

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Struct(Box::new(name.0.clone()), fields),
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

        if !self.match_(&[Token::Return]) {
            debug!("exit no return");
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit no tok");
            return Ok(None);
        };

        let expression = if let Some(expr) = self.parse_expression(ENTER)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
                Some(token.0.to_string()),
            );
            error!("exit", err);
            return Err(Box::new(err));
        };

        debug!("exit ok");

        Ok(Some((
            (
                DwarfExpression::Return(Box::new(expression.0)),
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
    }

    /// Parse a Some expression
    ///
    /// some -> SOME
    fn parse_some_literal(&mut self) -> Result<Option<Expression>> {
        debug!("enter parse_some_expression");

        if !self.match_(&[Token::Some]) {
            return Ok(None);
        }

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if !self.match_(&[Token::Punct('(')]) {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("(".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        }

        let expression = if let Some(expr) = self.parse_expression(PATH.1)? {
            expr
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("<expression -> there's a lot of them...>".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        };

        if !self.match_(&[Token::Punct(')')]) {
            let token = self.previous().unwrap();
            // 🚧 Use the unclosed_delimiter constructor
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some(")".to_owned())],
                Some(token.0.to_string()),
            );
            return Err(Box::new(err));
        }

        debug!("exit parse_some_expression");

        Ok(Some((
            (
                DwarfExpression::Some(Box::new(expression.0)),
                start..self.previous().unwrap().1.end,
            ),
            LITERAL,
        )))
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

        if !self.match_(&[Token::Punct('{')]) {
            debug!("parse_block_expression: no opening brace");
            return Ok(None);
        }

        let mut statements = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct('}')]) {
            // Ok, this is where I want to log, and then ignore errors. Now, how
            // do I do that?
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

        debug!("exit parse_block_expression");

        Ok(Some((
            (
                DwarfExpression::Block(statements),
                start..self.previous().unwrap().1.end,
            ),
            PATH,
        )))
    }

    fn parse_function(&mut self) -> Result<Option<Spanned<Item>>> {
        debug!("enter parse_function");

        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            debug!("exit parse_function: no token");
            return Ok(None);
        };

        if !self.match_(&[Token::Fn]) {
            debug!("exit parse_function: no fn");
            return Ok(None);
        }

        let name = if let Some(ident) = self.parse_ident() {
            ident
        } else {
            let token = self.previous().unwrap();
            let err = Simple::expected_input_found(
                token.1.clone(),
                [Some("identifier".to_owned())],
                Some(token.0.to_string()),
            );
            debug!("exit parse_function: no ident");
            return Err(Box::new(err));
        };

        if !self.match_(&[Token::Punct('(')]) {
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

        while !self.at_end() && !self.match_(&[Token::Punct(')')]) {
            match self.parse_param() {
                Ok(Some(param)) => {
                    params.push(param);
                    if self.peek().unwrap().0 == Token::Punct(',') {
                        self.advance();
                    }
                }
                Ok(None) => {
                    error!("no param");
                    error!("resynchronize looking for ')'");
                    while !self.at_end() && !self.match_(&[Token::Punct(')')]) {
                        self.advance();
                    }
                    error!("resynchronized");
                }
                Err(error) => {
                    self.errors.push(*error);

                    error!("resynchronize looking for ')'");
                    while !self.at_end() && !self.match_(&[Token::Punct(')')]) {
                        self.advance();
                    }
                    error!("resynchronized");
                }
            }
        }

        let return_type = if self.match_(&[Token::Punct('-')]) {
            if !self.match_(&[Token::Punct('>')]) {
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
                    while !self.at_end() && !self.match_(&[Token::Punct('{')]) {
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

        let body = if let Some(body) = self.parse_block_expression()? {
            body
        } else {
            let prev = self.previous().unwrap();
            let start = prev.1.start;
            let end = prev.1.end;
            let err = Simple::custom(start..end, "missing body");
            debug!("exit parse_function: no body");
            return Err(Box::new(err));
        };

        let end = body.0 .1.end;

        debug!("exit parse_function");

        Ok(Some((
            Item::Function(name, params, return_type, body.0),
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

        if !self.match_(&[Token::Punct('|')]) {
            debug!("exit parse_function: no fn");
            return Ok(None);
        }

        let mut params = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct('|')]) {
            match self.parse_param() {
                Ok(Some(param)) => {
                    params.push(param);
                    if self.peek().unwrap().0 == Token::Punct(',') {
                        self.advance();
                    }
                }
                Ok(None) => {
                    error!("no param");
                    error!("resynchronize looking for '|'");
                    while !self.at_end() && !self.match_(&[Token::Punct('|')]) {
                        self.advance();
                    }
                    error!("resynchronized");
                }
                Err(error) => {
                    self.errors.push(*error);

                    error!("resynchronize looking for '|'");
                    while !self.at_end() && !self.match_(&[Token::Punct('|')]) {
                        self.advance();
                    }
                    error!("resynchronized");
                }
            }
        }

        let return_type = if self.match_(&[Token::Punct('-')]) {
            if !self.match_(&[Token::Punct('>')]) {
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
                    while !self.at_end() && !self.match_(&[Token::Punct('|')]) {
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

        let end = body.0 .1.end;

        debug!("exit");

        Ok(Some((
            (
                DwarfExpression::Lambda(params, return_type, Box::new(body.0)),
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
            if !self.match_(&[Token::Punct(':')]) {
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

        // Match a boolean
        if self.match_(&[Token::Type(Type::Boolean)]) {
            debug!("exit parse_type: boolean");
            return Ok(Some((Type::Boolean, start..self.peek().unwrap().1.end)));
        }

        // Match empty
        if self.match_(&[Token::Punct('(')]) {
            if !self.match_(&[Token::Punct(')')]) {
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
            return Ok(Some((Type::Empty, start..self.peek().unwrap().1.end)));
        }

        // Match a float
        if self.match_(&[Token::Type(Type::Float)]) {
            debug!("exit parse_type: float");
            return Ok(Some((Type::Float, start..self.peek().unwrap().1.end)));
        }

        // Mtatch an integer
        if self.match_(&[Token::Type(Type::Integer)]) {
            debug!("exit parse_type: integer");
            return Ok(Some((Type::Integer, start..self.peek().unwrap().1.end)));
        }

        // Match a list
        if self.match_(&[Token::Punct('[')]) {
            let ty = if let Some(ty) = self.parse_type()? {
                ty
            } else {
                let start = self.previous().unwrap().1.end;
                let end = self.peek().unwrap().1.start;
                let err = Simple::custom(start..end, "missing type");
                return Err(Box::new(err));
            };

            if !self.match_(&[Token::Punct(']')]) {
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
                start..self.peek().unwrap().1.end,
            )));
        }

        // Match an option
        if self.match_(&[Token::Option]) {
            if !self.match_(&[Token::Punct('<')]) {
                let token = self.previous().unwrap();
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("'<'".to_owned())],
                    Some(token.0.to_string()),
                );
                return Err(Box::new(err));
            }

            let ty = self.parse_type()?;

            if ty.is_none() {
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
                return Err(Box::new(err));
            }

            if !self.match_(&[Token::Punct('>')]) {
                let token = self.previous().unwrap();
                // 🚧 use the unclosed_delimiter constructor
                let err = Simple::expected_input_found(
                    token.1.clone(),
                    [Some("'>'".to_owned())],
                    Some(token.0.to_string()),
                );
                return Err(Box::new(err));
            }

            debug!("exit parse_type: option", ty);
            return Ok(Some((
                Type::Option(Box::new(ty.unwrap())),
                start..self.peek().unwrap().1.end,
            )));
        }

        // Match Self
        if self.match_(&[Token::Self_]) {
            debug!("exit parse_type: self");
            return Ok(Some((Type::Self_, start..self.peek().unwrap().1.end)));
        }

        // Match String
        if self.match_(&[Token::Type(Type::String)]) {
            debug!("exit parse_type: string");
            return Ok(Some((Type::String, start..self.peek().unwrap().1.end)));
        }

        // Match Uuid
        if self.match_(&[Token::Uuid]) {
            debug!("exit parse_type: uuid");
            return Ok(Some((Type::Uuid, start..self.peek().unwrap().1.end)));
        }

        // Match User Defined Type
        if let Some(ident) = self.parse_ident() {
            debug!("exit parse_type: user defined", ident);
            return Ok(Some((
                Type::UserType(ident),
                start..self.peek().unwrap().1.end,
            )));
        }

        Ok(None)
    }

    /// Parse a Struct
    ///
    /// struct -> struct IDENT { struct_field* }
    fn parse_struct(&mut self) -> Result<Option<Spanned<Item>>> {
        let start = if let Some(tok) = self.peek() {
            tok.1.start
        } else {
            return Ok(None);
        };

        if !self.match_(&[Token::Struct]) {
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

        if !self.match_(&[Token::Punct('{')]) {
            let tok = self.previous().unwrap();
            return Err(Box::new(Simple::expected_input_found(
                tok.1.clone(),
                [Some("'{".to_owned())],
                Some(tok.0.to_string()),
            )));
        }

        let mut fields = Vec::new();

        while !self.at_end() && !self.match_(&[Token::Punct('}')]) {
            match self.parse_struct_field() {
                Ok(field) => {
                    fields.push(field);
                }
                Err(err) => {
                    return Err(err);
                }
            }
            self.match_(&[Token::Punct(',')]);
        }

        // 🚧 This isn't right, but maybe it's good enough.
        let end = if let Some(tok) = self.peek() {
            tok.1.end
        } else {
            self.previous().unwrap().1.end
        };

        Ok(Some((Item::Struct(name, fields), start..end)))
    }

    /// Parse an identifier
    ///
    /// ident -> IDENT
    fn parse_ident(&mut self) -> Option<Spanned<String>> {
        debug!("enter");

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
    fn parse_struct_field(&mut self) -> Result<(Spanned<String>, Spanned<Type>)> {
        debug!("enter parse_struct_field");

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

        if !self.match_(&[Token::Punct(':')]) {
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
                    Some("'Option<T>'".to_owned()),
                    Some("'[T]'".to_owned()),
                ],
                Some(tok.0.to_string()),
            );
            let err = err.with_label("expected type");
            return Err(Box::new(err));
        };

        debug!("exit parse_struct_field: ", (&name, &ty));

        Ok((name, ty))
    }

    fn match_(&mut self, tokens: &[Token]) -> bool {
        for tok in tokens {
            if self.check(tok) {
                self.advance();
                debug!("matched: ", tok);
                return true;
            }
        }

        false
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
/// Parses a single line of input.
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
        Err(report_errors(errs, parser.errors, "line", src))
    } else {
        Ok(ast)
    }
}

// This will return as much of the parsed ast as possible, even when hitting an
// error, which explains the return type.
// 🚧 WTF am I talking about?
pub fn parse_dwarf(name: &str, src: &str) -> Result<Vec<Spanned<Item>>, DwarfError> {
    let (tokens, errs) = lexer().parse_recovery_verbose(src);

    let mut parser = DwarfParser::new(tokens.unwrap());
    let (ast, parse_errs) = parser.parse_program();

    log::debug!("parse_dwarf: {:#?}", ast);

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

        dbg!(&ast);

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
    fn test_import() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            use foo;
            use foo::bar::baz::Baz;
            use foo::bar::Xyzzy as Plugh;
        "#;

        let ast = parse_dwarf("test_import", src);

        // dbg!(&ast);

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

        // dbg!(&ast);

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

        // dbg!(&ast);

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

        // dbg!(&ast);

        assert!(ast.is_ok());
    }

    #[test]
    fn test_field_access() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = "a.id;";

        let ast = parse_line(src);

        // dbg!(&ast);

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

        // dbg!(&ast);

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

        // dbg!(&ast);

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
        // dbg!(ast);
    }

    #[test]
    fn test_simple_expr() {
        let _ = env_logger::builder().is_test(true).try_init();

        let src = r#"
            1 + 1 <= 2;
        "#;

        let ast = parse_line(src);

        assert!(ast.is_ok());
        // dbg!(ast);
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

        dbg!(&ast);
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
        dbg!(&ast);
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
        dbg!(&ast);
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
        // dbg!(&ast);
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
}
