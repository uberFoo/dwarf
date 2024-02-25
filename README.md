<h1 align="center"><img width="555" src="doc/art/dwarf_sunburst.png" /></h1>

![Build Status](https://github.com/uberFoo/dwarf/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/uberFoo/dwarf/branch/develop/graph/badge.svg?token=D2DEOU0S6E)](https://codecov.io/gh/uberFoo/dwarf)
![Lines of Code](https://tokei.rs/b1/github/uberfoo/dwarf)

# The dwarf Programming Language

dwarf is a programming language based heavily upon, and implemented in, [Rust](https://www.rust-lang.org).
It's a typed, interpreted language that is designed to be easily embedded in other applications -- should one want to do so.


There is a [book 📒](https://uberfoo.github.io/assets/docs/dwarf/introduction.html) in progress.
Check it out!

*Nota Bene*: This is a work in progress.

I appreciate feedback.
Let me know if you love it, or hate it.
There are likely as not bugs, and corner cases.
File a bug report, and let me know.

> **Free Stuff**
>
> I'm giving away dwarf stickers to anyone who files a bug report.

## Installation

The easiest thing to do is to download a [release](https://github.com/uberFoo/dwarf/releases).

### MacOS

MacOs Gatekeeper will complain about the binary being from an unidentified developer, or that it can't check for malware.
Give this a go on the command line, replacing the path with the path to the downloaded binary:

```bash
xattr -d com.apple.quarantine ~/Downloads/dwarf-aarch64-apple-darwin
```

### Linux

> ⚠️ **Linux Build Problem:** I'm having some issues compiling a dependency on linux.

### Building from Source

The next easiest thing to do is to clone the repo and build it yourself.
Of course you'll need to have [rust installed ](https://rustup.rs).

```bash
git clone https://github.com/uberFoo/dwarf.git
cd dwarf
cargo install --path . --bin dwarf
```


Once you have a binary, give it a whirl:

```bash
dwarf https://raw.githubusercontent.com/uberFoo/dwarf/develop/mandelbrot.ore -- 50
```

## Beautiful Error Messages!

![error](doc/art/error.png)

Check out [ariadne](https://docs.rs/ariadne/latest/ariadne/) and you too can have cool error messages!

## Goals

* Typed, interpreted DSL
* Easily embeddable
* Rust-like syntax

## Non-Goals

* Interpreter / debugger performance
* 100% Rust feature / syntax parity

## On the Horizon

These are the thins that spring to mind when I think of what I'd most like to do next with dwarf.
In no particular order:

* [x] async
* [ ] VSCode integration
* [x] Multiple files (use items)
* [x] enums
* [x] lambdas
* [ ] Compiler for VM
* [ ] Visibility modifiers
* [x] Generics
* [ ] Macros (Yes, and I don't know why other interpreted languages don't have them. Am I crazy?)
* [x] Extensions via plugins
* [ ] Char type
* [x] if let expressions
* [ ] Doc Comments, with Doc Tests
* [x] match expression

### Supertype / subtype hierarchies

There will be some sort of support for OO-like behavior.
Minimally we need inheritance, and probably polymorphism.

### Virtual Machine

There's a sort-of hybrid-VM currently, but no compiler.

## Syntax

dwarf syntax is a strict subset of Rust's.
In fact, I use the Rust language syntax parser in `vscode` to highlight, navigate, and edit Dwarf source.

The bits that are missing include iterators, enumerations, paths, visibility modifiers, generics (for now), and the list goes on.

## REPL

dwarf has a REPL.
Pass the `-r` flag to start it.

## Architecture

### Parser

There is a hand written parser that takes care of turning the source into an AST.
The parser is great at parsing good code, and terrible and everything else.
Fixing this is on my todo list.

### Compiler

The AST is "compiled" into an in-memory representation that is comprised of the generated code.
It's a model, and I should probably include pictures, or some way to look at it, despite it's terrible layout.
The model looks like a standard UML class diagram, and it may as well be.
I sort of turn it sideways and treat it as an AST.

So what the compiler really does is translate one AST into another.
It really does more than that though.
It resolves as much type information as it can.
It resolves names.
It may do more, I'll have to look.

### Interpreter

After the compiler comes the interpreter.
It reads the model/AST and executes it.
I think that the default is to execute a `main` function.

If you start in the REPL, you get the execution in a loop.

## License

dwarf is distributed under the terms of both the MIT license and the Apache License (Version 2.0).

See [LICENSE-APACHE](LICENSE-APACHE), [LICENSE-MIT](LICENSE-MIT) for details.

<h1 align="center"><img width="555" src="doc/art/cwarf.png" /></h1>