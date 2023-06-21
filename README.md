<h1 align="left"><img width="440" src="doc/art/dwarf_sunburst.png" /></h1>

# The dwarf Programming Language

dwarf is a programming language based heavily upon, and implementd in, [Rust](https://www.rust-lang.org).
The language is interpreted (and slow) with a VM (fast-ish) on the way.
The long term plan is to turn the dwarf source into Rust source, and compile it with the Rust compiler.

See the [book](https://uberfoo.github.io/assets/docs/dwarf/introduction.html).

## Goals

*

## Non-Goals

* Interpreter / debugger performance
* 100% Rust feature / syntax parity

## On the Horizon

### Supertype / subtype hierarchies

There will be some sort of support for OO-like behavior

### Virtual Machine

There's a sort-of hybrid-VM currently, but no compiler.

## Syntax

dwarf syntax is a strict subset of Rust's.
In fact, I use the Rust language syntax parser in `vscode` to highlight, navigate, and edit Dwarf source.

The bits that are missing include iteraters, enumerations, paths, visibility modifiers, generics (for now), and the list goes on.
I recommend you just explore, or take a look at the parser source.
Maybe I'll put together a BNF grammar table below.

## REPL

## Debugger

## Source Code

About thirty percent of the source code (everything in `./src/lu_dog` ) is generated from models.
The models are `json` files located in `./src/models` .
These models are "compiled" using another project that I'm currently working on.
In fact, this is a daughter project of that other one.

## Architecture

### Parser

There is a hand written parser that takes care of turning the source into an AST.
The parser is great at parsing good code, and terrible and everyhing else.
Fixing this is on my todo list.

### Compiler

The AST is "compiled" into an in-memory representation that is comprised of the generated code.
It's a model, and I should probably include pictures, or some way to look at it, despite it's terrible layout.
The model looks like a standard UML class diagram, and it may as well be.
I sort of turn it sideways and treat it as an AST.

So what the compiler really does is transate one AST into another.
It really does more than that though.
It resolves as much type information as it can.
It resolves names.
It may do more, I'll have to look.

### Interpreter

After the compiler comes the interpreter.
It reads the model/AST and executes it.
I think that the default is to execute a `main` function.

If you start in the REPL, you get the execution in a loop.

### Debugger

## License

dwarfr is distributed under the terms of both the MIT license and the Apache License (Version 2.0).

See [LICENSE-APACHE](LICENSE-APACHE), [LICENSE-MIT](LICENSE-MIT) for details.