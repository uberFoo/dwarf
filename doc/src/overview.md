# Language Walkthrough

This walkthrough is organized as "nuggets" of information.
The order is somewhat arbitrary, and flows from general, need-to-know concepts and builds upon them.

## Golden Nuggets ⭐️🌟✨

0. The interpreter is called {{i: ChaCha}}, but the binary is called `dwarf`.
Go figure.

0. I call input files `.tao`, I don't remember why.
None of the tooling cares what you name them.

0. The interpreter looks for a {{i: `main`}} function, where it will begin execution.
```dwarf
fn main() {
    print("Hello, world!\n");
}
```
Passing {{i:command line arguments}} to main from the interpreter is [supported](./chacha.md#args).

0. In dwarf, just about everything is an {{i:expression}}.
The only thing that is not an expression is a {{i:`let`}} statement.
That's not true.
Items inside blocks are also statements.
So that's only two things that I can think of.
Putting a semicolon at the end of an expression makes it a statement, but it's still an expression underneath.
This is super powerful, and allows for some really cool things.
