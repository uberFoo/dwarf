# A Path to {{i: Async}}

## Async Implementation

dwarf is written in Rust, and Rust's asynchronous support comes in the form of {{i: [`futures`](https://docs.rs/futures/latest/futures/)}}
It thus makes sense that dwarf's async model be implemented using futures.
And indeed, that is part of the story.
The full story is however a bit more complex and nuanced.

A simple way of looking at dwarf is an expression evaluation engine.
`Value`s go in, and values come out.
Given this, we have a few places where it makes sense to crack the shell and look inside.

If you consider futures in Rust, it's all about the return type.
The return type is even named after the abstraction: `Future`.
Since evaluation of an expression returns a value, it makes sense to leverage that and add a `Future` to our Value enum.
Since this is Rust, what starts as a good idea quickly becomes complex.

## Delving into dwarf

There are a bare few things that we need to know about dwarf before we can start to discuss adding async support.

Perhaps the most significant aspect is that nearly everything in dwarf is an expression.
Expressions are evaluated using the `eval_expression` function in {{i: ChaCha}}, the interpreter.