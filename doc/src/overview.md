# Language Walkthrough

Thanks for reading thus far!
This section is a walkthrough of the language.
It is designed to provide information in an order useful to someone new to programming.

## A Quick Program

This will be a motivating example for the rest of the section:

```dwarf
fn print_number(n: int) {
    print("the number is {0}\n".format(n));
}

fn main() {
    let a = 42;
    print_number(a);
}
```

## Functions and Statements

Let's start with {{i: function}}s and {{i: statement}}s.
Functions are composed of statements between curly braces.
You already saw a function in the [introduction](./introduction.md).
That function printed {{i:`()`}} after the `Hello, world!` message.
In dwarf we just call that the {{i: empty}} type/value.
It's a type, and a value, and it's the only value of it's type.
A function (and any {{i: block}} for that matter) returns the value of it's last statement.
A statement that ends in a semi-colon has the empty value:

```dwarf
fn main() {
    print("");
}
```

On the other hand, a statement that does not terminate in a semi-colon has the value of it's expression:

```dwarf
fn main() -> int {
    42
}
```

> **Expressions**
>
> There will be a lot more to say about expressions later.
> For now just know that basically everything in dwarf is an expression.

### Let Statement

Not all statements are composed of expressions.
In particular is the {{i: `let`}} statement.
Since the let statement has no value, it must be terminated with a semi-colon.
The following is not valid and throws an error:

```dwarf
fn main() {
    let a = 42
}
```

The let statement is instead used to assign a value to a storage location, or memory.
The storage location is called a {{i: variable}}, and has a name.
You use the name to refer to the variable elsewhere in the function:

```dwarf
fn main() {
    let a = 42;
    print(a);
}
```

Getting back to functions, functions take inputs, do something with them, and return an output.
The following code defines a function that takes an integer, does some computation, and returns a string.

```dwarf
fn foo(a: int) -> string {
    let b = a * 2;
    // 🚧 This should not need a cast.
    "The value of b is {1}. This is here just to confuse you, {0}".format("hah!", b) as string
}

// 🚧 This should not need a cast.
print(foo(42) as string + "\n");
```


> **Advanced Statements**
>
> There is a third type of statement, called an {{i: item}} statement.
> It's useful for defining {{i: struct}}s and {{i: function}}s inside of a {{i: block expression}}.
> Below is an example of both:
> ```dwarf
> fn main() {
>     struct Point {
>         x: float,
>         y: float,
>     }
>
>    impl Point {
>        fn new(x: float, y: float) -> Point {
>            Point { x: x, y: y }
>        }
>    }
>
>     fn foo() -> Point {
>         Point::new(42.0, -3.14)
>     }
>
>     print(foo());
> }
> ```

## Conditional Expressions

Next up is the {{i: conditional expression}}.
Conditional expressions are used to make decisions.
They are composed of three parts: a {{i: condition}}, a {{i: then expression}}, and an {{i: else expression}}.
The condition is an expression that evaluates to a boolean value.
The then and else expressions are expressions that evaluate to the same type.
The type of the conditional expression is the type of the then and else expressions.
The following is an example of a conditional expression:

```dwarf
fn main() {
    let a = 42;
    let b = 69;
    let c = if a > b { a } else { b };
    print(c);
}
```
There are all the usual {{i: comparison operators}}: `==`, `!=`, `<`, `<=`, `>`, `>=`:

```dwarf
fn main() {
    chacha::assert_eq(42 == 42, true);
    chacha::assert_eq(42 != 42, false);
    chacha::assert_eq(42 < 42, false);
    chacha::assert_eq(42 <= 42, true);
    chacha::assert_eq(42 > 42, false);
    chacha::assert_eq(42 >= 42, true);
}
```

> **Expression Magic**
>
> Having everything as an expression pays great dividends.
> In the example above notice how we assign `c` to the result of the {{i: `if`}} expression.
> `if` actually has a value, which is the value of it's evaluation.
> It's expressions all the way down. 🥁


```dwarf
//macro_rules! println {
//    () => {
//        print("\n");
//    },
//    ($arg:expr) => {
//        print($arg as string + "\n");
//    },
//    ($fmt:literal, $($arg:expr),*) => {
//        print($fmt.format($($arg),*) + "\n");
//    }
//}

//printtln!();
print("\n");
//println!("Hello, world!");
print("Hello, world!" as string + "\n");
//println!("The answer is {0}, what's the {1}", 42, "question?");
print("The answer is {0}, what's the {1}".format(42, "question?") + "\n");
//println!(42);
print(42 as string + "\n");
```
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
