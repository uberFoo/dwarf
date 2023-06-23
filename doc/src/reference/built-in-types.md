# Built-in Types

dwarf contains a handful of primitive {{i: types}}.

## String {{hi: string}}
{{hi: format}}

The string type is esentially a wrapper for Rust's `String` type.
That is to say that it is a proper unicode string, encoded as UTF-8.

```dwarf
let s: string = "Hello, world!🎉💥";

// Note the addition operator below.
print(s + "\n");

// Strings are also iterable.
for c in s {
    print(c + "\n");
}

// Of course you can fetch the length of a string.
let len = s.len();

// Note that indexing into a string is zero based. Also, we are indexing by
// unicode graphemes, which is a "printable character".
chacha::assert_eq(s[len - 1], "💥");
chacha::assert_eq(s[len - 2], "🎉");
chacha::assert_eq(s[len - 9], " ");

// index into a string with a range.
print("{0}{1}\n".format(s[0..5], s[len - 3])); // Hello!

// Strings also support substitution via the `format` method.
// The string contains {?}, where ? is a number that corresponds to a positional
// argument to the `format` method. Like most everything else in dwarf, the
// arguments to the method are arbitrary expressions.
print("The length of the string is {0}. {0} * {0} = {1}\n".format(len, len * len));

let answer = 42;
let question = "Huh?";
let msg = "The answer to life, the universe, and everything is {1}. {0} Really, {1}\n";
print(msg.format(question, answer));
```