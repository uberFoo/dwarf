# Built-in Types

dwarf contains a handful of primitive {{i: types}}.

## String {{hi: string}}

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
// unicode grapheme's, which is a "printable" character.
chacha::assert_eq(s[len - 1], "💥");
chacha::assert_eq(s[len - 2], "🎉");
chacha::assert_eq(s[len - 9], " ");
```