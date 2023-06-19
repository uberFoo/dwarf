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

// Can we get the length of a string?
print(s.len() + "\n");

// Can we index into a string?
//print(s[s.len()] + "\n");
```
```