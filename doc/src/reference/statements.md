# Statements

dwarf, like Rust uses only a handful {{i: statement}}s.

## {{i: Let Statement}}

The {{i: `let`}} statement is used to assign a value to a variable.

```dwarf
let x = 42;
```

## {{i: Expression Statement}}

## {{i: Result Statement}}

```dwarf
fn empty() -> () {
    42;
}

fn value() -> int {
    42
}

print(empty());
print(value());

print({});
print({42});
```

## {{i: Item Statement}}

```dwarf
fn main () {
    struct Point {
        x: float,
        y: float,
    }
}
```
