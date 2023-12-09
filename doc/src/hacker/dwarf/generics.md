# Generic Types

For anyone not in the know, generics are a type of polymorphism.
They allow you to write code that works with multiple types.
They do this by inserting your type into the code at compile time.
You specify a placeholder for the type that is supplied by the user.

Here's a simple example:

```dwarf
// This is a generic function.
// It takes a type T and returns a value of type T.
fn id<T>(x: T) -> T {
    x
}

// This is a generic type.
// It takes a type T and stores a value of type T.
struct Box<T> {
    value: T,
}

impl Box<T> {
    fn display(self) {
        print("Box<{0}>\n".format(self.value));
    }
}

fn main() {
    // Here we call the generic function with an int.
    let x = id(42);
    print("{0}\n".format(x));

    // Here we create a Box that stores an int.
    let y = Box{value: 42};
    print("{0}\n".format(y));
    y.display();
}
```

Below is an approximation of (a part of) the model that is used to generate (a part of) the dwarf abstract syntax tree ([AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)).
The points worth reflecting upon are that `Type` is a generalization over all of the dwarf types.
Also, `Struct` and `Field` both have relationships to `Type`. separate from `R1`.

```mermaid
classDiagram
    Type <|-- Generic : R1
    Type <|-- Integer : R1
    Type <|-- Struct : R1
    Type <|-- Etc : R1
    Generic --> "0..1" Type : R2
    Struct "1" <-- "0..*" Field : R3
    Field --> Type : R4

    class Generic {
        place_holder: String
        type: Option~R2_Type~
    }

    class Struct {
        name: String
    }

    class Field {
        name: String
        struct: R3_Struct
        type: R4_Type
    }
```

```dwarf
let definition = "
    struct Box<T> {
        value: T,
    }
";

print(definition);
let ast = chacha::parse(definition);
print(ast);
```

Mention something about assuming the type of a field expression, and then having to use that to check type.