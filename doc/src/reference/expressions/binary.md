# Binary Expressions

## Equality

The {{i: equals}} operator ({{i: `==`}}) tests two expressions for equality and returns a {{i:bool}}.

```dwarf
fn main() {
    print(1 == 1); // true
    print(1 == 2); // false
}
```

> Equality is a tricky thing.
> There is reference equality, and value equality.
> I've taken the perspective that if you can point to two different things, even if they are identical, they are still two different things.
> This is reference equality.
>
> Equality is transitive.
> There is exactly one <word> unique representation of `1` in the language, and in the CPU.
> That is why the following works.
>
> ```dwarf
> fn main() {
>     let a = 1;
>     let b = 1;
>     print(a == b); // true
> }
> ```
>
> ```dwarf
> struct Foo {}
> struct Bar { bar: int }
>
> fn main() {
>     let a = Foo {};
>     let b = Foo {};
>     let e = b;
>     print(e == b); // *true*
>     let c = 1;
>     let d = c;
>     print(a == b); // *true*
>     print(c == d); // true? false? *true*!
>
>     let f = Bar { bar: 1 };
>     let g = Bar { bar: 1 };
>     let h = f;
>     print(h == f); // true
>     print(f == g); // true? false? *true*!
>     f.bar = 2;
>     print(f == g); // true? false? *false*!
>     print(h == f); // true? false? *true*!
> }
> ```
>
> That said, it's a matter of perspective.
> {{i: ChaCha}} knows about references, but dwarf does not.
