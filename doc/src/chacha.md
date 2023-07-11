# ChaCha Runtime Reference

The interpreter is started via the `dwarf` binary:

```console
Usage: dwarf [OPTIONS] [SOURCE] [-- <ARGS>...]

Arguments:
  [SOURCE]
          Dwarf Source File

          Local path, or URL of the source file to execute.

  [ARGS]...
          Dwarf main arguments

          These arguments are passed on to the dwarf `main` function.

Options:
  -d, --dap
          Debug Adapter Protocol (DAP) Backend

          Enable the DAP backend. This will start a TCP server on port 4711.

  -r, --repl
          Post-execution behavior

          Drop into the REPL after executing the source file.

  -b, --banner
          Print the dwarf banner

      --bless
          Bless a test

          This is only useful if you are writing tests for dwarf. I'd really like it if clap had hidden arguments.

      --uber
          Do uber stuff

          This is like sudo mode. You probably don't want this.

  -s, --stdin
          Stdin

          Read source file from stdin.

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

## args

Command line arguments are made available to a running program as a {{hi: vector}} [vector](./reference/built-in-types.md#vector) of {{hi: string}} [strings](./reference/built-in-types.md#string) via the `chacha::args` function:

```dwarf
fn main() {
    for arg in chacha::args() {
        print(arg + "\n");
    }
}
```

Misfortunately I haven't hacked all the HTTP nonsense necessary to pass command line arguments from here.
I might get to it before release.

## typeof

This is exactly what it sounds like.
Pass a value, and see what type it is:

```dwarf
print("42's type: {0}\n".format(chacha::typeof(42)));
print("42.0's type: {0}\n".format(chacha::typeof(42.0)));
print("true's type: {0}\n".format(chacha::typeof(true)));
print("false's type: {0}\n".format(chacha::typeof(false)));
print("[0, 1, 2, 3]'s type: {0}\n".format(chacha::typeof([0, 1, 2, 3])));
print("0..100's type: {0}\n".format(chacha::typeof(0..100)));
```

```dwarf
```

``` dwarf
# struct Foo {}
//print("Foo\{\}'s type: {0}\n".format(chacha::typeof(Foo{})));
print("{0}\n".format(chacha::typeof({})));
print("{0}\n".format(chacha::typeof(["0", "1", "2", "3"])));
print("\"42\"'s type: {0}\n".format(chacha::typeof("42")));
// print(chacha::typeof(()) );
```

```dwarf
# struct Complex {
#     re: float,
#     im: float,
# }

impl Complex {
    fn square(self) {
        self.re = self.re * self.re - self.im * self.im;
        self.im = 2.0 * self.re * self.im;
    }
}
```
