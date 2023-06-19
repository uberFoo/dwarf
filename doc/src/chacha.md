# ChaCha Runtime Reference

The interpreter is started via the `dwarf` binary:

```console
Usage: dwarf [OPTIONS] [SOURCE] [-- <ARGS>...]

Arguments:
  [SOURCE]
          Dwarf Source File

          Path to the source file to execute.

  [ARGS]...
          Dwarf main arguments

          These argumnets are passed on to the dwarf `main` function.

Options:
  -d, --dap
          Debug Adapter Protocol (DAP) Backend

          Enable the DAP backend. This will start a TCP server on port 4711.

  -r, --repl
          Post-execution behavior

          Drop into the REPL after executing the source file.

  -b, --banner
          Print the dwarf banner

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

```dwarf, editable
fn main() {
    print(chacha::typeof(42));
    print(chacha::typeof(42.0));
    print(chacha::typeof("42"));
    print(chacha::typeof(true));
    print(chacha::typeof(false));
    print(chacha::typeof({}));
    print(chacha::typeof([0, 1, 2, 3]));
    print(chacha::typeof(["0", "1", "2", "3"]));
    print(chacha::typeof(0..100));
//    print(chacha::typeof(()) );
//    print(chacha::typeof({42, 42.0, "42", true, false}));
}
```