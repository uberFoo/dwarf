This is supposed to be a README, but I'm using it for notes.

I was just thinking about code generation. I like the idea of a visitor pattern. The code that
does the traversal will accept tagged code fragments from the visitor. At the end the fragments
are put together according to the whims of the coder.

## Interpreter Notes

* The Proxy methods are not typechecked at runtime, so it's recommended to not call
them directly.