# Crowbar

**Crowbar** is a library for testing code, combining QuickCheck-style
  property-based testing and the magical bug-finding powers of
  [afl-fuzz](http://lcamtuf.coredump.cx/afl/).

Docs are pretty sparse at the moment, but there are [some examples](./examples).

Some brief hints:

1. Use an opam switch with AFL instrumentation enabled (e.g. `opam sw 4.04.0+afl`).
2. Run with `afl-fuzz -i in -o out -- ./_build/myprog.exe @@`.
3. If you run your executable without arguments, crowbar will perform some simple (non-AFL) testing instead.
