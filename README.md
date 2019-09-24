# Crowbar

**Crowbar** is a library for testing code, combining QuickCheck-style
  property-based testing and the magical bug-finding powers of
  [afl-fuzz](http://lcamtuf.coredump.cx/afl/).

## TL;DR

There are [some examples](./examples).

Some brief hints:

1. Use an opam switch with AFL instrumentation enabled (e.g. `opam sw 4.04.0+afl`).
2. Run in AFL mode with `afl-fuzz -i in -o out -- ./_build/myprog.exe @@`.
3. If you run your executable without arguments, crowbar will perform some simple (non-AFL) testing instead.
4. Test binaries have a small amount of documentation, available with `--help`.

## writing tests

To test your software, come up with a property you'd like to test, then decide on the input you'd like for Crowbar to vary.  A Crowbar test is some invocation of `Crowbar.check_eq` or `Crowbar.check`:

```ocaml
let identity x =
  Crowbar.check_eq x x
```

and instructions for running the test with generated items with `Crowbar.add_test`:

```ocaml
let () =
  Crowbar.(add_test ~name:"identity function" [int] (fun i -> identity i))
```

There are [more examples available](./examples), with varying levels complexity.

## building tests

Include `crowbar` in your list of dependencies via your favorite build system.  The resulting executable is a Crowbar test.  (Be sure to build a native-code executable, not bytecode.)

To build tests that run under AFL, you'll need to build your tests with a compiler that has AFL instrumentation enabled.  (You can also enable it specifically for your build, although this is not recommended if your code has any dependencies, including the OCaml standard library).  OCaml compiler variants with AFL enabled by default are available in `opam` with the `+afl` tag.  All versions published starting with 4.05.0 are available, along with a backported 4.04.0.

```shell
$ opam switch 4.06.0+afl
$ eval `opam config env`
$ ./build_my_rad_test.sh # or your relevant build runes
```

## running Tests

Crowbar tests have two modes:

* a simple quickcheck-like mode for testing propositions against totally random input
* a mode using [afl-persistent](https://github.com/stedolan/ocaml-afl-persistent) to get good performance from `afl-fuzz` with OCaml's instrumentation enabled

Crowbar tests can be directly invoked with `--help` for more documentation at runtime.

### fully random test mode

If you wish to use the quickcheck-like, fully random mode to run all tests distributed here, build the tests as above and then run the binary with no arguments.

```
$ ./my_rad_test.exe | head -5
the first test: PASS

the second test: PASS
```

### AFL mode requirements

To run the tests in AFL mode, you'll need to install American Fuzzy Lop ([latest source tarball](http://lcamtuf.coredump.cx/afl/releases/afl-latest.tgz), although your distribution may also have a package available).

Once `afl-fuzz` is available on your system, create an `input` directory with a non-empty file in it (or use `test/input`, conveniently provided in this repository), and an `output` directory for `afl-fuzz` to store its findings.  Then, invoke your test binary:

```
afl-fuzz -i test/input -o output ./my_rad_test.exe @@
```

This will launch AFL, which will generate new test cases and track the exploration of the state space.  When inputs are discovered which cause a property not to hold, they will be reported as crashes (along with actual crashes, although in the OCaml standard library these are rare).  See the [afl-fuzz documentation](https://lcamtuf.coredump.cx/afl/status_screen.txt) for more on AFL's excellent interface.

# What bugs have you found?

[An open issue](https://github.com/stedolan/crowbar/issues/2) has a list of issues discovered by testing with Crowbar.  If you use Crowbar to improve your software, please let us know!
