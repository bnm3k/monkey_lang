# Monkey Lang

An implementation of the Monkey Programming language in Rust based on Thorsten
Ball's [Writing An Interpreter in Go](https://interpreterbook.com/). Other than
the change in language used, in this implementation, all objects are
reference-counted rather than garbage-collected.

## Usage

Build:

```
cargo build --bin monkey --release -Z unstable-options --out-dir .
```

Run with a given file:

```
./monkey [filepath]
```

Or run REPL:

```
./monkey

Monkey lang
> puts("Hello World")
Hello World
null
>
```

## Examples

Create the following as `hello.mky`:

```
let fib = fn(n) {
    if (n < 2){ n }
    else { fib(n - 1) + fib(n - 2) }
};

let n = 20
let got = fib(n);
puts("Hello world: fib(" + str(n) + ") = " + str(got));
```

Then run it as follows:

```
./monkey hello.mky
```

This outputs:

```
Hello world: fib(20) = 6765
null
```

## Tests

To run tests:

```
cargo test
```

## License

All source code in this repository is licensed under the MIT License.
