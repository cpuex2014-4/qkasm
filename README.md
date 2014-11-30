# qkasm

## dependencies

- ocaml-nox
- menhir

## usage

```
$ make
...
$ ./asm_examples.sh
$ ./qkasm -disasm < examples/loopback
...
```

## examples

* examples/foo.s : assembly source file
* examples/foo : assembled binary file
* examples/foo.in : sample input file for the program
* examples/foo.bin : program and input combined

* examples/loopback.s : loopback, echo server
* examples/fib-loop.s : fibonacci by loop
* examples/fib-recur.s : fibonacci by recursion
* examples/floatfib.s : fibonacci by floating-point addition
* examples/mandelbrot.s : mandelbrot (96x64)
* examples/mandelbrot-large.s : mandelbrot (768x512)

