# Yet Another Language

As it has become common, this is a language to replace C.

```yal
module main;

@extern
func printf(fmt: [*]const u8, ...);

func main() i32 {
    printf("Hello, %s!\n".ptr, "World".ptr);

    return 0;
}
```

## How to build

The bootstrap compiler is made in C++ and uses CMake. To build, use the
standard commands.

```bash
cmake -S . -B build
cmake --build build
```

Then the compiler can be invoked with:

```bash
./build/cmd/yalc
```

The compiler will generate QBE IR code, so [QBE](https://c9x.me/compile/) is
needed to convert the output of the compiler to assembly. Then an assembler and
linker is needed to create the final executable.

So to create an executable from a file named `test.yal` named `test`:

```bash
./build/cmd/yalc test.yal -o test.qbe
qbe -o test.asm test.qbe
gcc -c -o test.o -xassembler test.asm
gcc -o test test.o
```

A Makefile is supplied to do the operations above.
