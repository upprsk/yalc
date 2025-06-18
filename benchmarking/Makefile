all: bench_yal bench_c bench_cpp bench_rs

bench_rs: bench.rs
	rustc -o bench_rs bench.rs -O

bench_cpp: bench.cpp
	g++ -o bench_cpp bench.cpp -O2 -std=c++23

bench_c: bench.c
	gcc -o bench_c bench.c -O2

bench_yal: bench.yal.o
	gcc -o bench_yal bench.yal.o

bench.yal.o: bench.yal.asm
	gcc -c -o bench.yal.o -xassembler bench.yal.asm

bench.yal.asm: bench.yal.qbe
	qbe -o bench.yal.asm bench.yal.qbe

# https://stackoverflow.com/a/3038439
.SUFFIXES: .yal
bench.yal.qbe: bench.yal c.yal
	../build/cmd/yalc bench.yal -o bench.yal.qbe

.PHONY: clean
clean:
	rm -r bench_yal bench.yal.o bench.yal.asm bench.yal.qbe bench_c bench_cpp bench_rs
