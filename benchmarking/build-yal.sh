#!/usr/bin/env bash

qbe -o bench.yal.asm bench.yal.qbe
gcc -c -o bench.yal.o -xassembler bench.yal.asm
gcc -o bench_yal bench.yal.o
