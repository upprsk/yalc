all: test

.PHONY: yalc
yalc:
	cmake --build build

test: test.yal yalc
	./build/cmd/yalc test.yal | qbe | gcc -o test -xassembler -
