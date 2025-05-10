CMAKE_J=1

all: test

test: test.yal.o test.c.o
	gcc -o test test.yal.o test.c.o

test.c.o: test.c
	gcc -c -o test.c.o test.c

test.yal.o: test.yal.asm
	gcc -c -o test.yal.o -xassembler test.yal.asm

test.yal.asm: test.yal.qbe
	qbe -o test.yal.asm test.yal.qbe

# https://stackoverflow.com/a/3038439
.SUFFIXES: .yal
test.yal.qbe: yalc test.yal test_c.yal
	./build/cmd/yalc test.yal -o test.yal.qbe

.PHONY: yalc
yalc:
	cmake --build build -j${CMAKE_J}

.PHONY: json
json: yalc
	@./build/cmd/yalc test.yal --dump-ast-json |\
		jq 'walk(if type == "object" then del(.loc, .ds) else . end)' |\
		wl-copy

.PHONY: clean
clean:
	rm -r test test.c.o test.yal.o test.yal.asm test.yal.qbe

.PHONY: clean_all
clean_all: clean
	cmake --build build -t clean
