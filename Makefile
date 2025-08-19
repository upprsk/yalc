.PHONY: all
all: build run

.PHONY: run
run:
	./build/cmd/metac/metac test.h cmd/yalc/type-info.gen.h -vp -vs

.PHONY: build
build:
	cmake --build build
