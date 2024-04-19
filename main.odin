package main

import "core:fmt"
import "core:os"
import "tokenizer"

main :: proc() {
	filename :: "test.yal"
	data, read_ok := os.read_entire_file(filename)
	if !read_ok {
		fmt.eprintln("error: failed to read", filename)
		os.exit(1)
	}

	defer delete(data)

	source_code := transmute(string)data
	fmt.println(source_code)

	tokens := tokenizer.tokenize(source_code)
	defer {
		delete(tokens)
	}

	fmt.println(tokens)
}
