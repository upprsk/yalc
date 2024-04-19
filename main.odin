package main

import "core:fmt"
import "core:os"
import "tokenizer"

// read_entire_file_from_filename :: proc(name: string, allocator := context.allocator, loc := #caller_location) -> (data: []u8, success: bool) {…}


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

	tokens, tokens_data := tokenizer.tokenize(source_code)
	defer {
		delete(tokens)
		delete(tokens_data)
	}

	fmt.println(tokens)
	fmt.println(tokens_data)
}
