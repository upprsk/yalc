package tokenizer

import "core:testing"

@(test)
test_parens :: proc(t: ^testing.T) {
	source :: "( ) [ ] { }"
	expected: #soa[dynamic]Token
	append_soa(
		&expected,
		Token{ty = .lparen, span = {begin = 0, end = 1}},
		Token{ty = .rparen, span = {begin = 2, end = 3}},
		Token{ty = .lbracket, span = {begin = 4, end = 5}},
		Token{ty = .rbracket, span = {begin = 6, end = 7}},
		Token{ty = .lcurly, span = {begin = 8, end = 9}},
		Token{ty = .rcurly, span = {begin = 10, end = 11}},
		Token{ty = .eof, span = {begin = 11, end = 11}},
	)

	tokens := tokenize(source)
	expect_soa_values(t, tokens, expected[:])
}

@(test)
test_symbols :: proc(t: ^testing.T) {
	source :: "= == . .. ,"
	expected: #soa[dynamic]Token
	append_soa(
		&expected,
		Token{ty = .equal, span = {begin = 0, end = 1}},
		Token{ty = .equal_equal, span = {begin = 2, end = 4}},
		Token{ty = .dot, span = {begin = 5, end = 6}},
		Token{ty = .dot_dot, span = {begin = 7, end = 9}},
		Token{ty = .comma, span = {begin = 10, end = 11}},
		Token{ty = .eof, span = {begin = 11, end = 11}},
	)

	tokens := tokenize(source)
	expect_soa_values(t, tokens, expected[:])
}

@(test)
test_arithmetic :: proc(t: ^testing.T) {
	source :: "+ ++ += - -- -= * ** *= / /="
	expected: #soa[dynamic]Token
	append_soa(
		&expected,
		Token{ty = .plus, span = {begin = 0, end = 1}},
		Token{ty = .plus_plus, span = {begin = 2, end = 4}},
		Token{ty = .plus_equal, span = {begin = 5, end = 7}},
		Token{ty = .minus, span = {begin = 8, end = 9}},
		Token{ty = .minus_minus, span = {begin = 10, end = 12}},
		Token{ty = .minus_equal, span = {begin = 13, end = 15}},
		Token{ty = .star, span = {begin = 16, end = 17}},
		Token{ty = .star_star, span = {begin = 18, end = 20}},
		Token{ty = .star_equal, span = {begin = 21, end = 23}},
		Token{ty = .slash, span = {begin = 24, end = 25}},
		Token{ty = .slash_equal, span = {begin = 26, end = 28}},
		Token{ty = .eof, span = {begin = 28, end = 28}},
	)

	tokens := tokenize(source)
	expect_soa_values(t, tokens, expected[:])
}

@(test)
test_others :: proc(t: ^testing.T) {
	source :: "// comment!\n// on two lines //"
	expected: #soa[dynamic]Token
	append_soa(
		&expected,
		Token{ty = .comment, span = {begin = 0, end = 11}},
		Token{ty = .comment, span = {begin = 12, end = 30}},
		Token{ty = .eof, span = {begin = 30, end = 30}},
	)

	tokens := tokenize(source)
	expect_soa_values(t, tokens, expected[:])
}

expect_soa_values :: proc(t: ^testing.T, value, expected: #soa[]$T) {
	testing.expect_value(t, len(value), len(expected))

	for i := 0; i < min(len(value), len(expected)); i += 1 {
		testing.expect_value(t, value[i], expected[i])
	}
}
