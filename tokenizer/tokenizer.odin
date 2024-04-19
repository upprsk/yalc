package tokenizer

import "core:unicode/utf8"

TokenType :: enum u8 {
	// parens
	lparen,
	rparen,
	lbracket,
	rbracket,
	lcurly,
	rcurly,

	// symbols
	equal,
	equal_equal,

	// arithmetic
	plus,
	plus_plus,
	plus_equal,
	minus,
	minus_minus,
	minus_equal,
	star,
	star_star,
	star_equal,
	slash,
	slash_equal,

	// special
	comment,
	eof,
	error,
}

TokenSpan :: struct {
	begin: u32,
	end:   u32,
}

State :: struct {
	source:  []rune,
	start:   u32,
	current: u32,
	tokens:  [dynamic]TokenType,
	data:    [dynamic]TokenSpan,
}

tokenize :: proc(source: string) -> ([]TokenType, []TokenSpan) {
	s := State {
		source = utf8.string_to_runes(source),
	}
	defer delete(s.source)

	for {
		tok, data := tokenize_one(&s)
		append(&s.tokens, tok)
		append(&s.data, data)
		if tok == .eof {break}
	}

	return s.tokens[:], s.data[:]
}

tokenize_one :: proc(s: ^State) -> (TokenType, TokenSpan) {
	skip_whitespace(s)

	s.start = s.current
	if is_at_end(s^) {
		return .eof, TokenSpan{}
	}

	c := peek(s^)
	advance(s)
	switch c {
	// parens
	case '(':
		return mktoken(s^, .lparen)
	case ')':
		return mktoken(s^, .rparen)
	case '[':
		return mktoken(s^, .lbracket)
	case ']':
		return mktoken(s^, .rbracket)
	case '{':
		return mktoken(s^, .lcurly)
	case '}':
		return mktoken(s^, .rcurly)

	// symbols
	case '=':
		return mktoken_any_of(s, peek(s^), []rune{'='}, []TokenType{.equal, .equal_equal})

	// arithmetic
	case '+':
		return mktoken_any_of(
			s,
			peek(s^),
			[]rune{'+', '='},
			[]TokenType{.plus, .plus_plus, .plus_equal},
		)
	case '-':
		return mktoken_any_of(
			s,
			peek(s^),
			[]rune{'-', '='},
			[]TokenType{.minus, .minus_minus, .minus_equal},
		)
	case '*':
		return mktoken_any_of(
			s,
			peek(s^),
			[]rune{'*', '='},
			[]TokenType{.star, .star_star, .star_equal},
		)
	case '/':
		if peek(s^) == '/' {
			// comment!
			for !is_at_end(s^) && peek(s^) != '\n' {
				advance(s)
			}

			return mktoken(s^, .comment)
		}

		return mktoken_any_of(s, peek(s^), []rune{'='}, []TokenType{.slash, .slash_equal})
	case:
		return mktoken(s^, .error)
	}
}

skip_whitespace :: proc(s: ^State) {
	for !is_at_end(s^) {
		switch peek(s^) {
		case ' ', '\t', '\r', '\n':
			advance(s)
		case:
			return
		}
	}
}

mktoken_any_of :: proc(
	s: ^State,
	c: rune,
	cases: []rune,
	tys: []TokenType,
) -> (
	TokenType,
	TokenSpan,
) {
	assert(len(tys) == len(cases) + 1)
	for ca, idx in cases {
		if c == ca {
			advance(s)
			return mktoken(s^, tys[idx + 1])
		}
	}

	return mktoken(s^, tys[0])
}

mktoken :: proc(s: State, ty: TokenType) -> (TokenType, TokenSpan) {
	return ty, TokenSpan{begin = s.start, end = s.current}
}

peek :: proc(s: State) -> rune {
	return s.source[s.current]
}

advance :: proc(s: ^State) {
	if is_at_end(s^) {return}

	s.current += 1
}

is_at_end :: proc(s: State) -> bool {
	return s.current == u32(len(s.source))
}
