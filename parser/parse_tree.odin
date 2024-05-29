package parser

import "../tokenizer"
import "core:fmt"
import "core:mem"
import "core:runtime"
import "core:slice"

ParseTree :: struct {
	nodes: #soa[]ParseTreeNode,
}

ParseTreeNodeKind :: enum {
	error,
	identifier,
}

NodeSpan :: struct {
	begin: u32,
	end:   u32,
}

ParseTreeNode :: struct {
	kind: ParseTreeNodeKind,
	kids: u32,
	span: NodeSpan,
}

State :: struct {
	tokens: #soa[]tokenizer.Token,
	nodes:  #soa[dynamic]ParseTreeNode,
}

@(require_results)
filter_soa :: proc(
	s: $S/#soa[]$U,
	f: proc(_: U) -> bool,
	allocator := context.allocator,
) -> (
	res: S,
	err: runtime.Allocator_Error,
) #optional_allocator_error {
	r := make_soa(#soa[dynamic]U, 0, 0, allocator) or_return
	for v in s {
		if f(v) {
			append_soa(&r, v)
		}
	}
	return r[:], nil
}

parse :: proc(tokens: #soa[]tokenizer.Token) -> ParseTree {
	// proc(s: $S/[]$U, f: proc(_: U) -> bool, allocator := context.allocator) -> (res: S, err: runtime.Allocator_Error)
	clean_tokens := filter_soa(tokens, proc(tok: tokenizer.Token) -> bool {
		return tok.ty != .comment
	})

	defer delete(clean_tokens)
	fmt.println(clean_tokens)

	state := State {
		tokens = clean_tokens,
	}

	for !match(&state, .eof) {
		parse_top_level_decl(&state)
	}

	return ParseTree{nodes = state.nodes[:]}
}

advance :: proc(s: ^State) {
	if len(s.tokens) > 0 {
		s.tokens = s.tokens[1:]
	}
}

current :: proc(s: State) -> tokenizer.Token {
	if len(s.tokens) > 0 {
		return s.tokens[0]
	}

	return tokenizer.Token{ty = .eof}
}

current_span :: proc(s: State) -> NodeSpan {
	span := current(s).span
	return NodeSpan{begin = span.begin, end = span.end}
}

check :: proc(s: State, expected: tokenizer.TokenType) -> bool {
	if current(s).ty != expected {return false}
	return true
}

match :: proc(s: ^State, expected: tokenizer.TokenType) -> bool {
	if check(s^, expected) {
		advance(s)
		return true
	}

	return false
}

match_seq :: proc(s: ^State, seq: ..tokenizer.TokenType) -> bool {
	for tok in seq {
		if !match(s, tok) {
			return false
		}
	}

	return true
}

add_node :: proc(s: ^State, node: ParseTreeNode) {
	append_soa(&s.nodes, node)
}

parse_top_level_decl :: proc(s: ^State) {
	if !match_seq(s, .ident, .lparen, .rparen) {
		add_node(s, ParseTreeNode{kind = .error, span = current_span(s^)})
		advance(s)

		// TODO:add error recovery
		return
	}
}
