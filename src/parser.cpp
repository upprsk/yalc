#include "parser.hpp"

#include <charconv>
#include <span>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "tokenizer.hpp"

namespace yal {

struct Parser {
    [[nodiscard]] constexpr auto peek() const -> Token const& {
        return tokens[current];
    }

    [[nodiscard]] constexpr auto span() const -> Span { return peek().span; }

    [[nodiscard]] constexpr auto is_at_end() const -> bool {
        return peek().is_eof();
    }

    constexpr void advance() {
        if (!is_at_end()) current++;
    }

    // ------------------------------------------------------------------------

    auto parse_file() -> NodeHandle { return primary(); }

    auto primary() -> NodeHandle { return number(); }

    auto number() -> NodeHandle {
        auto t = peek();
        if (t.is_int()) {
            advance();

            auto     s = t.span.str(source);
            uint64_t value = 0xDEAD'BEEF;

            // NOTE: ignoring errors
            auto [ptr, ec] =
                std::from_chars(s.data(), s.data() + s.length(), value);
            if (ec != std::errc{}) {
                er->report_bug(t.span, "invalid integer (from_chars)");
            }

            return ast.new_node_int(t.span, value);
        }

        er->report_error(t.span, "expected number, got {}", t.type);
        return ast.new_node_err(span());
    }

    // ------------------------------------------------------------------------

    std::span<Token const> tokens;
    std::string_view       source;
    ErrorReporter*         er;

    Ast    ast{};
    size_t current{};
};

auto parse(std::span<Token const> tokens, std::string_view source,
           ErrorReporter& er) -> std::pair<Ast, NodeHandle> {
    auto p = Parser{.tokens = tokens, .source = source, .er = &er};
    auto a = p.parse_file();

    return {p.ast, a};
}

}  // namespace yal
