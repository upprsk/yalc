#include "sema.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>

#include "parser.hpp"
#include "tokenizer.hpp"
#include "types.hpp"

using namespace yal;

using Catch::Matchers::ContainsSubstring;
using Catch::Matchers::Equals;

// NOLINTBEGIN(readability-function-cognitive-complexity)
// NOLINTBEGIN(readability-function-size)
// NOLINTBEGIN(modernize-use-designated-initializers)

TEST_CASE("empty source", "[sema]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = "";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());

    auto ts = TypeStore::new_store();
    auto m = sema(ast, ts, root, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(m.funcs.empty());
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-size)
// NOLINTEND(readability-function-cognitive-complexity)
