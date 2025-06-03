#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

#include <any>
#include <libassert/assert.hpp>
#include <nlohmann/json.hpp>

#include "error_reporter.hpp"
#include "file_store.hpp"
#include "tests.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

namespace yal::tests {
// NOLINTBEGIN(readability-function-cognitive-complexity)
// NOLINTBEGIN(readability-function-size)

auto integration() -> ut::Test {
    auto tb = ut::new_test("integration");

    std::vector<ut::TestParams> params;

    auto add_parametrized = [&](std::string name, std::string s) {
        params.push_back({std::move(name), std::move(s)});
    };

    // empty file
    add_parametrized("empty", "");
    add_parametrized("module", "module main;");
    add_parametrized(
        "module 2",
        "module main;\n\nfunc main() i32 {\n    return 0;\n    }\n");

    add_snap(tb, "empty file", params,
             [](FILE* out, ut::TestContext const& c) -> nlohmann::json {
                 auto const& p = c.params;
                 ASSUME(p.size() == 2);

                 auto const& name = std::any_cast<std::string>(p[0]);
                 auto const& source = std::any_cast<std::string>(p[1]);

                 auto fs = yal::FileStore{};
                 auto er = yal::ErrorReporter{&fs, out};

                 auto fileid = fs.add_file_and_contents(
                     fmt::format(":memory:{}", name), source);

                 auto erf = er.for_file(fileid);
                 auto tokens = tokenize(source, erf);

                 return tokens;
             });

    return tb;
}

// NOLINTEND(readability-function-size)
// NOLINTEND(readability-function-cognitive-complexity)
}  // namespace yal::tests
