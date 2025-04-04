#include "test_name_res.hpp"

#include "error_reporter.hpp"
#include "file-store.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "test_helpers.hpp"
#include "tokenizer.hpp"

using json = nlohmann::json;

auto gen_ast(std::string source) -> json {
    char*  buf = nullptr;
    size_t bufsize = 0;

    std::unique_ptr<FILE, void (*)(FILE*)> f = {open_memstream(&buf, &bufsize),
                                                [](auto f) { fclose(f); }};

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, f.get()};

    auto root = fs.add(":memory:", source);
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);
    fflush(f.get());

    if (fer.had_error()) {
        return json{
            {"stderr", std::string_view{buf, bufsize}}
        };
    }

    auto [ast, ast_root] = yal::parse(tokens, fer);
    fflush(f.get());

    if (fer.had_error()) {
        return json{
            {"stderr", std::string_view{buf, bufsize}}
        };
    }

    return ast.fatten(ast_root);
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name, [&]() { return gen_ast(source); });
}
