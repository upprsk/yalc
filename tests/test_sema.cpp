#include "test_sema.hpp"

#include "error_reporter.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;

auto gen_ast_typed(std::string source) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    auto root = fs.add(":memory:", source);
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);
    ms.flush();

    if (fer.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    auto [ast, ast_root] = yal::parse(tokens, fer);
    er.update_error_count(fer);
    ms.flush();

    if (fer.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    auto ts = yal::types::TypeStore{};
    auto prj_root =
        yal::resolve_names(ast, ast_root, er, fs, ts, {.single_file = true});
    ms.flush();

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::sema::sema_ast(ast, prj_root, er, ts, {.single_file = true});

    json j = *prj_root;
    j["ds"] = *ast.get_decl_store();

    return j;
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name, [&]() { return gen_ast_typed(source); });
}

auto test_sema(TestParams const& p) -> std::pair<int, int> {
    Context ctx{.tags = {"sema"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    run_test(ctx, p, "empty file", "");
    run_test(ctx, p, "minimal", "module main;");

    run_test(ctx, p, "minimal main", R"(module main;

func main() i32 {
    return 0;
})");

    fmt::println("sema tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
