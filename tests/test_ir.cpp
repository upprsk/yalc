#include "error_reporter.hpp"
#include "ir-build.hpp"
#include "ir.hpp"
#include "lower.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "test_lower.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;
namespace rv = std::ranges::views;

auto gen_ast_ir(std::string source) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    auto root = fs.add(":memory:", source);
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);

    auto [ast, ast_root] = yal::parse(tokens, fer);
    er.update_error_count(fer);

    auto ts = yal::types::TypeStore{};
    auto prj_root =
        yal::resolve_names(ast, ast_root, er, fs, ts, {.single_file = true});

    yal::sema::sema_ast(ast, prj_root, er, ts, {.single_file = true});
    ms.flush();

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::lower::lower_ast(ast, prj_root, er, ts, {.single_file = true});
    ms.flush();

    auto module = yal::ir::build(ast, prj_root, er, ts, {.single_file = true});
    if (er.had_error()) return 1;

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::ir::disasm_module(ms.f, module);
    ms.flush();

    return json{
        {"stdout", ms.str()}
    };
}

auto gen_ast_ir_many(std::vector<std::string> sources) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    std::unordered_set<yal::FileId> filenames;
    for (auto const& [idx, source] : rv::enumerate(sources)) {
        filenames.insert(fs.add(fmt::format(":memory-{}:", idx), source));
    }

    (void)fs.add_dir(":memory-dir:", filenames);

    auto root = fs.add(":memory-0:");
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);

    auto [ast, ast_root] = yal::parse(tokens, fer);
    er.update_error_count(fer);

    auto ts = yal::types::TypeStore{};
    auto prj_root = yal::resolve_names(ast, ast_root, er, fs, ts, {});

    yal::sema::sema_ast(ast, prj_root, er, ts, {});
    ms.flush();

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::lower::lower_ast(ast, prj_root, er, ts, {});
    ms.flush();

    auto module = yal::ir::build(ast, prj_root, er, ts, {});
    if (er.had_error()) return 1;

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::ir::disasm_module(ms.f, module);
    ms.flush();

    return json{
        {"stdout", ms.str()}
    };
}
static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name, [&]() { return gen_ast_ir(source); });
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::vector<std::string> source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name,
                        [&]() { return gen_ast_ir_many(source); });
}

auto test_ir(TestParams const& p) -> std::pair<int, int> {
    Context ctx{.tags = {"ir"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    ctx.tags.emplace_back("minimal");

    run_test(ctx, p, "print_int very simple",
             std::vector<std::string>{
                 R"(
module main;

@export
func main() i32 {
    c_print_int(69);
    return 0;
}
)",
                 R"(
module main;

@extern(link_name="print_int")
func c_print_int(v: i32);

@extern(link_name="print_str")
func c_print_str(s: [*]const u8, len: i32);

@extern(link_name="print_cstr")
func c_print_cstr(s: [*]const u8);

@extern(link_name="getchar")
func c_getchar() i32;
)",
             });

    run_test(ctx, p, "print_int in inner function very simple",
             std::vector<std::string>{
                 R"(
module main;

func print_answer() {
    c_print_int(42);
}

@export
func main() i32 {
    print_answer();
    return 0;
}
)",
                 R"(
module main;

@extern(link_name="print_int")
func c_print_int(v: i32);

@extern(link_name="print_str")
func c_print_str(s: [*]const u8, len: i32);

@extern(link_name="print_cstr")
func c_print_cstr(s: [*]const u8);

@extern(link_name="getchar")
func c_getchar() i32;
)",
             });

    ctx.tags.pop_back();

    fmt::println("ir tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
