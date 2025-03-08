#include "sema.hpp"

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>
#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <ranges>
#include <string>
#include <string_view>

#include "fmt/format.h"
#include "fmt/ranges.h"
#include "fmt/std.h"
#include "hlir.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"
#include "types.hpp"
#include "utils.hpp"

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

TEST_CASE("empty void func", "[sema]") {
    SKIP("implicit returns not implemented");

    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = "func f() {}";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());

    auto ts = TypeStore::new_store();
    auto m = sema(ast, ts, root, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(m.funcs.size() == 1);

    auto f = m.funcs.at(0);
    REQUIRE(f.name == "f");
    REQUIRE(f.type == ts.get_type_func({}, ts.get_type_void()));
    REQUIRE(f.locals.empty());
    REQUIRE(f.blocks.size() == 1);

    auto b = f.blocks.at(0);
    REQUIRE(b.consts.empty());
    REQUIRE(b.spans.size() == 1);
    REQUIRE(b.code.size() == 1);

    auto i = b.code.at(0);
    REQUIRE(i.kind == hlir::InstKind::Ret);
    REQUIRE(i.a == 0);
}

TEST_CASE("void func with just single return", "[sema]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = "func f() { return; }";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());

    auto ts = TypeStore::new_store();
    auto m = sema(ast, ts, root, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(m.funcs.size() == 1);

    auto f = m.funcs.at(0);
    REQUIRE(f.name == "f");
    REQUIRE(f.type == ts.get_type_func({}, ts.get_type_void()));
    REQUIRE(f.locals.empty());
    REQUIRE(f.blocks.size() == 1);

    auto b = f.blocks.at(0);
    REQUIRE(b.consts.empty());
    REQUIRE(b.spans.size() == 1);
    REQUIRE(b.code.size() == 1);

    auto i = b.code.at(0);
    REQUIRE(i.kind == hlir::InstKind::Ret);
    REQUIRE(i.a == 0);
}

TEST_CASE("func that adds two i32", "[sema]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("no parameters, just consts") {
        auto source = "func result() i32 { return 10 + 22; }";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse(tokens, source, er);

        REQUIRE_FALSE(er.had_error());

        auto ts = TypeStore::new_store();
        auto m = sema(ast, ts, root, er);

        REQUIRE_FALSE(er.had_error());
        REQUIRE(m.funcs.size() == 1);

        auto f = m.funcs.at(0);
        REQUIRE(f.name == "result");
        REQUIRE(f.type == ts.get_type_func({}, ts.get_type_i32()));

        std::vector<hlir::Local> expected_locals{};

        REQUIRE_THAT(f.locals, Equals(expected_locals));

        REQUIRE(f.blocks.size() == 1);

        auto b = f.blocks.at(0);

        std::vector<hlir::Value> expected_consts{
            {ts.get_type_i32(), uint64_t{10}},
            {ts.get_type_i32(), uint64_t{22}},
        };

        std::vector<hlir::Inst> expected_code{
            {hlir::InstKind::Const, 0},
            {hlir::InstKind::Const, 1},
            {  hlir::InstKind::Add, 0},
            {  hlir::InstKind::Ret, 0},
        };

        std::vector<Span> expected_spans{
            {27, 29},
            {32, 34},
            {27, 34},
            {20, 35},
        };

        REQUIRE_THAT(b.consts, Equals(expected_consts));
        REQUIRE_THAT(b.code, Equals(expected_code));
        REQUIRE_THAT(b.spans, Equals(expected_spans));
    }

    SECTION("one i32 parameter") {
        auto source = "func next(x: i32) i32 { return x + 1; }";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse(tokens, source, er);

        REQUIRE_FALSE(er.had_error());

        auto ts = TypeStore::new_store();
        auto m = sema(ast, ts, root, er);

        REQUIRE_FALSE(er.had_error());
        REQUIRE(m.funcs.size() == 1);

        auto f = m.funcs.at(0);
        REQUIRE(f.name == "next");
        REQUIRE(f.type == ts.get_type_func(std::array{ts.get_type_i32()},
                                           ts.get_type_i32()));

        std::vector<hlir::Local> expected_locals{
            {"x", ts.get_type_i32(), 0},
        };

        REQUIRE_THAT(f.locals, Equals(expected_locals));

        REQUIRE(f.blocks.size() == 1);

        auto b = f.blocks.at(0);

        std::vector<hlir::Value> expected_consts{
            {ts.get_type_i32(), uint64_t{1}},
        };

        std::vector<hlir::Inst> expected_code{
            {hlir::InstKind::LoadLocal, 0},
            {    hlir::InstKind::Const, 0},
            {      hlir::InstKind::Add, 0},
            {      hlir::InstKind::Ret, 0},
        };

        std::vector<Span> expected_spans{
            {31, 32},
            {35, 36},
            {31, 36},
            {24, 37},
        };

        REQUIRE_THAT(b.consts, Equals(expected_consts));
        REQUIRE_THAT(b.code, Equals(expected_code));
        REQUIRE_THAT(b.spans, Equals(expected_spans));
    }

    SECTION("two i32 parameters") {
        auto source = "func add(x: i32, y: i32) i32 { return x + y; }";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse(tokens, source, er);

        REQUIRE_FALSE(er.had_error());

        auto ts = TypeStore::new_store();
        auto m = sema(ast, ts, root, er);

        REQUIRE_FALSE(er.had_error());
        REQUIRE(m.funcs.size() == 1);

        auto f = m.funcs.at(0);
        REQUIRE(f.name == "add");
        REQUIRE(f.type == ts.get_type_func(
                              std::array{ts.get_type_i32(), ts.get_type_i32()},
                              ts.get_type_i32()));

        std::vector<hlir::Local> expected_locals{
            {"x", ts.get_type_i32(), 0},
            {"y", ts.get_type_i32(), 1},
        };

        REQUIRE_THAT(f.locals, Equals(expected_locals));

        REQUIRE(f.blocks.size() == 1);

        auto b = f.blocks.at(0);

        std::vector<hlir::Value> expected_consts{};

        std::vector<hlir::Inst> expected_code{
            {hlir::InstKind::LoadLocal, 0},
            {hlir::InstKind::LoadLocal, 1},
            {      hlir::InstKind::Add, 0},
            {      hlir::InstKind::Ret, 0},
        };

        std::vector<Span> expected_spans{
            {38, 39},
            {42, 43},
            {38, 43},
            {31, 44},
        };

        REQUIRE_THAT(b.consts, Equals(expected_consts));
        REQUIRE_THAT(b.code, Equals(expected_code));
        REQUIRE_THAT(b.spans, Equals(expected_spans));
    }
}

TEST_CASE("function with ifs", "[sema]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = R"~~(func zeroer(x: i32) i32 {
    if x == 0 { return x; }

    return 0;
})~~";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());

    auto ts = TypeStore::new_store();
    auto m = sema(ast, ts, root, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(m.funcs.size() == 1);

    auto f = m.funcs.at(0);
    REQUIRE(f.name == "zeroer");
    REQUIRE(f.type ==
            ts.get_type_func(std::array{ts.get_type_i32()}, ts.get_type_i32()));

    std::vector<hlir::Local> expected_locals{
        {"x", ts.get_type_i32(), 0},
    };

    REQUIRE_THAT(f.locals, Equals(expected_locals));

    REQUIRE(f.blocks.size() == 3);

    {
        auto b = f.blocks.at(0);

        std::vector<hlir::Value> expected_consts{
            {ts.get_type_i32(), uint64_t{0}},
        };

        std::vector<hlir::Inst> expected_code{
            {hlir::InstKind::LoadLocal, 0},
            {hlir::InstKind::Const, 0},
            {hlir::InstKind::Eq, 0},
            {hlir::InstKind::Branch, 1, 2},
        };

        std::vector<Span> expected_spans{
            {33, 34},
            {38, 39},
            {33, 39},
            {30, 53},
        };

        REQUIRE_THAT(b.consts, Equals(expected_consts));
        REQUIRE_THAT(b.code, Equals(expected_code));
        REQUIRE_THAT(b.spans, Equals(expected_spans));
    }

    {
        auto b = f.blocks.at(1);

        std::vector<hlir::Value> expected_consts{};

        std::vector<hlir::Inst> expected_code{
            {hlir::InstKind::LoadLocal, 0},
            {      hlir::InstKind::Ret, 0},
            {     hlir::InstKind::Jump, 2},
        };

        std::vector<Span> expected_spans{
            {49, 50},
            {42, 51},
            {30, 53},
        };

        REQUIRE_THAT(b.consts, Equals(expected_consts));
        REQUIRE_THAT(b.code, Equals(expected_code));
        REQUIRE_THAT(b.spans, Equals(expected_spans));
    }

    {
        auto b = f.blocks.at(2);

        std::vector<hlir::Value> expected_consts{
            {ts.get_type_i32(), uint64_t{0}},
        };

        std::vector<hlir::Inst> expected_code{
            {hlir::InstKind::Const, 0},
            {  hlir::InstKind::Ret, 0},
        };

        std::vector<Span> expected_spans{
            {66, 67},
            {59, 68},
        };

        REQUIRE_THAT(b.consts, Equals(expected_consts));
        REQUIRE_THAT(b.code, Equals(expected_code));
        REQUIRE_THAT(b.spans, Equals(expected_spans));
    }
}

auto find_root_folder(size_t limit = 3, std::filesystem::path dir =
                                            std::filesystem::current_path())
    -> std::filesystem::path {
    if (limit == 0) return {};

    for (auto const& f : std::filesystem::directory_iterator{dir}) {
        if (f.is_directory() && f.path().filename() == ".git")
            return f.path().parent_path();
    }

    return find_root_folder(limit - 1, dir.parent_path());
}

TEST_CASE("files") {
    auto root = find_root_folder();
    auto p = root.concat("/tests/cases");

    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    for (auto const& file : std::filesystem::directory_iterator{p}) {
        if (!file.is_regular_file()) continue;

        SECTION(file.path().string()) {
            auto contents = read_entire_file(file.path());
            REQUIRE(contents.has_value());

            std::string_view src = *contents;

            std::string_view skip_tag = "// SKIP:";
            if (src.starts_with(skip_tag)) {
                SKIP(src.substr(src.length(),
                                src.find('\n') - skip_tag.length()));
            }

            std::string_view output_tag = "// OUTPUT:\n";
            auto             it = src.find(output_tag);
            REQUIRE(it != std::string::npos);

            namespace sv = std::ranges::views;

            auto expected_output =
                src.substr(it + output_tag.length()) | sv::split('\n') |
                sv::transform([](auto it) { return std::string_view(it); }) |
                sv::filter([](auto it) { return it.starts_with("// "); }) |
                sv::transform([](auto it) { return it.substr(3); }) |
                sv::join_with('\n') | std::ranges::to<std::string>();

            auto er = ErrorReporter{*contents, file.path().string(), devnull};
            auto tokens = tokenize(src, er);
            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse(tokens, src, er);
            REQUIRE_FALSE(er.had_error());

            auto ts = TypeStore::new_store();
            auto m = sema(ast, ts, root, er);

            REQUIRE_FALSE(er.had_error());

            char*  buf{};
            size_t sizeloc{};
            auto   memstream = open_memstream(&buf, &sizeloc);
            std::unique_ptr<FILE, void (*)(FILE*)> _{memstream,
                                                     [](auto f) { fclose(f); }};

            std::unique_ptr<char, void (*)(char*)> __{buf,
                                                      [](auto b) { free(b); }};

            for (auto const& f : m.funcs) {
                f.disasm(memstream, ts);
            }

            // close memstream
            _ = nullptr;

            std::string_view ms{buf, sizeloc};
            REQUIRE(ms == expected_output);
        }
    }
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-size)
// NOLINTEND(readability-function-cognitive-complexity)
