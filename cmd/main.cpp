#include <memory>
#include <optional>

#include "cpptrace/from_current.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/ranges.h"
#include "parser.hpp"
#include "tokenizer.hpp"
#include "types.hpp"
#include "typing.hpp"

auto read_entire_file(std::string const& path) -> std::optional<std::string> {
    std::unique_ptr<FILE, void (*)(FILE*)> f = {fopen(path.c_str(), "rb"),
                                                [](auto f) { fclose(f); }};
    if (!f) return std::nullopt;

    fseek(f.get(), 0, SEEK_END);
    auto len = ftell(f.get());

    fseek(f.get(), 0, SEEK_SET);

    std::string s;
    s.resize(len);
    if (static_cast<decltype(len)>(fread(
            s.data(), sizeof(std::string::value_type), len, f.get())) != len)
        return std::nullopt;

    return s;
}

auto main(int argc, char** argv) -> int {
    CPPTRACE_TRY {
        if (argc < 2) {
            fmt::println(stderr, "usage: {} <program>", argv[0]);
            return 1;
        }

        auto contents = read_entire_file(argv[1]);
        if (!contents) {
            fmt::println(stderr, "failed to read file: {}", argv[1]);
            return 1;
        }

        auto er = yal::ErrorReporter{*contents, argv[1]};
        auto tokens = yal::tokenize(*contents, er);
        auto [ast, root] = yal::parse(tokens, *contents, er);

        fmt::println("{}", yal::FatNodeHandle{.ast = &ast, .node = root});

        auto ts = yal::TypeStore::new_store();
        yal::pass_add_types(root, ast, ts, er);

        return 0;
    }
    CPPTRACE_CATCH(std::exception const& ex) {
        fmt::println(stderr, "exception: {}", ex.what());
        cpptrace::from_current_exception().print();
    }
}
