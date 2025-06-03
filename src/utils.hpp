#pragma once

#include <optional>
#include <string>

namespace yal {

struct ArgIterator {
    int    argc;
    char** argv;

    constexpr auto next(std::string_view& arg) -> bool {
        if (argc == 0) return false;

        argc--;
        arg = std::string_view{*argv++};
        return true;
    }
};

auto read_entire_file(std::string const& path) -> std::optional<std::string>;
auto write_file(std::string const& path, std::string_view contents) -> bool;

struct MemStream {
    MemStream() : f{open_memstream(&buf, &bufsize)} {}
    ~MemStream() {
        fclose(f);
        f = nullptr;

        free(buf);
        buf = nullptr;
        bufsize = 0;
    }

    MemStream(MemStream const&) = delete;
    MemStream(MemStream&&) = default;
    auto operator=(MemStream const&) -> MemStream& = delete;
    auto operator=(MemStream&&) -> MemStream& = default;

    void flush() const { fflush(f); }

    [[nodiscard]] auto flush_str() const -> std::string_view {
        flush();
        return str();
    }

    [[nodiscard]] constexpr auto str() const -> std::string_view {
        return {buf, bufsize};
    }

    FILE* f{};

    char*  buf{};
    size_t bufsize{};
};

}  // namespace yal
