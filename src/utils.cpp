#include "utils.hpp"

#include <cstdio>
#include <memory>

namespace yal {

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

auto write_file(std::string const& path, std::string_view contents) -> bool {
    std::unique_ptr<FILE, void (*)(FILE*)> f = {fopen(path.c_str(), "wb"),
                                                [](auto f) { fclose(f); }};
    if (!f) return false;

    if (fwrite(contents.data(), sizeof(std::string::value_type),
               contents.size(), f.get()) != contents.size())
        return false;

    return true;
}

}  // namespace yal
