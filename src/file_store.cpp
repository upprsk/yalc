#include "file_store.hpp"

#include <algorithm>
#include <filesystem>
#include <nlohmann/json.hpp>
#include <string_view>

#include "utils.hpp"

namespace fs = std::filesystem;

namespace yal {

auto to_absolute_path(fs::path const &path) -> fs::path {
    return fs::absolute(path);
}

auto FileStore::add_file(std::string_view path) -> FileId {
    auto full_path = to_absolute_path(path).string();

    auto id = find_file_by_path(full_path);
    if (id.is_valid()) return id;

    auto contents = read_entire_file(full_path);
    if (!contents) return {};

    return add_file_and_contents_nocheck(path, full_path, *contents);
}

auto FileStore::add_file_and_contents(std::string_view path,
                                      std::string_view contents) -> FileId {
    auto full_path = to_absolute_path(path);
    return add_file_and_contents_full(path, full_path.string(), contents);
}

auto FileStore::add_file_and_contents_full(std::string_view path,
                                           std::string_view full_path,
                                           std::string_view contents)
    -> FileId {
    auto id = find_file_by_path(full_path);
    if (id.is_valid()) return id;

    return add_file_and_contents_nocheck(path, full_path, contents);
}

auto FileStore::add_file_and_contents_nocheck(std::string_view path,
                                              std::string_view full_path,
                                              std::string_view contents)
    -> FileId {
    auto id = FileId::from_raw_data(files.size());

    auto f = File{
        .id = id,
        .original_path = small_arena.alloc_string_view(path),
        .full_path = small_arena.alloc_string_view(full_path),
        .contents = big_arena.alloc_string_view(contents),
    };

    files.push_back(f);

    return id;
}

auto FileStore::get_file_by_id(FileId id) const -> std::optional<File> {
    if (id.value() < files.size()) return files.at(id.value());
    return std::nullopt;
};

auto FileStore::find_file_by_path(std::string_view full_path) const -> FileId {
    auto it = std::ranges::find_if(
        files, [&](File const &f) { return f.full_path == full_path; });
    if (it == files.end()) return {};

    return it->id;
}

auto FileStore::read_entire_file(std::string const &path)
    -> std::optional<std::string_view> {
    std::unique_ptr<FILE, void (*)(FILE *)> f = {fopen(path.c_str(), "rb"),
                                                 [](auto f) { fclose(f); }};
    if (!f) return std::nullopt;

    fseek(f.get(), 0, SEEK_END);
    auto len = ftell(f.get());

    fseek(f.get(), 0, SEEK_SET);

    auto s = big_arena.alloc_size<std::string_view::value_type>(len);
    if (static_cast<decltype(len)>(fread(
            s.data(), sizeof(std::string_view::value_type), len, f.get())) !=
        len)
        return std::nullopt;

    return std::string_view{s.begin(), s.end()};
}

// ============================================================================

void to_json(nlohmann::json &j, FileId const &id) { j = id; }
void to_json(nlohmann::json &j, DirId const &id) { j = id; }

}  // namespace yal

auto fmt::formatter<yal::FileId>::format(yal ::FileId const &p,
                                         format_context     &ctx) const
    -> format_context ::iterator {
    return fmt::format_to(ctx.out(), "FileId({})", p.value());
}

auto fmt::formatter<yal::DirId>::format(yal ::DirId const &p,
                                        format_context    &ctx) const
    -> format_context ::iterator {
    return fmt::format_to(ctx.out(), "DirId({})", p.value());
}
