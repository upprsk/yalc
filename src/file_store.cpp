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

auto FileStore::add_dir(std::string_view path) -> DirId {
    auto full_path = to_absolute_path(path);
    auto id = find_dir_by_path(full_path.string());
    if (id.is_valid()) return id;

    if (!fs::is_directory(full_path)) return {};  // not a directory

    auto root = fs::current_path();

    std::vector<FileId> files;
    for (auto const &item : fs::directory_iterator{full_path}) {
        if (!item.is_regular_file()) continue;

        if (item.path().filename().string().ends_with(".yal")) {
            auto id = add_file(fs::relative(item.path(), root).string());
            if (id.is_valid()) files.push_back(id);
        }
    }

    return add_dir_and_contents_nocheck(path, full_path.string(), files);
}

auto FileStore::add_dir_and_contents(std::string_view        path,
                                     std::span<FileId const> contents)
    -> DirId {
    auto full_path = to_absolute_path(path);
    return add_dir_and_contents_full(path, full_path.string(), contents);
}

auto FileStore::add_dir_and_contents_full(std::string_view        path,
                                          std::string_view        full_path,
                                          std::span<FileId const> contents)
    -> DirId {
    auto id = find_dir_by_path(full_path);
    if (id.is_valid()) return id;

    return add_dir_and_contents_nocheck(path, full_path, contents);
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

auto FileStore::get_dir_by_id(DirId id) const -> std::optional<Dir> {
    if (id.value() < dirs.size()) return dirs.at(id.value());
    return std::nullopt;
}

auto FileStore::find_dir_by_path(std::string_view full_path) const -> DirId {
    auto it = std::ranges::find_if(
        dirs, [&](Dir const &d) { return d.full_path == full_path; });
    if (it == dirs.end()) return {};

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

auto FileStore::add_dir_and_contents_nocheck(std::string_view        path,
                                             std::string_view        full_path,
                                             std::span<FileId const> contents)
    -> DirId {
    auto id = DirId::from_raw_data(dirs.size());
    auto d = Dir{
        .id = id,
        .original_path = small_arena.alloc_string_view(path),
        .full_path = small_arena.alloc_string_view(full_path),
        .files = small_arena.alloc<FileId>(contents),
    };

    dirs.push_back(d);

    return id;
}

// ============================================================================

void to_json(nlohmann::json &j, FileId const &id) { j = id.value(); }
void to_json(nlohmann::json &j, DirId const &id) { j = id.value(); }

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
