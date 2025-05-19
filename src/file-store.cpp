#include "file-store.hpp"

#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <ranges>

#include "fmt/ranges.h"
#include "nlohmann/json.hpp"
#include "utils.hpp"

namespace yal {

namespace rv = std::ranges::views;
namespace fs = std::filesystem;

static constexpr inline auto YAL_EXTENSION = ".yal";

/// Scan the directory containing the given path, returning a list of all source
/// files contained in it.
auto scan_dir_of_file(std::filesystem::path const &filepath)
    -> std::vector<std::filesystem::path> {
    // if relative paths are in use, we want to preserve the use of relative
    // paths. So, record if the given file has a relative path.
    auto is_relative = filepath.is_relative();

    return fs::directory_iterator{fs::absolute(filepath).parent_path()} |
           rv::filter([](auto it) { return it.is_regular_file(); }) |
           rv::transform([&](auto it) -> fs::path {
               return is_relative ? fs::relative(it) : it;
           }) |
           rv::filter([](auto it) {
               return it.filename().string().ends_with(YAL_EXTENSION);
           }) |
           std::ranges::to<std::vector>();
}

// ----------------------------------------------------------------------------

auto FileStore::add(std::string path) -> FileId {
    // dedup
    if (auto maybe_id = find_id_for(path); maybe_id.is_valid()) {
        return maybe_id;
    }

    auto contents = read_entire_file(path);
    if (!contents) return FileId::invalid();

    return new_id(path, *contents);
}

auto FileStore::add(std::string path, std::string contents) -> FileId {
    // dedup
    if (auto maybe_id = find_id_for(path); maybe_id.is_valid()) {
        return maybe_id;
    }

    return new_id(path, contents);
}

auto FileStore::open(FileId id, char const *modes) const -> File {
    auto const &filename = get_filename(id);

    auto f = fopen(filename.c_str(), modes);
    if (f == nullptr) return File::invalid();

    return {
        .handle = {f, [](FILE *f) { fclose(f); }}
    };
}

// NOTE: this is basically a dublicate of `read_entire_file`
auto FileStore::read(FileId id) const -> std::optional<std::string> {
    auto file = open(id);
    if (!file.is_valid()) return std::nullopt;

    auto f = file.handle.get();
    fseek(f, 0, SEEK_END);
    auto len = ftell(f);

    fseek(f, 0, SEEK_SET);

    std::string s;
    s.resize(len);
    if (static_cast<decltype(len)>(
            fread(s.data(), sizeof(std::string::value_type), len, f)) != len)
        return std::nullopt;

    return s;
}

auto FileStore::add_dir_for(FileId id) -> DirId {
    // Need a way to find the parent dir without doing a filesystem scan if the
    // needed dir is already in the store.
    for (uint32_t i = 0; auto const &files : dirs_to_files) {
        if (files.contains(id)) return DirId::from_raw_data(i);
        i++;
    }

    return add_dir_for(get_filename(id));
}

auto FileStore::add_dir_for(std::string path) -> DirId {
    auto files =
        scan_dir_of_file(path) |
        rv::transform([&](fs::path const &path) { return add(path); }) |
        std::ranges::to<std::unordered_set>();
    return add_dir(path, files);
}

auto FileStore::add_dir(std::string path, std::unordered_set<FileId> files)
    -> DirId {
    // dedup
    if (auto maybe_id = find_id_for_dir(path); maybe_id.is_valid()) {
        return maybe_id;
    }

    return new_dir_id(path, files);
}

auto FileStore::get_files_in_dir(DirId id) const -> std::vector<FileId> {
    auto const         &files = dirs_to_files.at(id.value());
    std::vector<FileId> result{files.begin(), files.end()};
    std::ranges::sort(result,
                      [](FileId l, FileId r) { return l.value() < r.value(); });

    return result;
}

auto FileStore::find_id_for(std::string const &s) const -> FileId {
    if (auto it = filename_to_id.find(s); it != end(filename_to_id)) {
        return it->second;
    }

    return FileId::invalid();
}

auto FileStore::find_id_for_dir(std::string const &s) const -> DirId {
    if (auto it = dirname_to_id.find(s); it != end(dirname_to_id)) {
        return it->second;
    }

    return DirId::invalid();
}

auto FileStore::new_id(std::string filename, std::string filedata) -> FileId {
    auto sz = filenames.size();
    auto id = FileId::from_raw_data(sz);

    filenames.push_back(filename);
    file_contents.push_back(filedata);
    filename_to_id[filename] = id;
    return id;
}

auto FileStore::new_dir_id(std::string                dirname,
                           std::unordered_set<FileId> children) -> DirId {
    auto sz = directories.size();
    auto id = DirId::from_raw_data(sz);

    directories.push_back(dirname);
    dirs_to_files.push_back(children);
    dirname_to_id[dirname] = id;
    return id;
}

void to_json(json &j, FileId const &n) { j = n.value(); }
void from_json(json const &j, FileId &n) {
    n = FileId::from_raw_data(j.get<uint32_t>());
}

void to_json(json &j, Location const &n) {
    j = json{
        {"fileid", n.fileid},
        {  "span",   n.span},
    };
}

void from_json(json const &j, Location &n) {
    j.at("fileid").get_to(n.fileid);
    j.at("span").get_to(n.span);
}

}  // namespace yal

auto fmt::formatter<yal::FileId>::format(yal::FileId     fileid,
                                         format_context &ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "FileId({})", fileid.value());
}

auto fmt::formatter<yal::Location>::format(yal::Location   loc,
                                           format_context &ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "Location({}, {})", loc.fileid, loc.span);
}
