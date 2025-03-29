#include "file-store.hpp"

#include <cstdio>

#include "nlohmann/json.hpp"
#include "utils.hpp"

namespace yal {

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

auto FileStore::find_id_for(std::string const &s) const -> FileId {
    if (auto it = filename_to_id.find(s); it != end(filename_to_id)) {
        return it->second;
    }

    return FileId::invalid();
}

auto FileStore::new_id(std::string filename, std::string filedata) -> FileId {
    auto sz = filenames.size();
    auto id = FileId::from_raw_data(sz);

    filenames.push_back(filename);
    file_contents.push_back(filedata);
    filename_to_id[filename] = id;
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
