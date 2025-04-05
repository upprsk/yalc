#pragma once

#include <cstdint>
#include <cstdio>
#include <iterator>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "libassert/assert.hpp"
#include "nlohmann/json_fwd.hpp"
#include "span.hpp"

namespace yal {
using json = nlohmann::json;

/// A unique identifier for a file. This is used on imports and error reporting
/// as a lightweight way to find the correct path.
class FileId {
    static constexpr auto const INVALID_DATA = 0xFFFF'FFFF;

    constexpr explicit FileId(uint32_t data) : data{data} {}

public:
    // ------------
    // Constructors
    // ------------

    /// Default initialize to invalid.
    constexpr FileId() = default;
    constexpr FileId(FileId const &) = default;
    constexpr FileId(FileId &&) = default;
    constexpr auto operator=(FileId const &) -> FileId & = default;
    constexpr auto operator=(FileId &&) -> FileId & = default;

    // create a new invalid handle
    static constexpr auto invalid() -> FileId { return FileId{}; }

    // create a handle with the given value
    static constexpr auto from_raw_data(uint32_t raw_data) -> FileId {
        return FileId{raw_data};
    }

public:
    constexpr auto operator==(FileId const &o) const -> bool = default;

    // ------------

    /// Get the internal data. Make sure to use it correctly.
    [[nodiscard]] constexpr auto value() const -> uint32_t { return data; }

    [[nodiscard]] constexpr auto is_valid() const -> bool {
        return data != INVALID_DATA;
    }

private:
    uint32_t data{INVALID_DATA};
};

struct Location {
    FileId fileid;
    Span   span;

    [[nodiscard]] constexpr auto str(std::string_view source) const
        -> std::string_view {
        return span.str(source);
    }

    [[nodiscard]] constexpr auto extend(Location const &other) const
        -> Location {
        ASSERT(fileid == other.fileid);
        return {.fileid = fileid, .span = span.extend(other.span)};
    }

    [[nodiscard]] constexpr auto extend(Span const &other) const -> Location {
        return {.fileid = fileid, .span = span.extend(other)};
    }

    constexpr auto operator==(Location const &o) const -> bool = default;
};

class FileStore {
public:
    struct File {
        std::unique_ptr<FILE, void (*)(FILE *)> handle;

        static constexpr auto invalid() -> File {
            return {
                .handle = {nullptr, nullptr}
            };
        }

        [[nodiscard]] constexpr auto is_valid() const -> bool {
            return handle != nullptr;
        }
    };

public:
    /// Add a new filename and it's contents to the store. In case the filename
    /// is already present, the existing id is returned. The contents of the
    /// file are read and saved internally. In case reading fails, returns an
    /// invalid FileId.
    [[nodiscard]] auto add(std::string path) -> FileId;

    /// Add a new filename and it's contents to the store. In case the filename
    /// is already present, the existing id is returned.
    [[nodiscard]] auto add(std::string path, std::string contents) -> FileId;

    /// Get the filename for a given id. In case the id is invalid, will panic.
    [[nodiscard]] constexpr auto get_filename(FileId id) const
        -> std::string const & {
        return filenames.at(id.value());
    }

    /// Get the file contents for a given id. In case the id is invalid, will
    /// panic.
    [[nodiscard]] constexpr auto get_contents(FileId id) const
        -> std::string const & {
        return file_contents.at(id.value());
    }

    /// Open the file for the given FileId. In case the file could not be
    /// opened, returns an invalid File. In case the FileId is invalid, panics.
    [[nodiscard]] auto open(FileId id, char const *modes = "rb") const -> File;

    /// Open the file for the given FileId and read it into a string. In case
    /// the file could not be opened, returns nullopt. In case the FileId is
    /// invalid, panics.
    [[nodiscard]] auto read(FileId id) const -> std::optional<std::string>;

private:
    /// Use the reverse map to find the id for a given filename. Returns an
    /// invalid FileId in case the filename is not in the store.
    [[nodiscard]] auto find_id_for(std::string const &s) const -> FileId;

    /// Adds a new filename/data to the store and get it's id.
    [[nodiscard]] auto new_id(std::string filename, std::string filedata)
        -> FileId;

private:
    // TODO: use a more efficient way of storing strings. An arena for example,
    // as nothing will be removed from here
    std::vector<std::string> filenames;

    // The contents of each file are also stored here. In case we need it, there
    /// The contents of each file are also stored here. In case we need it, there
    /// could be some off-loading to disk to use less memory.
    ///
    /// > Even big codebases only don't go into more than some MB in size (it's
    /// just text). That is very little next to the AST for the exact same code.
    /// As such this is FINE.
    std::vector<std::string> file_contents;

    // TODO: use a better hashmap implementation
    std::unordered_map<std::string, FileId> filename_to_id;
};

void to_json(json &j, FileId const &n);
void from_json(json const &j, FileId &n);
void to_json(json &j, Location const &n);
void from_json(json const &j, Location &n);

}  // namespace yal

template <>
struct fmt::formatter<yal::FileId> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::FileId fileid, format_context &ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::Location> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::Location loc, format_context &ctx) const
        -> format_context::iterator;
};
