#pragma once

#include <fmt/base.h>

#include <cstdint>
#include <nlohmann/json_fwd.hpp>
#include <span>
#include <string>
#include <string_view>

#include "arena.hpp"
#include "macros.hpp"

namespace yal {

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

    [[nodiscard]] constexpr auto is_invalid() const -> bool {
        return !is_valid();
    }

private:
    uint32_t data{INVALID_DATA};
};

/// A unique identifier for a directory.
class DirId {
    static constexpr auto const INVALID_DATA = 0xFFFF'FFFF;

    constexpr explicit DirId(uint32_t data) : data{data} {}

public:
    // ------------
    // Constructors
    // ------------

    /// Default initialize to invalid.
    constexpr DirId() = default;
    constexpr DirId(DirId const &) = default;
    constexpr DirId(DirId &&) = default;
    constexpr auto operator=(DirId const &) -> DirId & = default;
    constexpr auto operator=(DirId &&) -> DirId & = default;

    // create a handle with the given value
    static constexpr auto from_raw_data(uint32_t raw_data) -> DirId {
        return DirId{raw_data};
    }

public:
    constexpr auto operator==(DirId const &o) const -> bool = default;

    // ------------

    /// Get the internal data. Make sure to use it correctly.
    [[nodiscard]] constexpr auto value() const -> uint32_t { return data; }

    [[nodiscard]] constexpr auto is_valid() const -> bool {
        return data != INVALID_DATA;
    }

    [[nodiscard]] constexpr auto is_invalid() const -> bool {
        return !is_valid();
    }

private:
    uint32_t data{INVALID_DATA};
};

class FileStore {
public:
    struct File {
        FileId           id;
        std::string_view original_path;
        std::string_view full_path;
        std::string_view contents;
    };

    struct Dir {
        DirId             id;
        std::string_view  original_path;
        std::string_view  full_path;
        std::span<FileId> files;
    };

    constexpr FileStore() = default;

    // add a file to the store. Its contents are read from the filesystem. In
    // case the read fails, an invalid `FileId` is returned. In case the file
    // has already been added to the store, it is returned directly instead.
    [[nodiscard]] auto add_file(std::string_view path) -> FileId;

    // add a file and its contents to the store. In case the file has already
    // been added to the store, it is returned directly instead.
    [[nodiscard]] auto add_file_and_contents(std::string_view path,
                                             std::string_view contents)
        -> FileId;

    // add a file and its contents to the store. In case the file has already
    // been added to the store, it is returned directly instead.
    [[nodiscard]] auto add_file_and_contents_full(std::string_view path,
                                                  std::string_view full_path,
                                                  std::string_view contents)
        -> FileId;

    // ------------------------------------------------------------------------

    // add a directory to the store. Its contents are scanned from the
    // filesystem, adding each `.yal` file to it. In case it fails, an invalid
    // `DirId` is returned. Note that failing to read files while scanning will
    // not be reported here.  In case the directory has already been added to
    // the store, it is returned instead.
    [[nodiscard]] auto add_dir(std::string_view path) -> DirId;

    // add a directory and its contents to the store. In case the directory has
    // already been added to the store, it is returned instead.
    [[nodiscard]] auto add_dir_and_contents(std::string_view        path,
                                            std::span<FileId const> contents)
        -> DirId;

    // add a directory and its contents (files that interest us) to the store.
    // In case the file has already been added to the store, it is returned
    // directly instead.
    [[nodiscard]] auto add_dir_and_contents_full(
        std::string_view path, std::string_view full_path,
        std::span<FileId const> contents) -> DirId;

    // ------------------------------------------------------------------------

    // get a file by id
    [[nodiscard]] auto get_file_by_id(FileId id) const -> std::optional<File>;

    // find a file in the store given the full path.
    [[nodiscard]] auto find_file_by_path(std::string_view full_path) const
        -> FileId;

    // get a directory by id
    [[nodiscard]] auto get_dir_by_id(DirId id) const -> std::optional<Dir>;

    // find a directory in the store given the full path.
    [[nodiscard]] auto find_dir_by_path(std::string_view full_path) const
        -> DirId;

private:
    // read the contents of a file into the `big_arena`.
    [[nodiscard]] auto read_entire_file(std::string const &full_path)
        -> std::optional<std::string_view>;

    // add a file to the store without checking that id is present
    [[nodiscard]] auto add_file_and_contents_nocheck(std::string_view path,
                                                     std::string_view full_path,
                                                     std::string_view contents)
        -> FileId;

    // add a directory to the store without checking that id is present
    [[nodiscard]] auto add_dir_and_contents_nocheck(
        std::string_view path, std::string_view full_path,
        std::span<FileId const> contents) -> DirId;

private:
    // list of all files, indexable by `FileId`
    std::vector<File> files;

    // list of all directories, indexable by `DirId`
    std::vector<Dir> dirs;

    // This arena stores is meant to store small data, like the filepaths.
    mem::Arena small_arena;
    // This arena stores is meant to store large data, like the contents of
    // files.
    mem::Arena big_arena;
};

// ============================================================================

void to_json(nlohmann::json &j, FileId const &id);
void to_json(nlohmann::json &j, DirId const &id);

}  // namespace yal

// ============================================================================

define_formatter_from_string_view(yal::FileId);
define_formatter_from_string_view(yal::DirId);
