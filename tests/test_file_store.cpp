#include <filesystem>
#include <libassert/assert.hpp>
#include <string_view>

#include "file_store.hpp"
#include "tests.hpp"

namespace yal::tests {
// NOLINTBEGIN(readability-function-cognitive-complexity)

auto file_store() -> ut::Test {
    auto tb = ut::new_test("file store");

    tb.add_test("create, get and check file with contents", [](auto) {
        auto fs = yal::FileStore{};

        std::string_view filepath = ":memory:";
        std::string_view content = "some content";

        auto id = fs.add_file_and_contents(filepath, content);
        ASSERT(id.is_valid(), "invalid file");

        auto f = fs.get_file_by_id(id);
        ASSERT(f.has_value());

        ASSERT(f->id == id);
        ASSERT(f->original_path == filepath);
        ASSERT(f->contents == content);

        auto ex_full_path = std::filesystem::absolute(filepath);
        ASSERT(f->full_path == ex_full_path.string());

        return true;
    });

    tb.add_test("add file and contents twice", [](auto) {
        auto fs = yal::FileStore{};

        std::string_view filepath = ":memory:";
        std::string_view content = "some content";

        auto id = fs.add_file_and_contents(filepath, content);
        ASSERT(id.is_valid(), "invalid file");

        auto f = fs.get_file_by_id(id);
        ASSERT(f.has_value());

        ASSERT(f->id == id);
        ASSERT(f->original_path == filepath);
        ASSERT(f->contents == content);

        auto ex_full_path = std::filesystem::absolute(filepath);
        ASSERT(f->full_path == ex_full_path.string());

        // add again
        auto new_id = fs.add_file_and_contents(filepath, content);
        ASSERT(id == new_id);

        f = fs.get_file_by_id(id);
        ASSERT(f.has_value());

        ASSERT(f->id == id);
        ASSERT(f->original_path == filepath);
        ASSERT(f->contents == content);

        return true;
    });

    tb.add_test("add file twice", [](auto) {
        auto fs = yal::FileStore{};

        std::string_view filepath = ":memory:";
        std::string_view content = "some content";

        auto id = fs.add_file_and_contents(filepath, content);
        ASSERT(id.is_valid(), "invalid file");

        auto f = fs.get_file_by_id(id);
        ASSERT(f.has_value());

        ASSERT(f->id == id);
        ASSERT(f->original_path == filepath);
        ASSERT(f->contents == content);

        auto ex_full_path = std::filesystem::absolute(filepath);
        ASSERT(f->full_path == ex_full_path.string());

        // add again
        auto new_id = fs.add_file(filepath);
        ASSERT(id == new_id);

        f = fs.get_file_by_id(id);
        ASSERT(f.has_value());

        ASSERT(f->id == id);
        ASSERT(f->original_path == filepath);
        ASSERT(f->contents == content);

        return true;
    });

    tb.add_test("add file from filesystem", [](auto) {
        auto fs = yal::FileStore{};
        auto test_dir = std::filesystem::path{"tests"} / "test_data";

        auto a_path = test_dir / "a.yal";
        auto a_full_path = std::filesystem::absolute(a_path);
        auto a_id = fs.add_file(a_path.string());
        ASSERT(a_id.is_valid());

        auto b_path = test_dir / "b.yal";
        auto b_full_path = std::filesystem::absolute(b_path);
        auto b_id = fs.add_file(b_path.string());
        ASSERT(b_id.is_valid());

        auto c_path = test_dir / "c.yal";
        auto c_full_path = std::filesystem::absolute(c_path);
        auto c_id = fs.add_file(c_path.string());
        ASSERT(c_id.is_valid());

        {
            auto f = fs.get_file_by_id(a_id);
            ASSERT(f.has_value());

            ASSERT(f->id == a_id);
            ASSERT(f->original_path == a_path.string());
            ASSERT(f->contents == R"~~~(// a.yal

module main;

// vim: ft=yal
)~~~");

            ASSERT(f->full_path == a_full_path);
        }

        {
            auto f = fs.get_file_by_id(b_id);
            ASSERT(f.has_value());

            ASSERT(f->id == b_id);
            ASSERT(f->original_path == b_path.string());
            ASSERT(f->contents == R"~~~(// b.yal

module main;

// vim: ft=yal
)~~~");

            ASSERT(f->full_path == b_full_path);
        }

        {
            auto f = fs.get_file_by_id(c_id);
            ASSERT(f.has_value());

            ASSERT(f->id == c_id);
            ASSERT(f->original_path == c_path.string());
            ASSERT(f->contents == R"~~~(// c.yal

module main;

// vim: ft=yal
)~~~");

            ASSERT(f->full_path == c_full_path);
        }

        return true;
    });

    tb.add_test("add directory with contents", [](auto) {
        auto fs = yal::FileStore{};

        std::string_view filepath = ":memory:";
        std::string_view content = "some content";

        auto id = fs.add_file_and_contents(filepath, content);
        ASSERT(id.is_valid(), "invalid file");

        auto dirid = fs.add_dir_and_contents("test_data", std::array{id});
        ASSERT(dirid.is_valid(), "invalid file");

        auto d = fs.get_dir_by_id(dirid);
        ASSERT(d.has_value());

        ASSERT(d->id == dirid);
        ASSERT(d->original_path == "test_data");
        ASSERT(d->files.size() == 1);
        ASSERT(d->files[0] == id);

        auto ex_full_path = std::filesystem::absolute("test_data");
        ASSERT(d->full_path == ex_full_path);

        auto f = fs.get_file_by_id(d->files[0]);
        ASSERT(f->id == id);
        ASSERT(f->original_path == filepath);
        ASSERT(f->contents == content);

        ex_full_path = std::filesystem::absolute(filepath);
        ASSERT(f->full_path == ex_full_path.string());

        return true;
    });

    tb.add_test("add file from dir in filesystem", [](auto) {
        auto fs = yal::FileStore{};
        auto test_dir = std::filesystem::path{"tests"} / "test_data";

        auto a_path = test_dir / "a.yal";
        auto a_full_path = std::filesystem::absolute(a_path);

        auto b_path = test_dir / "b.yal";
        auto b_full_path = std::filesystem::absolute(b_path);

        auto c_path = test_dir / "c.yal";
        auto c_full_path = std::filesystem::absolute(c_path);

        auto dirid = fs.add_dir(test_dir.string());
        ASSERT(dirid.is_valid(), "invalid file");

        auto d = fs.get_dir_by_id(dirid);
        ASSERT(d.has_value());

        ASSERT(d->id == dirid);
        ASSERT(d->original_path == test_dir.string());
        ASSERT(d->files.size() == 3);

        auto ex_full_path = std::filesystem::absolute(test_dir);
        ASSERT(d->full_path == ex_full_path);

        // NOTE: fs order may vary
        auto a_id = d->files[1];
        auto b_id = d->files[0];
        auto c_id = d->files[2];

        {
            auto f = fs.get_file_by_id(a_id);
            ASSERT(f.has_value());

            ASSERT(f->id == a_id);
            ASSERT(f->original_path == a_path.string());
            ASSERT(f->contents == R"~~~(// a.yal

module main;

// vim: ft=yal
)~~~");

            ASSERT(f->full_path == a_full_path);
        }

        {
            auto f = fs.get_file_by_id(b_id);
            ASSERT(f.has_value());

            ASSERT(f->id == b_id);
            ASSERT(f->original_path == b_path.string());
            ASSERT(f->contents == R"~~~(// b.yal

module main;

// vim: ft=yal
)~~~");

            ASSERT(f->full_path == b_full_path);
        }

        {
            auto f = fs.get_file_by_id(c_id);
            ASSERT(f.has_value());

            ASSERT(f->id == c_id);
            ASSERT(f->original_path == c_path.string());
            ASSERT(f->contents == R"~~~(// c.yal

module main;

// vim: ft=yal
)~~~");

            ASSERT(f->full_path == c_full_path);
        }

        return true;
    });

    return tb;
}

// NOLINTEND(readability-function-cognitive-complexity)
}  // namespace yal::tests
