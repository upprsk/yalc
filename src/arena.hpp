#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <utility>

namespace yal::mem {

class Arena {
    static constexpr auto BLOCK_SIZE = (1 << 10) * 4;  // 4KiB

    struct Block {
        Block*   next{};
        uint8_t* end{};
        uint8_t* head{};
        uint8_t  data[];

        constexpr auto forward(std::size_t sz) -> void* {
            if (!fits(sz)) return nullptr;

            auto ptr = head;
            head += (sz + (sizeof(uintptr_t) - 1)) & -sizeof(uintptr_t);

            return ptr;
        }

        [[nodiscard]] constexpr auto fits(std::size_t sz) const -> bool {
            return sz <= available();
        }

        [[nodiscard]] constexpr auto used() const -> std::size_t {
            return head - data;
        }

        [[nodiscard]] constexpr auto available() const -> std::size_t {
            return end - head;
        }
    };

public:
    Arena() = default;

    constexpr Arena(Arena const& o) = delete;
    constexpr Arena(Arena&& o) : head{o.head} { o.head = nullptr; }

    constexpr auto operator=(Arena const&) -> Arena& = delete;
    constexpr auto operator=(Arena&& o) -> Arena& {
        head = o.head;
        o.head = nullptr;

        return *this;
    }

    ~Arena();

    template <typename T, typename... Args>
    [[nodiscard]] auto create(Args&&... args) -> T* {
        auto data = mem_alloc(sizeof(T));
        return new (data) T(std::forward<Args>(args)...);
    }

    template <typename T, typename U>
    [[nodiscard]] auto alloc(U&& from) -> std::span<T> {
        auto data =
            static_cast<T*>(mem_alloc(std::ranges::size(from) * sizeof(T)));
        std::span to{data, std::ranges::size(from)};

        std::ranges::copy(from, to.begin());
        return to;
    }

    template <typename T>
    [[nodiscard]] auto alloc_size(std::size_t sz) -> std::span<T> {
        auto      data = static_cast<T*>(mem_alloc(sz * sizeof(T)));
        std::span to{data, sz};
        return to;
    }

    [[nodiscard]] auto alloc_string(std::string_view s) -> std::span<char>;
    [[nodiscard]] auto alloc_string_view(std::string_view s)
        -> std::string_view {
        auto str = alloc_string(s);
        return {str.begin(), str.end()};
    }

    [[nodiscard]] auto mem_alloc(std::size_t sz) -> void*;

private:
    [[nodiscard]] auto get_block_with_at_least(std::size_t sz) -> Block*;
    [[nodiscard]] auto current_block() -> Block*;
    [[nodiscard]] auto new_block() -> Block*;

private:
    Block* head{};
};

}  // namespace yal::mem
