#include "arena.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <new>

#include "libassert/assert.hpp"

namespace yal::mem {

Arena::~Arena() {
    for (auto it = head; it != nullptr;) {
        auto blk = it;
        it = it->next;
        free(blk);
    }

    head = nullptr;
}

auto Arena::mem_alloc(std::size_t sz) -> void* {
    ASSERT(sz > 0);

    auto blk = get_block_with_at_least(sz);
    auto ptr = blk->forward(sz);
    ASSERT(ptr != nullptr);

    return ptr;
}

auto Arena::get_block_with_at_least(std::size_t sz) -> Block* {
    if (sz > BLOCK_SIZE) throw std::bad_alloc{};

    auto blk = current_block();
    if (blk->fits(sz)) return blk;

    return new_block();
}

auto Arena::current_block() -> Block* {
    if (head != nullptr) return head;

    head = new_block();
    return head;
}

auto Arena::new_block() -> Block* {
    auto mem = malloc(BLOCK_SIZE);
    auto bytes = static_cast<uint8_t*>(mem);
    auto blk = static_cast<Block*>(mem);

    *blk = {.next = head, .end = bytes + BLOCK_SIZE, .head = blk->data};
    head = blk;
    return head;
}

auto Arena::alloc_string(std::string_view s) -> std::span<char> {
    if (s.size() == 0) return {};

    auto      mem = static_cast<char*>(mem_alloc(s.size()));
    std::span dst{mem, mem + s.size()};
    std::ranges::copy(s, dst.begin());

    return dst;
}

}  // namespace yal::mem
