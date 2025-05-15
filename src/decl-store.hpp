#pragma once

#include <cstdint>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "arena.hpp"
#include "ast-node.hpp"
#include "value.hpp"

namespace yal {

struct DeclFlags {
    enum Flags {
        None = 0,
        Extern = 1 << 0,
        Private = 1 << 1,
        PrivateFile = 1 << 2,
        Export = 1 << 3,

        // this flag is set when a local variable has it's address taken and
        // needs to be allocated on the stack
        StackVar = 1 << 4,
    };

    [[nodiscard]] constexpr auto has_extern() const -> bool {
        return flags & Extern;
    }

    [[nodiscard]] constexpr auto has_private() const -> bool {
        return flags & Private;
    }

    [[nodiscard]] constexpr auto has_private_file() const -> bool {
        return flags & PrivateFile;
    }

    [[nodiscard]] constexpr auto has_export() const -> bool {
        return flags & Export;
    }

    [[nodiscard]] constexpr auto has_stack_var() const -> bool {
        return flags & StackVar;
    }

    constexpr void set_extern() { flags = static_cast<Flags>(flags | Extern); }
    constexpr void set_export() { flags = static_cast<Flags>(flags | Export); }
    constexpr void set_stack_var() {
        flags = static_cast<Flags>(flags | StackVar);
    }

    struct Builder {
        [[nodiscard]] constexpr auto set_extern() const -> Builder {
            return {static_cast<Flags>(flags | Extern)};
        }

        [[nodiscard]] constexpr auto set_private() const -> Builder {
            return {static_cast<Flags>(flags | Private)};
        }

        [[nodiscard]] constexpr auto set_private_file() const -> Builder {
            return {static_cast<Flags>(flags | PrivateFile)};
        }

        [[nodiscard]] constexpr auto set_export() const -> Builder {
            return {static_cast<Flags>(flags | Export)};
        }

        [[nodiscard]] constexpr auto set_stack_var() const -> Builder {
            return {static_cast<Flags>(flags | StackVar)};
        }

        [[nodiscard]] constexpr auto build() const -> DeclFlags {
            return {flags};
        }

        Flags flags = None;
    };

    static constexpr auto builder() -> Builder { return {}; }

    Flags flags = None;
};

struct Decl {
public:
    uint32_t uid;

    /// This is the name that the declaration has globally. This prepends the
    /// names of all scopes above it an joins them with '.'.
    std::string_view full_name;

    /// This is the name that the declaration has locally. This is what is used
    /// by lexical scoping.
    std::string_view local_name;

    std::string_view link_name;

    /// The node that declared this. It may be an invalid id if the value is a
    /// buitin or something that does not come from the AST.
    ast::Node const* node;

    /// Flags about the declaration. These further describe properties of the
    /// declaration, like if it is external, private, private(file), etc...
    DeclFlags flags;

    /// The value of the declaration. In case the actual value is not compile
    /// time know, then this holds an empty value.
    Value value;

    [[nodiscard]] constexpr auto is_private_file() const -> bool {
        return flags.has_private_file();
    }

    [[nodiscard]] constexpr auto is_stack_var() const -> bool {
        return flags.has_stack_var();
    }

    [[nodiscard]] constexpr auto get_type() const -> types::Type* {
        return value.type;
    }

private:
};

class DeclStore {
    struct DeclItem {
        DeclItem* next;
        Decl      decl;
    };

public:
    // https://www.studyplan.dev/pro-cpp/iterator-concepts
    struct Iterator {
        using iterator_category = std::forward_iterator_tag;
        using value_type = Decl;
        using element_type = value_type;
        using pointer = value_type*;
        using reference = value_type&;
        using difference_type = std::ptrdiff_t;

        constexpr Iterator() = default;
        constexpr Iterator(DeclItem* it) : item{it} {}

        constexpr auto operator*() const -> reference { return item->decl; }
        constexpr auto operator->() const -> pointer { return &operator*(); }

        constexpr auto operator++() -> Iterator& {
            item = item->next;
            return *this;
        }

        constexpr auto operator++(int) -> Iterator {
            Iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        constexpr auto operator==(Iterator const& b) const -> bool {
            return item == b.item;
        }

        DeclItem* item{};
    };

    auto new_decl(std::string_view full_name, std::string_view local_name,
                  std::string_view link_name, ast::Node* declarator,
                  DeclFlags flags, Value value) -> Decl*;

    [[nodiscard]] auto begin() const -> Iterator { return {head}; }
    [[nodiscard]] auto end() const -> Iterator { return {}; }

private:
    auto new_string(std::string_view base) -> std::string_view {
        return names.alloc_string_view(base);
    }

private:
    DeclItem* head{};

    uint32_t next_uid{};

    mem::Arena decls;
    mem::Arena names;
};

static_assert(std::forward_iterator<DeclStore::Iterator>);
static_assert(std::ranges::forward_range<DeclStore>);

void to_json(json& j, DeclFlags const& n);
void to_json(json& j, Decl const& n);
void to_json(json& j, DeclStore const& n);

}  // namespace yal

template <>
struct fmt::formatter<yal::Decl> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::Decl const& d, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::DeclFlags> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::DeclFlags flags, format_context& ctx) const
        -> format_context::iterator;
};
