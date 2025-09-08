#include "ast_sort.hpp"

#include <fmt/ranges.h>

#include <deque>
#include <ranges>
#include <string_view>

#include "error_reporter.hpp"
#include "node.hpp"
#include "symbol.hpp"

namespace yal::ast::sort {

struct GlobalSymbol {
    std::string_view name;

    Decl*         decl;
    GlobalSymbol* next;

    int                        in_degree = 0;
    std::vector<GlobalSymbol*> required_by;
};

struct GlobalScope {
    ankerl::unordered_dense::segmented_map<std::string_view, GlobalSymbol*>
        items;

    [[nodiscard]] auto lookup(std::string_view key) const -> GlobalSymbol* {
        auto it = items.find(key);
        if (it == items.end()) return nullptr;

        return it->second;
    }
};

struct LocalScope {
    LocalScope*                                              parent = nullptr;
    ankerl::unordered_dense::segmented_set<std::string_view> locals;

    GlobalSymbol* current_symbol;

    [[nodiscard]] auto has_local(std::string_view key) const -> bool {
        if (locals.contains(key)) return true;
        return parent ? parent->has_local(key) : false;
    }
};

struct State {
    GlobalScope scope = {};
    mem::Arena  arena;

    GlobalSymbol* global_symbols = nullptr;

    ankerl::unordered_dense::map<Decl*, GlobalSymbol*> gsym_map;

    ErrorReporter& er;
    SymbolStore&   ss;

    ~State() {
        auto it = global_symbols;
        while (it) {
            auto next = it->next;
            *it = {};  // re-initialize is equivalent to free in this case
            it = next;
        }

        global_symbols = nullptr;
    }

    /// Allocate a new GlobalSymbol.
    ///
    /// because `GlobalSymbol` contains a vector, special care is needed to
    /// later free the pointer. This is done in the destructor of `State`.
    auto new_global_decl(std::string_view name, Decl* decl) -> GlobalSymbol* {
        auto gsym = arena.create<GlobalSymbol>(GlobalSymbol{
            .name = name,
            .decl = decl,
            .next = global_symbols,
            .required_by = {},
        });

        global_symbols = gsym;
        gsym_map[decl] = gsym;

        return gsym;
    }
};

// ============================================================================

void scan_expr_for_global_refs(State& s, LocalScope& scope, ast::Expr* expr) {
    if (!expr) return;

    switch (expr->kind) {
        case ExprKind::Err: break;

        case ExprKind::Neg:
        case ExprKind::Add:
        case ExprKind::Sub:
        case ExprKind::Mul:
        case ExprKind::Div:
        case ExprKind::Mod: {
            auto& arith = expr->as_arith();
            scan_expr_for_global_refs(s, scope, arith.lhs);
            scan_expr_for_global_refs(s, scope, arith.rhs);
        } break;

        case ExprKind::Id: {
            auto& id = expr->as_id();
            if (!scope.has_local(id.value)) {
                // this is not a local variable, so it should be a global (that
                // is, if it is not undefined).
                if (auto g = s.scope.lookup(id.value)) {
                    g->required_by.push_back(scope.current_symbol);
                    scope.current_symbol->in_degree++;
                }
            }
        } break;

        case ExprKind::Int:
        case ExprKind::String: break;
    }
}

void scan_attributes_for_global_refs(State& s, LocalScope& scope,
                                     std::span<DeclAttribute> attributes,
                                     GlobalSymbol*            gsym) {
    for (auto const& attr : attributes) {
        if (!attr.qualified_name.empty()) {
            if (auto g = s.scope.lookup(attr.qualified_name)) {
                g->required_by.push_back(gsym);
                gsym->in_degree++;
            }
        }

        for (auto const& arg : attr.args) {
            scan_expr_for_global_refs(s, scope, arg);
        }

        for (auto const& kv : attr.kwargs) {
            scan_expr_for_global_refs(s, scope, kv.value);
        }
    }
}

void scan_decl_for_global_refs(State& s, ast::Decl* decl, GlobalSymbol* gsym) {
    if (!decl) return;

    switch (decl->kind) {
        case DeclKind::Err: break;

        case DeclKind::Func: {
            auto& func = decl->as_func();

            auto scope = LocalScope{
                .locals = {},
                .current_symbol = gsym,
            };

            scan_attributes_for_global_refs(s, scope, func.attributes, gsym);

            for (auto const& param : func.params) {
                scope.locals.insert(param.name);
                scan_expr_for_global_refs(s, scope, param.type_expr);
            }

            for (auto const& ret : func.rets) {
                if (!ret.name.empty()) scope.locals.insert(ret.name);
                scan_expr_for_global_refs(s, scope, ret.type_expr);
            }
        } break;

        case DeclKind::Var:
        case DeclKind::Def: {
            auto& var = decl->as_var();

            auto scope = LocalScope{
                .locals = {},
                .current_symbol = gsym,
            };

            scan_attributes_for_global_refs(s, scope, var.attributes, gsym);
            scan_expr_for_global_refs(s, scope, var.type_expr);
            scan_expr_for_global_refs(s, scope, var.init);
        } break;

        case DeclKind::MultiVar:
        case DeclKind::MultiDef: {
            auto& mvar = decl->as_multi_var();

            auto scope = LocalScope{
                .locals = {},
                .current_symbol = gsym,
            };

            scan_attributes_for_global_refs(s, scope, mvar.attributes, gsym);

            for (auto const& type : mvar.types) {
                scan_expr_for_global_refs(s, scope, type);
            }

            for (auto const& init : mvar.inits) {
                scan_expr_for_global_refs(s, scope, init);
            }
        } break;
    }
}

void hoist_one_decl(State& s, ast::Decl* decl) {
    if (!decl) return;

    switch (decl->kind) {
        case DeclKind::Err: break;

        case DeclKind::Func: {
            auto& func = decl->as_func();
            func.sym = s.ss.new_sym(decl->loc, func.name, func.name);

            // TODO: handle namespacing/attached types
            auto gd = s.new_global_decl(func.name, decl);
            s.scope.items[func.name] = gd;
        } break;

        case DeclKind::Var:
        case DeclKind::Def: {
            auto& var = decl->as_var();
            var.sym = s.ss.new_sym(decl->loc, var.name, var.name);

            auto gd = s.new_global_decl(var.name, decl);
            s.scope.items[var.name] = gd;
        } break;

        case DeclKind::MultiVar:
        case DeclKind::MultiDef: {
            auto& mvar = decl->as_multi_var();

            auto joined_name = fmt::to_string(fmt::join(
                mvar.names | std::views::transform(
                                 [](MultiVarName const& n) { return n.name; }),
                "_"));
            auto gd =
                s.new_global_decl(s.arena.alloc_string_view(joined_name), decl);

            for (auto& name : mvar.names) {
                name.sym = s.ss.new_sym(decl->loc, name.name, name.name);
                s.scope.items[name.name] = gd;
            }
        } break;
    }
}

/// Report all nodes that are (probably) part of the cycle captured in
/// `cycle_nodes`.
void report_cycle(State& s, GlobalSymbol* cycle_root) {
    ankerl::unordered_dense::segmented_set<GlobalSymbol*> visited;
    std::vector<GlobalSymbol*>                            worklist;

    // populate the start of the DFS (we don't just push n here because
    // that means it would be reported below again)
    for (auto dep : cycle_root->required_by) {
        if (dep->in_degree > 0) worklist.push_back(dep);
    }

    while (!worklist.empty()) {
        auto n = worklist.back();
        worklist.pop_back();

        // second will be true if the element was actually inserted, so
        // this will continue if the node has already been visited
        if (!visited.insert(n).second) continue;

        s.er.report_note(n->decl->loc,
                         "this declaration is also part of the cycle");

        for (auto dep : n->required_by) {
            if (dep->in_degree > 0) worklist.push_back(dep);
        }
    }
}

auto sort_globals(State& s) -> std::vector<GlobalSymbol*> {
    std::vector<GlobalSymbol*> sorted;
    std::deque<GlobalSymbol*>  queue;

    {
        ankerl::unordered_dense::segmented_set<GlobalSymbol*> found;

        for (auto const& [_, n] : s.scope.items) {
            if (n->in_degree == 0) {
                if (!found.contains(n)) {
                    found.insert(n);
                    queue.push_back(n);
                }
            }
        }
    }

    while (true) {
        while (!queue.empty()) {
            auto n = queue.front();
            queue.pop_front();

            sorted.push_back(n);
            for (auto const& dep : n->required_by) {
                if (--dep->in_degree == 0) {
                    queue.push_back(dep);
                }
            }
        }

        std::vector<GlobalSymbol*> cycle_nodes;
        for (auto const& [_, unordered] : s.scope.items) {
            if (unordered->in_degree > 0) {
                cycle_nodes.push_back(unordered);
            }
        }

        if (cycle_nodes.empty()) {
            break;
        }

        std::ranges::sort(cycle_nodes, [](GlobalSymbol* a, GlobalSymbol* b) {
            if (a->in_degree != b->in_degree)
                return a->in_degree < b->in_degree;

            // break ties with source location (assumes both are in the same
            // file)
            return a->decl->loc.span.begin < b->decl->loc.span.begin;
        });

        auto n = cycle_nodes.front();
        n->in_degree--;
        if (n->in_degree == 0) {
            s.er.report_error(n->decl->loc,
                              "global declaration is part of a cycle");

            // add this back into the queue now that we removed one of the links
            queue.push_back(n);

            report_cycle(s, n);
        }
    }

    return sorted;
}

void perform_sort(ErrorReporter& er, SymbolStore& ss, Module const& module) {
    auto s = State{.arena = {}, .gsym_map = {}, .er = er, .ss = ss};

    for (auto const& file : module.files) {
        for (auto const& decl : file.get_declarations()) {
            hoist_one_decl(s, decl);
        }
    }

    for (auto const& file : module.files) {
        for (auto const& decl : file.get_declarations()) {
            scan_decl_for_global_refs(s, decl, s.gsym_map.at(decl));
        }
    }

    for (auto const& [_, n] : s.scope.items) {
        er.report_note(
            n->decl->loc, "gsym {} in_degree={}, required_by={}", n->name,
            n->in_degree,
            n->required_by |
                std::views::transform([](GlobalSymbol* g) { return g->name; }));
    }

    auto sorted = sort_globals(s);
    for (auto const& [idx, gsym] : std::views::enumerate(sorted)) {
        auto decl = gsym->decl;
        er.report_debug(decl->loc, "[{}] found gsym: {} ({})", idx, gsym->name,
                        fmt::ptr(gsym));
    }
}

}  // namespace yal::ast::sort
