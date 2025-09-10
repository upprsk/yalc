#include "ast_sort.hpp"

#include <fmt/ranges.h>

#include <deque>
#include <ranges>
#include <string_view>

#include "ast.hpp"
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
        case ExprKind::Mod:
        case ExprKind::Cast: {
            auto& arith = expr->as_arith();
            scan_expr_for_global_refs(s, scope, arith.lhs);
            scan_expr_for_global_refs(s, scope, arith.rhs);
        } break;

        case ExprKind::Field: {
            auto& field = expr->as_field();
            scan_expr_for_global_refs(s, scope, field.obj);
        } break;

        case ExprKind::Call: {
            auto& call = expr->as_call();
            scan_expr_for_global_refs(s, scope, call.callee);
            for (auto const& arg : call.args) {
                scan_expr_for_global_refs(s, scope, arg);
            }
        } break;

        case ExprKind::Ptr:
        case ExprKind::MultiPtr:
        case ExprKind::Slice: {
            auto& ptr = expr->as_ptr();
            scan_expr_for_global_refs(s, scope, ptr.inner);
        } break;

        case ExprKind::Array: {
            auto& arr = expr->as_array();
            scan_expr_for_global_refs(s, scope, arr.count);
            scan_expr_for_global_refs(s, scope, arr.inner);
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

        case ExprKind::Kw:
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
        case DeclKind::Err:
        case DeclKind::Import: break;

        case DeclKind::Func: {
            auto& func = decl->as_func();

            auto scope = LocalScope{
                .locals = {},
                .current_symbol = gsym,
            };

            if (!func.attached_type.empty()) {
                if (auto g = s.scope.lookup(func.attached_type)) {
                    g->required_by.push_back(scope.current_symbol);
                    scope.current_symbol->in_degree++;
                }
            }

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

        case DeclKind::Import: {
            auto& imp = decl->as_import();

            auto gd = s.new_global_decl(imp.name, decl);
            s.scope.items[imp.name] = gd;
        } break;

        case DeclKind::Func: {
            auto& func = decl->as_func();

            auto gd = s.new_global_decl(func.name, decl);

            if (func.attached_type.empty()) {
                if (auto g = s.scope.lookup(func.name)) {
                    s.er.report_error(
                        decl->loc,
                        "duplicate global identifier {:?}, can not "
                        "redeclare global",
                        func.name);
                    s.er.report_note(g->decl->loc, "first defined here");
                }

                s.scope.items[func.name] = gd;
            }
        } break;

        case DeclKind::Var:
        case DeclKind::Def: {
            auto& var = decl->as_var();

            if (auto g = s.scope.lookup(var.name)) {
                s.er.report_error(decl->loc,
                                  "duplicate global identifier {:?}, can not "
                                  "redeclare global",
                                  var.name);
                s.er.report_note(g->decl->loc, "first defined here");
            }

            auto gd = s.new_global_decl(var.name, decl);
            s.scope.items[var.name] = gd;
        } break;

        case DeclKind::MultiVar:
        case DeclKind::MultiDef: {
            auto& mvar = decl->as_multi_var();

            auto joined_name = fmt::format(
                "", fmt::join(mvar.names | std::views::transform(
                                               [](MultiVarName const& n) {
                                                   return n.name;
                                               }),
                              "_"));
            auto gd =
                s.new_global_decl(s.arena.alloc_string_view(joined_name), decl);

            for (auto& name : mvar.names) {
                if (auto g = s.scope.lookup(name.name)) {
                    s.er.report_error(
                        decl->loc,
                        "duplicate global identifier {:?}, can not "
                        "redeclare global",
                        name.name);
                    s.er.report_note(g->decl->loc, "first defined here");
                }

                s.scope.items[name.name] = gd;
            }
        } break;
    }
}

/// Report all nodes that are (probably) part of the cycle.
void report_cycle(ErrorReporter& er, GlobalSymbol* cycle_root) {
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

        er.report_note(n->decl->loc,
                       "this declaration is also part of the cycle");

        for (auto dep : n->required_by) {
            if (dep->in_degree > 0) worklist.push_back(dep);
        }
    }
}

auto vector_without_duplicates(State& s) -> std::vector<GlobalSymbol*> {
    std::vector<GlobalSymbol*>                            all_symbols;
    ankerl::unordered_dense::segmented_set<GlobalSymbol*> found;

    for (auto const& [_, n] : s.scope.items) {
        if (!found.contains(n)) {
            found.insert(n);
            all_symbols.push_back(n);
        }
    }

    return all_symbols;
}

auto sort_globals(ErrorReporter& er, std::vector<GlobalSymbol*> items)
    -> std::vector<GlobalSymbol*> {
    std::vector<GlobalSymbol*> sorted;
    std::deque<GlobalSymbol*>  queue;

    for (auto const& n : items) {
        if (n->in_degree == 0) queue.push_back(n);
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
        for (auto const& unordered : items) {
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
            er.report_error(n->decl->loc,
                            "global declaration is part of a cycle");

            // add this back into the queue now that we removed one of the links
            queue.push_back(n);

            report_cycle(er, n);
        }
    }

    return sorted;
}

auto perform_sort(ErrorReporter& er, Module&& module, Options const& opt)
    -> FlatModule {
    auto s = State{.arena = {}, .gsym_map = {}, .er = er};

    for (auto const& file : module.files) {
        for (auto const& decl : file.get_declarations()) {
            hoist_one_decl(s, decl);
        }
    }

    for (auto const& file : module.files) {
        for (auto const& decl : file.get_declarations()) {
            auto it = s.gsym_map.find(decl);
            if (it != s.gsym_map.end()) {
                scan_decl_for_global_refs(s, decl, it->second);
            }
        }
    }

    // create list without deplicates
    auto all_symbols = vector_without_duplicates(s);

    if (opt.verbose_deps) {
        for (auto const& [_, n] : s.scope.items) {
            er.report_note(
                n->decl->loc, "gsym {} in_degree={}, required_by={}", n->name,
                n->in_degree,
                n->required_by | std::views::transform(
                                     [](GlobalSymbol* g) { return g->name; }));
        }
    }

    auto sorted = sort_globals(er, all_symbols);

    if (opt.verbose_sort) {
        for (auto const& [idx, gsym] : std::views::enumerate(sorted)) {
            auto decl = gsym->decl;
            er.report_debug(decl->loc, "[{}] found gsym: {} ({})", idx,
                            gsym->name, fmt::ptr(gsym));
        }
    }

    auto fm = FlatModule{};
    fm.name = module.name;

    for (auto&& file : module.files) {
        fm.node_arena.move_from(std::move(file.node_arena));
        fm.strings_arena.move_from(std::move(file.strings_arena));
    }

    for (auto const& sym : sorted) {
        fm.declarations.push_back(sym->decl);
    }

    return fm;
}

}  // namespace yal::ast::sort
