#include "ast.hpp"

#include "ast-node-visitor.hpp"
#include "fmt/format.h"
#include "nlohmann/json.hpp"

namespace yal::ast {

struct JsonVisitor : public Visitor {
    explicit constexpr JsonVisitor(json& j, bool show_loc)
        : j{j}, show_loc{show_loc} {}

    void visit_before(Ast& /*ast*/, Node const& node) override {
        // fmt::println(stderr, "JsonVisitor(id={}, kind={})", node.get_id(),
        //              node.get_kind());

        j = json{
            {"kind", fmt::to_string(node.get_kind())},
            {  "id",                   node.get_id()},
        };

        if (show_loc) j["loc"] = node.get_loc();
    }

    void visit_err(Ast& ast, Node const& node) override {
        if (node.get_first().is_valid()) {
            j["error"] =
                ast.get_bytes_as_string_view(node.get_first().as_bytes());
        }
    }

    void visit_id(Ast& /*ast*/, Node const& /*node*/,
                  std::string_view id) override {
        j["value"] = id;
    }

    void visit_kw_lit(Ast& /*ast*/, Node const& /*node*/,
                      std::string_view id) override {
        j["value"] = id;
    }

    void visit_int(Ast& /*ast*/, Node const& /*node*/,
                   uint64_t value) override {
        j["value"] = value;
    }

    void visit_double(Ast& /*ast*/, Node const& /*node*/,
                      double value) override {
        j["value"] = value;
    }

    void visit_float(Ast& /*ast*/, Node const& /*node*/, float value) override {
        j["value"] = value;
    }

    void visit_str(Ast& /*ast*/, Node const& /*node*/,
                   std::string_view bytes) override {
        j["value"] = bytes;
    }

    void visit_char(Ast& /*ast*/, Node const& /*node*/,
                    uint32_t character) override {
        j["value"] = character;
    }

    // ========================================================================

    void visit_module(Ast& ast, Node const& /*node*/, std::string_view name,
                      std::span<NodeId const> children) override {
        j["name"] = name;

        auto arr = json::array();
        for (auto const& child : children) {
            arr.push_back(ast.fatten(child));
        }
    }

    void visit_source_file(Ast& ast, Node const& /*node*/, NodeId mod,
                           std::span<NodeId const> children) override {
        auto arr = json::array();
        for (auto const& child : children) arr.push_back(ast.fatten(child));

        j["mod"] = ast.fatten(mod);
        j["children"] = arr;
    }

    void visit_module_decl(Ast& /*ast*/, Node const& /*node*/,
                           std::string_view name) override {
        j["name"] = name;
    }

    void visit_func_decl(Ast&                    ast, Node const& /*node*/,
                         std::span<NodeId const> decorators, NodeId name,
                         std::span<NodeId const> gargs,
                         std::span<NodeId const> args, NodeId ret, NodeId body,
                         bool is_c_varargs) override {
        auto buf = json::array();
        for (auto const& dec : decorators) buf.push_back(ast.fatten(dec));
        j["decorators"] = buf;

        buf = json::array();
        for (auto const& garg : gargs) buf.push_back(ast.fatten(garg));
        j["gargs"] = buf;

        buf = json::array();
        for (auto const& arg : args) buf.push_back(ast.fatten(arg));
        j["args"] = buf;
        j["is_c_varargs"] = is_c_varargs;

        j["name"] = ast.fatten(name);
        if (ret.is_valid()) j["ret"] = ast.fatten(ret);
        if (body.is_valid()) j["body"] = ast.fatten(body);
    }

    void visit_top_var_decl(Ast&                    ast, Node const& /*node*/,
                            std::span<NodeId const> decorators,
                            NodeId                  child) override {
        j["decorators"] = ast.fatten(decorators);
        j["child"] = ast.fatten(child);
    }

    void visit_top_def_decl(Ast&                    ast, Node const& /*node*/,
                            std::span<NodeId const> decorators,
                            NodeId                  child) override {
        j["decorators"] = ast.fatten(decorators);
        j["child"] = ast.fatten(child);
    }

    void visit_id_pack(Ast&                    ast, Node const& /*node*/,
                       std::span<NodeId const> ids) override {
        auto arr = json::array();
        for (auto const& id : ids)
            arr.push_back(ast.get_identifier(id.as_id()));

        j["names"] = arr;
    }

    void visit_func_param(Ast& ast, Node const& /*node*/, std::string_view name,
                          NodeId type) override {
        j["name"] = name;
        if (type.is_valid()) j["type"] = ast.fatten(type);
    }

    void visit_func_ret_pack(Ast&                    ast, Node const& /*node*/,
                             std::span<NodeId const> ret) override {
        auto arr = json::array();

        ASSERT((ret.size() & 1) == 0);
        for (size_t i = 0; i < ret.size(); i += 2) {
            if (ret[i].is_valid()) {
                arr.push_back(json{
                    {"name", ast.get_identifier(ret[i].as_id())},
                    {"type",             ast.fatten(ret[i + 1])},
                });
            } else {
                arr.push_back(json{
                    {"type", ast.fatten(ret[i + 1])},
                });
            }
        }

        j["values"] = arr;
    }

    void visit_decorator(Ast& ast, Node const& /*node*/, std::string_view name,
                         std::span<NodeId const> params) override {
        j["name"] = name;

        auto arr = json::array();

        ASSERT((params.size() & 1) == 0);
        for (size_t i = 0; i < params.size(); i += 2) {
            auto item = json::object();

            if (params[i].is_valid()) {
                item["key"] = ast.get_identifier(params[i].as_id());
            }

            if (params[i + 1].is_valid()) {
                item["value"] = ast.fatten(params[i + 1]);
            }

            arr.push_back(item);
        }

        j["params"] = arr;
    }

    // ========================================================================

    void visit_expr_pack(Ast&                    ast, Node const& /*node*/,
                         std::span<NodeId const> children) override {
        auto arr = json::array();
        for (auto const& child : children) arr.push_back(ast.fatten(child));

        j["children"] = arr;
    }

#define VISIT_BIN(name)                                                       \
    void visit_##name(Ast& ast, Node const& /*node*/, NodeId lhs, NodeId rhs) \
        override {                                                            \
        j["left"] = ast.fatten(lhs);                                          \
        j["right"] = ast.fatten(rhs);                                         \
    }

    VISIT_BIN(add);
    VISIT_BIN(sub);
    VISIT_BIN(mul);
    VISIT_BIN(div);
    VISIT_BIN(mod);
    VISIT_BIN(left_shift);
    VISIT_BIN(right_shift);
    VISIT_BIN(equal);
    VISIT_BIN(not_equal);
    VISIT_BIN(less);
    VISIT_BIN(less_equal);
    VISIT_BIN(greater);
    VISIT_BIN(greater_equal);
    VISIT_BIN(band);
    VISIT_BIN(bor);
    VISIT_BIN(bxor);
    VISIT_BIN(land);
    VISIT_BIN(lor);
    VISIT_BIN(cast);

#undef VISIT_BIN

#define VISIT_UNARY(name)                                                      \
    void visit_##name(Ast& ast, Node const& /*node*/, NodeId child) override { \
        j["child"] = ast.fatten(child);                                        \
    }

    VISIT_UNARY(addrof);
    VISIT_UNARY(lnot);
    VISIT_UNARY(bnot);
    VISIT_UNARY(neg);

#undef VISIT_UNARY

    void visit_struct_type(Ast&                    ast, const Node& /*node*/,
                           std::span<const NodeId> fields) override {
        j["fields"] = ast.fatten(fields);
    }

    void visit_struct_field(Ast&             ast, Node const& /*node*/,
                            std::string_view name, NodeId type,
                            NodeId init) override {
        j["name"] = name;
        j["type"] = ast.fatten(type);
        if (init.is_valid()) j["init"] = ast.fatten(init);
    }

    void visit_ptr(Ast& ast, Node const& /*node*/, bool is_const,
                   NodeId inner) override {
        j["is_const"] = is_const;
        j["inner"] = ast.fatten(inner);
    }

    void visit_mptr(Ast& ast, Node const& /*node*/, bool is_const,
                    NodeId inner) override {
        j["is_const"] = is_const;
        j["inner"] = ast.fatten(inner);
    }

    void visit_slice(Ast& ast, Node const& /*node*/, bool is_const,
                     NodeId inner) override {
        j["is_const"] = is_const;
        j["inner"] = ast.fatten(inner);
    }

    void visit_array_type(Ast& ast, Node const& /*node*/, bool is_const,
                          NodeId size, NodeId inner) override {
        j["is_const"] = is_const;
        j["size"] = ast.fatten(size);
        j["inner"] = ast.fatten(inner);
    }

    void visit_array(Ast& ast, Node const& /*node*/, NodeId size, NodeId inner,
                     std::span<NodeId const> items) override {
        if (size.is_valid()) j["size"] = ast.fatten(size);
        j["inner"] = ast.fatten(inner);
        j["items"] = ast.fatten(items);
    }

    void visit_lit(Ast&                    ast, Node const& /*node*/,
                   std::span<NodeId const> items) override {
        auto arr = json::array();

        ASSERT((items.size() & 1) == 0);
        for (size_t i = 0; i < items.size(); i += 2) {
            if (items[i].is_valid()) {
                arr.push_back(json{
                    { "name",     ast.fatten(items[i])},
                    {"value", ast.fatten(items[i + 1])},
                });
            } else {
                arr.push_back(json{
                    {"value", ast.fatten(items[i + 1])},
                });
            }
        }

        j["items"] = arr;
    }

    // ========================================================================

    void visit_block(Ast&                    ast, Node const& /*node*/,
                     std::span<NodeId const> children) override {
        auto arr = json::array();
        for (auto const& child : children) arr.push_back(ast.fatten(child));
        j["children"] = arr;
    }

    void visit_expr_stmt(Ast&   ast, Node const& /*node*/,
                         NodeId child) override {
        j["child"] = ast.fatten(child);
    }

    void visit_return_stmt(Ast&   ast, Node const& /*node*/,
                           NodeId child) override {
        if (child.is_valid()) j["child"] = ast.fatten(child);
    }

    void visit_if_stmt(Ast& ast, Node const& /*node*/, NodeId cond, NodeId wt,
                       NodeId wf) override {
        j["cond"] = ast.fatten(cond);
        j["wt"] = ast.fatten(wt);
        if (wf.is_valid()) j["wf"] = ast.fatten(wf);
    }

    void visit_while_stmt(Ast& ast, Node const& /*node*/, NodeId cond,
                          NodeId body) override {
        j["cond"] = ast.fatten(cond);
        j["body"] = ast.fatten(body);
    }

    void visit_var_decl(Ast& ast, Node const& /*node*/, NodeId ids,
                        NodeId types, NodeId inits) override {
        j["ids"] = ast.fatten(ids);
        if (types.is_valid()) j["types"] = ast.fatten(types);
        if (inits.is_valid()) j["inits"] = ast.fatten(inits);
    }

    void visit_def_decl(Ast& ast, Node const& /*node*/, NodeId ids,
                        NodeId types, NodeId inits) override {
        j["ids"] = ast.fatten(ids);
        if (types.is_valid()) j["types"] = ast.fatten(types);
        j["inits"] = ast.fatten(inits);
    }

    json& j;
    bool  show_loc;
};

// TODO: print the ast?
// For now json should be more than enough
auto FatNodeId::dump_to_ctx(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "FatNodeId({})", id);
}

void to_json(json& j, FatNodeId const& n) {
    auto v = JsonVisitor{j, true};
    v.visit(*n.ast, n.id);
}

void to_json(json& j, FatNodeArray const& n) {
    j = json{};
    for (auto const& id : n.ids) {
        j.push_back(n.ast->fatten(id));
    }
}

}  // namespace yal::ast

auto fmt::formatter<yal::ast::FatNodeId>::format(yal::ast::FatNodeId n,
                                                 format_context&     ctx) const
    -> format_context::iterator {
    return n.dump_to_ctx(ctx);
}
