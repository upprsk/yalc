#include "ast.hpp"

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "fmt/format.h"
#include "nlohmann/json.hpp"

namespace yal::ast {

struct JsonVisitor : public Visitor<> {
    explicit constexpr JsonVisitor(Ast const& ast, json& j, bool show_loc)
        : Visitor<>{ast}, j{j}, show_loc{show_loc} {}

    void visit_before(Node const& node) override {
        // fmt::println(stderr, "JsonVisitor(id={}, kind={})", node.get_id(),
        //              node.get_kind());

        j = json{
            {"kind", fmt::to_string(node.get_kind())},
            {  "id",                   node.get_id()},
        };

        if (show_loc) j["loc"] = node.get_loc();
    }

    void visit_err(Node const& node) override {
        if (node.get_first().is_valid()) {
            j["error"] =
                ast->get_bytes_as_string_view(node.get_first().as_bytes());
        }
    }

    void visit_id(Node const& /*node*/, std::string_view id) override {
        j["value"] = id;
    }

    void visit_kw_lit(Node const& /*node*/, std::string_view id) override {
        j["value"] = id;
    }

    void visit_int(Node const& /*node*/, uint64_t value) override {
        j["value"] = value;
    }

    void visit_double(Node const& /*node*/, double value) override {
        j["value"] = value;
    }

    void visit_float(Node const& /*node*/, float value) override {
        j["value"] = value;
    }

    void visit_str(Node const& /*node*/, std::string_view bytes) override {
        j["value"] = bytes;
    }

    void visit_char(Node const& /*node*/, uint32_t character) override {
        j["value"] = character;
    }

    // ========================================================================

    void visit_module(Node const& /*node*/, conv::Module const& data) override {
        j["name"] = data.name;

        auto arr = json::array();
        for (auto const& child : data.children) {
            arr.push_back(ast->fatten(child));
        }
    }

    void visit_source_file(Node const& /*node*/,
                           conv::SourceFile const& data) override {
        auto arr = json::array();
        for (auto const& child : data.children)
            arr.push_back(ast->fatten(child));

        j["mod"] = ast->fatten(data.mod);
        j["children"] = arr;
    }

    void visit_module_decl(Node const& /*node*/,
                           std::string_view name) override {
        j["name"] = name;
    }

    void visit_func_decl(Node const& /*node*/,
                         conv::FuncDecl const& data) override {
        auto buf = json::array();
        for (auto const& dec : data.decorators) buf.push_back(ast->fatten(dec));
        j["decorators"] = buf;

        buf = json::array();
        for (auto const& garg : data.gargs) buf.push_back(ast->fatten(garg));
        j["gargs"] = buf;

        buf = json::array();
        for (auto const& arg : data.args) buf.push_back(ast->fatten(arg));
        j["args"] = buf;
        j["is_c_varargs"] = data.is_c_varargs;

        j["name"] = ast->fatten(data.name);
        if (data.ret.is_valid()) j["ret"] = ast->fatten(data.ret);
        if (data.body.is_valid()) j["body"] = ast->fatten(data.body);
    }

    void visit_top_var_decl(Node const& /*node*/,
                            conv::TopVarDecl const& data) override {
        j["decorators"] = ast->fatten(data.decorators);
        j["child"] = ast->fatten(data.decl);
    }

    void visit_top_def_decl(Node const& /*node*/,
                            conv::TopDefDecl const& data) override {
        j["decorators"] = ast->fatten(data.decorators);
        j["child"] = ast->fatten(data.decl);
    }

    void visit_id_pack(Node const& /*node*/,
                       conv::IdPack const& data) override {
        auto arr = json::array();
        for (auto const& id : data.ids)
            arr.push_back(ast->get_identifier(id.as_id()));

        j["names"] = arr;
    }

    void visit_func_param(Node const& /*node*/,
                          conv::FuncParam const& data) override {
        j["name"] = data.name;
        if (data.type.is_valid()) j["type"] = ast->fatten(data.type);
    }

    void visit_func_ret_pack(Node const& /*node*/,
                             conv::RetPack const& data) override {
        auto ret = data.ret;
        auto arr = json::array();

        ASSERT((ret.size() & 1) == 0);
        for (size_t i = 0; i < ret.size(); i += 2) {
            if (ret[i].is_valid()) {
                arr.push_back(json{
                    {"name", ast->get_identifier(ret[i].as_id())},
                    {"type",             ast->fatten(ret[i + 1])},
                });
            } else {
                arr.push_back(json{
                    {"type", ast->fatten(ret[i + 1])},
                });
            }
        }

        j["values"] = arr;
    }

    void visit_decorator(Node const& /*node*/,
                         conv::Decorator const& data) override {
        auto params = data.params;

        auto arr = json::array();

        ASSERT((params.size() & 1) == 0);
        for (size_t i = 0; i < params.size(); i += 2) {
            auto item = json::object();

            if (params[i].is_valid()) {
                item["key"] = ast->get_identifier(params[i].as_id());
            }

            if (params[i + 1].is_valid()) {
                item["value"] = ast->fatten(params[i + 1]);
            }

            arr.push_back(item);
        }

        j["name"] = data.name;
        j["params"] = arr;
    }

    // ========================================================================

    void visit_expr_pack(Node const& /*node*/,
                         conv::ExprPack const& data) override {
        auto arr = json::array();
        for (auto const& child : data.items) arr.push_back(ast->fatten(child));

        j["children"] = arr;
    }

#define VISIT_BIN(name)                                               \
    void visit_##name(Node const& /*node*/, conv::Binary const& data) \
        override {                                                    \
        j["left"] = ast->fatten(data.lhs);                            \
        j["right"] = ast->fatten(data.rhs);                           \
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

#define VISIT_UNARY(name)                                            \
    void visit_##name(Node const& /*node*/, conv::Unary const& data) \
        override {                                                   \
        j["child"] = ast->fatten(data.child);                        \
    }

    VISIT_UNARY(addrof);
    VISIT_UNARY(lnot);
    VISIT_UNARY(bnot);
    VISIT_UNARY(neg);

#undef VISIT_UNARY

    void visit_struct_type(Node const& /*node*/,
                           conv::StructType const& data) override {
        j["fields"] = ast->fatten(data.fields);
    }

    void visit_struct_field(Node const& /*node*/,
                            conv::StructField const& data) override {
        j["name"] = data.name;
        j["type"] = ast->fatten(data.type);
        if (data.init.is_valid()) j["init"] = ast->fatten(data.init);
    }

    void visit_ptr(Node const& /*node*/, conv::Ptr const& data) override {
        j["is_const"] = data.is_const;
        j["inner"] = ast->fatten(data.inner);
    }

    void visit_mptr(Node const& /*node*/, conv::MultiPtr const& data) override {
        j["is_const"] = data.is_const;
        j["inner"] = ast->fatten(data.inner);
    }

    void visit_slice(Node const& /*node*/, conv::Slice const& data) override {
        j["is_const"] = data.is_const;
        j["inner"] = ast->fatten(data.inner);
    }

    void visit_array_type(Node const& /*node*/,
                          conv::ArrayType const& data) override {
        j["is_const"] = data.is_const;
        j["size"] = ast->fatten(data.size);
        j["inner"] = ast->fatten(data.inner);
    }

    void visit_array(Node const& /*node*/, conv::Array const& data) override {
        if (data.size.is_valid()) j["size"] = ast->fatten(data.size);
        j["inner"] = ast->fatten(data.inner);
        j["items"] = ast->fatten(data.items);
    }

    void visit_lit(Node const& /*node*/, conv::Lit const& data) override {
        auto items = data.items;
        auto arr = json::array();

        ASSERT((items.size() & 1) == 0);
        for (size_t i = 0; i < items.size(); i += 2) {
            if (items[i].is_valid()) {
                arr.push_back(json{
                    { "name",     ast->fatten(items[i])},
                    {"value", ast->fatten(items[i + 1])},
                });
            } else {
                arr.push_back(json{
                    {"value", ast->fatten(items[i + 1])},
                });
            }
        }

        j["items"] = arr;
    }

    void visit_call(Node const& /*node*/, conv::Call const& data) override {
        auto arr = json::array();
        for (auto const& arg : data.args) arr.push_back(ast->fatten(arg));
        j["callee"] = ast->fatten(data.callee);
        j["args"] = arr;
    }

    void visit_field(Node const& /*node*/, conv::Field const& data) override {
        j["name"] = data.name;
        j["receiver"] = ast->fatten(data.receiver);
    }

    // ========================================================================

    void visit_block(Node const& /*node*/, conv::Block const& data) override {
        auto arr = json::array();
        for (auto const& child : data.items) arr.push_back(ast->fatten(child));
        j["children"] = arr;
    }

    void visit_expr_stmt(Node const& /*node*/,
                         conv::Unary const& data) override {
        j["child"] = ast->fatten(data.child);
    }

    void visit_return_stmt(Node const& /*node*/,
                           conv::Unary const& data) override {
        if (data.child.is_valid()) j["child"] = ast->fatten(data.child);
    }

    void visit_if_stmt(Node const& /*node*/,
                       conv::IfStmt const& data) override {
        j["cond"] = ast->fatten(data.cond);
        j["wt"] = ast->fatten(data.wt);
        if (data.wf.is_valid()) j["wf"] = ast->fatten(data.wf);
    }

    void visit_while_stmt(Node const& /*node*/,
                          conv::WhileStmt const& data) override {
        j["cond"] = ast->fatten(data.cond);
        j["body"] = ast->fatten(data.body);
    }

    void visit_var_decl(Node const& /*node*/,
                        conv::VarDecl const& data) override {
        j["ids"] = ast->fatten(data.ids);
        if (data.types.is_valid()) j["types"] = ast->fatten(data.types);
        if (data.inits.is_valid()) j["inits"] = ast->fatten(data.inits);
    }

    void visit_def_decl(Node const& /*node*/,
                        conv::DefDecl const& data) override {
        j["ids"] = ast->fatten(data.ids);
        if (data.types.is_valid()) j["types"] = ast->fatten(data.types);
        j["inits"] = ast->fatten(data.inits);
    }

#define VISIT_ASSIGN(name)                                            \
    void visit_##name(Node const& /*node*/, conv::Assign const& data) \
        override {                                                    \
        j["lhs"] = ast->fatten(data.lhs);                             \
        j["rhs"] = ast->fatten(data.rhs);                             \
    }

    VISIT_ASSIGN(assign);
    VISIT_ASSIGN(assign_add);
    VISIT_ASSIGN(assign_sub);
    VISIT_ASSIGN(assign_mul);
    VISIT_ASSIGN(assign_div);
    VISIT_ASSIGN(assign_mod);
    VISIT_ASSIGN(assign_left_shift);
    VISIT_ASSIGN(assign_right_shift);
    VISIT_ASSIGN(assign_band);
    VISIT_ASSIGN(assign_bor);
    VISIT_ASSIGN(assign_bxor);

#undef VISIT_ASSIGN

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
    auto v = JsonVisitor{*n.ast, j, true};
    v.visit(n.id);
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
