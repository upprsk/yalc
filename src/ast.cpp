#include "ast.hpp"

#include "ast-node-visitor.hpp"
#include "fmt/format.h"
#include "nlohmann/json.hpp"

namespace yal::ast {

struct JsonVisitor : public Visitor {
    explicit constexpr JsonVisitor(json& j) : j{j} {}

    void visit_before(Ast& /*ast*/, Node const& node) override {
        // fmt::println(stderr, "JsonVisitor(id={}, kind={})", node.get_id(),
        //              node.get_kind());

        j = json{
            {"kind", fmt::to_string(node.get_kind())},
            {  "id",                   node.get_id()},
            { "loc",                  node.get_loc()},
        };
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
                         std::span<NodeId const> args, NodeId ret,
                         NodeId body) override {
        auto buf = json::array();
        for (auto const& dec : decorators) buf.push_back(ast.fatten(dec));
        j["decorators"] = buf;

        buf = json::array();
        for (auto const& garg : gargs) buf.push_back(ast.fatten(garg));
        j["gargs"] = buf;

        buf = json::array();
        for (auto const& arg : args) buf.push_back(ast.fatten(arg));
        j["args"] = buf;

        j["name"] = ast.fatten(name);
        if (ret.is_valid()) j["ret"] = ast.fatten(ret);
        if (body.is_valid()) j["body"] = ast.fatten(body);
    }

    void visit_func_id(Ast&                    ast, Node const& /*node*/,
                       std::span<NodeId const> ids) override {
        auto arr = json::array();
        for (auto const& id : ids)
            arr.push_back(ast.get_identifier(id.as_id()));

        j["names"] = arr;
    }

    void visit_func_param(Ast& ast, Node const& /*node*/, std::string_view name,
                          NodeId type) override {
        j["name"] = name;
    }

    void visit_block(Ast&                    ast, Node const& /*node*/,
                     std::span<NodeId const> children) override {
        auto arr = json::array();
        for (auto const& child : children) arr.push_back(ast.fatten(child));
        j["children"] = arr;
    }

    json& j;
};

// TODO: print the ast?
// For now json should be more than enough
auto FatNodeId::dump_to_ctx(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "FatNodeId({})", id);
}

void to_json(json& j, FatNodeId const& n) {
    auto v = JsonVisitor{j};
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
