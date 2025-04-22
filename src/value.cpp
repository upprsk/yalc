#include "value.hpp"

#include <string_view>
#include <variant>

#include "libassert/assert.hpp"
#include "nlohmann/json.hpp"

namespace yal {

void to_json(json& j, Value const& v) {
    j = json{
        {"type", {}},
        {"data", {}},
    };

    if (v.type) {
        j["type"] = *v.type;
    }

    if (std::holds_alternative<yal::types::Type*>(v.data)) {
        ASSERT(v.get_data_type() != nullptr);
        j["data"] = *v.get_data_type();
    } else if (std::holds_alternative<bool>(v.data)) {
        j["data"] = v.get_data_bool();
    } else if (std::holds_alternative<uint64_t>(v.data)) {
        j["data"] = v.get_data_uint();
    } else if (std::holds_alternative<std::string_view>(v.data)) {
        j["data"] = v.get_data_strv();
    }
}

}  // namespace yal

auto fmt::formatter<yal::Value>::format(yal::Value const& v,
                                        format_context&   ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "Value({}", *v.type);

    if (std::holds_alternative<yal::types::Type*>(v.data)) {
        if (v.get_data_type())
            fmt::format_to(ctx.out(), ", {}", *v.get_data_type());
        else
            fmt::format_to(ctx.out(), ", Type<null>");
    } else if (std::holds_alternative<bool>(v.data)) {
        fmt::format_to(ctx.out(), ", {}", v.get_data_bool());
    } else if (std::holds_alternative<uint64_t>(v.data)) {
        fmt::format_to(ctx.out(), ", {}", v.get_data_uint());
    } else if (std::holds_alternative<std::string_view>(v.data)) {
        fmt::format_to(ctx.out(), ", {:?}", v.get_data_strv());
    }

    return fmt::format_to(ctx.out(), ")");
}
