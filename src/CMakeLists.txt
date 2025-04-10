add_library(yal
    ast-node-conv.cpp
    ast-node-conv.hpp
    ast-node-id.cpp
    ast-node-id.hpp
    ast-node-visitor.cpp
    ast-node-visitor.hpp
    ast.cpp
    ast.hpp
    decl-id.cpp
    decl-id.hpp
    decl-store.cpp
    decl-store.hpp
    error_reporter.cpp
    error_reporter.hpp
    file-store.cpp
    file-store.hpp
    name-order.cpp
    name-order.hpp
    name-res.cpp
    name-res.hpp
    parser.cpp
    parser.hpp
    span.cpp
    span.hpp
    tokenizer.cpp
    tokenizer.hpp
    type-id.cpp
    type-id.hpp
    types.cpp
    types.hpp
    typing.cpp
    typing.hpp
    utils.cpp
    utils.hpp
    value.cpp
    value.hpp
    yal.cpp
    yal.hpp
)
target_include_directories(yal PUBLIC .)
target_compile_options(yal PRIVATE -Wall -Wextra)
target_link_libraries(yal PUBLIC fmt::fmt libassert::assert nlohmann_json::nlohmann_json)

# add_mocked_library(yal)
