add_library(yal
    arena.cpp
    arena.hpp
    ast-node-conv.cpp
    ast-node-conv.hpp
    ast-node-visitor.cpp
    ast-node-visitor.hpp
    ast-node.cpp
    ast-node.hpp
    ast.cpp
    ast.hpp
    codegen/codegen_qbe.cpp
    codegen/codegen_qbe.hpp
    decl-store.cpp
    decl-store.hpp
    error_reporter.cpp
    error_reporter.hpp
    file-store.cpp
    file-store.hpp
    ir-build.cpp
    ir-build.hpp
    ir.cpp
    ir.hpp
    lower.cpp
    lower.hpp
    name-order.cpp
    name-order.hpp
    name-res.cpp
    name-res.hpp
    parser.cpp
    parser.hpp
    sema.cpp
    sema.hpp
    span.cpp
    span.hpp
    tokenizer.cpp
    tokenizer.hpp
    types.cpp
    types.hpp
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

target_compile_options(yal PUBLIC -fsanitize=address)
target_link_options(yal PUBLIC -fsanitize=address)

# add_mocked_library(yal)
