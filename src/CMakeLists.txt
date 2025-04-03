add_library(yal
    ast-node-conv.cpp
    ast-node-conv.hpp
    ast-node-id.cpp
    ast-node-id.hpp
    ast-node-visitor.cpp
    ast-node-visitor.hpp
    ast.cpp
    ast.hpp
    error_reporter.cpp
    error_reporter.hpp
    file-store.cpp
    file-store.hpp
    name-res.cpp
    name-res.hpp
    parser.cpp
    parser.hpp
    span.cpp
    span.hpp
    tokenizer.cpp
    tokenizer.hpp
    utils.cpp
    utils.hpp
)
target_include_directories(yal PUBLIC .)
target_compile_options(yal PRIVATE -Wall -Wextra)
target_link_libraries(yal PUBLIC fmt::fmt libassert::assert nlohmann_json::nlohmann_json)

# add_mocked_library(yal)
