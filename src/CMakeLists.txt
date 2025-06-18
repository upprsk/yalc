add_library(yal
    arena.cpp
    arena.hpp
    ast.hpp
    error_reporter.cpp
    error_reporter.hpp
    file_store.cpp
    file_store.hpp
    location.cpp
    location.hpp
    macros.hpp
    node.cpp
    node.hpp
    parser.cpp
    parser.hpp
    tokenizer.cpp
    tokenizer.hpp
    utils.cpp
    utils.hpp
    yal.cpp
    yal.hpp
)
target_include_directories(yal PUBLIC .)
target_compile_options(yal PRIVATE -Wall -Wextra)
target_link_libraries(yal PUBLIC
    fmt::fmt
    libassert::assert
    nlohmann_json::nlohmann_json
    unordered_dense::unordered_dense
)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(yal PUBLIC -fsanitize=address)
    target_link_options(yal PUBLIC -fsanitize=address)
endif()
