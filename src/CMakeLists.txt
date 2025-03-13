add_library(yal
    ast.cpp
    ast.hpp
    codegen_qbe.cpp
    codegen_qbe.hpp
    error_reporter.cpp
    error_reporter.hpp
    hlir.cpp
    hlir.hpp
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
)
target_include_directories(yal PUBLIC .)
target_compile_options(yal PRIVATE -Wall -Wextra)
target_link_libraries(yal PUBLIC fmt::fmt libassert::assert)
target_compile_definitions(yal PUBLIC LIBASSERT_USE_FMT=1)

# add_mocked_library(yal)
