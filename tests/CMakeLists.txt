add_executable(tests
    subprocess.hpp
    test_helpers.cpp
    test_helpers.hpp
    test_main.cpp
    test_name_res.cpp
    test_name_res.hpp
    test_parser.cpp
    test_parser.hpp
    test_sema.cpp
    test_sema.hpp
    test_tokenizer.cpp
    test_tokenizer.hpp
)
target_include_directories(tests PUBLIC .)
target_compile_options(tests PRIVATE -Wall -Wextra)
target_link_libraries(tests PUBLIC yal fmt::fmt libassert::assert nlohmann_json::nlohmann_json)

add_test(
    NAME tests
    COMMAND tests
)
