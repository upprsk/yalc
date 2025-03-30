add_executable(tests
    test_main.cpp
    test_tokenizer.cpp
    test_tokenizer.hpp
)
target_include_directories(tests PUBLIC .)
target_compile_options(tests PRIVATE -Wall -Wextra)
target_link_libraries(tests PUBLIC yal fmt::fmt libassert::assert nlohmann_json::nlohmann_json)
