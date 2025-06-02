add_executable(tests
    test_main.cpp
)
target_include_directories(tests PRIVATE .)
target_link_libraries(tests PUBLIC yal)
target_compile_options(tests PRIVATE -Wall -Wextra)

target_compile_options(tests PUBLIC -fsanitize=address)
target_link_options(tests PUBLIC -fsanitize=address)
