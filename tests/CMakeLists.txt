add_executable(tests
    argparse.cpp
    argparse.hpp
    test_file_store.cpp
    test_integration.cpp
    test_main.cpp
    tests.cpp
    tests.hpp
)
target_include_directories(tests PRIVATE .)
target_link_libraries(tests PUBLIC yal)
target_compile_options(tests PRIVATE -Wall -Wextra)

target_compile_options(tests PUBLIC -fsanitize=address)
target_link_options(tests PUBLIC -fsanitize=address)
