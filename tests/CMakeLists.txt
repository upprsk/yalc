set(TEST_FILES
  ast.cpp
  parser.cpp
  tokenizer.cpp
)

add_executable(tests ${TEST_FILES})
target_link_libraries(tests yal Catch2::Catch2WithMain)
add_test(NAME tests
         COMMAND tests)


# add_executable(test_coverage ${TEST_FILES})
# target_link_libraries(test_coverage yal_COVERAGE Catch2::Catch2WithMain)
# target_compile_options(test_coverage PRIVATE -coverage)
# target_link_options(test_coverage PRIVATE -coverage)
# add_test(NAME coverage
#          COMMAND test_coverage)
