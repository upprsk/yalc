add_executable(tests
  test.h
  test_main.c
  test_tokenizer.c
)

target_link_libraries(tests PRIVATE yal)
