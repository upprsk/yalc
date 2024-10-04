add_executable(tests
  test.h
  test_lib_allocator.c
  test_lib_da.c
  test_lib_lst.c
  test_lib_map.c
  test_main.c
  test_tokenizer.c
)

target_link_libraries(tests PRIVATE lib yal)
