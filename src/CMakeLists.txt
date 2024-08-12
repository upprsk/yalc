add_library(yal
  allocator.h
  allocator.c
  span.h
  da.h
  da.c
  tokenizer.h
  tokenizer.c)

target_include_directories(yal PUBLIC .)
target_link_libraries(yal PUBLIC munit)
target_compile_options(yal PUBLIC -Wall -Wextra)
