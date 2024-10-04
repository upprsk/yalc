add_library(yal
  errors.c
  errors.h
  span.h
  tokenizer.c
  tokenizer.h
)

target_include_directories(yal PUBLIC .)
target_link_libraries(yal PUBLIC munit lib)
target_compile_options(yal PUBLIC -Wall -Wextra)
