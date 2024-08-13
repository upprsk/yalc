add_library(yal
  allocator.c
  allocator.h
  arena/arena.c
  arena/arena.h
  ast.c
  ast.h
  da.c
  da.h
  errors.c
  errors.h
  parser.c
  parser.h
  span.h
  tokenizer.c
  tokenizer.h
  typecheck.c
  typecheck.h
  typestore.c
  typestore.h
)

target_include_directories(yal PUBLIC .)
target_link_libraries(yal PUBLIC munit)
target_compile_options(yal PUBLIC -Wall -Wextra)
