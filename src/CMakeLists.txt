add_library(yal
  ast.c
  ast.h
  common.h
  errors.c
  errors.h
  ir.c
  ir.h
  irgen.c
  irgen.h
  parser.c
  parser.h
  sema.c
  sema.h
  span.h
  tokenizer.c
  tokenizer.h
  tstore.c
  tstore.h
)

target_include_directories(yal PUBLIC .)
target_link_libraries(yal PUBLIC munit lib)
target_compile_options(yal PUBLIC -Wall -Wextra)
