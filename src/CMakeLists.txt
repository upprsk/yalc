add_library(yal
  allocator.c
  allocator.h
  arena/arena.c
  arena/arena.h
  ast.c
  ast.h
  codegen/codegen_c.c
  codegen/codegen_c.h
  codegen/codegen_mips.c
  codegen/codegen_mips.h
  common.h
  da.c
  da.h
  depg.c
  depg.h
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
