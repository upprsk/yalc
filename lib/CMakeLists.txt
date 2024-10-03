add_library(lib
  alloc/allocator.c
  alloc/allocator.h
  da/da.c
  da/da.h
  utils/minmax.h
)

target_include_directories(lib PUBLIC .)
target_link_libraries(lib PUBLIC munit)
