add_library(lib
  alloc/allocator.c
  alloc/allocator.h
  da/da.c
  da/da.h
  lst/lst.h
  map/map.h
  utils/minmax.h
)

target_compile_options(lib PUBLIC -Wall -Wextra)
target_include_directories(lib PUBLIC .)
target_link_libraries(lib PUBLIC munit)
