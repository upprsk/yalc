add_library(ulib
    arena.c
    arena.h
    da.h
    defer.h
    string_view.h
    typeinfo.c
    typeinfo.h
)

target_include_directories(ulib PUBLIC .)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(ulib PUBLIC -fsanitize=address)
    target_link_options(ulib PUBLIC -fsanitize=address)
endif()
