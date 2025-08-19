add_executable(metac
    metac.c
    options.c
    options.h
)

target_include_directories(metac PUBLIC .)
target_link_libraries(metac PUBLIC ulib clang)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(metac PUBLIC -fsanitize=address)
    target_link_options(metac PUBLIC -fsanitize=address)
endif()
