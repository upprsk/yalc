add_executable(yalc
    main.c
)

target_include_directories(yalc PUBLIC .)
target_link_libraries(yalc PUBLIC ulib)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(yalc PUBLIC -fsanitize=address)
    target_link_options(yalc PUBLIC -fsanitize=address)
endif()
