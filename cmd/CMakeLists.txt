add_executable(yalc
    main.cpp
)
target_include_directories(yalc PRIVATE .)
target_link_libraries(yalc PUBLIC yal)
target_compile_options(yalc PRIVATE -Wall -Wextra)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(yalc PUBLIC -fsanitize=address)
    target_link_options(yalc PUBLIC -fsanitize=address)
endif()
