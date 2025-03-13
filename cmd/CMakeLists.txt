add_executable(yalc
    argparser.cpp
    argparser.hpp
    main.cpp
)
target_include_directories(yalc PRIVATE .)
target_link_libraries(yalc PUBLIC yal)
target_compile_options(yalc PRIVATE -Wall -Wextra)
