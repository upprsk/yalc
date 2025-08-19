cmake_minimum_required(VERSION 3.11...3.31)

project(
  yal
  VERSION 1.0
  LANGUAGES C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_COLOR_DIAGNOSTICS ON)

add_compile_options(-Wall -Wextra)

add_subdirectory(src/ulib)

add_subdirectory(cmd/yalc)
add_subdirectory(cmd/metac)

