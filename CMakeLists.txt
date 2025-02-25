cmake_minimum_required(VERSION 3.11...3.30)

project(
  yal
  VERSION 1.0
  LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_COLOR_DIAGNOSTICS ON)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    SET(CMAKE_ENABLE_EXPORTS TRUE)
endif()

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    include(CTest)
endif()

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(Dupe)

include(FetchContent)

FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG        11.1.3)
FetchContent_MakeAvailable(fmt)

FetchContent_Declare(
  cpptrace
  GIT_REPOSITORY https://github.com/jeremy-rifkin/cpptrace.git
  GIT_TAG        v0.7.5
)
FetchContent_MakeAvailable(cpptrace)

FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v3.4.0 # or a later release
)
FetchContent_MakeAvailable(Catch2)

add_subdirectory(src)
add_subdirectory(cmd)

# testing
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
    add_subdirectory(tests)
endif()

