cmake_minimum_required(VERSION 3.11...3.30)

project(
  yalc
  VERSION 0.1
  LANGUAGES C CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 11)

set(FETCHCONTENT_UPDATES_DISCONNECTED ON CACHE BOOL "Do not update everytime I run cmake, please")

# Find packages go here.
include(FetchContent)

FetchContent_Declare(
  munit_content
  GIT_REPOSITORY https://github.com/nemequ/munit.git
  GIT_TAG master
)

FetchContent_GetProperties(munit_content)
if(NOT munit_POPULATED)
  FetchContent_MakeAvailable(munit_content)

  add_library(munit ${munit_content_SOURCE_DIR}/munit.h ${munit_content_SOURCE_DIR}/munit.c)
  target_include_directories(munit PUBLIC ${munit_content_SOURCE_DIR})
  target_compile_definitions(munit PUBLIC -DMUNIT_ENABLE_ASSERT_ALIASES=1)
endif()

add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(app)
add_subdirectory(tests)
