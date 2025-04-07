cmake_minimum_required(VERSION 4.0)

list(PREPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

option(SHORTY_IS_IN_MODULE "build shorty library as a c++ module" ON)

if (SHORTY_IS_IN_MODULE)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD "a9e1cf81-9932-4810-974b-6eccaf14e457")
set(CMAKE_COLOR_DIAGNOSTICS ON)

set(CMAKE_CXX_STANDARD_REQUIRED OFF)
set(CMAKE_CXX_MODULE_STD 1)
endif()

include(colors)

project(shorty VERSION 1.0 LANGUAGES CXX)


add_subdirectory(source)
add_subdirectory(single-header EXCLUDE_FROM_ALL)


add_executable(test test.cpp)
target_link_libraries(test PRIVATE shorty)

add_executable(test-single-header test-single-header.cpp)
target_link_libraries(test-single-header PRIVATE shorty-single-header)
