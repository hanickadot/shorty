add_executable(test test.cpp)
target_link_libraries(test PRIVATE shorty)

add_executable(test-single-header test-single-header.cpp)
target_link_libraries(test-single-header PRIVATE shorty-single-header)

add_custom_target(examples DEPENDS test test-single-header)
