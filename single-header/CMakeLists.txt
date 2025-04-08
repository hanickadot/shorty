add_custom_target(single-header DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/shorty.hpp)

message(STATUS "${SHORTY_HEADERS}")

add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/shorty.hpp COMMAND ${CMAKE_COMMAND} 	-DINPUT=${CMAKE_SOURCE_DIR}/source/shorty/shorty.hpp
	-DTEMPORARY=${CMAKE_CURRENT_BINARY_DIR}/tmp.hpp
	-DTEMPLATE=${CMAKE_CURRENT_SOURCE_DIR}/shorty.hpp.in
	-DOUTPUT=${CMAKE_CURRENT_SOURCE_DIR}/shorty.hpp 
	-DINCLUDE_DIRECTORY=${CMAKE_SOURCE_DIR}/source/shorty
	-P ${CMAKE_CURRENT_SOURCE_DIR}/generate.cmake
	DEPENDS 
	${CMAKE_CURRENT_SOURCE_DIR}/generate.cmake
	${SHORTY_HEADERS}
)

add_library(shorty-single-header INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/shorty.hpp)
add_dependencies(shorty-single-header single-header)
target_compile_features(shorty-single-header INTERFACE cxx_std_26)
target_include_directories(shorty-single-header INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})