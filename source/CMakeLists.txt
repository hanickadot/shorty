set(SHORTY_SOURCES shorty.cppm literals.cppm leaves.cppm ops.cppm compatibility/pack-indexing.cppm compatibility/concepts.cppm)

set(SHORTY_HEADERS ${SHORTY_SOURCES})
list(TRANSFORM SHORTY_HEADERS REPLACE "(.*)\.cppm$" "\\1.hpp")

list(TRANSFORM SHORTY_HEADERS PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/shorty/")
list(TRANSFORM SHORTY_SOURCES PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/shorty/")


if(SHORTY_IS_IN_MODULE)
	add_library(shorty)
	target_sources(shorty PUBLIC FILE_SET modules TYPE CXX_MODULES FILES ${SHORTY_SOURCES})
	target_compile_features(shorty PUBLIC cxx_std_26)
	target_include_directories(shorty PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
	#set_target_properties(shorty PROPERTIES CXX_MODULE_STD ON)
	target_compile_definitions(shorty PUBLIC SHORTY_IS_IN_MODULE)
else()
	add_library(shorty INTERFACE)
	target_sources(shorty INTERFACE FILE_SET headers TYPE HEADERS FILES ${SHORTY_HEADERS})
	target_compile_features(shorty INTERFACE cxx_std_26)
	target_include_directories(shorty INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
endif()

set(SHORTY_HEADERS ${SHORTY_HEADERS} PARENT_SCOPE)