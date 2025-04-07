set(SHORTY_SOURCES shorty/shorty.cppm shorty/literals.cppm shorty/leaves.cppm shorty/ops.cppm shorty/compatibility/pack-indexing.cppm shorty/compatibility/concepts.cppm)


if(SHORTY_IS_IN_MODULE)
	add_library(shorty)
	target_sources(shorty PUBLIC FILE_SET modules TYPE CXX_MODULES FILES ${SHORTY_SOURCES})
	target_compile_features(shorty PUBLIC cxx_std_26)
	target_include_directories(shorty PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
	#set_target_properties(shorty PROPERTIES CXX_MODULE_STD ON)
	target_compile_definitions(shorty PUBLIC SHORTY_IS_IN_MODULE)
else()
	add_library(shorty INTERFACE)
	list(TRANSFORM SHORTY_SOURCES REPLACE "(.*)\.cppm$" "\\1.hpp")
	target_sources(shorty INTERFACE FILE_SET headers TYPE HEADERS FILES ${SHORTY_SOURCES})
	target_compile_features(shorty INTERFACE cxx_std_26)
	target_include_directories(shorty INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
	
endif()
