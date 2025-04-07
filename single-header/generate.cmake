message(STATUS "Generating shorty.hpp")

# use QUOM to create basic single header
set(COMMAND python3 -m quom ${INPUT} tmp.hpp --include_guard "SHORTY_SHORTY_.*" --include_directory ${INCLUDE_DIRECTORY} --trim)

execute_process(COMMAND ${COMMAND} ERROR_VARIABLE ERRORS OUTPUT_VARIABLE OUTPUTS RESULT_VARIABLE RESULT_VAR)

file (READ tmp.hpp CONTENT_TMP)

string(REGEX REPLACE "[^\n]*SHORTY_IS_IN_MODULE" "" CONTENT_TMP2 "${CONTENT_TMP}")
string(REGEX REPLACE "#define SHORTY_EXPORT[^\n]*\n" "" CONTENT_TMP3 "${CONTENT_TMP2}")
string(REGEX REPLACE "SHORTY_EXPORT *" "" CONTENT_TMP4 "${CONTENT_TMP3}")

#message(STATUS "${CONTENT_TMP4}")

set(CONTENT_OF_SINGLE_HEADER "${CONTENT_TMP4}")

configure_file(${TEMPLATE} ${OUTPUT} @ONLY)

execute_process(COMMAND clang-format -i ${OUTPUT} RESULT_VARIABLE RESULT_VAR)

#message(STATUS "clang-format = ${RESULT_VAR}")

