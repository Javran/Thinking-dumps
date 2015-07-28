project (javev C)
cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

# ==== BEGIN BOILERPLATE

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -Wall -Wextra -pedantic -std=c99")
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

###############################################################################
# Set build features
set(CMAKE_BUILD_TYPE Debug)

###############################################################################
include(CheckCSourceCompiles)
include(CheckCSourceRuns)
include(CheckFunctionExists)
include(CheckIncludeFile)
include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)

###############################################################################
# Check headers
set(INCLUDES "")
macro(ck_check_include_file header var)
  check_include_files("${INCLUDES};${header}" ${var})
  if(${var})
    set(INCLUDES ${INCLUDES} ${header})
  endif(${var})
endmacro(ck_check_include_file)

ck_check_include_file("stdlib.h" HAVE_STDLIB_H)

###############################################################################
# Check for integer types
# (The following are used in check.h. Regardless if they are used in
# the project, they will need to be checked in order to use Check).
check_type_size(intmax_t INTMAX_T)
check_type_size(uintmax_t UINTMAX_T)

check_type_size(pid_t PID_T)
if(NOT HAVE_PID_T)
  if(WIN32)
    set(pid_t "int")
  else(WIN32)
    MESSAGE(FATAL_ERROR "pid_t doesn't exist on this platform?")
  endif(WIN32)
endif(NOT HAVE_PID_T)

###############################################################################
# Check libraries

###############################################################################
# Generate "config.h" from "cmake/config.h.cmake"
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/config.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/config.h)
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})
add_definitions(-DHAVE_CONFIG_H)
set(CONFIG_HEADER ${CMAKE_CURRENT_BINARY_DIR}/config.h)


# ==== END BOILERPLATE

###############################################################################
# Subdirectories
add_subdirectory(tests)

set(javev_SRCS
  Util.c DynArr.c Frame.c Environment.c Evaluate.c FunctionObject.c
  Parser.c SExp.c Syntax.c Token.c Tokenizer.c PointerManager.c Primitives.c
  EvalSimple.c EvalSeq.c EvalCond.c EvalSetDefine.c EvalApp.c)

set(javev_HDRS
  Common.h
  Util.h DynArr.h Frame.h Environment.h Evaluate.h FunctionObject.h
  Parser.h SExp.h Syntax.h Token.h Tokenizer.h PointerManager.h Primitives.h
  EvalSimple.h EvalSeq.h EvalCond.h EvalSetDefine.h EvalApp.h)

add_library(javev ${javev_SRCS}  ${javev_HDRS})

add_executable(evaluator main.c ${javev_HDRS})
target_link_libraries(evaluator javev)

###############################################################################
# Unit tests

find_program( MEMORYCHECK_COMMAND valgrind )
set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full --error-exitcode=1" )

function(add_memcheck_test name binary)
  set(memcheck_command "${MEMORYCHECK_COMMAND} ${MEMORYCHECK_COMMAND_OPTIONS}")
  separate_arguments(memcheck_command)
  add_test(${name} ${binary} ${ARGN})
  add_test(memcheck_${name} ${memcheck_command} ./${binary} ${ARGN})
endfunction(add_memcheck_test)

function(set_memcheck_test_properties name)
  set_tests_properties(${name} ${ARGN})
  set_tests_properties(memcheck_${name} ${ARGN})
endfunction(set_memcheck_test_properties)

enable_testing()
add_memcheck_test(run_unit_tests tests/run_unit_tests)
