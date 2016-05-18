
# Copyright Louis Dionne 2015
# Modified Work Copyright Barrett Adair 2015
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.0)
project(bind_parser CXX)
enable_testing()

if(BUILD_CXX_STD)
else()
  # Defaults to C++14 if not set:
  set(BUILD_CXX_STD 14) 
endif()

set (CMAKE_CXX_STANDARD ${bind_parser_CXX_STD})

# Setting up CMake options and compiler flags (more flags can be set on a per-target basis or in subdirectories)

include(CheckCXXCompilerFlag)
macro(bind_parser_append_flag testname flag)
    check_cxx_compiler_flag(${flag} ${testname})
    if (${testname})
        add_compile_options(${flag})
    endif()
endmacro()

bind_parser_append_flag(bind_parser_HAS_WERROR -Werror)
bind_parser_append_flag(bind_parser_HAS_WX -WX)
bind_parser_append_flag(bind_parser_HAS_QUNUSED_ARGUMENTS          -Qunused-arguments)
bind_parser_append_flag(bind_parser_HAS_WNO_UNUSED_LOCAL_TYPEDEFS  -Wno-unused-local-typedefs)
bind_parser_append_flag(bind_parser_HAS_WWRITE_STRINGS             -Wwrite-strings)

if(MSVC AND NOT ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
    #disable warning about symbol truncation. Doesn't matter, affected types are not linked
    bind_parser_append_flag(bind_parser_HAS_WD4503                     -wd4503)
    bind_parser_append_flag(bind_parser_HAS_W3                         -W3)
else()
    bind_parser_append_flag(bind_parser_HAS_W                          -W)
    bind_parser_append_flag(bind_parser_HAS_WALL                       -Wall)
    bind_parser_append_flag(bind_parser_HAS_WEXTRA                     -Wextra)
endif()

if(NOT MSVC)
    bind_parser_append_flag(bind_parser_HAS_FTEMPLATE_BACKTRACE_LIMIT  -ftemplate-backtrace-limit=0)
    bind_parser_append_flag(bind_parser_HAS_PEDANTIC                   -pedantic)
    bind_parser_append_flag(bind_parser_HAS_STDCXX1Y                   -std=c++1y)
endif()

##############################################################################
# bind_parser_target_name_for(<output variable> <source file> [ext])
# Returns the target name associated to a source file. If the path of the
# source file relative from the root of bind_parser is `path/to/source/file.ext`,
# the target name associated to it will be `path.to.source.file`.
#
# The extension of the file should be specified as a last argument. If no
# extension is specified, the `.cpp` extension is assumed.
##############################################################################

function(bind_parser_target_name_for out file)
    file(RELATIVE_PATH _relative ${bind_parser_SOURCE_DIR} ${file})
    string(REPLACE ".cpp" "" _name ${_relative})
    string(REGEX REPLACE "/" "." _name ${_name})
    set(${out} "${_name}" PARENT_SCOPE)
endfunction()


##############################################################################
# Setup the `check` target to build and then run all the tests and examples.
##############################################################################

add_custom_target(check
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Build and then run all the tests and examples.")

add_subdirectory(test)
