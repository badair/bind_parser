
# Copyright Louis Dionne 2015
# Modified Work Copyright Barrett Adair 2015
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

add_custom_target(tests COMMENT "Build all the unit tests.")

##############################################################################
#   bind_parser_add_unit_test(<name> ...)
#
# Equivalent to `bind_parser_add_test`, except the test is also added as a
# dependency of the `tests` target.
##############################################################################

include_directories(${bind_parser_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_LIST_DIR})

file(GLOB_RECURSE UNIT_TESTS "*.cpp")

foreach(_file IN LISTS UNIT_TESTS)
    bind_parser_target_name_for(_target "${_file}")
    add_executable(${_target} EXCLUDE_FROM_ALL "${_file}")
    add_dependencies(tests ${_target})
    add_test(${_target} ${CMAKE_CURRENT_BINARY_DIR}/${_target})
endforeach()

add_dependencies(check tests)


