/*

Copyright Barrett Adair 2016
Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

*/

#include <cassert>
#include <type_traits>
#include <functional>
#include <iostream>
#include <sstream>
#include <memory>
#include <iostream>
#include <cstdint>
#include "bind_parser.hpp"

struct Vampire {};
struct Robot {};
struct Animal {};
struct Dog : Animal {};
struct Poodle : Dog {};
struct ScaryMonster : Poodle, Robot, Vampire {};

int main() {

    {
        using test = bind_parser::detail::best_match<
            Vampire, Robot, Poodle, Animal, Dog, ScaryMonster>;

        using expect = ScaryMonster;

        static_assert(std::is_same<test, expect>::value, "");
    }
}
