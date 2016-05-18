/*<-

Copyright Barrett Adair 2016
Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

->*/

#undef NDEBUG


#include <type_traits>
#include <functional>
#include <tuple>
#include <cassert>

#include "bind_parser.hpp"

struct Vampire {};
struct Robot {};
struct Animal {};
struct Dog : Animal {};
struct Poodle : Dog {};
struct ScaryMonster : Poodle, Robot, Vampire {};

auto take_vampire(const Vampire&) { return 0; }
auto take_robot(const Robot&) { return 0; }
auto take_dog(const Dog&) { return 0; }
auto take_scary_monster(const ScaryMonster&) { return 0; }

int f(int, int, int, int) { return 0; }

using namespace std::placeholders;

int main() {

    ScaryMonster monster{};

    auto b = bind_parser::bind(
        &f,
        bind_parser::bind(&take_vampire, _1),
        bind_parser::bind(&take_robot, _1),
        bind_parser::bind(&take_dog, _1),
        bind_parser::bind(&take_scary_monster, _1)
    );

    {
        using args = bind_parser::args<decltype(b)>;
        using expect = std::tuple<const ScaryMonster&>;
        static_assert(std::is_same<args, expect>::value, "");
    }
    
    {
        using type = bind_parser::function_type<decltype(b)>;
        using expect = int(const ScaryMonster&);
        static_assert(std::is_same<type, expect>::value, "");
    }

    assert(b(monster) == 0);

    return 0;
}
