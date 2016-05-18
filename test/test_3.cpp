/*<-

Copyright Barrett Adair 2016
Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

->*/

/* In this test, the last _1 placeholder in the bind
expression forces all other _1 slots to accept ScaryMonster,
because ScaryMonster is the narrowest of all _1 parameters. */

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
struct ScaryMonster : Vampire, Robot, Poodle {};

auto vampire_to_robot(Vampire) {
    return Robot{};
}

auto robot_to_dog = [](Robot){
    return Dog{};
};

struct converter {
    auto dog_to_vampire(Dog) {
        return Vampire{};
    }
};

int take_all(Vampire, Robot, Animal, Dog, Poodle, ScaryMonster) {
    return 0;
}

using namespace std::placeholders;

int main() {

    auto b =
        bind_parser::bind(&take_all,
            bind_parser::bind(&converter::dog_to_vampire,
                converter{},
                bind_parser::bind(robot_to_dog,
                    bind_parser::bind(&vampire_to_robot, _1)
                )
            ),
            bind_parser::bind(&vampire_to_robot, _3),
            Animal{},
            _1,
            _2,
            _1
        );

    {
        using args = bind_parser::args<decltype(b)>;
        using expect = std::tuple<ScaryMonster, Poodle, Vampire>;
        static_assert(std::is_same<args, expect>::value, "");
    }
    
    {
        using type = bind_parser::function_type<decltype(b)>;
        using expect = int(ScaryMonster, Poodle, Vampire);
        static_assert(std::is_same<type, expect>::value, "");
    }

    assert(b(ScaryMonster{}, Poodle{}, Vampire{}) == 0);

    return 0;
}
