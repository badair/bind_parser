/*

Copyright Barrett Adair 2016
Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

*/

#undef NDEBUG

#include <type_traits>
#include <functional>
#include <iostream>
#include <sstream>
#include <memory>
#include <iostream>
#include <cstdint>
#include <cassert>

#include "bind_parser.hpp"

using namespace std::placeholders;

struct Letter {
    virtual operator const char*() const volatile = 0;
};

#define DEFINE_TEST_LETTER(L) \
struct L : Letter { operator const char*() const volatile override { return #L; } }

DEFINE_TEST_LETTER(A);
DEFINE_TEST_LETTER(B);
DEFINE_TEST_LETTER(C);
DEFINE_TEST_LETTER(D);
DEFINE_TEST_LETTER(E);
DEFINE_TEST_LETTER(F);
DEFINE_TEST_LETTER(G);

// functions `ordered_letters`, `BEEF_returns_D`, `BEEF_returns_G`,
// and `BEEF_returns_B` are used to set up a complex bind expression
// with bind_parser::bind

auto ordered_letters(A a, B b, C c, D d, E e, F f, G g) {
    std::stringstream ss{};
    ss << a << b << c << d << e << f << g;
    return ss.str();
}

auto BEEF_returns_D(B, E, E, F) {
    return D{};
}

auto BEEF_returns_G(B, E, E, F) {
    return G{};
}

auto BEEF_returns_B(B, E, E, F) {
    return B{};
}

template <typename F, typename Tuple, std::size_t... I>
constexpr auto
apply_helper(F&& f, Tuple&& t, std::index_sequence<I...>) {
    return std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...);
}

//used to apply the expected_args tuple to std::bind
template <typename F, typename Tuple>
constexpr auto
apply(F&& f, Tuple&& t) {
    return apply_helper(
        std::forward<F>(f),
        std::forward<Tuple>(t),
        std::make_index_sequence<
            std::tuple_size<std::remove_reference_t<Tuple>>::value>{}
    );
}

const auto a = A{};
const auto b = B{};
const auto c = C{};
const auto d = D{};
const auto e = E{};
const auto f = F{};
const auto g = G{};

// This macro lets us create a complex bind expression
// with both `std::bind` and `bind_parser::bind`
#define BIND_WITH(bind_name)                     \
        bind_name(&ordered_letters,              \
            _1,                                  \
            _2,                                  \
            _3,                                  \
                bind_name(&BEEF_returns_D,       \
                _2,                              \
                e,                               \
                _4,                              \
                _7                               \
            ),                                   \
            _5,                                  \
            _6,                                  \
                bind_name(&BEEF_returns_G,       \
                    bind_name(&BEEF_returns_B,   \
                    b,                           \
                    _10,                         \
                    e,                           \
                    f                            \
                ),                               \
                _9,                              \
                e,                               \
                _8                               \
            )                                    \
        )                                        \
/**/

int main() {

    assert(ordered_letters(a, b, c, d, e, f, g) == "ABCDEFG");

    auto parser_bind = BIND_WITH(bind_parser::bind);
    auto std_bind = BIND_WITH(std::bind);

    // these are the argument types as we expect due to the arrangement
    // of the bind expression's placeholders
    using expected_args = std::tuple<A, B, C, E, E, F, F, F, E, E>;
    using args = bind_parser::args<decltype(parser_bind)>;

    static_assert(std::is_same<args, expected_args>::value, "");
    assert(apply(parser_bind, expected_args{}) == "ABCDEFG");
    assert(apply(std_bind, expected_args{}) == "ABCDEFG");
}
