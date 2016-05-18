/*<-
Copyright Barrett Adair 2016
Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE.md or copy at http ://boost.org/LICENSE_1_0.txt)
->*/

#ifndef EXAMPLE_MAKE_FUNCTION_HPP
#define EXAMPLE_MAKE_FUNCTION_HPP

#include <functional>
#include <type_traits>
#include <utility>

#include "bind_parser.hpp"

namespace example {

    // make_function turns a non-overloaded callable into a type-erased std::function object
    template<typename T>
    inline decltype(auto) make_function(T&& t) {

        using no_ref = typename std::remove_reference<T>::type;
        using f = bind_parser::function_type<no_ref>;
        using result_type = std::function<f>;
        return result_type{ std::forward<T>(t) };
    }

    // this make_function overload turns a bind expression into a type-erased std::function object
    template<typename T, typename First, typename... Others>
    inline decltype(auto) make_function(T&& t, First&& first, Others&&... others) {

        // bind_parser::bind is a compile-time parser of placeholder expressions,
        // for the purpose of retaining more type information than std::bind normally
        // allows - specifically,  bind_parser::bind is used to determine the de-facto
        // signature of the std::bind return type, with special considerations for
        // conversions between reused placeholders and nested placeholder expressions.
        //  For the sake of convenience, bind_parser::bind is also a thin forwarding
        // wrapper around std::bind.

        using bind_expr = decltype(bind_parser::bind(
                std::forward<T>(t),
                std::forward<First>(first),
                std::forward<Others>(others)...
        ));

        using f = bind_parser::function_type<bind_expr>;
        using result_type = std::function<f>;

        return result_type{ std::bind(
                std::forward<T>(t),
                std::forward<First>(first),
                std::forward<Others>(others)...
        )};
    }
}
