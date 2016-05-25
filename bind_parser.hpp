/*!
Copyright (c) 2001 David Abrahams
Copyright (c) 2005 Peter Dimov
Copyright (c) 2013-2016 Louis Dionne
Copyright (c) 2016 Barrett Adair

Distributed under the Boost Software License, Version 1.0.
(See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)
*/

#ifndef BIND_PARSER_HPP
#define BIND_PARSER_HPP

#include <tuple>
#include <type_traits>
#include <functional>
#include <utility>
#include <callable_traits/args.hpp>
#include <callable_traits/result_of.hpp>
#include <callable_traits/function_type.hpp>

namespace bind_parser {

    namespace detail {

        template<typename Callable, typename... Args>
        struct bind_expression;

        template<typename BindExpr>
        struct bind_expression_parser;

        struct invalid_type{};

        // a faster version of std::decay_t
        template<typename T>
        using shallow_decay = typename std::remove_cv<
            typename std::remove_reference<T>::type
        >::type;

        // shorthand for std::tuple_element_t
        template<std::size_t I, typename Tup>
        using at = typename std::tuple_element<I, Tup>::type;

        //used to prepend a type to a tuple
        template <typename...> struct prepend;

        template <> struct prepend<> {
            using type = std::tuple<>;
        };

        template <typename T, typename... Args>
        struct prepend<T, std::tuple<Args...> > {
            using type = std::tuple<T, Args...>;
        };

        template<typename...>
        struct build_function_t;

        template<typename Return, typename... Args>
        struct build_function_t<Return, std::tuple<Args...>>{
            using type = Return(Args...);
        };

        template<typename Ret, typename Tup>
        using build_function =
            typename build_function_t<Ret, Tup>::type;

        using empty_seq = std::index_sequence<>;

        template <typename Pred, std::size_t Insert, bool IsInsertionPoint,
            typename Left, std::size_t ...Right>
        struct insert;

        // We did not find the insertion point; continue processing elements
        // recursively.
        template <typename Pred, std::size_t Insert, std::size_t ...Left,
            std::size_t Right1, std::size_t Right2, std::size_t ...Right>
        struct insert<Pred, Insert, false, std::index_sequence<Left...>,
            Right1, Right2, Right...> {
            using type = typename insert<
                Pred, Insert, Pred::template apply<Insert, Right2>::value,
                std::index_sequence<Left..., Right1>,
                Right2, Right...
            >::type;
        };

        // We did not find the insertion point, but there is only one element
        // left. We insert at the end of the list, and we're done.
        template <typename Pred, std::size_t Insert, std::size_t ...Left, std::size_t Last>
        struct insert<Pred, Insert, false, std::index_sequence<Left...>, Last> {
            using type = std::index_sequence<Left..., Last, Insert>;
        };

        // We found the insertion point, we're done.
        template <typename Pred, std::size_t Insert, std::size_t ...Left, std::size_t ...Right>
        struct insert<Pred, Insert, true, std::index_sequence<Left...>, Right...> {
            using type = std::index_sequence<Left..., Insert, Right...>;
        };

        template <typename Pred, typename Result, std::size_t ...T>
        struct insertion_sort;

        template <typename Pred, std::size_t Result1, std::size_t ...Results,
            std::size_t T, std::size_t ...Ts>
        struct insertion_sort<Pred, std::index_sequence<Result1, Results...>, T, Ts...> {
            static constexpr bool pred_result = Pred::template apply<T, Result1>::value;
            using insert_result = typename insert<
                Pred, T, pred_result, empty_seq, Result1, Results...
            >::type;
            using type = typename insertion_sort<Pred, insert_result, Ts...>::type;
        };

        template <typename Pred, std::size_t I, std::size_t ...Is>
        struct insertion_sort<Pred, empty_seq, I, Is...> {
            using type = typename insertion_sort<
                Pred, std::index_sequence<I>, Is...
            >::type;
        };

        template <typename Pred, typename Result>
        struct insertion_sort<Pred, Result> {
            using type = Result;
        };

        template <typename Pred, typename Indices>
        struct sort_indices;

        template <typename Pred, std::size_t ...i>
        struct sort_indices<Pred, std::index_sequence<i...>> {
            using type = typename insertion_sort<Pred, empty_seq, i...>::type;
        };

        template<typename Tup, typename Pred>
        struct sort_impl {
            static constexpr std::size_t len = std::tuple_size<Tup>::value;
            using indices = typename sort_indices<Pred, std::make_index_sequence<len>>::type;
            using type = typename sort_impl<Tup, indices>::type;
        };

        template <typename Tup, std::size_t ...I>
        struct sort_impl<Tup, std::index_sequence<I...>> {
            using type = std::tuple<at<I, Tup>...>;
        };

        template <typename Tup, template<class, class> class Pred>
        struct predicate {
            template <std::size_t I, std::size_t J>
            using apply = Pred<at<I, Tup>, at<J, Tup>>;
        };

        template<typename Tup, template<class, class> class Pred>
        using tuple_sort = typename sort_impl<Tup, predicate<Tup, Pred>>::type;

                template<typename T, template<class> class Pred>
        struct group_by_value {
            static constexpr auto value = Pred<T>::value;
        };

        template<typename Tup, template<class> class Pred>
        struct distinct_group_by_values;

        template<typename Last, template<class> class Pred>
        struct distinct_group_by_values<std::tuple<Last>, Pred> {
            using type = std::tuple<group_by_value<Last, Pred>>;
        };

        template<typename Head, typename Next, typename... Tail, template<class> class Pred>
        struct distinct_group_by_values<std::tuple<Head, Next, Tail...>, Pred> {

            static constexpr const auto is_in_same_group =
                Pred<Head>::value == Pred<Next>::value;

            using next = typename distinct_group_by_values<std::tuple<Next, Tail...>, Pred>::type;

            using type = typename std::conditional<
                is_in_same_group,
                next,
                typename prepend<group_by_value<Head, Pred>, next>::type
            >::type;
        };

        template <typename Pred, typename...> struct group_by_filter_impl;

        template <typename Pred> struct group_by_filter_impl<Pred> {
            using type = std::tuple<>;
        };

        template <typename Pred, typename Head, typename ...Tail>
        struct group_by_filter_impl<Pred, Head, Tail...> {
            using pred_result = decltype(std::declval<Pred>()(std::declval<Head>()));
            using type = typename std::conditional<
                pred_result::value,
                typename prepend<Head, typename group_by_filter_impl<Pred, Tail...>::type>::type,
                typename group_by_filter_impl<Pred, Tail...>::type
            >::type;
        };

        template <typename Pred, typename Tup>
        struct group_by_filter;

        template <typename Pred, typename... Ts>
        struct group_by_filter<Pred, std::tuple<Ts...>> {
            using type = typename group_by_filter_impl<Pred, Ts...>::type;
        };

        template<typename DistinctGroupByValues, template<class> class Pred, std::size_t I>
        struct filter_predicate {

            using compare_against = at<I, DistinctGroupByValues>;

            template<typename T>
            auto operator()(T) ->
                std::integral_constant<bool, Pred<T>::value == compare_against::value>;
        };

        template<typename Tup, typename DistinctGroupByValues, template<class> class Pred, typename ValuesSeq>
        struct group_by_impl;

        template<typename Tup, typename DistinctGroupByValues, template<class> class Pred, std::size_t... I>
        struct group_by_impl<Tup, DistinctGroupByValues, Pred, std::index_sequence<I...>> {

            using type = std::tuple<
                typename group_by_filter<
                    filter_predicate<DistinctGroupByValues, Pred, I>,
                    Tup
                >::type...
            >;
        };

        template<typename Tup, template<class> class Pred>
        struct group_by_t {

            template<typename T, typename U>
            struct sort_predicate {
                static constexpr const auto left = Pred<T>::value;
                static constexpr const auto right = Pred<U>::value;
                static constexpr const bool value = left < right;
            };

            using group_by_values = typename distinct_group_by_values<Tup, Pred>::type;

            using type = typename group_by_impl<
                tuple_sort<Tup, sort_predicate>,
                group_by_values,
                Pred,
                std::make_index_sequence<std::tuple_size<group_by_values>::value>
            >::type;
        };

        template<typename Tup, template<class> class Pred>
        using tuple_group_by = typename group_by_t<Tup, Pred>::type;

        //template_worm is only used in unevaluated contexts
        struct template_worm {

            template<typename T>
            operator T& () const;

            template<typename T>
            operator T&& () const;

            template_worm() = default;

            template<typename... T>
            template_worm(T&&...);

            template_worm operator+() const;
            template_worm operator-() const;
            template_worm operator*() const;
            template_worm operator&() const;
            template_worm operator!() const;
            template_worm operator~() const;
            template_worm operator()(...) const;
        };

#define BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(...)  \
                                                            \
template<typename T>                                        \
constexpr inline auto                                       \
__VA_ARGS__ (template_worm, T&&) -> template_worm;          \
                                                            \
template<typename T>                                        \
constexpr inline auto                                       \
__VA_ARGS__ (T&&, template_worm) -> template_worm;          \
                                                            \
constexpr inline auto                                       \
__VA_ARGS__ (template_worm, template_worm) -> template_worm;\
/**/

        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator+)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator-)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator/)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator*)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator==)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator!=)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator&&)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator||)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator|)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator&)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator%)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator,)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator<<)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator>>)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator<)
        BIND_PARSER_TEMPLATE_WORM_BINARY_OPERATOR(operator>)

        template<int I>
        struct placeholder {

            placeholder() = default;

            template<typename T>
            placeholder(T const &) {
                static_assert(I == std::is_placeholder<T>::value, "Invalid placeholder");
            }
        };
    }
}

namespace std {

    template<int I>
    struct is_placeholder< bind_parser::detail::placeholder<I> > {
        static constexpr const int value = I;
    };
}

namespace bind_parser {

    namespace detail {

        template<typename Expression, std::size_t OriginalArgIndex, std::size_t PhValue>
        struct ph_route {
            using expression = Expression;
            static constexpr const auto original_arg_index = OriginalArgIndex;
            static constexpr const auto ph_value = PhValue;
        };

        template<typename Expression, typename, typename> struct argument_routing {};

        template<typename Expression, std::size_t... I, typename Tuple>
        struct argument_routing<Expression, std::index_sequence<I...>, Tuple> {
            using type =
                std::tuple<
                    ph_route<
                        Expression,
                        I,
                        std::is_placeholder<typename std::tuple_element<I, Tuple>::type>::value
                    >...
                >;
        };

        //base case
        template <typename...> struct placeholder_routes_detail;
        template <> struct placeholder_routes_detail<> {
            using type = std::tuple<>;
        };

        template <typename Head, typename ...Tail>
        struct placeholder_routes_detail<Head, Tail...> {
            //TODO - is there a faster way to do this?
            using type = typename std::conditional<
                Head::ph_value == 0,
                typename placeholder_routes_detail<Tail...>::type,
                typename prepend<
                    Head,
                    typename placeholder_routes_detail<Tail...>::type
                >::type
            >::type;
        };

        template <typename... Args>
        struct placeholder_routes_detail<std::tuple<Args...> > {
            using type = typename placeholder_routes_detail<Args...>::type;
        };

        template<typename PhLeft, typename PhRight>
        struct compare_placeholders {
            static constexpr bool value =
                std::is_placeholder<PhLeft>::value < std::is_placeholder<PhRight>::value;
        };

        template <typename Expression, typename...> struct placeholder_routes;
        template <typename Expression> struct placeholder_routes<Expression> { using type = std::tuple<>; };

        template <typename Expression, typename... Args>
        struct placeholder_routes<Expression, std::tuple<Args...> >
        {
            using routed_placeholders = typename placeholder_routes_detail<
                typename argument_routing<
                    Expression,
                    std::make_index_sequence<sizeof...(Args)>,
                    std::tuple<Args...>
                >::type
            >::type;

            using type = tuple_sort<routed_placeholders, compare_placeholders>;
        };

        template<typename T>
        struct bind_value {};

        template<typename T, typename NoRef>
        struct categorize_bind_arg {
            using type = typename std::conditional<
                std::is_placeholder< NoRef >::value == 0,
                bind_value<T>,
                placeholder<std::is_placeholder< NoRef >::value>
            >::type;
        };

        template<typename T, typename Ref>
        struct categorize_bind_arg< Ref, bind_value<T>> {
            using type = detail::bind_value<T>;
        };

        template<typename T, typename Ref>
        struct categorize_bind_arg< Ref, std::reference_wrapper<T> > {
            using type = std::reference_wrapper<T>;
        };

        template<int I, typename Ref>
        struct categorize_bind_arg< Ref, placeholder<I> > {
            using type = placeholder<I>;
        };

        template<typename Ref, typename Callable, typename... Args>
        struct categorize_bind_arg<Ref, bind_expression<Callable, Args...>> {

            using return_type = typename bind_expression<Callable, Args...>::return_type;

            using type = typename std::conditional<
                std::is_same<return_type, invalid_type>::value,
                template_worm,
                return_type
            >::type;
        };

        template <typename T>
        struct is_parser_bind
            : std::false_type {};

        template <typename Callable, typename... Args>
        struct is_parser_bind<bind_expression<Callable, Args...>>
            : std::true_type {};

        template<typename PhRouteLeft, typename PhRouteRight>
        struct compare_ph_value {
            static constexpr bool value =
                PhRouteLeft::ph_value < PhRouteRight::ph_value;
        };

        template <typename...> struct bind_expressions_filter;

        template <> struct bind_expressions_filter<> {
            using type = std::tuple<>;
        };

        template <typename Head, typename... Tail>
        struct bind_expressions_filter<Head, Tail...> {

            using filtered_tail = typename bind_expressions_filter<Tail...>::type;
            using decayed_head = shallow_decay<Head>;

            using type = typename std::conditional<
                is_parser_bind<decayed_head>::value,
                typename prepend<decayed_head, filtered_tail>::type,
                filtered_tail
            >::type;
        };

        template <typename... Ts>
        struct remove_non_bind_expressions {
            using type = typename bind_expressions_filter<Ts...>::type;
        };

        template<typename T>
        struct is_empty_tuple : std::false_type{};

        template<>
        struct is_empty_tuple<std::tuple<>> : std::true_type{};

        template<typename T>
        struct is_not_empty_tuple : std::true_type {};

        template<>
        struct is_not_empty_tuple<std::tuple<>> : std::false_type {};

        template <typename BindExpr, typename = std::true_type>
        struct flatten_bind_expressions;

        template <typename... BindExprs>
        struct flatten_bind_expressions<
            std::tuple<BindExprs...>,
            std::true_type
        > {
            using type = decltype(std::tuple_cat(
                std::declval<typename flatten_bind_expressions<BindExprs>::type>()...
            ));
        };

        template <typename BindExpr>
        struct flatten_bind_expressions<
            BindExpr,
            typename is_not_empty_tuple<typename BindExpr::inner_bind_expressions>::type
        > {
            using type = typename prepend<
                BindExpr,
                typename flatten_bind_expressions<
                    typename BindExpr::inner_bind_expressions
                >::type
            >::type;
        };

        //base case - bind expression has no inner bind expressions
        template <typename BindExpr>
        struct flatten_bind_expressions<
            BindExpr,
            typename is_empty_tuple<typename BindExpr::inner_bind_expressions>::type
        > {
            using type = std::tuple<BindExpr>;
        };

        template<typename T, typename std::enable_if<
            is_parser_bind<shallow_decay<T>>::value, int>::type = 0>
        inline constexpr decltype(auto)
        unwrap_std_bind(T&& t){
            return t.get_std_bind();
        }

        template<typename T, typename std::enable_if<
            !is_parser_bind<shallow_decay<T>>::value, int>::type = 0>
        inline constexpr T&& unwrap_std_bind(T&& t){
            return std::forward<T>(t);
        }

                template <template<class> class, typename...>
        struct filter_impl;

        template <template<class> class Pred>
        struct filter_impl<Pred> {
            using type = std::tuple<>;
        };

        template <template<class> class Pred, typename Head, typename ...Tail>
        struct filter_impl<Pred, Head, Tail...>
        {
            using type = typename std::conditional<Pred<Head>::value,
                typename prepend<Head, typename filter_impl<Pred, Tail...>::type>::type,
                typename filter_impl<Pred, Tail...>::type
            >::type;
        };

        template<template<class> class Pred, typename... Ts>
        using filter = typename filter_impl<Pred, Ts...>::type;

        template<typename T>
        struct can_convert {

            using is_non_reference = std::is_same<T, typename std::remove_reference<T>::type>;

            using arg_type = typename std::conditional<
                is_non_reference::value,
                T&,
                T
            >::type;

            //todo - this should probably use std::is_convertible instead
            template<typename K>
            struct apply {

                struct bad{};

                template<typename U, typename Ret = decltype(std::declval<U>()(std::declval<arg_type>()))>
                static Ret test(U);

                template<typename>
                static bad test(...);

                using test_type = void(*)(K);
                using result = decltype(test<test_type>(test_type{}));

                static constexpr const bool value = !std::is_same<result, bad>::value;
            };
        };

        template<typename T, typename Tup>
        struct conversion_result {
            using key = T;
            using successful_conversions = Tup;
            static constexpr const std::size_t count = std::tuple_size<Tup>::value;
        };

        template<typename T, typename... Ts>
        using map_conversions =
            conversion_result<T, filter<can_convert<T>::template apply, Ts...>>;

        template<typename T, typename U>
        struct conversion_result_sort_predicate {

            using candidate = typename T::key;
            using other = typename U::key;

            using no_ref = typename std::remove_reference<candidate>::type;
            using no_ref_other = typename std::remove_reference<other>::type;

            static constexpr bool const is_better_match = T::count > U::count;
            static constexpr bool const is_same_match = T::count == U::count;

            static constexpr bool const is_lref = std::is_lvalue_reference<candidate>::value;
            static constexpr bool const is_rref = std::is_rvalue_reference<candidate>::value;
            static constexpr bool const is_ref = is_lref || is_rref;
            static constexpr bool const is_const = std::is_const<no_ref>::value;

            static constexpr bool const is_other_lref = std::is_lvalue_reference<other>::value;
            static constexpr bool const is_other_rref = std::is_rvalue_reference<other>::value;
            static constexpr bool const is_other_ref = is_other_lref || is_other_rref;
            static constexpr bool const is_other_const = std::is_const<no_ref_other>::value;

            static constexpr bool const has_better_reference =
                (!is_ref && is_other_ref) || (is_lref && is_other_rref);

            static constexpr bool const has_same_reference =
                is_lref == is_other_lref && is_rref == is_other_rref;

            static constexpr bool const has_better_const = (is_const && !is_other_const);

            static constexpr bool const has_same_const =is_const == is_other_const;

            static constexpr const bool value =
                is_better_match
                || (is_same_match && has_better_reference)
                || (is_same_match && has_same_reference && has_better_const);
        };

        template<typename... Ts>
        struct sorted_cartesian_product_of_conversions {
            using type = tuple_sort<
                std::tuple<map_conversions<Ts, Ts...>...>,
                conversion_result_sort_predicate
            >;
        };

        template<typename... Ts>
        using best_conversion_result = at<0,
            typename sorted_cartesian_product_of_conversions<Ts...>::type>;

        template<typename... Ts>
        using has_valid_match = std::integral_constant<bool,
            best_conversion_result<Ts...>::count == sizeof...(Ts)
        >;

        template<typename T>
        struct is_invalid : std::is_same<T, invalid_type>::type {};

        template<typename... Ts>
        using remove_invalid_types = filter<is_invalid, Ts...>;

        template<typename... Ts>
        struct best_match_t {

            using has_valid = has_valid_match<Ts...>;

            static_assert(has_valid::value, "Conversion not found for all parameter types.");

            using result = typename best_conversion_result<Ts...>::key;

            using type = typename std::enable_if<
                has_valid::value,
                result
            >::type;
        };

        template<typename... Ts>
        using best_match = typename best_match_t<Ts...>::type;

        template<typename Callable, typename... Args>
        struct bind_expression {

            private:

            using bind_type = typename std::remove_reference<decltype(
                std::bind(std::declval<Callable>(), unwrap_std_bind(std::declval<Args>())...)
            )>::type;

            bind_type std_bind;

            public:

            using bind_args_tuple = std::tuple<
                typename categorize_bind_arg<
                    Args,
                    typename std::remove_reference<Args>::type
                >::type...
            >;

            using inner_bind_expressions =
                typename remove_non_bind_expressions<Args...>::type;

            using flattened_bind_expressions =
                typename flatten_bind_expressions<bind_expression>::type;

            using placeholder_route_map = typename placeholder_routes<
                bind_expression,
                bind_args_tuple
            >::type;

            using no_ref = typename std::remove_reference<Callable>::type;
            using original_args = callable_traits::args<no_ref>;
            using return_type = callable_traits::result_of<no_ref>;
            using result_type = return_type;

            inline bind_type&
            get_std_bind() & {
                return std_bind;
            }

            inline bind_type&&
            get_std_bind() && {
                return std::move(std_bind);
            }

            inline constexpr
            bind_expression(Callable c, Args... args)
                : std_bind(
                    std::bind(static_cast<Callable>(c),
                        unwrap_std_bind(static_cast<Args>(args))...)) {}

            template<typename... Rgs>
            inline decltype(auto)
            operator()(Rgs&&... args) {
                return std_bind(std::forward<Rgs>(args)...);
            }

            inline operator bind_type&() {
                return std_bind;
            }
        };

        template<typename BindExpressionTuple>
        struct combine_placeholder_routes;

        template<typename... BindExprs>
        struct combine_placeholder_routes<std::tuple<BindExprs...>> {

            using placeholder_route_map = decltype(std::tuple_cat(std::declval<
                typename placeholder_routes<
                    BindExprs,
                    typename BindExprs::bind_args_tuple
            >::type>()...));

            using type = tuple_sort<
                placeholder_route_map,
                compare_ph_value
            >;
        };

        template<typename T>
        struct ph_value_wrapper {
            static constexpr const auto value = T::ph_value;
        };

        template <typename...> struct filter_invalid_args;

        template <> struct filter_invalid_args<> {
            using type = std::tuple<>;
        };

        template <typename Head, typename ...Tail>
        struct filter_invalid_args<Head, Tail...> {

            static constexpr const auto is_legal_arg =
                !std::is_same<Head, template_worm>{}
                && !std::is_same<Head, invalid_type>{};

            using type = typename std::conditional<
                is_legal_arg,
                typename prepend<Head, typename filter_invalid_args<Tail...>::type>::type,
                typename filter_invalid_args<Tail...>::type
            >::type;
        };

        template<typename Tup>
        struct valid_args;

        template<typename... Ts>
        struct valid_args<std::tuple<Ts...>> {
            using filtered_args = typename filter_invalid_args<Ts...>::type;
            using type = typename std::conditional<
                std::is_same<filtered_args, std::tuple<>>::value,
                invalid_type,
                filtered_args
            >::type;
        };

        template<typename Tup>
        struct find_best_match;

        template<typename... Ts>
        struct find_best_match<std::tuple<Ts...>> {
            using type = best_match<Ts...>;
        };

        template<typename Tup>
        struct common_expected_arg_types;

        template<typename... PhRoutes, typename... OtherGroupTuples>
        struct common_expected_arg_types<std::tuple<std::tuple<PhRoutes...>, OtherGroupTuples...>> {

            using expected_types = std::tuple<
                at<PhRoutes::original_arg_index, typename PhRoutes::expression::original_args>...>;

            using type = typename prepend<
                typename find_best_match<typename valid_args<expected_types>::type>::type,
                typename common_expected_arg_types<std::tuple<OtherGroupTuples...>>::type
            >::type;
        };

        template<>
        struct common_expected_arg_types<std::tuple<>> {
            using type = std::tuple<>;
        };

        template<typename T>
        struct bind_expression_parser
        {
            static constexpr const bool value = false;

            using arg_types = invalid_type;
            using return_type = invalid_type;
            using function_type = invalid_type;
        };

        template<typename Callable, typename... Args>
        struct bind_expression_parser<bind_expression<Callable, Args...>> {

            static constexpr const bool value = true;

            using root_expression = bind_expression<Callable, Args...>;

            using flattened_bind_expressions =
                typename root_expression::flattened_bind_expressions;

            using placeholder_routes = typename combine_placeholder_routes<
                flattened_bind_expressions
            >::type;

            using grouped_placeholders = tuple_group_by<
                placeholder_routes,
                ph_value_wrapper
            >;

            using args = typename common_expected_arg_types<grouped_placeholders>::type;
            using return_type = typename root_expression::return_type;
            using function_type = build_function<return_type, args>;
        };
    }

    template<typename BindExpr>
    using args = typename detail::bind_expression_parser<BindExpr>::args;

    template<typename BindExpr>
    using result_of = typename detail::bind_expression_parser<BindExpr>::return_type;

    template<typename BindExpr>
    using function_type = typename detail::bind_expression_parser<BindExpr>::function_type;

    template<typename T, typename... Args>
    inline constexpr auto
    bind(T&& t, Args&&... args) ->
        detail::bind_expression<T&&, Args&&...> {

        return {
            ::std::forward<T>(t),
            ::std::forward<Args>(args)...
        };
    }

    // make_function turns a non-overloaded callable into a type-erased std::function object
    template<typename T>
    inline decltype(auto) make_function(T&& t) {

        using no_ref = typename ::std::remove_reference<T>::type;
        using f = ::callable_traits::function_type<no_ref>;
        return ::std::function<f>{ ::std::forward<T>(t) };
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

        using bind_expr = decltype( ::bind_parser::bind(
                ::std::forward<T>(t),
                ::std::forward<First>(first),
                ::std::forward<Others>(others)...
        ));

        using f = ::bind_parser::function_type<bind_expr>;

        return ::std::function<f>{ ::std::bind(
                ::std::forward<T>(t),
                ::std::forward<First>(first),
                ::std::forward<Others>(others)...
        )};
    }
}

#endif //#ifndef BIND_PARSER_HPP
