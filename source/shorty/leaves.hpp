#ifndef SHORTY_SHORTY_LEAVES_HPP
#define SHORTY_SHORTY_LEAVES_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#include "ops.hpp"
#include "compatibility/concepts.hpp"
#include "compatibility/pack-indexing.hpp"
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <concepts>
#define SHORTY_EXPORT
#endif // SHORTY_IS_IN_MODULE

namespace shorty {

template <typename...> struct identify;
template <auto...> struct identify_value;

template <typename Op, typename... Operands> struct node;

template <typename T> using select = std::remove_cvref_t<T>;

SHORTY_EXPORT struct argument_info;

template <typename T>
concept knows_minimal_argument_count = requires {
	requires std::same_as<std::remove_cvref_t<decltype(T::argc_at_least)>, std::size_t>;
};

template <typename T> concept knows_exact_argument_count = requires {
	requires std::same_as<std::remove_cvref_t<decltype(T::argc)>, std::size_t>;
};

template <typename T> concept can_calculate_exact_argument_count = requires {
	{ T::calculate_argument_count() } noexcept -> std::same_as<argument_info>;
};

struct ast_node;

#if __cpp_constexpr_exceptions >= 202411L
struct shorty_constraint_failure {
	const char * msg;

	static consteval auto inconsistent() noexcept {
		return shorty_constraint_failure{"inconsistent number of required arguments"};
	}

	static consteval auto not_enough() noexcept {
		return shorty_constraint_failure{"not enough arguments provided (expected more)"};
	}

	static consteval auto too_much() noexcept {
		return shorty_constraint_failure{"more arguments provided than needed"};
	}

	constexpr const char * what() const {
		return msg;
	}
};
#endif

SHORTY_EXPORT struct argument_info {
	std::size_t min = 0;
	std::size_t exact = std::dynamic_extent;
	bool consistent = true;

	consteval friend argument_info operator+(const argument_info & lhs, const argument_info & rhs) noexcept {
		return argument_info{
			.min = std::max(lhs.min, rhs.min),
			.exact = (lhs.exact == std::dynamic_extent) ? rhs.exact : lhs.exact,
			.consistent = lhs.exact == rhs.exact,
		};
	}

	consteval std::size_t exact_or(std::size_t provided) const noexcept {
		return exact == std::dynamic_extent ? provided : exact;
	}

	consteval bool validate_with(std::size_t provided) const {
		// TODO probably throw exception here so we have nice diag

		if (!consistent) {
#if __cpp_constexpr_exceptions >= 202411L
			throw shorty_constraint_failure::inconsistent();
#else
			return false;
#endif
		}

		if (min > provided) {
#if __cpp_constexpr_exceptions >= 202411L
			throw shorty_constraint_failure::not_enough();
#else
			return false;
#endif
		}

		if (exact != std::dynamic_extent && exact != provided) {
#if __cpp_constexpr_exceptions >= 202411L
			throw shorty_constraint_failure::too_much();
#else
			return false;
#endif
		}

		return true;
	}

	static constexpr argument_info from_min(std::size_t c) {
		return {.min = c, .exact = std::dynamic_extent, .consistent = true};
	}

	static constexpr argument_info from_exact(std::size_t c) {
		return {.min = c, .exact = c, .consistent = true};
	}

	template <typename T> static consteval argument_info from() {
		if constexpr (can_calculate_exact_argument_count<T>) {
			return T::calculate_argument_count();
		} else if constexpr (knows_minimal_argument_count<T>) {
			return argument_info::from_min(std::remove_cvref_t<T>::argc_at_least);
		} else if constexpr (knows_exact_argument_count<T>) {
			return argument_info::from_exact(std::remove_cvref_t<T>::argc);
		} else if constexpr (std::convertible_to<ast_node, T>) {
			return {};
		} else {
			return {};
		}
	}
};

template <typename Self, typename... Args> concept valid_call = []() -> bool {
	if constexpr (sizeof...(Args) == 1 && tuple_like<first<Args...>>) {
		const std::size_t count = std::tuple_size_v<std::remove_cvref_t<first<Args...>>>;
		return argument_info::from<std::remove_cvref_t<Self>>().validate_with(count);
	} else {
		return argument_info::from<std::remove_cvref_t<Self>>().validate_with(sizeof...(Args));
	}
}();

// basic ast_node which is used for ADL lookup of operators
struct ast_node {
	template <typename Lhs, typename Rhs> friend constexpr auto operator<(Lhs && lhs, Rhs && rhs) {
		return node<ops::less, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator<=(Lhs && lhs, Rhs && rhs) {
		return node<ops::less_equal, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator==(Lhs && lhs, Rhs && rhs) {
		return node<ops::equal, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator!=(Lhs && lhs, Rhs && rhs) {
		return node<ops::not_equal, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator>=(Lhs && lhs, Rhs && rhs) {
		return node<ops::greater_equal, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator>(Lhs && lhs, Rhs && rhs) {
		return node<ops::greater, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}

	template <typename Lhs, typename Rhs> friend constexpr auto operator+(Lhs && lhs, Rhs && rhs) {
		return node<std::plus<>, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator-(Lhs && lhs, Rhs && rhs) {
		return node<std::minus<>, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator*(Lhs && lhs, Rhs && rhs) {
		return node<std::multiplies<>, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator/(Lhs && lhs, Rhs && rhs) {
		return node<std::divides<>, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}
	template <typename Lhs, typename Rhs> friend constexpr auto operator%(Lhs && lhs, Rhs && rhs) {
		return node<std::modulus<>, select<Lhs>, select<Rhs>>{std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)};
	}

	template <typename Self> constexpr auto operator+(this Self && self) {
		return node<ops::unary_plus, select<Self>>{std::forward<Self>(self)};
	}
	template <typename Self> constexpr auto operator-(this Self && self) {
		return node<ops::unary_minus, select<Self>>{std::forward<Self>(self)};
	}
	template <typename Self, typename... Args> constexpr auto operator[](this Self && self, Args &&... args) {
		return node<ops::index, select<Self>, select<Args>...>{std::forward<Self>(self), std::forward<Args>(args)...};
	}

	constexpr auto deleted_operator_call() const = delete;

	// this is only called outside

	template <typename Self, typename... Args> constexpr auto operator()(this Self && self, Args &&... args) requires(valid_call<Self, Args...>)
	{
		if constexpr (sizeof...(Args) == 1 && tuple_like<first<Args...>>) {
			auto && first_arg = first_thing(std::forward<Args>(args)...);
#if __cpp_structured_bindings >= 202411L
			auto && [... subargs] = first_arg;
			return std::forward<Self>(self).operator()(std::forward<decltype(subargs)>(subargs)...);
#else
			return [&]<std::size_t... Idx>(std::index_sequence<Idx...>) {
				return std::forward<Self>(self).operator()(std::get<Idx>(first_arg)...);
			}(std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<decltype(first_arg)>>>());
#endif
		} else {
			return self.eval(std::forward<Args>(args)...);
		}
	}
};

template <typename T, typename... Args> constexpr decltype(auto) evaluate(T && obj, [[maybe_unused]] Args &&... args) {
	if constexpr (requires { obj(std::forward<Args>(args)...); }) {
		return obj.eval(std::forward<Args>(args)...);
	} else {
		return obj;
	}
}

template <typename Op, typename... Operands> struct node: ast_node {
	using base_node = node;

	static constexpr auto op = Op{};
	[[no_unique_address]] std::tuple<Operands...> ast_operands;

	node(node &&) = default;
	node(const node &) = default;
	constexpr node(std::convertible_to<Operands> auto &&... _ast_operands): ast_operands{std::forward<decltype(_ast_operands)>(_ast_operands)...} { }

	static consteval argument_info calculate_argument_count() noexcept {
		return (argument_info::from<std::remove_cvref_t<Operands>>() + ...); // merge together
	}

	template <typename... Args> constexpr auto eval(Args &&... args) const {
#if __cpp_structured_bindings >= 202411L
		auto && [... operands] = ast_operands;
		return op(evaluate(std::forward<decltype(operands)>(operands), std::forward<Args>(args)...)...);
#else
		return [&]<std::size_t... Idx>(std::index_sequence<Idx...>) {
			return op(evaluate(std::get<Idx>(ast_operands), std::forward<Args>(args)...)...);
		}(std::make_index_sequence<std::tuple_size<decltype(ast_operands)>::value>());
#endif
	}
};

// this will return Nth argument from the shorty's lambda
SHORTY_EXPORT template <unsigned N> struct nth_argument: ast_node {
	static constexpr std::size_t argc_at_least = N + 1;

	template <typename... Args> constexpr auto eval(Args &&... args) const {
		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		return args...[N];
#else
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
};

// same as `nth-argument` but need exactly `Count` arguments of shorty's lambda
SHORTY_EXPORT template <unsigned N, unsigned Count> struct nth_argument_of_k: ast_node {
	static constexpr std::size_t argc = Count;

	template <typename... Args> constexpr auto eval(Args &&... args) const {
		static_assert(Count == sizeof...(Args));
		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		return args...[N];
#else
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
};

static_assert(knows_exact_argument_count<nth_argument_of_k<2, 3>>);

// same as `nth-argument` but `Query` callable must return true for argument of `std::type_identity<T>`
SHORTY_EXPORT template <unsigned N, auto Query> struct nth_argument_with_query: ast_node {
	static constexpr std::size_t argc_at_least = N + 1;

	template <typename... Args> constexpr auto eval(Args &&... args) const {
		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		static_assert(Query(std::type_identity<Args... [N]>{}));
		return args...[N];
#else
		static_assert(Query(std::type_identity<decltype(extract_nth<N>(std::forward<Args>(args)...))>{}));
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
};

// number of arguments provided to shorty's lambda
SHORTY_EXPORT struct argument_count: ast_node {
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return sizeof...(Args);
	}
};

// wrap all provided arguments to shorty's lambda and return a tuple
SHORTY_EXPORT struct pack_arguments: ast_node {
	template <typename... Args> constexpr auto eval(Args &&... args) const {
		return std::tuple(std::forward<Args>(args)...);
	}
};

// store CNTTP constant
SHORTY_EXPORT template <auto V> struct constant: ast_node {
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return V;
	}
};

// store a value of any type
SHORTY_EXPORT template <typename T> struct value: ast_node {
	T data;
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return data;
	}
};

// store a reference to a string literals
SHORTY_EXPORT template <typename CharT, std::size_t N> struct static_string: ast_node {
	using type = const CharT (&)[N];
	type data;
	consteval static_string(const CharT (&_data)[N]) noexcept: data{_data} { }
	template <typename... Args> constexpr type eval(Args &&...) const {
		return data;
	}
};

// external const/non-const reference
SHORTY_EXPORT template <typename T> struct reference: ast_node {
	T & value;
	constexpr reference(T & _value) noexcept: value{_value} { }
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return value;
	}
};

} // namespace shorty

#endif // SHORTY_SHORTY_LEAVES_HPP
