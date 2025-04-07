#ifndef SHORTY_SHORTY_HPP
#define SHORTY_SHORTY_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#ifndef SHORTY_SHORTY_LITERALS_HPP
#define SHORTY_SHORTY_LITERALS_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#ifndef SHORTY_SHORTY_LEAVES_HPP
#define SHORTY_SHORTY_LEAVES_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#ifndef SHORTY_SHORTY_OPS_HPP
#define SHORTY_SHORTY_OPS_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#include <utility>
#define SHORTY_EXPORT
#endif // SHORTY_IS_IN_MODULE

namespace shorty::ops {
struct compare {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) <=> std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) <=> std::forward<decltype(rhs)>(rhs);
	}
};
struct less {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) < std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) < std::forward<decltype(rhs)>(rhs);
	}
};
struct less_equal {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) <= std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) <= std::forward<decltype(rhs)>(rhs);
	}
};
struct equal {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) == std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) == std::forward<decltype(rhs)>(rhs);
	}
};
struct not_equal {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) != std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) != std::forward<decltype(rhs)>(rhs);
	}
};
struct greater_equal {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) >= std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) >= std::forward<decltype(rhs)>(rhs);
	}
};
struct greater {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) > std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) > std::forward<decltype(rhs)>(rhs);
	}
};
struct unary_plus {
	static constexpr auto operator()(auto && val) noexcept(noexcept(+std::forward<decltype(val)>(val))) {
		return +std::forward<decltype(val)>(val);
	}
};
struct unary_minus {
	static constexpr auto operator()(auto && val) noexcept(noexcept(-std::forward<decltype(val)>(val))) {
		return -std::forward<decltype(val)>(val);
	}
};
struct index {
	static constexpr auto operator()(auto && subject, auto &&... args) noexcept(noexcept(std::forward<decltype(subject)>(subject)[std::forward<decltype(args)>(args)...])) {
		return std::forward<decltype(subject)>(subject)[std::forward<decltype(args)>(args)...];
	}
};
} // namespace shorty::ops

#endif // SHORTY_SHORTY_OPS_HPP

#ifndef SHORTY_SHORTY_COMPATIBILITY_CONCEPTS_HPP
#define SHORTY_SHORTY_COMPATIBILITY_CONCEPTS_HPP

#ifdef SHORTY_IS_IN_MODULE
// nothing :)
#else // SHORTY_IS_IN_MODULE
#include <array>
#include <ranges>
#include <tuple>
#include <complex>
#endif // SHORTY_IS_IN_MODULE

namespace shorty {

template <typename> constexpr bool is_tuple_like = false;
template <typename T, std::size_t Extent> constexpr bool is_tuple_like<std::array<T, Extent>> = true;
template <typename... Ts> constexpr bool is_tuple_like<std::tuple<Ts...>> = true;
template <typename A, typename B> constexpr bool is_tuple_like<std::pair<A, B>> = true;
template <typename T> constexpr bool is_tuple_like<std::complex<T>> = true;
// TODO subrange

template <typename T> concept tuple_like = is_tuple_like<std::remove_cvref_t<T>>;

} // namespace shorty

#endif // SHORTY_SHORTY_COMPATIBILITY_CONCEPTS_HPP

#ifndef SHORTY_SHORTY_COMPATIBILITY_PACK_INDEXING_HPP
#define SHORTY_SHORTY_COMPATIBILITY_PACK_INDEXING_HPP

#ifdef SHORTY_IS_IN_MODULE
// nothing :)
#else // SHORTY_IS_IN_MODULE
#include <utility>
#endif // SHORTY_IS_IN_MODULE

namespace shorty {

#if __cpp_pack_indexing < 202311L
template <size_t N, typename First, typename... Args> constexpr auto extract_nth(First && first, Args &&... args) noexcept {
	if constexpr (N == 0u) {
		return first;
	} else {
		return extract_nth<N - 1u>(std::forward<Args>(args)...);
	}
}

template <typename... Ts> using first = Ts...[0];

#else
template <typename...> struct head_helper;
template <typename Head, typename... Ts> struct head_helper<Head, Ts...> {
	using result = Head;
};

template <typename... Ts> using first = head_helper<Ts...>::result;

#endif

} // namespace shorty

#endif // SHORTY_SHORTY_COMPATIBILITY_PACK_INDEXING_HPP

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

	consteval bool validate_with(std::size_t provided) const noexcept {
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
	template <typename Self> static constexpr bool valid_call(std::size_t n) {
		// TODO throw exceptions for nice diagnostics
		return argument_info::from<std::remove_cvref_t<Self>>().validate_with(n);
	}

	template <typename Self, typename... Args> constexpr auto operator()(this Self && self, Args &&... args) requires(valid_call<Self>(sizeof...(Args)))
	{
		return self.eval(std::forward<Args>(args)...);
	}
	template <typename Self, tuple_like TupleArgs> constexpr auto operator()(this Self && self, TupleArgs && arg) requires(valid_call<Self>(std::tuple_size_v<std::remove_cvref_t<TupleArgs>>))
	{
#if __cpp_structured_bindings >= 202411L
		auto && [... args] = arg;
		return std::forward<Self>(self).operator()(std::forward<decltype(args)>(args)...);
#else
		return [&]<std::size_t... Idx>(std::index_sequence<Idx...>) {
			return std::forward<Self>(self).operator()(std::get<Idx>(arg)...);
		}(std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<decltype(arg)>>>());
#endif
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

#define SHORTY_EXPORT
#endif // SHORTY_IS_IN_MODULE

SHORTY_EXPORT namespace shorty::literals {
	template <unsigned N> constexpr auto $arg = shorty::nth_argument<N>{};

	constexpr auto $args = shorty::pack_arguments{};
	constexpr auto $argc = shorty::argument_count{};

	// nth-argument
	constexpr auto $0 = $arg<0>;
	constexpr auto $1 = $arg<1>;
	constexpr auto $2 = $arg<2>;
	constexpr auto $3 = $arg<3>;
	constexpr auto $4 = $arg<4>;
	constexpr auto $5 = $arg<5>;
	constexpr auto $6 = $arg<6>;
	constexpr auto $7 = $arg<7>;
	constexpr auto $8 = $arg<8>;
	constexpr auto $9 = $arg<9>;

	// for niceness
	constexpr auto $it = shorty::nth_argument_with_query<0, []<typename T> { return std::input_or_output_iterator<T>; }>{};

	constexpr auto $i = shorty::nth_argument_of_k<0, 1>{};
	constexpr auto $n = shorty::nth_argument_of_k<0, 1>{};
	constexpr auto $k = shorty::nth_argument_of_k<0, 1>{};

	constexpr auto $lhs = shorty::nth_argument_of_k<0, 2>{};
	constexpr auto $rhs = shorty::nth_argument_of_k<1, 2>{};

	constexpr auto $x = shorty::nth_argument_of_k<0, 3>{};
	constexpr auto $y = shorty::nth_argument_of_k<1, 3>{};
	constexpr auto $z = shorty::nth_argument_of_k<2, 3>{};

	constexpr auto $a = shorty::nth_argument_of_k<0, 3>{};
	constexpr auto $b = shorty::nth_argument_of_k<1, 3>{};
	constexpr auto $c = shorty::nth_argument_of_k<2, 3>{};

	// reference to external variable
	template <typename T> constexpr auto $(T & ref) {
		return shorty::reference<T>(ref);
	}

	template <typename T> constexpr auto $(const T & ref) {
		return shorty::reference<const T>(ref);
	}

	//// apply function on result
	// template <auto CustomOp> constexpr auto $(auto &&... args) {
	//	return shorty::apply<CustomOp>(std::forward<decltype(args)>(args)...);
	// }
	//
	//// cast to type
	// template <typename T> constexpr auto $(auto &&... args) {
	//	return shorty::cast<T>(std::forward<decltype(args)>(args)...);
	// }
	//
	// compile time known constant
	template <auto V> constexpr auto $const = shorty::constant<V>{};

} // namespace shorty::literals

#endif // SHORTY_SHORTY_LITERALS_HPP

#define SHORTY_EXPORT
#endif // SHORTY_IS_IN_MODULE

SHORTY_EXPORT template <typename...> struct identify;

namespace shorty {

}

#endif // SHORTY_SHORTY_HPP
#include <functional>
#include <ranges>
#include <tuple>
#include <utility>
#include <concepts>

namespace shorty {

struct ast_node { };

#if __cpp_pack_indexing < 202311L
template <size_t N, typename First, typename... Args> constexpr auto extract_nth(First && first, Args &&... args) noexcept {
	if constexpr (N == 0u) {
		return first;
	} else {
		return extract_nth<N - 1u>(std::forward<Args>(args)...);
	}
}
#endif

template <unsigned N> struct nth_argument {
	template <typename... Args> constexpr auto operator()(Args &&... args) const {
		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		return args...[N];
#else
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
	constexpr operator ast_node() const { return {}; }
};

template <unsigned N, unsigned Count> struct nth_argument_of_k {
	template <typename... Args> constexpr auto operator()(Args &&... args) const {
		static_assert(Count == sizeof...(Args));
		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		return args...[N];
#else
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
	constexpr operator ast_node() const { return {}; }
};

template <unsigned N, auto Query> struct nth_argument_with_query {
	template <typename... Args> constexpr auto operator()(Args &&... args) const {
		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		static_assert(Query(std::type_identity<Args... [N]> { }));
		return args...[N];
#else
		static_assert(Query(std::type_identity<decltype(extract_nth<N>(std::forward<Args>(args)...))>{}));
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
	constexpr operator ast_node() const { return {}; }
};

struct argument_count {
	template <typename... Args> constexpr auto operator()(Args &&...) const {
		return sizeof...(Args);
	}
	constexpr operator ast_node() const { return {}; }
};

struct pack_arguments {
	template <typename... Args> constexpr auto operator()(Args &&... args) const {
		return std::tuple(std::forward<Args>(args)...);
	}
	constexpr operator ast_node() const { return {}; }
};

template <auto V> struct constant {
	template <typename... Args> constexpr auto operator()(Args &&...) const {
		return V;
	}
	constexpr operator ast_node() const { return {}; }
};

template <typename T> struct value {
	T & data;
	template <typename... Args> constexpr auto operator()(Args &&...) const {
		return data;
	}
	constexpr operator ast_node() const { return {}; }
};

template <typename CharT, size_t N> struct static_string {
	using type = const CharT (&)[N];
	type data;
	consteval static_string(const CharT (&_data)[N]) noexcept: data{_data} { }
	template <typename... Args> constexpr type operator()(Args &&...) const {
		return data;
	}
	constexpr operator ast_node() const { return {}; }
};

template <typename T> struct reference {
	T & value;
	template <typename... Args> constexpr auto operator()(Args &&...) const {
		return value;
	}
	constexpr operator ast_node() const { return {}; }
};

template <typename T, typename... Args> constexpr decltype(auto) evaluate(T && obj, [[maybe_unused]] Args &&... args) {
	if constexpr (requires {
					  obj(std::forward<Args>(args)...);
				  }) {
		return obj(std::forward<Args>(args)...);
	} else {
		return obj;
	}
}

template <typename Op, typename... Operands> struct node {
	using base_node = node;

	static constexpr auto op = Op{};
	[[no_unique_address]] std::tuple<Operands...> ast_operands;

	node(node &&) = default;
	node(const node &) = default;
	constexpr node(std::convertible_to<Operands> auto &&... _ast_operands): ast_operands{std::forward<decltype(_ast_operands)>(_ast_operands)...} { }

	template <typename... Args> constexpr auto operator()(Args &&... args) const {
#if __cpp_structured_bindings >= 202411L
		auto && [... operands] = ast_operands;
		return op(evaluate(std::forward<decltype(operands)>(operands), std::forward<Args>(args)...)...);
#else
		return [&]<std::size_t... Idx>(std::index_sequence<Idx...>) {
			return op(evaluate(std::get<Idx>(ast_operands), std::forward<Args>(args)...)...);
		}(std::make_index_sequence<std::tuple_size<decltype(ast_operands)>::value>());
#endif
	}

	constexpr operator ast_node() const { return {}; }
};

template <typename T> concept is_node = requires(const T & maybe_node) {
	typename T::base_node;
	{ node{std::declval<T::base_node>()} } -> std::same_as<typename T::base_node>;
	{ node{maybe_node} } -> std::same_as<typename T::base_node>;
};

// just to make sure nodes and arguments are not references
// template <typename T, typename Y = T>  auto fix_type(std::remove_cvref_t<Y> *) -> std::remove_cvref_t<T>;

// everything else can be
// template <typename Sink> auto fix_type(...) -> value<std::remove_reference_t<Sink>>;

template <typename T, typename Y = T> auto wrap(Y *) -> std::remove_cvref_t<T> requires std::convertible_to<std::remove_cvref_t<T>, ast_node>;

template <typename T> struct wrapper {
	using result = value<std::remove_cvref_t<T>>;
};

template <typename CharT, size_t N> struct wrapper<const CharT (&)[N]> {
	using result = static_string<CharT, N>;
};

template <std::convertible_to<ast_node> T> struct wrapper<T> {
	using result = std::remove_cvref_t<T>;
};

// template <typename T> using select = decltype(fix_type<T>(nullptr));
template <typename T> using select = wrapper<T>::result;

// SUPPORT FOR UNARY and BINARY OPS
template <typename Op, typename A> struct unary_op: node<Op, A> { };
template <typename Op, typename A, typename B> struct binary_op: node<Op, A, B> { };

template <typename Op> struct node_builder {
	template <typename... Args> constexpr auto operator()(Args &&... args) {
		if constexpr (sizeof...(Args) == 1) {
			return unary_op<Op, select<Args>...>(std::forward<Args>(args)...);
		} else if constexpr (sizeof...(Args) == 2) {
			return binary_op<Op, select<Args>...>(std::forward<Args>(args)...);
		} else {
			return node<Op, select<Args>...>(std::forward<Args>(args)...);
		}
	}
};

template <typename Op> constexpr auto build = node_builder<Op>{};

// I wish I have the universal template arguments
template <template <typename...> typename ExactNode> struct exact_node_builder {
	template <typename... Args> static constexpr auto operator()(Args &&... args) {
		using node_type = ExactNode<select<Args>...>::base_node;
		return ExactNode<select<Args>...>{node_type{std::forward<Args>(args)...}};
	}
};

template <template <typename...> typename ExactNode> constexpr auto exact_build = exact_node_builder<ExactNode>{};

// CASTING
template <typename T> struct cast_to {
	static constexpr auto operator()(auto && arg) {
		return static_cast<T>(arg);
	}
};

template <typename T, typename A> struct cast_op: node<cast_to<T>, A> { };

template <typename T> constexpr auto cast(auto &&... args) {
	return cast_op<T, select<decltype(args)>...>(std::forward<decltype(args)>(args)...);
}

template <auto Op> struct apply_at {
	template <typename... Ts> constexpr auto operator()(std::tuple<Ts...> && pack) const {
#if __cpp_structured_bindings >= 202411L
		auto && [... args] = pack;
		static_assert(std::invocable<decltype(Op), decltype(args)...>);
		return Op(std::forward<decltype(args)>(args)...);
#else
		return [&]<std::size_t... Idx>(std::index_sequence<Idx...>) {
			static_assert(std::invocable<decltype(Op), decltype(std::get<Idx>(pack))...>);
			return Op(std::get<Idx>(pack)...);
		}(std::make_index_sequence<sizeof...(Ts)>());
#endif
	}

	template <typename... Args> constexpr auto operator()(Args &&... args) const {
		static_assert(std::invocable<decltype(Op), Args...>);
		return Op(std::forward<Args>(args)...);
	}
};

template <auto Op, typename... Args> struct apply_at_op: node<apply_at<Op>, Args...> { };

template <auto Op> constexpr auto apply(auto &&... args) {
	return apply_at_op<Op, select<decltype(args)>...>(std::forward<decltype(args)>(args)...);
}

// SUPPORT FOR RELATIONAL OPS
template <auto Op> struct relational {
	static constexpr auto operator()(auto && lhs, auto && rhs) {
		if constexpr (Op == std::is_eq) {
			return std::forward<decltype(lhs)>(lhs) == std::forward<decltype(rhs)>(rhs);
		} else if constexpr (Op == std::is_neq) {
			return std::forward<decltype(lhs)>(lhs) != std::forward<decltype(rhs)>(rhs);
		} else {
			return Op(std::forward<decltype(lhs)>(lhs) <=> std::forward<decltype(rhs)>(rhs));
		}
	}
};

template <typename Lhs, typename Rhs> struct compare: node<std::compare_three_way, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct less: node<relational<std::is_lt>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct less_equal: node<relational<std::is_lteq>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct equal: node<relational<std::is_eq>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct not_equal: node<relational<std::is_neq>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct greater: node<relational<std::is_gt>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct greater_equal: node<relational<std::is_gteq>, Lhs, Rhs> { };

constexpr auto operator<=>(auto && lhs, auto && rhs) { return exact_build<compare>(lhs, rhs); }
constexpr auto operator<(auto && lhs, auto && rhs) { return exact_build<less>(lhs, rhs); }
constexpr auto operator<=(auto && lhs, auto && rhs) { return exact_build<less_equal>(lhs, rhs); }
constexpr auto operator==(auto && lhs, auto && rhs) { return exact_build<equal>(lhs, rhs); }
constexpr auto operator!=(auto && lhs, auto && rhs) { return exact_build<not_equal>(lhs, rhs); }
constexpr auto operator>(auto && lhs, auto && rhs) { return exact_build<greater>(lhs, rhs); }
constexpr auto operator>=(auto && lhs, auto && rhs) { return exact_build<greater_equal>(lhs, rhs); }

// basic math operators
template <typename...> struct plus;

struct unary_plus_op {
	constexpr decltype(auto) operator()(auto && v) const {
		return +std::forward<decltype(v)>(v);
	}
};

template <typename Lhs, typename Rhs> struct plus<Lhs, Rhs>: node<std::plus<>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct minus: node<std::minus<>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct multiplies: node<std::multiplies<>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct divides: node<std::divides<>, Lhs, Rhs> { };
template <typename Lhs, typename Rhs> struct modulus: node<std::modulus<>, Lhs, Rhs> { };
template <typename T> struct negate: node<std::negate<>, T> { };
template <typename T> struct plus<T>: node<unary_plus_op, T> { };

constexpr auto operator+(auto && lhs, auto && rhs) { return exact_build<plus>(lhs, rhs); }
constexpr auto operator-(auto && lhs, auto && rhs) { return exact_build<minus>(lhs, rhs); }
constexpr auto operator*(auto && lhs, auto && rhs) { return exact_build<multiplies>(lhs, rhs); }
constexpr auto operator/(auto && lhs, auto && rhs) { return exact_build<divides>(lhs, rhs); }
constexpr auto operator%(auto && lhs, auto && rhs) { return exact_build<modulus>(lhs, rhs); }
constexpr auto operator-(auto && v) { return exact_build<negate>(v); }
constexpr auto operator+(auto && v) { return exact_build<plus>(v); }

struct index_access {
	constexpr decltype(auto) operator()(auto &&... args) const {
		return false;
	}
};

// constexpr auto operator[](auto && ... args) { return exact_build<index_access>(std::forward<Args>(args)...); }

} // namespace shorty

namespace placeholders {
template <unsigned N> constexpr auto $arg = shorty::nth_argument<N>{};
constexpr auto $args = shorty::pack_arguments{};
constexpr auto $argc = shorty::argument_count{};

// nth-argument
constexpr auto $0 = $arg<0>;
constexpr auto $1 = $arg<1>;
constexpr auto $2 = $arg<2>;
constexpr auto $3 = $arg<3>;
constexpr auto $4 = $arg<4>;
constexpr auto $5 = $arg<5>;
constexpr auto $6 = $arg<6>;
constexpr auto $7 = $arg<7>;
constexpr auto $8 = $arg<8>;
constexpr auto $9 = $arg<9>;

// for niceness
constexpr auto $it = shorty::nth_argument_with_query<0, []<typename T> { return std::input_or_output_iterator<T>; }>{};

constexpr auto $i = shorty::nth_argument_of_k<0, 1>{};
constexpr auto $n = shorty::nth_argument_of_k<0, 1>{};
constexpr auto $k = shorty::nth_argument_of_k<0, 1>{};

constexpr auto $lhs = shorty::nth_argument_of_k<0, 2>{};
constexpr auto $rhs = shorty::nth_argument_of_k<1, 2>{};

constexpr auto $x = shorty::nth_argument_of_k<0, 3>{};
constexpr auto $y = shorty::nth_argument_of_k<1, 3>{};
constexpr auto $z = shorty::nth_argument_of_k<2, 3>{};

constexpr auto $a = shorty::nth_argument_of_k<0, 3>{};
constexpr auto $b = shorty::nth_argument_of_k<1, 3>{};
constexpr auto $c = shorty::nth_argument_of_k<2, 3>{};

// reference to external variable
template <typename T> constexpr auto $(T & ref) {
	return shorty::reference<T>(ref);
}

template <typename T> constexpr auto $(const T & ref) {
	return shorty::reference<const T>(ref);
}

// apply function on result
template <auto CustomOp> constexpr auto $(auto &&... args) {
	return shorty::apply<CustomOp>(std::forward<decltype(args)>(args)...);
}

// cast to type
template <typename T> constexpr auto $(auto &&... args) {
	return shorty::cast<T>(std::forward<decltype(args)>(args)...);
}

// compile time known constant
template <auto V> constexpr auto $const = shorty::constant<V>{};
} // namespace placeholders

#include <algorithm>
#include <array>
#include <format>
#include <iostream>
#include <print>
#include <span>

template <typename...> struct identify;

inline namespace here {

using namespace placeholders;

void sort(std::span<int> subject) {
	std::ranges::sort(subject, $lhs < $rhs);
}

void transform_insitu(std::ranges::range auto && subject, auto && cb) {
	std::ranges::transform(subject, subject.begin(), cb);
}

void transform(std::span<int> subject, float & coefficient) {
	transform_insitu(subject, $<int>($0 * $(coefficient)));
}

template <size_t N> struct size;

bool contains(std::span<const std::string> dic) {
	auto f = $0 > "hana";
	// identify<decltype(f)> i;
	// identify<decltype(f)> i;
	return std::ranges::find_if(dic, f) != dic.end();
}

} // namespace here

#include <cmath>

float pythagorus(float a, float b) {
	constexpr auto sqrt = [](std::floating_point auto val) { return std::sqrt(val); };
	constexpr auto f = $<sqrt>(($0 * $0) + ($1 * $1));
	return f(a, b);
}

#ifdef EXECUTE

int main() {
	auto arr = std::array{1, 1, 2, 2, 3};
	float coefficient = 1.33;
	transform(arr, coefficient);
	sort(arr);
	for (int v: arr) {
		std::println("{}", v);
	}

	constexpr auto sum = [](auto... v) { return (v + ...); };

	auto avg_window = arr | std::views::transform($0 * 5);

	std::println("sliding average:");
	for (auto v: avg_window) {
		std::println("{}", v);
	}

	using namespace std::string_literals;
	auto arr2 = std::array{"anna"s, "beata"s, "jana"s};
	if (contains(arr2)) {
		std::println("something after hana");
	}
}
#endif
