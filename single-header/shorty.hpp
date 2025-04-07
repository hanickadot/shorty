#ifndef SHORTY_HPP
#define SHORTY_HPP

#include <utility>

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

// nothing :)

#include <array>
#include <ranges>
#include <tuple>
#include <complex>

namespace shorty {

template <typename> constexpr bool is_tuple_like = false;
template <typename T, std::size_t Extent> constexpr bool is_tuple_like<std::array<T, Extent>> = true;
template <typename... Ts> constexpr bool is_tuple_like<std::tuple<Ts...>> = true;
template <typename A, typename B> constexpr bool is_tuple_like<std::pair<A, B>> = true;
template <typename T> constexpr bool is_tuple_like<std::complex<T>> = true;
// TODO subrange

template <typename T> concept tuple_like = is_tuple_like<std::remove_cvref_t<T>>;

} // namespace shorty

// nothing :)

#include <utility>

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
template <typename... Ts> constexpr auto first_thing(Ts &&... args) {
	return args...[0];
}

#else
template <typename...> struct head_helper;
template <typename Head, typename... Ts> struct head_helper<Head, Ts...> {
	using result = Head;
};
template <typename Head, typename... Ts> constexpr auto first_thing(Head && head, Ts &&...) {
	return head;
}

template <typename... Ts> using first = head_helper<Ts...>::result;

#endif

} // namespace shorty

#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <concepts>

namespace shorty {

template <typename...> struct identify;
template <auto...> struct identify_value;

template <typename Op, typename... Operands> struct node;

template <typename T> using select = std::remove_cvref_t<T>;

struct argument_info;

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

struct argument_info {
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

consteval bool validate_minimal_number_of_arguments(std::size_t provided, std::size_t expected) {
	if (expected > provided) {
#if __cpp_constexpr_exceptions >= 202411L
		throw shorty_constraint_failure::not_enough();
#else
		return false;
#endif
	} else {
		return true;
	}
}

consteval bool validate_exact_number_of_arguments(std::size_t provided, std::size_t expected) {
	if (expected != std::dynamic_extent && expected != provided) {
#if __cpp_constexpr_exceptions >= 202411L
		throw shorty_constraint_failure::too_much();
#else
		return false;
#endif
	} else {
		return true;
	}
}

template <typename... Args> consteval std::size_t gather_number_of_arguments() {
	if constexpr (sizeof...(Args) == 1 && tuple_like<first<Args...>>) {
		const std::size_t count = std::tuple_size_v<std::remove_cvref_t<first<Args...>>>;
		return count;
	} else {
		return sizeof...(Args);
	}
}

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

	template <typename Self> static constexpr auto call_info = argument_info::from<std::remove_cvref_t<Self>>();
	template <typename... Args> static constexpr auto number_of_arguments = gather_number_of_arguments<Args...>();

	// this is only called outside
	// this gymnastics is also to improve error message
	template <typename Self, typename... Args, auto argn = number_of_arguments<Args...>, auto min_argn_expected = call_info<Self>.min, auto exact_argn_expected = call_info<Self>.exact>
	constexpr auto operator()(this Self && self, Args &&... args) requires(validate_minimal_number_of_arguments(argn, min_argn_expected) && validate_exact_number_of_arguments(argn, exact_argn_expected))
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
	static constexpr bool consistent = (argument_info::from<std::remove_cvref_t<Operands>>() + ...).consistent;
	static_assert(consistent, "expected number of arguments is not consistent (hint: mixing something like $lhs and $x?)");
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
template <unsigned N> struct nth_argument: ast_node {
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
template <unsigned N, unsigned Count> struct nth_argument_of_k: ast_node {
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
template <unsigned N, auto Query> struct nth_argument_with_query: ast_node {
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
struct argument_count: ast_node {
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return sizeof...(Args);
	}
};

// wrap all provided arguments to shorty's lambda and return a tuple
struct pack_arguments: ast_node {
	template <typename... Args> constexpr auto eval(Args &&... args) const {
		return std::tuple(std::forward<Args>(args)...);
	}
};

// store CNTTP constant
template <auto V> struct constant: ast_node {
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return V;
	}
};

// store a value of any type
template <typename T> struct value: ast_node {
	T data;
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return data;
	}
};

// store a reference to a string literals
template <typename CharT, std::size_t N> struct static_string: ast_node {
	using type = const CharT (&)[N];
	type data;
	consteval static_string(const CharT (&_data)[N]) noexcept: data{_data} { }
	template <typename... Args> constexpr type eval(Args &&...) const {
		return data;
	}
};

// external const/non-const reference
template <typename T> struct reference: ast_node {
	T & value;
	constexpr reference(T & _value) noexcept: value{_value} { }
	template <typename... Args> constexpr auto eval(Args &&...) const {
		return value;
	}
};

} // namespace shorty

namespace shorty::literals {
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

template <typename...> struct identify;

namespace shorty {

}

#endif
