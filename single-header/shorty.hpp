#ifndef SHORTY_HPP
#define SHORTY_HPP

#include <utility>

namespace shorty {
// groups
struct default_group {
	// no parameter
};
struct binary_ops {
	static constexpr std::size_t arity = 2;
};
struct unary_ops {
	static constexpr std::size_t arity = 1;
};
template <std::size_t Arity, auto> struct group {
	static constexpr std::size_t arity = Arity;
};
struct xyz {
};
struct abc {
};
struct ink {
};

} // namespace shorty

namespace shorty::ops {

// ops
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

template <typename...> struct identify;

struct index {
	static constexpr auto operator()(auto && subject, auto &&... args) noexcept(noexcept(std::forward<decltype(subject)>(subject)[std::forward<decltype(args)>(args)...])) {
		return std::forward<decltype(subject)>(subject)[std::forward<decltype(args)>(args)...];
	}
};

template <typename T> struct cast {
	static constexpr auto operator()(auto && arg) noexcept(noexcept(static_cast<T>(std::forward<decltype(arg)>(arg)))) {
		return static_cast<T>(std::forward<decltype(arg)>(arg));
	}
};

template <auto Op> struct call {
	static constexpr auto operator()(auto &&... args) noexcept(noexcept(Op(std::forward<decltype(args)>(args)...))) {
		return Op(std::forward<decltype(args)>(args)...);
	}
	//
	// static constexpr auto operator()(tuple_like auto & arg) noexcept(noexcept(Op(std::forward<decltype(args)>(args)...))) {
	//	return Op(std::forward<decltype(args)>(args)...);
	//}
};

template <typename Op> struct callable {
	static constexpr auto operator()(auto &&... args) noexcept(noexcept(Op{}(std::forward<decltype(args)>(args)...))) {
		return Op{}(std::forward<decltype(args)>(args)...);
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

#if __cpp_pack_indexing >= 202311L
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

template <size_t N, typename First, typename... Args> constexpr auto extract_nth(First && first, Args &&... args) noexcept {
	if constexpr (N == 0u) {
		return first;
	} else {
		return extract_nth<N - 1u>(std::forward<Args>(args)...);
	}
}

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

template <typename T>
concept knows_minimal_argument_count = requires {
	requires std::same_as<std::remove_cvref_t<decltype(T::argc_at_least)>, std::size_t>;
};

template <typename T> concept can_calculate_exact_argument_count = requires {
	{ T::arg_info.consistent } -> std::convertible_to<bool>;
};

template <typename T> concept has_group_type = requires {
	typename std::remove_cvref_t<T>::group;
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

struct incosistent_groups {
	static constexpr bool consistent = false;
};

struct neutral_info {
	static constexpr bool consistent = true;

	consteval friend neutral_info operator+(neutral_info, neutral_info) noexcept {
		return {};
	}
};

consteval incosistent_groups operator+(incosistent_groups, incosistent_groups) noexcept {
	return {};
}

template <typename Group> struct argument_info {
	std::size_t min = 0;
	static constexpr std::size_t exact = 0;
	static constexpr bool consistent = true;

	consteval operator incosistent_groups() const noexcept {
		return {};
	}

	consteval friend argument_info operator+(const argument_info & lhs, const argument_info & rhs) noexcept {
		return argument_info{
			.min = std::max(lhs.min, rhs.min),
		};
	}

	consteval friend argument_info operator+(neutral_info, const argument_info & rhs) noexcept {
		return rhs;
	}

	consteval friend argument_info operator+(const argument_info & lhs, neutral_info) noexcept {
		return lhs;
	}

	static consteval std::size_t exact_or(std::size_t provided) noexcept {
		return exact == std::dynamic_extent ? provided : exact;
	}

	template <typename T> static consteval argument_info from() noexcept {
		if constexpr (knows_minimal_argument_count<T>) {
			return {.min = std::remove_cvref_t<T>::argc_at_least};
		} else {
			return {};
		}
	}
};

template <typename T> static constexpr auto argument_info_about = [] {
	if constexpr (can_calculate_exact_argument_count<T>) {
		return T::arg_info;
	} else if constexpr (has_group_type<T>) {
		return argument_info<typename T::group>::template from<T>();
	} else if constexpr (knows_minimal_argument_count<T>) {
		return argument_info<default_group>{.min = std::remove_cvref_t<T>::argc_at_least};
	} else if constexpr (std::convertible_to<ast_node, T>) {
		return neutral_info{};
	} else {
		return neutral_info{};
	}
}();

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

	template <typename Self> static constexpr auto call_info = argument_info_about<std::remove_cvref_t<Self>>;
	template <typename... Args> static constexpr auto number_of_arguments = gather_number_of_arguments<Args...>();

	// this is only called outside
	// this gymnastics is also to improve error message
	template <typename Self, typename... Args, auto argn = number_of_arguments<Args...>, auto min_argn_expected = call_info<Self>.min>
	[[nodiscard, gnu::flatten, gnu::always_inline]]
	constexpr auto operator() // calling shorty
		(this Self && self, Args &&... args) requires(validate_minimal_number_of_arguments(argn, min_argn_expected))
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
	if constexpr (requires { obj.eval(std::forward<Args>(args)...); }) {
		return obj.eval(std::forward<Args>(args)...);
	} else {
		return obj;
	}
}

template <typename Op, typename... Operands> struct node: ast_node {
	static constexpr auto arg_info = (argument_info_about<std::remove_cvref_t<Operands>> + ... + neutral_info{});
	static constexpr bool consistent = arg_info.consistent;
	static_assert(consistent, "expected number of arguments is not consistent (hint: mixing something like $lhs and $x?)");
	using base_node = node;

	static constexpr auto op = Op{};
	[[no_unique_address]] std::tuple<Operands...> ast_operands;

	node(node &&) = default;
	node(const node &) = default;
	constexpr node(std::convertible_to<Operands> auto &&... _ast_operands): ast_operands{std::forward<decltype(_ast_operands)>(_ast_operands)...} { }

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
	using group = default_group;

	template <typename... Args> constexpr auto eval(Args &&... args) const {
		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		return args...[N];
#else
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
};

template <unsigned N, typename Group = void> struct nth_argument_with_group: ast_node {
	static constexpr std::size_t argc_at_least = N + 1;
	using group = Group;

	template <typename... Args> constexpr auto eval(Args &&... args) const {
		if constexpr (requires { Group::arity; }) {
			static_assert(Group::arity == sizeof...(Args));
		}

		static_assert(N < sizeof...(Args));
#if __cpp_pack_indexing >= 202311L
		return args...[N];
#else
		return extract_nth<N>(std::forward<Args>(args)...);
#endif
	}
};

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

// #if __cpp_template_parameters >= 202502L
//	template <template <typename> concept C> constexpr auto @0 = $arg<0>;
//	template <template <typename> concept C> constexpr auto @1 = $arg<1>;
//	template <template <typename> concept C> constexpr auto @2 = $arg<2>;
//	template <template <typename> concept C> constexpr auto @3 = $arg<3>;
//	template <template <typename> concept C> constexpr auto @4 = $arg<4>;
//	template <template <typename> concept C> constexpr auto @5 = $arg<5>;
//	template <template <typename> concept C> constexpr auto @6 = $arg<6>;
//	template <template <typename> concept C> constexpr auto @7 = $arg<7>;
//	template <template <typename> concept C> constexpr auto @8 = $arg<8>;
//	template <template <typename> concept C> constexpr auto @9 = $arg<9>;
// #endif

// for niceness
constexpr auto $it = shorty::nth_argument_with_query<0, []<typename T> { return std::input_or_output_iterator<T>; }>{};

constexpr auto $i = shorty::nth_argument_with_group<0, shorty::group<1, 'i'>>{};
constexpr auto $n = shorty::nth_argument_with_group<0, shorty::group<1, 'n'>>{};
constexpr auto $k = shorty::nth_argument_with_group<0, shorty::group<1, 'k'>>{};

constexpr auto $lhs = shorty::nth_argument_with_group<0, shorty::binary_ops>{};
constexpr auto $rhs = shorty::nth_argument_with_group<1, shorty::binary_ops>{};

constexpr auto $in = shorty::nth_argument_with_group<0, shorty::unary_ops>{};

constexpr auto $x = shorty::nth_argument_with_group<0, shorty::xyz>{};
constexpr auto $y = shorty::nth_argument_with_group<1, shorty::xyz>{};
constexpr auto $z = shorty::nth_argument_with_group<2, shorty::xyz>{};

constexpr auto $a = shorty::nth_argument_with_group<0, shorty::abc>{};
constexpr auto $b = shorty::nth_argument_with_group<1, shorty::abc>{};
constexpr auto $c = shorty::nth_argument_with_group<2, shorty::abc>{};
constexpr auto $d = shorty::nth_argument_with_group<3, shorty::abc>{};
constexpr auto $e = shorty::nth_argument_with_group<4, shorty::abc>{};
constexpr auto $f = shorty::nth_argument_with_group<5, shorty::abc>{};

// `$cast<T>(expr)` or `$<T>(expr)` are lazy static_cast
template <typename T> constexpr auto $cast(auto && arg) {
	return node<ops::cast<T>, select<decltype(arg)>>{std::forward<decltype(arg)>(arg)};
}
template <typename T> constexpr auto $(auto && arg) {
	return node<ops::cast<T>, select<decltype(arg)>>{std::forward<decltype(arg)>(arg)};
}

// `$<fnc>(args...)` or `$call<fnc>(args...)` are lazy function
template <auto Op> constexpr auto $(auto &&... arg) {
	return node<ops::call<Op>, select<decltype(arg)>...>{std::forward<decltype(arg)>(arg)...};
}

template <auto Op> constexpr auto $call(auto &&... arg) {
	return node<ops::call<Op>, select<decltype(arg)>...>{std::forward<decltype(arg)>(arg)...};
}

template <typename T> concept callable = requires {
	T::operator();
};

// `$<callable>(args...)` or `$call<callable>(args...)` are lazy function
template <callable Op> constexpr auto $(auto &&... arg) {
	return node<ops::callable<Op>, select<decltype(arg)>...>{std::forward<decltype(arg)>(arg)...};
}
template <typename Op> constexpr auto $call(auto &&... arg) {
	return node<ops::callable<Op>, select<decltype(arg)>...>{std::forward<decltype(arg)>(arg)...};
}

// `$value(VALUE)
constexpr auto $value(auto && ref) noexcept {
	return shorty::value(std::forward<decltype(ref)>(ref));
}
constexpr auto $val(auto && ref) noexcept {
	return shorty::value(std::forward<decltype(ref)>(ref));
}
// `$(REF)` or `$(CREF)`
constexpr auto $(auto & ref) noexcept {
	return shorty::reference<std::remove_reference_t<decltype(ref)>>(ref);
}
constexpr auto $(const auto & ref) noexcept {
	return shorty::reference<const std::remove_reference_t<decltype(ref)>>(ref);
}
constexpr auto $ref(auto & ref) noexcept {
	return shorty::reference<std::remove_reference_t<decltype(ref)>>(ref);
}
constexpr auto $ref(const auto & ref) noexcept {
	return shorty::reference<const std::remove_reference_t<decltype(ref)>>(ref);
}
// `$const<VALUE>` (CNTTP constant)
template <auto V> constexpr auto $const = shorty::constant<V>{};
template <auto V> constexpr auto $fixed = shorty::constant<V>{};

} // namespace shorty::literals

template <typename...> struct identify;

namespace shorty {

}

#endif
