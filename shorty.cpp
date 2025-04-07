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