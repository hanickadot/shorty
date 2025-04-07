#ifndef SHORTY_SHORTY_LITERALS_HPP
#define SHORTY_SHORTY_LITERALS_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#include "leaves.hpp"
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

#endif // SHORTY_SHORTY_LITERALS_HPP
