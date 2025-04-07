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
