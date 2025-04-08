#ifndef SHORTY_SHORTY_OPS_HPP
#define SHORTY_SHORTY_OPS_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#include "compatibility/concepts.hpp"
#include <utility>
#define SHORTY_EXPORT
#endif // SHORTY_IS_IN_MODULE

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
template <typename Op = void> struct assign;
template <typename Op> struct assign {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) = Op{}(std::forward<decltype(lhs)>(lhs), std::forward<decltype(rhs)>(rhs)))) {
		return lhs = Op{}(lhs, rhs);
	}
};

template <> struct assign<void> {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::forward<decltype(lhs)>(lhs) = std::forward<decltype(rhs)>(rhs))) {
		return std::forward<decltype(lhs)>(lhs) = std::forward<decltype(rhs)>(rhs);
	}
};

struct tuplize {
	static constexpr auto operator()(auto && lhs, auto && rhs) noexcept(noexcept(std::make_tuple(std::forward<decltype(lhs)>(lhs), std::forward<decltype(rhs)>(rhs)))) {
		return std::make_tuple(std::forward<decltype(lhs)>(lhs), std::forward<decltype(rhs)>(rhs));
	}
	static constexpr auto operator()(tuple_like auto && lhs, auto && rhs) noexcept(noexcept(std::tuple_cat(std::forward<decltype(lhs)>(lhs), std::make_tuple(std::forward<decltype(rhs)>(rhs))))) {
		return std::tuple_cat(std::forward<decltype(lhs)>(lhs), std::make_tuple(std::forward<decltype(rhs)>(rhs)));
	}
	static constexpr auto operator()(auto && lhs, tuple_like auto && rhs) noexcept(noexcept(std::tuple_cat(std::forward<decltype(lhs)>(lhs), std::make_tuple(std::forward<decltype(rhs)>(rhs))))) {
		return std::tuple_cat(std::make_tuple(std::forward<decltype(lhs)>(lhs)), std::forward<decltype(rhs)>(rhs));
	}
	static constexpr auto operator()(tuple_like auto && lhs, tuple_like auto && rhs) noexcept(noexcept(std::tuple_cat(std::forward<decltype(lhs)>(lhs), std::make_tuple(std::forward<decltype(rhs)>(rhs))))) {
		return std::tuple_cat(std::make_tuple(std::forward<decltype(lhs)>(lhs)), std::make_tuple(std::forward<decltype(rhs)>(rhs)));
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

#endif // SHORTY_SHORTY_OPS_HPP
