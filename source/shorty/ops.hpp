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
