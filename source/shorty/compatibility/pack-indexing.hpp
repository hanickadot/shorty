#ifndef SHORTY_SHORTY_COMPATIBILITY_PACK_INDEXING_HPP
#define SHORTY_SHORTY_COMPATIBILITY_PACK_INDEXING_HPP

#ifdef SHORTY_IS_IN_MODULE
// nothing :)
#else // SHORTY_IS_IN_MODULE
#include <utility>
#endif // SHORTY_IS_IN_MODULE

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

#endif // SHORTY_SHORTY_COMPATIBILITY_PACK_INDEXING_HPP
