#ifndef SHORTY_SHORTY_COMPATIBILITY_CONCEPTS_HPP
#define SHORTY_SHORTY_COMPATIBILITY_CONCEPTS_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
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

SHORTY_EXPORT template <typename T> concept tuple_like = is_tuple_like<std::remove_cvref_t<T>>;

} // namespace shorty

#endif // SHORTY_SHORTY_COMPATIBILITY_CONCEPTS_HPP
