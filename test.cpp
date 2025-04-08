#ifdef SHORTY_IS_IN_MODULE
import shorty;
import std;
#else
#include <shorty/shorty.hpp>
#include <algorithm>
#include <print>
#include <ranges>
#endif

int main() {
	using namespace shorty::literals;

	auto data = std::array{1, 2, 3, 4, 5};
	std::ranges::sort(data, $lhs < $rhs);

	auto indices = std::array{0, 0, 4, 4, 1};
	for (auto v: indices | std::views::transform($(data)[$0])) {
		std::println("{}", v);
	}

	std::println("zip:");
	auto a = std::array{1, 2, 3, 4, 5};
	auto b = std::array{2, 0, 1, 1, 3};
	auto c = std::array{1, 1, 1, 0, 1};
	for (auto v: std::views::zip(a, b, c) | std::views::transform(($a + $b) * $c)) {
		std::println("{}", v);
	}

	std::println("squares:");
	constexpr auto sqrt = [](std::floating_point auto v) {
		return std::sqrt(v);
	};

	auto values = std::array{1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
	for (auto v: values | std::views::transform($<sqrt>($0))) {
		std::println("{}", v);
	}

	auto cast_to_int = $<int>($0);

	// auto wrong_expr = $x + $a;
	auto expr = $0 + $1;

	auto make_tuple = ($0, $1, $2);
	std::tuple<int, int, int> r = make_tuple(1, 2, 3);

	constexpr auto sum = [](shorty::tuple_like auto && tpl) {
		return [&]<std::size_t... Idx>(std::index_sequence<Idx...>) {
			return (std::get<Idx>(tpl) + ... + 0);
		}(std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<decltype(tpl)>>>());
	};

	auto test = $<sum>(($0, $1, $2, $3)); // double paranthesis are explicit, so we get tuple
	auto r2 = test(1, 2, 3, 4);
	std::println("{} == 10", r2);

	// identify<decltype(r)> i;
	//  expr(1, 2);
	//  expr(std::tuple{1});

	// auto expr2 = $lhs + $x;
}