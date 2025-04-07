#include <algorithm>
#include <print>
#include <ranges>
#include <shorty.hpp>

int main() {
	using namespace shorty::literals;

	auto data = std::array{1, 2, 3, 4, 5};
	auto indices = std::array{0, 0, 4, 4, 1};

	std::ranges::sort(indices, $lhs < $rhs);

	// for (auto v: indices | std::views::transform($(data)[$0])) {
	//	std::println("{}", v);
	// }

	std::println("zip:");
	auto a = std::array{1, 2, 3, 4, 5};
	auto b = std::array{2, 0, 1, 1, 3};
	for (auto v: std::views::zip(a, b) | std::views::transform($0 + $1)) {
		std::println("{}", v);
	}
}
