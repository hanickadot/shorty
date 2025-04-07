# terser (shorter) lambda == SHORTY

This library is intended to give terser syntax than C++'s lambdas, not replace C++ with lazy DSL.

```c++
import shorty;
using namespace shorty::literals;

// you no longer need to remember if it's `std::less` or `std::greater`
std::ranges::sort(subject, $lhs > $rhs);

// filter only even
subject | std::views::filter(($i % 2) == 0);

// zip together and transform
std::views::zip(A,B,C,D) | std::views::transform($a * $b + $c * $d);

// call external function
auto pythagorean = $<sqrt>($a * $a + $b * $b); // or $call<sqrt>;
pythagorean(3.0, 4.0) == 5.0;

// casting
auto to_int = $<int>($0); // or $cast<int>;
to_int(4.3) == 4; 

// remap by index
auto indices = std::vector<int>{...};
auto mapping = indices | $(data)[$i];
```

## accessing arguments

If shorty callable gets only one argument and the argument is `tuple_like` then it's automatically unwrapper. Same applies to any lazy calls.

By default arguments are accessible using `$0`...`$9` syntax (for first 10) or `$arg<N>`. But also you can use following special arguments:

- `$lhs` and `$rhs` for binary callables (it limits them to two arguments)
- `$it` for first and only argument, which is also compatible with `std::input_or_output_iterator`
- `$a` ... `$f` for first 6 arguments but names instead of numbers (can't be mixed with other types of arguments)
- `$x`, `$y`, `$z` for first 3 arguments (again can't be mixed with other types of arguments)
- `$i`, `$n`, `$k`, `$in` ... for first and only argument (can't be mixed too)

## querying arguments

- `$argc` for getting number of arguments as `size_t`
- `$args` to get all arguments as a tuple

## capturing

- `$(v)` or `$ref(v)` capture by reference (or const reference, depending on const qualifier of `v`)
- `$value(v)`, `$val(v)`, or `$copy(v)` capture by copy
- `$fixed<v>` or `$const<v>` capture by CNTTP (only for structural types)

## calling functions

- `$<callable>(args...)` or `$call<callable>(args...)` call `callable` with arguments `args`
- `$<CallableType>(args...)` or `$call<CallableType>(args...)` call new instance of type `CallableType` with arguments `args`

(TODO: calling also unwraps tuple for you)

## casting

- `$<T>(expr)` or `$cast<T>(expr)` will `static_cast` expression result into `T`


