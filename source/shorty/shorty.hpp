#ifndef SHORTY_SHORTY_HPP
#define SHORTY_SHORTY_HPP

#ifdef SHORTY_IS_IN_MODULE
#define SHORTY_EXPORT export
#else // SHORTY_IS_IN_MODULE
#include "literals.hpp"
#define SHORTY_EXPORT
#endif // SHORTY_IS_IN_MODULE

SHORTY_EXPORT template <typename...> struct identify;

namespace shorty {

}

#endif // SHORTY_SHORTY_HPP
