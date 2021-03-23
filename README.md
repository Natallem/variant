# Variant
Own realization for C++ variation of algebraic types - variant.

Behaviour is the same as described on the [cppreference](https://en.cppreference.com/w/cpp/utility/variant), without realization for constructors and assignment operators with initializer_list,
monostate, std::hash.

### Supports 
* exception guarantees
* constexpr capabilities of variant
* triviality of some functions