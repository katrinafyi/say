#include <concepts>
#include <optional>
#include <iostream>
#include <functional>



template<template <typename> class f, typename a, typename b>
concept FunctorConcept = requires (f<a> f1, b(*func)(const a&)) {
    { f<a>::fmap(f1, func) } -> std::same_as<f<b>>;
};

template <template <typename> class f>
struct Applicative {
    template<typename a>
    static f<a> pure(const a&);
};


template <template <typename> class m>
struct Monad {
    template <typename a>
    static m<a> mreturn(const a&) = delete;

    template <typename a, typename b>
    static m<b> mbind(const m<a>&, std::function<m<b>(const a&)>) = delete;
};

template <typename a>
struct Maybe {
    bool isNothing;
    a value;
};

template <>
struct Monad<std::optional> {
    template <typename a>
    using m = std::optional<a>;

    template <typename a>
    static m<a> mreturn(const a& x) {
        return {x};
    }

    template <typename a, typename b>
    static std::optional<b> mbind(const std::optional<a>& x, std::function<std::optional<b>(const a&)> f) {
        return x ? f(*x) : std::nullopt;
    };
};

template<template <typename> class f, typename a, typename b>
concept MonadConcept = requires (const a& x, const f<a>& y, std::function<f<b>(const a&)> func) {
    { Monad<f>::mreturn(x) } -> std::same_as<f<a>>;
    { Monad<f>::mbind(y, func) } -> std::same_as<f<b>>;
};

// Functor is a generic struct with a single class parameter.
// that parameter, f, takes a single type parameter.
template <template <typename> class f>
struct Functor {
    // template<typename a, typename b>
    // static f<b> fmap(const f<a>&, b(*)(const a&));

    template <typename a, typename b>
    requires MonadConcept<f, a, b>
    static f<b> fmap(const f<a>& x, std::function<b(const a&)> g) {
        auto l = [&g](const a& x) -> f<b> { b y = g(x); return Monad<f>::mreturn(y); };
        std::function<f<b>(const a&)> aa{l};
        return Monad<f>::mbind(x, aa);
    };
};



int main(int argc, char** argv) {
    // Maybe<int> x = Monad<Maybe>::mreturn(10);
    Monad<std::optional> x;
    // Monad<std::optional>::m a;
    std::optional<int> a = x.mreturn(10);

    std::cout << *a << std::endl;
    auto y = Functor<std::optional>::fmap<int, char>(a, [](const int& x) { return 'a'; });
    std::cout << *y << std::endl;
}