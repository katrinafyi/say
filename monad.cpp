#include <cassert>
#include <concepts>
#include <optional>
#include <iostream>
#include <functional>
#include <utility>



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

template <template <typename> class m, typename a>
struct Ghost {
    std::optional<a> val;

    operator a() const {
        assert(this->val.has_value());
        return *this->val;
    }
};


template <template <typename> class m, typename a>
struct Neg {
    m<a> computation;
};

template <template <typename> class m, typename a>
Neg<m,a> operator-(m<a>& monad) {
    return {monad};
}


// maybe<int> m1, m2;
// maybe<int> val = 
//   (a < -m1, b < -m2), just (a + b)
// 
// m1 >>= \a -> m2 >>= \b -> Just (a + b)

template <template <typename> class m, typename a>
Neg<m, a> operator<(Ghost<m,a>& var, Neg<m, a> x) {
    auto lam = [&](const a& val) { var.val = {val}; return Monad<m>::mreturn(val); };
    std::function<m<a>(const a&)> f{lam};
    return { Monad<m>::mbind(x.computation, f) };
}

template <template <typename> class m, typename a, typename b>
Neg<m,std::pair<a,b>> operator,(Neg<m,a> first, Neg<m,b> second) {
    using pair = std::pair<a,b>;

    auto lam = [&](const a& val) -> m<pair> { 
        auto lam2 = [&](const b& val2) -> m<pair> {
            return Monad<m>::mreturn(std::pair{ val, val2 });
        };
        std::function<m<pair>(const b&)> f{lam2};
        return Monad<m>::mbind(second.computation, f);
    };

    std::function<m<std::pair<a,b>>(const a&)> f{lam};
    return { Monad<m>::mbind(first.computation, f) };
}

int main(int argc, char** argv) {

    // Maybe<int> x = Monad<Maybe>::mreturn(10);

    // std::cout << *a << std::endl;
    // auto y = Functor<std::optional>::fmap<int, char>(a, [](const int& x) { return 'a'; });
    // std::cout << *y << std::endl;

    // // Neg<std::optional, int> neg = -a;
    // // Neg<std::optional, int> bound = b < neg;
    // Neg<std::optional, std::pair<int, int>> bound2 = operator,<std::optional, int, int>(x1 <- a , x2 <- b);
    // std::cout << "bound: " << (int)x1 << std::endl; 
    // std::cout << "bound: " << (int)x2;
    // std::optional<int> result = (operator,<std::optional, int, int>(x1 <- a , x2 <- b), Monad<std::optional>::mreturn(x1 + x2));
    // std::cout << "result: " << *result;

    Monad<std::optional> Maybe;
    std::optional<int> a = Maybe.mreturn(10);
    std::optional<int> b = Maybe.mreturn(2);

    Ghost<std::optional, int> x1, x2{};
    std::optional<int> result = (
        x1 <- a,  // draw x1 from a
        x2 <- b,  // draw x2 from b
        Maybe.mreturn(x1 + x2),  // discarded computation
        Maybe.mreturn(x1 * x2)   // returns last computation
    );
    std::cout << "result: " << *result;


}