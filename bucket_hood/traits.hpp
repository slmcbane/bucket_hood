#ifndef BUCKET_HOOD_TRAITS_HPP
#define BUCKET_HOOD_TRAITS_HPP

#include <functional>
#include <string_view>
#include <type_traits>

namespace bucket_hood {

/*
 * Since we target C++17 we need a way to tell if a hash function has a transparent option w.r.t.
 * a given type. Ex: std::hash<std::string_view> can be used to hash a string_view if the key type
 * T is std::string (or const char*, or a list of other types).
 *
 * If we have such a case, 'is_transparent_hash' is a std::true_type and it defines 'hash_type'. The
 * user may specialize this template for their own hash functions as desired. Having a specializtion
 * may avoid unnecessary constructor calls in 'emplace' and enables the use of 'find' with heterogeneous
 * keys.
 *
 * In C++20 the necessary machinery is standardized, but I don't want to require C++20 for this library.
 */
template < class Hash, class... Other >
struct is_transparent_hash : std::false_type {};

// Example specialization.
// TODO: Flesh out this header.
template <>
struct is_transparent_hash< std::hash< std::string >, std::string_view > : std::true_type {
    typedef std::hash< std::string_view > hash;
};

template < class T >
struct is_transparent_hash< std::hash< T >, T > : std::true_type {
    typedef std::hash< T > hash;
};

static_assert( is_transparent_hash< std::hash< int >, int >::value );

/*
 * Same purpose as above: allow avoiding unnecessary constructors if two types can be compared using
 * Compare without converting one to the other.
 */
template < class Compare, class... Other >
struct is_transparent_comparison : std::false_type {};

template < class... T >
struct is_transparent_comparison< std::equal_to<>, T... > : std::true_type {};

template <>
struct is_transparent_comparison< std::equal_to< std::string >, std::string_view > : std::true_type {};

template < class T >
struct is_transparent_comparison< std::equal_to< T >, T > : std::true_type {};

/*
 * Find a hash function for Args... if one exists. If sizeof...(Args) == 1 and std::decay_t<Args> is T,
 * then this is the hash function given. Otherwise, use the 'transparent' trait to determine it.
 */
template < class Hash, class T, class... Args >
struct hash_finder : is_transparent_hash< Hash, Args... > {};

template < class Hash, class T >
struct hash_finder< Hash, T, const T& > : std::true_type {
    typedef Hash hash;
};

template < class Hash, class T >
struct hash_finder< Hash, T, T&& > : std::true_type {
    typedef Hash hash;
};

template < class Hash, class T >
struct hash_finder< Hash, T, T& > : std::true_type {
    typedef Hash hash;
};

template < class Hash, class T >
struct hash_finder< Hash, T, const T&& > : std::true_type {
    typedef Hash hash;
};

template < class Hash, class T >
struct hash_finder< Hash, T, T > : std::true_type {
    typedef Hash hash;
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_TRAITS_HPP

