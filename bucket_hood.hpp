/*
 * Copyright 2024 Sean McBane
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the “Software”), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
 * following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
 * LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
 * NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef BUCKET_HOOD_HPP_GUARD
#define BUCKET_HOOD_HPP_GUARD

#include <cstdint>

#include <algorithm>
#include <bit>
#include <cassert>
#include <concepts>
#include <cstring>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <type_traits>
#include <utility>

/********************************************************************************
 * Core types and constants
 *******************************************************************************/
#ifndef BUCKET_HOOD_64_BIT_SIZE
#define BUCKET_HOOD_SIZE_TYPE uint32_t
#else
#define BUCKET_HOOD_SIZE_TYPE uint64_t
#endif

namespace bucket_hood {

typedef BUCKET_HOOD_SIZE_TYPE size_type;
static_assert( std::disjunction_v< std::is_same< size_type, uint32_t >, std::is_same< size_type, uint64_t > > );

} // namespace bucket_hood

/********************************************************************************
 * Wrappers for compiler intrinsics.
 *******************************************************************************/

#define BH_ALWAYS_INLINE inline __attribute__( ( always_inline ) )
#define BH_NEVER_INLINE __attribute__( ( noinline ) )

namespace bucket_hood {

BH_ALWAYS_INLINE bool likely( long condition ) noexcept { return __builtin_expect( condition, 1 ); }
BH_ALWAYS_INLINE bool unlikely( long condition ) noexcept { return __builtin_expect( condition, 0 ); }
[[noreturn]] inline void unreachable() noexcept { __builtin_unreachable(); }

/********************************************************************************
 * Safely multiply two size_types; protect against overflow.
 * Probably not a concern for 64-bit size_type but for the 32-bit default it can
 * definitely realistically happen.
 *******************************************************************************/

BH_ALWAYS_INLINE size_type bounds_checked_mul( size_type a, size_type b ) {
    if constexpr ( std::is_same_v< size_type, uint32_t > ) {
        uint64_t a_ = a;
        uint64_t b_ = b;
        uint64_t c = a_ * b_;
        assert( c <= std::numeric_limits< size_type >::max() && "Overflowed 32-bit size_type" );
        return c;
    } else {
        // Currently not doing any check for 64-bit. Surely you would exhaust memory first.
        return a * b;
    }
}

/********************************************************************************
 * Hashing functions and utilities.
 *******************************************************************************/

// Adapted from wyhash.
// TODO: make portable.
BH_ALWAYS_INLINE uint64_t hash_mix( uint64_t a ) noexcept {
    uint64_t b = 0x9e3779b97fa7c15ull;
    __uint128_t c = static_cast< __uint128_t >( a ) * b;
    a = static_cast< uint64_t >( c );
    b = static_cast< uint64_t >( c >> 64 );
    return a ^ b;
}

// Specialize bucket_hood::known_good for your hash function before instantiating any bucket_hood
// templates if you know it is avalanching.
// Unlike some open addressing implementations, however, you will only get bad performance, not errors,
// if you use a poor hash function.
template < class Hash >
struct known_good : std::false_type {};

template < class BaseHash >
class selected_hash {
  public:
    BH_ALWAYS_INLINE uint64_t operator()( const auto& key ) const {
        if constexpr ( known_good< BaseHash >::value ) {
            return m_hash( key );
        } else {
            return hash_mix( static_cast< uint64_t >( m_hash( key ) ) );
        }
    }

  private:
    [[no_unique_address]] BaseHash m_hash;
};

/*********************************************************************************
 * My own Optional type, which supports optional references and monadic operations
 ********************************************************************************/

// If I included the header separately don't import all of this...
#ifndef SLM_OPTIONAL_HPP

/*
 * Some is a proxy reference type to be used only for initializing an Optional.
 * It always forms a reference to the value passed to it and uses other than
 * immediate initialization of an Optional are undefined.
 */
template < class T >
class Some;

template < class T >
class Some< const T& > {
  public:
    constexpr Some( const T& x ) noexcept : m_ref{ x } {}
    constexpr const T& unwrap() const noexcept { return m_ref; }

  private:
    const T& m_ref;
};

template < class T >
class Some< T&& > {
  public:
    constexpr Some( T&& x ) noexcept : m_ref{ x } {}
    constexpr T&& unwrap() const noexcept { return static_cast< T&& >( m_ref ); }

  private:
    T& m_ref;
};

/*
 * SomeRef is similar, but denotes that an optional should contain a reference rather
 * than the object itself. SomeRef always wraps an lvalue reference.
 */
template < class T >
class SomeRef {
  public:
    constexpr SomeRef( T& r ) noexcept : m_ref{ r } {}

    SomeRef( T&& ) = delete;

    constexpr T& unwrap() const noexcept { return m_ref; }

  private:
    T& m_ref;
};

template < class T >
Some( T&& x ) -> Some< T&& >;

/*
 * For non-const lvalue reference we construct a Some<const T&>, because the appropriate
 * constructor for Optional is the one that invokes copy-construction on T.
 */
template < class T >
Some( T& x ) -> Some< const T& >;

template < class T >
Some( const T& x ) -> Some< const T& >;

/*
 * Equivalent to std::nullopt_t and std::nullopt. I prefer to spell this as 'None'.
 */
struct NoneType {};

inline constexpr NoneType None;

namespace optional_detail {

template < class T >
struct is_some : std::false_type {};

template < class T >
struct is_some< Some< T > > : std::true_type {};

/*
 * gcc errors saying requires clauses are different on the different declarations of Optional
 * unless we wrap the conditions in a concept. Interestingly, clang accepts it without the
 * concept.
 */
template < class T >
concept AllowedOptional = std::negation_v< std::disjunction< std::is_same< T, NoneType >, is_some< T > > >;

} // namespace optional_detail

template < optional_detail::AllowedOptional T >
class Optional;

namespace optional_detail {

/*
 * These concepts are useful for defaulting copy/move assignment operators that should be
 * trivial.
 */
template < class T >
concept trivially_copy_assignable =
    std::is_trivially_copy_assignable_v< T > && std::is_trivially_copy_constructible_v< T > &&
    std::is_trivially_destructible_v< T >;

template < class T >
concept trivially_move_assignable =
    std::is_trivially_move_constructible_v< T > && std::is_trivially_move_assignable_v< T > &&
    std::is_trivially_destructible_v< T >;

template < class T >
constexpr inline bool is_optional = false;

template < class T >
constexpr inline bool is_optional< Optional< T > > = true;

/*
 * This concept is needed as a constraint on the comparison operators for non-optional type in
 * order to avoid a circular concept evaluation.
 */
template < class T >
concept NotOptional = !is_optional< std::remove_cvref_t< T > >;

template < class F, class T >
using bare_result_type = std::remove_cvref_t< std::invoke_result_t< F, T > >;

template < class F, class T >
BH_ALWAYS_INLINE constexpr bare_result_type< F, T > maybe_invoke( F&& f, T&& arg, bool cond ) {
    if ( cond ) {
        return std::invoke( std::forward< F >( f ), std::forward< T >( arg ) );
    } else {
        return bare_result_type< F, T >{};
    }
}

} // namespace optional_detail

template < optional_detail::AllowedOptional T >
class Optional {
  public:
    constexpr Optional() noexcept : empty_{} {}

    constexpr Optional( NoneType ) noexcept {}

    /*
     * Note the use of requires to make copy/move construction/assignment trivial if T is
     * trivially copy/move constructible/assignable.
     */
    constexpr Optional( const Optional& )
    requires std::is_trivially_copy_constructible_v< T >
    = default;

    constexpr Optional( const Optional& other ) noexcept( std::is_nothrow_copy_constructible_v< T > ) {
        if ( other.m_engaged ) {
            std::construct_at( &m_payload, other.m_payload );
            m_engaged = true;
        }
    }

    constexpr Optional( Optional&& )
    requires std::is_trivially_move_constructible_v< T >
    = default;

    constexpr Optional( Optional&& other ) noexcept( std::is_nothrow_move_constructible_v< T > ) {
        if ( other.m_engaged ) {
            std::construct_at( &m_payload, std::move( other.m_payload ) );
            m_engaged = true;
        }
    }

    template < class... Args >
    constexpr explicit Optional( std::in_place_t, Args&&... args )
        : m_payload( std::forward< Args >( args )... ), m_engaged{ true } {}

    template < class U >
    constexpr Optional( Some< const U& > x ) : m_payload( x.unwrap() ), m_engaged{ true } {}

    template < class U >
    constexpr Optional( Some< U&& > x ) : m_payload( x.unwrap() ), m_engaged{ true } {}

    constexpr ~Optional()
    requires std::is_trivially_destructible_v< T >
    = default;

    constexpr ~Optional() noexcept( std::is_nothrow_destructible_v< T > ) {
        if ( m_engaged ) {
            m_payload.~T();
        }
    }

    constexpr Optional& operator=( NoneType ) noexcept( std::is_nothrow_destructible_v< T > ) {
        if constexpr ( !std::is_trivially_destructible_v< T > ) {
            if ( m_engaged ) {
                m_payload.~T();
            }
        }
        m_engaged = false;
        return *this;
    }

    constexpr Optional& operator=( const Optional& )
    requires optional_detail::trivially_copy_assignable< T >
    = default;

    constexpr Optional& operator=( const Optional& other ) noexcept(
        std::conjunction_v< std::is_nothrow_copy_constructible< T >, std::is_nothrow_copy_assignable< T >,
                            std::is_nothrow_destructible< T > > ) {
        if ( m_engaged && other.m_engaged ) {
            m_payload = other.m_payload;
        } else if ( m_engaged ) {
            m_payload.~T();
            m_engaged = false;
        } else if ( other.m_engaged ) {
            std::construct_at( &m_payload, other.m_payload );
            m_engaged = true;
        }
        return *this;
    }

    constexpr Optional& operator=( Optional&& )
    requires optional_detail::trivially_move_assignable< T >
    = default;

    constexpr Optional& operator=( Optional&& other ) noexcept(
        std::conjunction_v< std::is_nothrow_move_assignable< T >, std::is_nothrow_move_constructible< T >,
                            std::is_nothrow_destructible< T > > ) {
        if ( m_engaged && other.m_engaged ) {
            m_payload = static_cast< T&& >( other.m_payload );
        } else if ( m_engaged ) {
            m_payload.~T();
            m_engaged = false;
        } else if ( other.m_engaged ) {
            std::construct_at( &m_payload, static_cast< T&& >( other.m_payload ) );
            m_engaged = true;
        }
        return *this;
    }

    template < class U >
    constexpr Optional& operator=( Some< const U& > x ) {
        if ( m_engaged ) {
            m_payload = x.unwrap();
        } else {
            std::construct_at( &m_payload, x.unwrap() );
            m_engaged = true;
        }
        return *this;
    }

    template < class U >
    constexpr Optional& operator=( Some< U&& > x ) {
        if ( m_engaged ) {
            m_payload = x.unwrap();
        } else {
            std::construct_at( &m_payload, x.unwrap() );
            m_engaged = true;
        }
        return *this;
    }

    constexpr const T* operator->() const noexcept {
        assert( m_engaged && "dereferencing disengaged Optional" );
        return &m_payload;
    }

    constexpr T* operator->() noexcept {
        assert( m_engaged && "dereferencing disengaged Optional" );
        return &m_payload;
    }

    constexpr const T& operator*() const& noexcept {
        assert( m_engaged && "dereferencing disengaged Optional" );
        return m_payload;
    }

    constexpr T& operator*() & noexcept {
        assert( m_engaged && "dereferencing disengaged Optional" );
        return m_payload;
    }

    constexpr const T&& operator*() const&& noexcept {
        assert( m_engaged && "dereferencing disengaged Optional" );
        return static_cast< const T&& >( m_payload );
    }

    constexpr T&& operator*() && noexcept {
        assert( m_engaged && "dereferencing disengaged Optional" );
        return static_cast< T&& >( m_payload );
    }

    constexpr explicit operator bool() const noexcept { return m_engaged; }

    constexpr bool has_value() const noexcept { return m_engaged; }

    constexpr const T& value() const& noexcept {
        if ( !m_engaged ) {
            std::abort();
        }
        return m_payload;
    }

    constexpr T& value() & noexcept {
        if ( !m_engaged ) {
            std::abort();
        }
        return m_payload;
    }

    constexpr T&& value() && noexcept {
        if ( !m_engaged ) {
            std::abort();
        }
        return static_cast< T&& >( m_payload );
    }

    constexpr const T&& value() const&& noexcept {
        if ( !m_engaged ) {
            std::abort();
        }
        return static_cast< const T&& >( m_payload );
    }

    template < class U >
    constexpr T value_or( U&& default_value ) const& {
        return m_engaged ? m_payload : static_cast< T >( std::forward< U >( default_value ) );
    }

    template < class U >
    constexpr T value_or( U&& default_value ) && {
        return m_engaged ? std::move( m_payload ) : static_cast< T >( std::forward< U >( default_value ) );
    }

    template < class F >
    constexpr auto and_then( F&& f ) &
    requires optional_detail::is_optional< optional_detail::bare_result_type< F, T& > >
    {
        return optional_detail::maybe_invoke( std::forward< F >( f ), m_payload, m_engaged );
    }

    template < class F >
    constexpr auto and_then( F&& f ) const&
    requires optional_detail::is_optional< optional_detail::bare_result_type< F, const T& > >
    {
        return optional_detail::maybe_invoke( std::forward< F >( f ), m_payload, m_engaged );
    }

    template < class F >
    constexpr auto and_then( F&& f ) &&
    requires optional_detail::is_optional< optional_detail::bare_result_type< F, T > >
    {
        return optional_detail::maybe_invoke( std::forward< F >( f ), std::move( m_payload ), m_engaged );
    }

    template < class F >
    constexpr auto and_then( F&& f ) const&&
    requires optional_detail::is_optional< optional_detail::bare_result_type< F, const T > >
    {
        return optional_detail::maybe_invoke( std::forward< F >( f ), std::move( m_payload ), m_engaged );
    }

    template < class F >
    constexpr auto transform( F&& f ) & {
        using result_type = std::invoke_result_t< F, T& >;
        if constexpr ( std::is_rvalue_reference_v< result_type > ) {
            return m_engaged
                       ? Optional< std::remove_cvref_t< result_type > >( std::forward< F >( f ), m_payload )
                       : None;
        } else if constexpr ( std::is_reference_v< result_type > ) {
            return m_engaged
                       ? Optional< result_type >( SomeRef( std::invoke( std::forward< F >( f ), m_payload ) ) )
                       : None;
        } else {
            return m_engaged ? Optional< result_type >( std::forward< F >( f ), m_payload ) : None;
        }
    }

    template < class F >
    constexpr auto transform( F&& f ) const& {
        using result_type = std::invoke_result_t< F, const T& >;
        if constexpr ( std::is_rvalue_reference_v< result_type > ) {
            return m_engaged
                       ? Optional< std::remove_cvref_t< result_type > >( std::forward< F >( f ), m_payload )
                       : None;
        } else if constexpr ( std::is_reference_v< result_type > ) {
            return m_engaged
                       ? Optional< result_type >( SomeRef( std::invoke( std::forward< F >( f ), m_payload ) ) )
                       : None;
        } else {
            return m_engaged ? Optional< result_type >( std::forward< F >( f ), m_payload ) : None;
        }
    }

    template < class F >
    constexpr auto transform( F&& f ) && {
        using result_type = std::invoke_result_t< F, T&& >;
        if constexpr ( std::is_rvalue_reference_v< result_type > ) {
            return m_engaged ? Optional< std::remove_cvref_t< result_type > >( std::forward< F >( f ),
                                                                               std::move( m_payload ) )
                             : None;
        } else if constexpr ( std::is_reference_v< result_type > ) {
            return m_engaged ? Optional< result_type >(
                                   SomeRef( std::invoke( std::forward< F >( f ), std::move( m_payload ) ) ) )
                             : None;
        } else {
            return m_engaged ? Optional< result_type >( std::forward< F >( f ), std::move( m_payload ) ) : None;
        }
    }

    template < class F >
    constexpr auto transform( F&& f ) const&& {
        using result_type = std::invoke_result_t< F, const T&& >;
        if constexpr ( std::is_rvalue_reference_v< result_type > ) {
            return m_engaged ? Optional< std::remove_cvref_t< result_type > >( std::forward< F >( f ),
                                                                               std::move( m_payload ) )
                             : None;
        } else if constexpr ( std::is_reference_v< result_type > ) {
            return m_engaged ? Optional< result_type >(
                                   SomeRef( std::invoke( std::forward< F >( f ), std::move( m_payload ) ) ) )
                             : None;
        } else {
            return m_engaged ? Optional< result_type >( std::forward< F >( f ), std::move( m_payload ) ) : None;
        }
    }

    template < class F >
    constexpr Optional or_else( F&& f ) const&
    requires std::conjunction_v<
        std::is_same< std::remove_cvref_t< std::invoke_result_t< F > >, Optional< T > >,
        std::is_copy_constructible< T >, std::is_invocable< F > >
    {
        return m_engaged ? *this : std::invoke( std::forward< F >( f ) );
    }

    template < class F >
    constexpr Optional or_else( F&& f ) &&
    requires std::conjunction_v<
        std::is_same< std::remove_cvref_t< std::invoke_result_t< F > >, Optional< T > >,
        std::is_move_constructible< T >, std::is_invocable< F > >
    {
        return m_engaged ? std::move( *this ) : std::invoke( std::forward< F >( f ) );
    }

    constexpr void swap( Optional& other ) noexcept(
        std::conjunction_v< std::is_nothrow_move_constructible< T >, std::is_nothrow_swappable< T >,
                            std::is_nothrow_destructible< T > > ) {
        using std::swap;
        if ( m_engaged ) {
            if ( other.m_engaged ) {
                swap( m_payload, other.m_payload );
            } else {
                std::construct_at( &other.m_payload, std::move( m_payload ) );
                other.m_engaged = true;
                m_payload.~T();
                m_engaged = false;
            }
        } else if ( other.m_engaged ) {
            std::construct_at( &m_payload, std::move( other.m_payload ) );
            m_engaged = true;
            other.m_payload.~T();
            other.m_engaged = false;
        }
    }

    friend void swap( Optional& a, Optional& b ) noexcept( noexcept( a.swap( b ) ) ) { a.swap( b ); }

    constexpr void reset() noexcept {
        if ( m_engaged ) {
            m_payload.~T();
            m_engaged = false;
        }
    }

    template < class... Args >
    constexpr T& emplace( Args&&... args ) {
        reset();
        std::construct_at( &m_payload, std::forward< Args >( args )... );
        m_engaged = true;
        return m_payload;
    }

  private:
    struct Empty {};

    union {
        Empty empty_;
        T m_payload;
    };
    bool m_engaged{ false };

    template < optional_detail::AllowedOptional U >
    friend class Optional;

    template < class F, class U >
    constexpr explicit Optional( F&& f, U&& u )
    requires std::invocable< F&&, U&& >
        : m_payload( std::invoke( std::forward< F >( f ), std::forward< U >( u ) ) ), m_engaged{ true } {}
};

template < optional_detail::AllowedOptional T >
class Optional< T& > {
  public:
    constexpr Optional() = default;
    constexpr Optional( const Optional& ) = default;
    constexpr Optional( Optional&& ) = default;
    constexpr Optional( NoneType ) noexcept {}

    constexpr Optional( SomeRef< T > ref ) noexcept : m_ptr{ &ref.unwrap() } {}

    constexpr Optional( SomeRef< std::remove_const_t< T > > ref ) noexcept
    requires std::is_const_v< T >
        : m_ptr{ &ref.unwrap() } {}

    constexpr Optional( const Optional< std::remove_const_t< T >& >& other ) noexcept
    requires std::is_const_v< T >
        : m_ptr{ other.m_ptr } {}

    constexpr Optional& operator=( NoneType ) noexcept {
        m_ptr = nullptr;
        return *this;
    }
    constexpr Optional& operator=( const Optional& other ) = default;
    constexpr Optional& operator=( Optional&& other ) = default;
    constexpr Optional& operator=( SomeRef< T > ref ) noexcept {
        m_ptr = &ref.unwrap();
        return *this;
    }

    constexpr T& operator*() const noexcept {
        assert( m_ptr && "Disengaged optional access" );
        return *m_ptr;
    }

    constexpr T* operator->() const noexcept {
        assert( m_ptr && "Disengaged optional access" );
        return m_ptr;
    }

    constexpr explicit operator bool() const noexcept { return m_ptr != nullptr; }

    constexpr bool has_value() const noexcept { return (bool)*this; }

    constexpr T& value() const noexcept {
        assert( m_ptr && "Disengaged optional access" );
        return *m_ptr;
    }

    constexpr T& value_or( T& default_value ) const noexcept { return m_ptr ? *m_ptr : default_value; }

    template < class F >
    constexpr auto and_then( F&& f ) const
    requires optional_detail::is_optional< optional_detail::bare_result_type< F, T& > >
    {
        return m_ptr ? std::invoke( std::forward< F >( f ), *m_ptr )
                     : optional_detail::bare_result_type< F, T& >();
    }

    template < class F >
    constexpr auto transform( F&& f ) const {
        using result_type = std::invoke_result_t< F, T& >;
        if constexpr ( std::is_rvalue_reference_v< result_type > ) {
            return m_ptr ? Optional< std::remove_cvref_t< result_type > >( std::forward< F >( f ), *m_ptr )
                         : None;
        } else if constexpr ( std::is_reference_v< result_type > ) {
            return m_ptr ? Optional( SomeRef( std::invoke( std::forward< F >( f ), *m_ptr ) ) ) : None;
        } else {
            return m_ptr ? Optional< result_type >( std::forward< F >( f ), *m_ptr ) : None;
        }
    }

    template < class F >
    constexpr Optional or_else( F&& f ) const
    requires std::conjunction_v<
        std::disjunction< std::is_same< std::remove_cvref_t< std::invoke_result_t< F > >, Optional< T& > >,
                          std::is_same< std::remove_cvref_t< std::invoke_result_t< F > >,
                                        Optional< std::remove_const_t< T >& > > >,
        std::is_invocable< F > >
    {
        return m_ptr ? *this : std::invoke( std::forward< F >( f ) );
    }

    constexpr void swap( Optional& other ) noexcept { std::swap( m_ptr, other.m_ptr ); }
    friend void swap( Optional& a, Optional& b ) noexcept { a.swap( b ); }

    constexpr void reset() noexcept { m_ptr = nullptr; }

  private:
    template < optional_detail::AllowedOptional U >
    friend class Optional;

    T* m_ptr{ nullptr };
};

/******************************************************************************************
 * Deduction guides
 *****************************************************************************************/
template < class T >
Optional( Some< const T& > ) -> Optional< T >;

template < class T >
Optional( Some< T&& > ) -> Optional< T >;

template < class T >
Optional( Some< const T&& > ) -> Optional< T >;

template < class T >
Optional( SomeRef< T > ) -> Optional< T& >;

template < class T >
Optional( SomeRef< const T > ) -> Optional< const T& >;

/******************************************************************************************
 * Comparison operators
 *****************************************************************************************/

template < class T, std::equality_comparable_with< T > U >
inline constexpr bool operator==( const Optional< T >& a, const Optional< U >& b ) {
    if ( a ) {
        if ( b ) {
            return *a == *b;
        }
        return false;
    } else if ( b ) {
        return false;
    }
    return true;
}

template < class T, std::three_way_comparable_with< T > U >
inline constexpr std::compare_three_way_result_t< T, U > operator<=>( const Optional< T >& a,
                                                                      const Optional< U >& b ) {
    if ( a ) {
        if ( b ) {
            return *a <=> *b;
        }
        return std::compare_three_way_result_t< T, U >::greater;
    } else if ( b ) {
        return std::compare_three_way_result_t< T, U >::less;
    }
    return std::compare_three_way_result_t< T, U >::equivalent;
}

template < class T >
inline constexpr bool operator==( const Optional< T >& a, const NoneType& ) {
    return !a;
}

template < class T >
inline constexpr std::strong_ordering operator<=>( const Optional< T >& a, const NoneType& ) {
    return a ? std::strong_ordering::greater : std::strong_ordering::equivalent;
}

/*
 * Without the NotOptional constraint you get a circular evaluation of constraints
 * from recursion in the standard lib.
 */
template < class T, optional_detail::NotOptional U >
requires std::equality_comparable_with< T, U >
inline constexpr bool operator==( const Optional< T >& a, const U& b ) {
    return a && *a == b;
}

template < class T, optional_detail::NotOptional U >
requires std::three_way_comparable_with< T, U >
inline constexpr std::compare_three_way_result_t< T, U > operator<=>( const Optional< T >& a, const U& b ) {
    if ( a ) {
        return *a <=> b;
    }
    return std::compare_three_way_result_t< T, U >::less;
}

#else

using slm::None;
using slm::Optional;
using slm::SomeRef;

#endif // SLM_OPTIONAL_HPP

/********************************************************************************
 * Configuration structs to set the behavior of assignment in the backend set
 * implementation. This is needed to implement unordered_map generically with a
 * set as backend.
 *******************************************************************************/
template < class T >
struct default_assign {
    static void do_assignment( T& a, const T& b ) noexcept( std::is_nothrow_copy_assignable_v< T > ) { a = b; }

    static void do_assignment( T& a, T&& b ) noexcept( std::is_nothrow_move_assignable_v< T > ) {
        a = std::move( b );
    }

    template < class... Args >
    static void do_assignment( T& a, Args&&... args ) noexcept( std::is_nothrow_constructible_v< T, Args... > &&
                                                                std::is_nothrow_move_assignable_v< T > ) {
        a = T( std::forward< Args >( args )... );
    }
};

/********************************************************************************
 * Base iterator class.
 *******************************************************************************/
struct EmptySlotTag {};

template < class Bucket, bool is_const >
struct SetIterator {
    typedef typename Bucket::value_type value_type;
    typedef std::conditional_t< is_const, const value_type*, value_type* > pointer;
    typedef std::conditional_t< is_const, const value_type&, value_type& > reference;
    typedef std::ptrdiff_t difference_type;
    typedef std::forward_iterator_tag iterator_category;
    typedef typename Bucket::mask_type mask_type;
    static_assert( std::is_unsigned_v< mask_type > );

    SetIterator() noexcept = default;

    explicit SetIterator( Bucket* bucket )
        : m_bucket{ bucket }, m_slot_index{ std::numeric_limits< mask_type >::digits } {
        assert( m_bucket->is_sentinel() );
    }

    explicit SetIterator( Bucket* bucket, int slot_index ) noexcept
        : m_bucket{ bucket }, m_slot_index{ slot_index } {
        assert( m_bucket );
        m_bucket_mask = m_bucket->occupied_mask();
        assert( m_bucket_mask & ( mask_type( 1 ) << slot_index ) );
        m_bucket_mask &= ( ~mask_type( 0 ) << slot_index );
        m_bucket_mask ^= ( mask_type( 1 ) << slot_index );
    }

    explicit SetIterator( Bucket* bucket, int slot_index, EmptySlotTag ) noexcept : m_bucket{ bucket } {
        assert( m_bucket );
        m_bucket_mask = m_bucket->occupied_mask();
        m_bucket_mask &= ( ~mask_type( 0 ) << slot_index );
        scan_to_slot();
    }

    SetIterator& operator++() noexcept {
        scan_to_slot();
        return *this;
    }

    SetIterator operator++( int ) noexcept {
        auto out = *this;
        this->operator++();
        return out;
    }

    template < bool B >
    bool operator==( const SetIterator< Bucket, B >& other ) const noexcept {
        return m_bucket == other.m_bucket && m_slot_index == other.m_slot_index;
    }

    template < bool B >
    bool operator!=( const SetIterator< Bucket, B >& other ) const noexcept {
        return !( *this == other );
    }

    reference operator*() const noexcept { return m_bucket->get( m_slot_index ); }
    pointer operator->() const noexcept { return &m_bucket->get( m_slot_index ); }

  private:
    template < class >
    friend class HashSetBase;

    Bucket* m_bucket{ nullptr };
    mask_type m_bucket_mask{ 0 };
    int m_slot_index{ 0 };

    void scan_to_slot() noexcept {
        m_slot_index = std::countr_zero( m_bucket_mask );
        while ( !m_bucket_mask && !m_bucket->is_sentinel() ) {
            ++m_bucket;
            m_bucket_mask = m_bucket->occupied_mask();
            m_slot_index = std::countr_zero( m_bucket_mask );
            if ( unlikely( m_bucket->is_sentinel() ) ) {
                return;
            }
        }
        m_bucket_mask ^= ( mask_type( 1 ) << m_slot_index );
    }
};

// To use heterogeneous lookup, specialize is_transparent_hash for your hash type
// Comparison is assumed to also be transparent if is_transparent_hash is specialized.
template < class Hash, class K >
struct is_transparent_hash : std::false_type {};

// Broken out this way so that if a concept is not satisfied it's easier to identify the problem.
template < class Hash, class K >
concept transparent_hash = is_transparent_hash< Hash, std::remove_cvref_t< K > >::value;

template < class K, class Traits >
concept same_as_key = std::is_same_v< std::remove_cvref_t< K >, typename Traits::key_type >;

template < class K, class Traits >
concept transparent_key =
    same_as_key< K, Traits > || transparent_hash< typename Traits::original_hash_type, K >;

// Construct tag to buckets to indicate we should construct an end sentinel.
struct EndSentinelTag {};

template < class Traits >
class HashSetBase {
    typedef Traits::bucket_type bucket_type;
    typedef bucket_type::value_type element_type;
    typedef bucket_type::mask_type mask_type;
    typedef Traits::key_type key_type;
    typedef Traits::value_type value_type;

    struct Location {
        bucket_type* bucket;
        int slot;
        int probe_length;

        BH_ALWAYS_INLINE bool new_insertion() const {
            return !bucket || ( slot < 0 || !bucket->occupied( slot ) );
        }
    };

    static_assert( std::is_trivially_destructible_v< bucket_type > );
    static_assert( std::is_trivially_copyable_v< bucket_type > );

    static inline const bucket_type end_sentinel = bucket_type( EndSentinelTag{} );
    bucket_type* m_buckets{ const_cast< bucket_type* >( &end_sentinel ) };
    // bitwise and with m_bitmask gives us bucket index.
    size_type m_bitmask{ 0 };
    size_type m_occupied = 0;
    size_type m_rehash = 0;
    float m_max_load_factor{ bucket_type::default_load_factor };
    [[no_unique_address]] Traits m_traits;

    int evict( bucket_type* bucket ) {
        bucket_type* original_bucket = bucket;
        int slot = bucket->min_probe_length_slot();
        int original_slot = slot;
        auto [ evicted, hash_bits, probe_len ] = bucket->extract( slot, m_traits );

        size_type bucket_index = bucket - m_buckets + 1;
        bucket_index &= m_bitmask;
        probe_len++;
        while ( likely( probe_len ) ) {
            bucket = m_buckets + bucket_index;
            auto empty_mask = bucket->empty_slots();
            if ( empty_mask ) {
                slot = std::countr_zero( empty_mask );
                bucket->emplace( slot, std::move( evicted ), hash_bits, probe_len, m_traits );
                return original_slot;
            } else if ( bucket->all_probe_lengths_shorter_than( probe_len ) ) {
                slot = bucket->min_probe_length_slot();
                bucket->swap( slot, evicted, hash_bits, probe_len );
            }

            bucket_index = ( bucket_index + 1 ) & m_bitmask;
            probe_len++;
        }

        // Failed due to overflowed probe length. Reinsert the removed item, and return a negative
        // slot number to signal failure. Caller is responsible for rehashing.
        original_bucket->emplace( original_slot, std::move( evicted ), hash_bits, probe_len, m_traits );
        return -1;
    }

    BH_ALWAYS_INLINE void destroy_entries() {
        if constexpr ( !std::is_trivially_destructible_v< element_type > ) {
            for ( size_type i = 0; i < num_buckets(); ++i ) {
                m_traits.destroy_at( m_buckets + i );
            }
        }
    }

    BH_ALWAYS_INLINE void rehash_insert( element_type&& element ) {
        const auto& key = Traits::get_key( element );
        size_type hash_val = hash( key );
        auto location = locate( key, hash_val );
        assert( location.new_insertion() );
        assert( location.bucket );
        emplace_at( location, std::move( element ), hash_val );
    }

    BH_ALWAYS_INLINE Optional< element_type& > unchecked_emplace( const Location& where, auto&& val,
                                                                  size_type hash_val ) {
        m_occupied++;
        return SomeRef( where.bucket->emplace( where.slot, std::forward< decltype( val ) >( val ), hash_val,
                                               where.probe_length, m_traits ) );
    }

  protected:
    HashSetBase() = default;

    HashSetBase( HashSetBase&& other )
        : m_buckets{ other.m_buckets }, m_bitmask{ other.m_bitmask }, m_occupied{ other.m_occupied },
          m_rehash{ other.m_rehash }, m_max_load_factor{ other.m_max_load_factor },
          m_traits{ std::move( other.m_traits ) } {
        other.m_buckets = const_cast< bucket_type* >( &end_sentinel );
        other.m_bitmask = other.m_occupied = other.m_rehash = 0;
    }

    HashSetBase& operator=( HashSetBase&& other ) {
        if ( this == &other ) {
            return *this;
        }
        destroy_entries();
        if ( m_bitmask ) {
            m_traits.deallocate( m_buckets, num_buckets() + 1 );
        }

        m_buckets = other.m_buckets;
        m_bitmask = other.m_bitmask;
        m_occupied = other.m_occupied;
        m_rehash = other.m_rehash;
        m_max_load_factor = other.m_max_load_factor;
        m_traits = std::move( other.m_traits );
        other.m_buckets = const_cast< bucket_type* >( &end_sentinel );
        other.m_bitmask = other.m_occupied = other.m_rehash = 0;
        return *this;
    }

    HashSetBase( const HashSetBase& other )
        : m_bitmask{ other.m_bitmask }, m_occupied{ other.m_occupied }, m_rehash{ other.m_rehash },
          m_max_load_factor{ other.m_max_load_factor }, m_traits{ other.m_traits } {
        if ( !m_bitmask ) {
            return;
        }
        m_buckets = m_traits.allocate( num_buckets() + 1 );
        if constexpr ( std::is_trivially_copyable_v< value_type > ) {
            std::memcpy( m_buckets, other.m_buckets, ( num_buckets() + 1 ) * sizeof( bucket_type ) );
        } else {
            for ( size_type i = 0; i < num_buckets(); ++i ) {
                m_traits.construct_at( m_buckets + i, other.m_buckets[ i ], m_traits );
            }
            m_traits.construct_at( m_buckets + num_buckets(), EndSentinelTag{} );
        }
    }

    HashSetBase& operator=( const HashSetBase& other ) {
        if ( this == &other ) {
            return *this;
        }
        if ( other.empty() ) {
            clear();
            m_max_load_factor = other.m_max_load_factor;
            m_traits = other.m_traits;
            return *this;
        }

        destroy_entries();

        m_occupied = other.m_occupied;
        m_rehash = other.m_rehash;
        m_max_load_factor = other.m_max_load_factor;
        if ( m_bitmask != other.m_bitmask ) {
            if ( m_bitmask ) {
                m_traits.deallocate( m_buckets, num_buckets() + 1 );
            }
            m_bitmask = other.m_bitmask;
            m_traits = other.m_traits;
            m_buckets = m_traits.allocate( num_buckets() + 1 );
            m_traits.construct_at( m_buckets + num_buckets(), EndSentinelTag{} );
        } else {
            m_traits = other.m_traits;
        }

        if constexpr ( std::is_trivially_copyable_v< value_type > ) {
            std::memcpy( m_buckets, other.m_buckets, ( num_buckets() + 1 ) * sizeof( bucket_type ) );
        } else {
            for ( size_type i = 0; i < num_buckets(); ++i ) {
                m_traits.construct_at( m_buckets + i, other.m_buckets[ i ], m_traits );
            }
        }

        return *this;
    }

    void swap( HashSetBase& other ) {
        using std::swap;
        swap( m_buckets, other.m_buckets );
        swap( m_bitmask, other.m_bitmask );
        swap( m_occupied, other.m_occupied );
        swap( m_rehash, other.m_rehash );
        swap( m_max_load_factor, other.m_max_load_factor );
        swap( m_traits, other.m_traits );
    }

    friend void swap( HashSetBase& a, HashSetBase& b ) { a.swap( b ); }

    ~HashSetBase() {
        destroy_entries();
        if ( m_bitmask ) {
            m_traits.deallocate( m_buckets, num_buckets() + 1 );
        }
    }

    static constexpr auto max_num_buckets = std::bit_floor( std::numeric_limits< size_type >::max() );

    BH_ALWAYS_INLINE bool should_rehash() const { return m_occupied >= m_rehash; }

    BH_ALWAYS_INLINE size_type hash( const key_type& key ) const {
        return static_cast< size_type >( m_traits.hash( key ) );
    }

    SetIterator< bucket_type, false > begin() {
        return m_bitmask ? SetIterator< bucket_type, false >( m_buckets, 0, EmptySlotTag{} ) : end();
    }

    SetIterator< bucket_type, false > end() {
        return SetIterator< bucket_type, false >( m_buckets == &end_sentinel ? &end_sentinel
                                                                             : m_buckets + num_buckets() );
    }

    SetIterator< bucket_type, true > cbegin() const {
        return m_bitmask ? SetIterator< bucket_type, true >( m_buckets, 0, EmptySlotTag{} ) : cend();
    }

    SetIterator< bucket_type, true > cend() const {
        return SetIterator< bucket_type, true >( m_buckets == &end_sentinel
                                                     ? const_cast< bucket_type* >( &end_sentinel )
                                                     : m_buckets + num_buckets() );
    }

    BH_ALWAYS_INLINE Optional< element_type& > emplace_at( Location where, auto&& val, size_type hash_val ) {
        while ( unlikely( where.slot < 0 ) ) {
            int evicted_slot = evict( where.bucket );
            where.slot = evicted_slot;
            if ( unlikely( evicted_slot < 0 ) ) {
                rehash( 0 );
                where = locate( val, hash_val );
            }
        }
        return unchecked_emplace( where, std::forward< decltype( val ) >( val ), hash_val );
    }

    BH_ALWAYS_INLINE void erase_at( bucket_type* bucket, size_type slot ) {
        assert( bucket );
        size_type bucket_index = bucket - m_buckets;
        assert( bucket_index < num_buckets() );
        assert( slot < bucket_type::num_slots );
        m_occupied--;
        do {
            mask_type empty_mask = bucket->empty_slots();
            if ( empty_mask ) {
                bucket->destroy( slot, m_traits );
                return;
            }

            // If this bucket was full we need to check if we can shift backward.
            bucket_index++;
            bucket_index &= m_bitmask;
            bucket_type* next_bucket = m_buckets + bucket_index;
            auto [ max_slot, max_probe_length ] = next_bucket->max_probe_length_slot();

            // If all probe lengths in next bucket are 0 then we cannot shift backward and we are done
            if ( max_probe_length == 0 ) {
                bucket->destroy( slot, m_traits );
                return;
            }

            // Actually shift backward.
            // steal_from should leave the next bucket with bookkeeping data intact so that 'empty_slots'
            // returns a correct mask.
            bucket->steal_from( slot, next_bucket, max_slot );
            bucket = next_bucket;
            slot = max_slot;
        } while ( true );
    }

    /*
     * Find an existing key or the location to insert one. Returned is pointer to the bucket,
     * slot index, and the probe length. If we need to evict an entry the returned slot index
     * is < 0 to indicate the fact. If probe length overflows an 8-bit integer the returned
     * probe length is 256 and returned bucket is nullptr (this requires a rehash).
     */
    template < transparent_key< Traits > K >
    BH_ALWAYS_INLINE Location locate( K&& key, size_type hash_val ) const {
        auto bucket_index = hash_val & m_bitmask;
        int probe_length = 0;
        auto check_bits = bucket_type::get_check_bits( hash_val );

        do {
            bucket_type* bucket = m_buckets + bucket_index;
            auto match_mask = bucket->matching_slots( check_bits );
            while ( match_mask ) {
                int slot = std::countr_zero( match_mask );
                if ( likely( m_traits.compare( bucket->get( slot ), key ) ) ) {
                    return { bucket, slot, probe_length };
                }
                match_mask ^= mask_type( 1 ) << slot;
            }

            auto empty_mask = bucket->empty_slots();
            if ( empty_mask ) {
                int slot = std::countr_zero( empty_mask );
                return { bucket, slot, probe_length };
            }

            if ( unlikely( bucket->all_probe_lengths_shorter_than( probe_length ) ) ) {
                return { bucket, -1, probe_length };
            }

            probe_length++;
            bucket_index = ( bucket_index + 1 ) & m_bitmask;
        } while ( probe_length < 256 );

        return { nullptr, 0, 256 };
    }

    template < bool B >
    BH_ALWAYS_INLINE static bucket_type* get_bucket( const SetIterator< bucket_type, B >& it ) {
        return it.m_bucket;
    }

    template < bool B >
    BH_ALWAYS_INLINE static int get_slot( const SetIterator< bucket_type, B >& it ) {
        return it.m_slot_index;
    }

  public:
    size_type size() const { return m_occupied; }

    bool empty() const { return m_occupied == 0; }

    BH_ALWAYS_INLINE size_type num_buckets() const { return size_type( 1 ) << std::countr_one( m_bitmask ); }

    size_type capacity() const { return m_rehash; }

    void clear() {
        destroy_entries();
        if ( m_bitmask ) {
            m_traits.deallocate( m_buckets, num_buckets() + 1 );
        }

        m_buckets = const_cast< bucket_type* >( &end_sentinel );
        m_occupied = m_bitmask = m_rehash = 0;
    }

    void set_max_load_factor( float lf ) {
        assert( lf > 0 && lf < 1 && "Max load factor must be in (0, 1)" );
        m_max_load_factor = lf;
        if ( m_buckets != &end_sentinel ) {
            m_rehash = ( num_buckets() * m_max_load_factor ) * bucket_type::num_slots;
        } else {
            assert( m_rehash == 0 );
        }
    }

    void rehash( size_type sz ) {
        size_type current_num_buckets = num_buckets();
        size_type new_num_buckets;
        double lf = m_max_load_factor;
        if ( sz ) {
            double new_num_buckets_ = ( sz / lf ) / bucket_type::num_slots;
            assert( new_num_buckets_ * bucket_type::num_slots < static_cast< float >( max_num_buckets ) &&
                    "Overflowed size_type" );
            new_num_buckets = new_num_buckets_;
            new_num_buckets = std::bit_ceil( new_num_buckets );
            size_type new_capacity = lf * ( new_num_buckets * bucket_type::num_slots );
            if ( new_capacity < sz ) {
                new_num_buckets *= 2;
            }

            if ( new_num_buckets <= current_num_buckets ) {
                return;
            }
        } else {
            new_num_buckets = bounds_checked_mul( current_num_buckets, 2 );
        }
        bucket_type* new_buckets = m_traits.allocate( new_num_buckets + 1 );
        for ( size_type i = 0; i < new_num_buckets; ++i ) {
            m_traits.construct_at( new_buckets + i );
        }
        m_traits.construct_at( new_buckets + new_num_buckets, EndSentinelTag{} );

        std::swap( new_buckets, m_buckets );
        m_occupied = 0;
        m_bitmask = ~( ~size_type( 0 ) << std::countr_zero( new_num_buckets ) );
        m_rehash = lf * ( new_num_buckets * bucket_type::num_slots );

        if ( current_num_buckets > 1 ) {
            SetIterator< bucket_type, false > it{ new_buckets, 0, EmptySlotTag{} };
            SetIterator< bucket_type, false > end_( new_buckets + current_num_buckets );
            while ( it != end_ ) {
                rehash_insert( std::move( *it ) );
                it->~element_type();
                ++it;
            }
            m_traits.deallocate( new_buckets, current_num_buckets + 1 );
        }
    }
};

template < class T, class Hash, class Compare, class Allocator, template < class > class Bucket >
class TraitsForSet {
    [[no_unique_address]] selected_hash< Hash > m_hash;
    [[no_unique_address]] Compare m_comparison;
    [[no_unique_address]] Allocator m_allocator;

  public:
    typedef T key_type;
    typedef T value_type;
    typedef Bucket< T > bucket_type;
    typedef selected_hash< Hash > hash_type;
    typedef Hash original_hash_type;
    typedef Compare comparison_type;
    typedef std::allocator_traits< Allocator >::template rebind_alloc< bucket_type > bucket_allocator;
    typedef std::allocator_traits< Allocator >::template rebind_alloc< value_type > value_allocator;

    static const key_type& get_key( const key_type& key ) { return key; }

    TraitsForSet() = default;
    TraitsForSet( const Allocator& alloc ) : m_allocator{ alloc } {}
    TraitsForSet( TraitsForSet&& ) = default;
    TraitsForSet( const TraitsForSet& other )
        : m_hash{ other.m_hash }, m_comparison{ other.m_comparison },
          m_allocator{
              std::allocator_traits< Allocator >::select_on_container_copy_construction( other.m_allocator ) } {
    }

    TraitsForSet& operator=( TraitsForSet&& other ) {
        m_hash = std::move( other.m_hash );
        m_comparison = std::move( other.m_comparison );
        if constexpr ( std::allocator_traits< Allocator >::propagate_on_container_move_assignment::value ) {
            m_allocator = std::move( other.m_allocator );
        }
        return *this;
    }

    TraitsForSet& operator=( const TraitsForSet& other ) {
        m_hash = other.m_hash;
        m_comparison = std::move( other.m_comparison );
        if constexpr ( std::allocator_traits< Allocator >::propagate_on_container_copy_assignment::value ) {
            m_allocator = other.m_allocator;
        }
        return *this;
    }

    friend void swap( TraitsForSet& a, TraitsForSet& b ) {
        using std::swap;
        swap( a.m_hash, b.m_hash );
        swap( a.m_comparison, b.m_comparison );
        if constexpr ( std::allocator_traits< Allocator >::propagate_on_container_swap::value ) {
            swap( a.m_allocator, b.m_allocator );
        }
    }

    auto hash( auto&& key ) const { return m_hash( key ); }
    bool compare( auto&& k1, auto&& k2 ) const { return m_comparison( k1, k2 ); }

    bucket_type* allocate( size_type n ) {
        bucket_allocator alloc( m_allocator );
        return std::allocator_traits< bucket_allocator >::allocate( alloc, n );
    }

    void deallocate( bucket_type* mem, size_type n ) {
        bucket_allocator alloc( m_allocator );
        std::allocator_traits< bucket_allocator >::deallocate( alloc, mem, n );
    }

    BH_ALWAYS_INLINE void construct_at( value_type* where, auto&&... args ) {
        value_allocator alloc( m_allocator );
        std::allocator_traits< value_allocator >::construct( m_allocator, where,
                                                             std::forward< decltype( args ) >( args )... );
    }

    BH_ALWAYS_INLINE void destroy_at( value_type* where ) {
        value_allocator alloc( m_allocator );
        std::allocator_traits< value_allocator >::destroy( alloc, where );
    }

    template < class... Args >
    BH_ALWAYS_INLINE void construct_at( bucket_type* where, Args&&... args ) {
        bucket_allocator alloc( m_allocator );
        std::allocator_traits< bucket_allocator >::construct( alloc, where, std::forward< Args >( args )... );
    }

    BH_ALWAYS_INLINE void destroy_at( bucket_type* where ) {
        using mask_type = typename bucket_type::mask_type;
        mask_type occupied_mask = where->occupied_mask();
        while ( occupied_mask ) {
            int occupied_slot = std::countr_zero( occupied_mask );
            occupied_mask ^= mask_type( 1 ) << occupied_slot;
            destroy_at( &where->get( occupied_slot ) );
        }
    }
};

#ifndef BUCKET_HOOD_BUCKET_OVERRIDE

// I've defined it like this just to silence some linter errors until there's an actual implementation.
template < class T >
using SelectedBucket = std::hash< T >;

#else

template < class T >
using SelectedBucket = BUCKET_HOOD_BUCKET_OVERRIDE< T >;

#endif

template < class Key, class Hash = std::hash< Key >, class KeyEqual = std::equal_to< Key >,
           class Allocator = std::allocator< Key > >
class unordered_set : public HashSetBase< TraitsForSet< Key, Hash, KeyEqual, Allocator, SelectedBucket > > {
    typedef TraitsForSet< Key, Hash, KeyEqual, Allocator, SelectedBucket > Traits;
    typedef HashSetBase< Traits > Base;

  public:
    typedef Key value_type;
    typedef const Key* pointer;
    typedef const Key& reference;
    typedef SetIterator< typename Traits::bucket_type, true > iterator;
    typedef iterator const_iterator;

    iterator begin() const { return Base::cbegin(); }
    iterator end() const { return Base::cend(); }

    template < transparent_key< Traits > K >
    bool contains( const K& key ) const {
        auto [ bucket, slot, _ ] = this->locate( key, this->hash( key ) );
        return bucket && slot >= 0 && bucket->occupied( slot );
    }

    template < transparent_key< Traits > K >
    iterator find_iterator( const K& key ) const {
        auto [ bucket, slot, _ ] = this->locate( key, this->hash( key ) );
        if ( bucket && slot >= 0 && bucket->occupied( slot ) ) {
            return iterator( bucket, slot );
        }
        return end();
    }

    // For compatibility with std interface, for testing.
    template < transparent_key< Traits > K >
    size_type count( const K& key ) const {
        return contains( key );
    }

    template < transparent_key< Traits > K >
    requires std::constructible_from< Key, K&& >
    Optional< reference > emplace( K&& key ) {
        size_type hash_val = this->hash( key );
        auto location = this->locate( key, hash_val );
        if ( !location.new_insertion() ) {
            return None;
        }

        while ( unlikely( this->should_rehash() || !location.bucket ) ) {
            this->rehash( 0 );
            location = this->locate( key, hash_val );
        }

        return this->emplace_at( location, std::forward< K >( key ), hash_val );
    }

    template < class... Args >
    Optional< reference > emplace( Args&&... args ) {
        return emplace( Key( std::forward< Args >( args )... ) );
    }

    template < transparent_key< Traits > K >
    Optional< reference > insert( K&& key ) {
        return emplace( std::forward< K >( key ) );
    }

    template < transparent_key< Traits > K >
    bool erase( K&& key ) {
        auto location = this->locate( key, this->hash( key ) );
        if ( location.new_insertion() ) {
            return false;
        }
        this->erase_at( location.bucket, location.slot );
        return true;
    }

    iterator erase( iterator it ) {
        this->erase_at( this->get_bucket( it ), this->get_slot( it ) );
        return ++it;
    }

    template < class K, class H, class E, class A >
    requires transparent_key< K, Traits > &&
             transparent_key< K, TraitsForSet< K, selected_hash< H >, E, A, SelectedBucket > >
    bool operator==( const unordered_set< K, H, E, A >& other ) const {
        if ( this->size() != other.size() ) {
            return false;
        }

        for ( const auto& key : *this ) {
            if ( !other.contains( key ) ) {
                return false;
            }
        }

        return true;
    }
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_HPP_GUARD
