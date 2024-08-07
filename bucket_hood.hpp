#ifndef BUCKET_HOOD_HPP_GUARD
#define BUCKET_HOOD_HPP_GUARD

#include <cstdint>

#include <concepts>
#include <format>
#include <functional>
#include <iostream>
#include <iterator>
#include <source_location>
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

inline constexpr float default_load_factor = 0.95;
typedef BUCKET_HOOD_SIZE_TYPE size_type;

} // namespace bucket_hood

/********************************************************************************
 * Wrappers for compiler intrinsics.
 *******************************************************************************/

#define BH_ALWAYS_INLINE inline __attribute__( ( always_inline ) )
#define BH_NEVER_INLINE __attribute__( ( noinline ) )

namespace bucket_hood {

BH_ALWAYS_INLINE bool likely( long condition ) noexcept { return __builtin_expect( condition, 1 ); }
BH_ALWAYS_INLINE bool unlikely( long condition ) noexcept { return __builtin_expect( condition, 0 ); }
BH_ALWAYS_INLINE int ctz( unsigned x ) noexcept { return __builtin_ctz( x ); }
[[noreturn]] inline void unreachable() noexcept { __builtin_unreachable(); }

/********************************************************************************
 * Custom assertion macro
 *******************************************************************************/

// Leave the possibility open of using this together with my standalone header
#ifndef SLM_CHECK_REQUIRE_HPP

inline BH_NEVER_INLINE void simple_fail( const char* condition_text,
                                         const std::source_location& loc ) noexcept {
    std::format_to( std::ostreambuf_iterator( std::cerr ), "Assertion error in {:s}:{:d}:{:s}: {:s}\n",
                    loc.file_name(), loc.line(), loc.function_name(), condition_text );
    std::abort();
}

template < class... Args >
BH_NEVER_INLINE void fail_with_message( const char* condition_text, const std::source_location& loc,
                                        const char* msg_format, Args&&... args ) noexcept {
    auto msg = std::vformat( msg_format, std::make_format_args( args... ) );
    std::format_to( std::ostreambuf_iterator( std::cerr ),
                    "Assertion error in {:s}:{:d}:{:s}: {:s}; message: {:s}\n", loc.file_name(), loc.line(),
                    loc.function_name(), condition_text, msg );
    std::abort();
}

template < class... Args >
BH_ALWAYS_INLINE constexpr void check( bool condition, const char* condition_text,
                                       const std::source_location& loc, Args&&... args ) {
    if ( std::is_constant_evaluated() && !condition ) {
        *(volatile int*)nullptr = 0;
    } else if ( !condition ) [[unlikely]] {
        if constexpr ( sizeof...( Args ) == 0 ) {
            simple_fail( condition_text, loc );
        } else {
            fail_with_message( condition_text, loc, std::forward< Args >( args )... );
        }
    }
}

#define REQUIRE( condition, ... )                                                                              \
    check( condition, #condition, std::source_location::current() __VA_OPT__(, ) __VA_ARGS__ );

#ifndef NDEBUG
#define CHECK( condition, ... ) REQUIRE( condition __VA_OPT__(, ) __VA_ARGS__ )
#else
#define CHECK( condition, ... )
#endif

#endif // SLM_CHECK_REQUIRE_HPP

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
        CHECK( m_engaged, "dereferencing disengaged Optional" );
        return &m_payload;
    }

    constexpr T* operator->() noexcept {
        CHECK( m_engaged, "dereferencing disengaged Optional" );
        return &m_payload;
    }

    constexpr const T& operator*() const& noexcept {
        CHECK( m_engaged, "dereferencing disengaged Optional" );
        return m_payload;
    }

    constexpr T& operator*() & noexcept {
        CHECK( m_engaged, "dereferencing disengaged Optional" );
        return m_payload;
    }

    constexpr const T&& operator*() const&& noexcept {
        CHECK( m_engaged, "dereferencing disengaged Optional" );
        return static_cast< const T&& >( m_payload );
    }

    constexpr T&& operator*() && noexcept {
        CHECK( m_engaged, "dereferencing disengaged Optional" );
        return static_cast< T&& >( m_payload );
    }

    constexpr explicit operator bool() const noexcept { return m_engaged; }

    constexpr bool has_value() const noexcept { return m_engaged; }

    constexpr const T& value() const& noexcept {
        REQUIRE( m_engaged, "dereferencing disengaged Optional" );
        return m_payload;
    }

    constexpr T& value() & noexcept {
        REQUIRE( m_engaged, "dereferencing disengaged Optional" );
        return m_payload;
    }

    constexpr T&& value() && noexcept {
        REQUIRE( m_engaged, "dereferencing disengaged Optional" );
        return static_cast< T&& >( m_payload );
    }

    constexpr const T&& value() const&& noexcept {
        REQUIRE( m_engaged, "dereferencing disengaged Optional" );
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
        CHECK( m_ptr, "Disengaged optional access" );
        return *m_ptr;
    }

    constexpr T* operator->() const noexcept {
        CHECK( m_ptr, "Disengaged optional access" );
        return m_ptr;
    }

    constexpr explicit operator bool() const noexcept { return m_ptr != nullptr; }

    constexpr bool has_value() const noexcept { return (bool)*this; }

    constexpr T& value() const noexcept {
        REQUIRE( m_ptr, "Disengaged optional access" );
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

    SetIterator() noexcept = default;

    explicit SetIterator( const Bucket* bucket, int slot_index ) noexcept
        : m_bucket{ bucket }, m_slot_index{ slot_index } {
        CHECK( m_bucket );
        m_bucket_mask = m_bucket->occupied_mask();
        CHECK( m_bucket_mask & ( 1 << slot_index ) );
        m_bucket_mask &= ( ~0u << m_slot_index );
    }

    explicit SetIterator( const Bucket* bucket, int slot_index, EmptySlotTag ) noexcept : m_bucket{ bucket } {
        CHECK( m_bucket );
        m_bucket_mask = Bucket::occupied_mask( *m_bucket );
        m_bucket_mask &= ( ~0u << slot_index );
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

    reference operator*() const noexcept { return m_bucket->slots[ m_slot_index ].get(); }
    pointer operator->() const noexcept { return &reference(); }

  private:
    std::conditional_t< is_const, const Bucket*, Bucket* > m_bucket{ nullptr };
    int m_bucket_mask{ 0 };
    int m_slot_index{ 0 };

    void scan_to_slot() noexcept {
        while ( !m_bucket_mask ) {
            m_bucket_mask = Bucket::occupied_mask( *++m_bucket );
        }
        m_slot_index = ctz( m_bucket_mask );
        m_bucket_mask ^= ( 1 << m_slot_index );
    }
};

template < class Bucket >
concept valid_bucket_type =
    alignof( typename Bucket::bucket_type ) >= 64 && requires( Bucket& b, uint8_t low_bits ) {
        { b.matching_slots( low_bits ) } -> std::same_as< int >;
        { b.empty_slots() } -> std::same_as< int >;
        { b.get( std::declval< int >() ) };
    };

template < class Traits >
concept valid_traits_type = std::is_default_constructible_v< Traits > &&
                            std::is_copy_constructible_v< Traits > && std::is_move_constructible_v< Traits > &&
                            valid_bucket_type< typename Traits::bucket_type > && requires( Traits& t ) {
                                {
                                    t.allocate( std::declval< size_type >() )
                                } -> std::same_as< typename Traits::bucket_type* >;
                                {
                                    t.deallocate( std::declval< typename Traits::bucket_type* >() )
                                } -> std::same_as< void >;
                                {
                                    t.comparison()( std::declval< const typename Traits::value_type& >(),
                                                    std::declval< const typename Traits::value_type& >() )
                                } -> std::same_as< bool >;
                            };

template < class Traits, class K >
concept has_transparent_comparison = Traits::comparison_type::template is_transparent< K >;

template < class T, class Policy >
concept is_policy = requires( T& ref, const T& const_ref ) {
    Policy::from_value( ref );
    Policy::from_value( const_ref );
    Policy::from_null();
    static_cast< decltype( Policy::from_value( ref ) ) >( Policy::from_null() );
    static_cast< decltype( Policy::from_value( const_ref ) ) >( Policy::from_null() );
};

struct return_value {
    template < class T, class U >
    BH_ALWAYS_INLINE static auto from_value( std::pair< T, U >& pair ) -> Optional< U& > {
        return SomeRef( pair.second );
    }

    template < class T, class U >
    BH_ALWAYS_INLINE static auto from_value( const std::pair< T, U >& pair ) -> Optional< const U& > {
        return SomeRef( pair.second );
    }

    BH_ALWAYS_INLINE static auto from_value( auto& x ) { return Optional( SomeRef( x ) ); }

    BH_ALWAYS_INLINE static auto from_null() { return None; }
};

template < class Traits >
requires valid_traits_type< Traits >
class HashSetBase {
  public:
    typedef Traits::bucket_type bucket_type;
    typedef Traits::value_type value_type;

    struct Location {
        bucket_type* bucket;
        int slot;
        int probe_length;
    };

    BH_ALWAYS_INLINE uint64_t bucket_index( uint64_t hash_val ) const {
        static constexpr uint64_t all_ones = ~uint64_t( 0 );
        return hash_val & ~( all_ones << bitshift() );
    }

    template < is_policy< value_type > Policy = return_value, has_transparent_comparison< Traits > K >
    auto find( K&& key ) {
        uint64_t hash_val = m_traits.hash( key );
        auto [ bucket, slot, probe_length ] = find_( key, hash_val );
        if ( !bucket || slot < 0 || !( bucket->occupied( slot ) ) ) {
            return Policy::from_null();
        }
        return Policy::from_value( bucket->get( slot ) );
    }

    template < is_policy< value_type > Policy = return_value, has_transparent_comparison< Traits > K >
    auto find( K&& key ) const {
        uint64_t hash_val = m_traits.hash( key );
        auto [ bucket, slot, probe_length ] = find_( key, hash_val );
        if ( !bucket || slot < 0 || !( bucket->occupied( slot ) ) ) {
            return Policy::from_null();
        }
        return Policy::from_value( bucket->get( slot ) );
    }

  private:
    struct PackedPointerAndShift {
        static constexpr uintptr_t shift_mask = 0x1f;
        static constexpr uintptr_t ptr_mask = ~shift_mask;
        uintptr_t ptr_and_shift = 0;

        BH_ALWAYS_INLINE bucket_type* get_pointer() const {
            return reinterpret_cast< bucket_type* >( ptr_and_shift & ptr_mask );
        }
        BH_ALWAYS_INLINE uintptr_t get_shift() const { return ptr_and_shift & shift_mask; }

        BH_ALWAYS_INLINE void set_shift( uintptr_t shift ) {
            ptr_and_shift = ( ptr_and_shift & ptr_mask ) | shift;
        }
        BH_ALWAYS_INLINE void set_ptr( bucket_type* ptr ) {
            ptr_and_shift = ( ptr_and_shift & shift_mask ) | reinterpret_cast< uintptr_t >( ptr );
        }

        PackedPointerAndShift( bucket_type* ptr, int shift ) {
            set_shift( shift );
            set_ptr( ptr );
        }
    };

    static constexpr bucket_type end_sentinel = bucket_type::end_sentinel();
    PackedPointerAndShift m_buckets_and_shift{ &end_sentinel, 0 };
    size_type m_occupied = 0;
    size_type m_rehash = 0;
    [[no_unique_address]] Traits m_traits;

    BH_ALWAYS_INLINE bucket_type* buckets() const { return m_buckets_and_shift.get_pointer(); }

    /*
     * Find an existing key or the location to insert one. Returned is pointer to the bucket,
     * slot index, and the probe length. If we need to evict an entry the returned slot index
     * is < 0 to indicate the fact. If probe length overflows an 8-bit integer the returned
     * probe length is 256 and returned bucket is nullptr (this requires a rehash).
     */
    template < has_transparent_comparison< Traits > K >
    BH_ALWAYS_INLINE Location find_( K&& key, uint64_t hash_val ) const {
        auto bucket_index = hash_val & ( uint64_t( 1 ) << bitshift() );
        bucket_type* bucket = buckets() + bucket_index;
        int probe_length = 0;
        uint8_t low_bits = ( hash_val & 0xff ) | 0x80;

        do {
            int match_mask = bucket->matching_slots( low_bits );
            while ( match_mask ) {
                int slot = ctz( match_mask );
                if ( likely( m_traits.comparison()( bucket->get( slot ), key ) ) ) {
                    return { bucket, slot, probe_length };
                }
                match_mask ^= 1 << slot;
            }

            int empty_mask = bucket->empty_slots();
            if ( empty_mask ) {
                int slot = ctz( empty_mask );
                return { bucket, slot, probe_length };
            }

            if ( unlikely( bucket->all_probe_lengths_shorter_than( probe_length ) ) ) {
                return { bucket, -1, probe_length };
            }

            probe_length++;
            bucket_index++;
        } while ( probe_length < 256 );

        return { nullptr, 0, 256 };
    }

    BH_ALWAYS_INLINE uintptr_t bitshift() const { return m_buckets_and_shift.get_shift(); }
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_HPP_GUARD
