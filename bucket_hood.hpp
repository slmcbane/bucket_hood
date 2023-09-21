#ifndef BUCKET_HOOD_HPP_GUARD
#define BUCKET_HOOD_HPP_GUARD

#include <cassert>
#include <cstdint>

#include <iterator>
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

namespace bucket_hood {

BH_ALWAYS_INLINE bool likely( long condition ) noexcept { return __builtin_expect( condition, 1 ); }
BH_ALWAYS_INLINE bool unlikely( long condition ) noexcept { return __builtin_expect( condition, 0 ); }
BH_ALWAYS_INLINE int ctz( unsigned x ) noexcept { return __builtin_ctz( x ); }
[[noreturn]] inline void unreachable() noexcept { __builtin_unreachable(); }

/********************************************************************************
 * Utility types
 *******************************************************************************/
namespace detail {

template < class T >
class EmptyBase : private T {
  protected:
    EmptyBase() noexcept( std::is_nothrow_default_constructible_v< T > ) = default;
    EmptyBase( const T& t ) noexcept( std::is_nothrow_copy_constructible_v< T > ) : T( t ) {}
    EmptyBase( T&& t ) noexcept( std::is_nothrow_move_constructible_v< T > ) : T( std::move( t ) ) {}

    BH_ALWAYS_INLINE const T& get() const noexcept { return static_cast< const T& >( *this ); }
};

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

// Mix the result of invoking Hash, unconditionally.
template < class Hash >
class mixed_hash : EmptyBase< Hash > {
  public:
    template < class T >
    BH_ALWAYS_INLINE uint64_t operator()( const T& x ) const noexcept {
        return hash_mix( static_cast< uint64_t >( hash()( x ) ) );
    }

  protected:
    mixed_hash() noexcept( std::is_trivially_default_constructible_v< Hash > ) = default;

    mixed_hash( const Hash& h ) noexcept( std::is_trivially_copy_constructible_v< Hash > )
        : EmptyBase< Hash >{ h } {}

  private:
    const Hash& hash() const noexcept { return EmptyBase< Hash >::get(); }
};

template < class Hash, bool = known_good< Hash >::value >
class selected_hash : public EmptyBase< mixed_hash< Hash > > {
  public:
    using Base = EmptyBase< mixed_hash< Hash > >;
    selected_hash() = default;
    selected_hash( const Hash& h ) noexcept( std::is_trivially_copy_constructible_v< Hash > ) : Base{ h } {}
};

template < class Hash >
class selected_hash< Hash, true > : public EmptyBase< Hash > {
  public:
    using Base = EmptyBase< Hash >;
    selected_hash() = default;
    selected_hash( const Hash& h ) noexcept( std::is_trivially_copy_constructible_v< Hash > ) : Base{ h } {}
};

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
    static void do_assignment( T& a, Args&&... args ) noexcept(
        std::is_nothrow_constructible_v< T, Args... >&& std::is_nothrow_move_assignable_v< T > ) {
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
        assert( m_bucket );
        m_bucket_mask = m_bucket->occupied_mask();
        assert( m_bucket_mask & ( 1 << slot_index ) );
        m_bucket_mask &= ( ~0u << m_slot_index );
    }

    explicit SetIterator( const Bucket* bucket, int slot_index, EmptySlotTag ) noexcept : m_bucket{ bucket } {
        assert( m_bucket );
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

} // namespace detail

} // namespace bucket_hood

#endif // BUCKET_HOOD_HPP_GUARD

