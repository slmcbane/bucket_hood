#ifndef BUCKET_HOOD_HPP_GUARD
#define BUCKET_HOOD_HPP_GUARD

#include <cstdint>

#include <concepts>
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

    /*
     * Find an existing key or the location to insert one. Returned is pointer to the bucket,
     * slot index, and the probe length. If we need to evict an entry the returned slot index
     * is < 0 to indicate the fact. If probe length overflows an 8-bit integer the returned
     * probe length is 256 and returned bucket is nullptr (this requires a rehash).
     */
    template < has_transparent_comparison< Traits > K >
    Location find( K&& key, uint64_t hash_val ) const {
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
    BH_ALWAYS_INLINE uintptr_t bitshift() const { return m_buckets_and_shift.get_shift(); }
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_HPP_GUARD
