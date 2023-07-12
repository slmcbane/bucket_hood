#ifndef BUCKET_HOOD_DETAIL_HPP
#define BUCKET_HOOD_DETAIL_HPP

#include <cassert>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <limits>
#include <memory>
#include <type_traits>

#ifndef BUCKET_HOOD_64_BIT_SIZE
#define BUCKET_HOOD_SIZE_TYPE uint32_t
#else
#define BUCKET_HOOD_SIZE_TYPE uint64_t
#endif

// All compile builtins should be wrapped in macros below so that porting to another compiler
// is easily done by adding ifdefs.

#define LIKELY( x ) __builtin_expect( x, 1 )
#define UNLIKELY( x ) __builtin_expect( x, 0 )
#define CTZ( x ) __builtin_ctz( x )

// This one could be defined as nothing without breaking anything, but does provide the
// compiler with useful information.
#define UNREACHABLE() __builtin_unreachable()

// Can be undefined if desired.
#define ALWAYS_INLINE inline __attribute__( ( always_inline ) )

namespace bucket_hood {

typedef BUCKET_HOOD_SIZE_TYPE size_type;

inline constexpr float default_load_factor = 0.9999;

static_assert( default_load_factor < 1, "Load factor == 1 does not work" );

// Get the number of bits in the hash result at compile time (either 32 or 64). If std::size_t ever
// becomes more than 64 bits will have to be revisited. Judged unlikely.
template < class Hash, class T >
struct HashBits {
    using hash_result = std::invoke_result_t< Hash, const T& >;
    static constexpr bool size_leq_32 = sizeof( hash_result ) <= sizeof( uint32_t );
    static constexpr uint8_t value = size_leq_32 ? 32 : 64;
};

template < class Hash, class T >
constexpr inline uint8_t hash_bits = HashBits< Hash, T >::value;

template < class T >
struct Slot {
    alignas( T ) char storage[ sizeof( T ) ];

    template < class... Args >
    void emplace( Args&&... args ) noexcept( std::is_nothrow_constructible_v< T, Args&&... > ) {
        new ( storage ) T( static_cast< Args&& >( args )... );
    }

    T& get() noexcept { return *reinterpret_cast< T* >( storage ); }
    const T& get() const noexcept { return *reinterpret_cast< const T* >( storage ); }

    void destroy() noexcept( std::is_nothrow_destructible_v< T > ) { get().~T(); }
};

template < class Rebound, class Allocator >
typename std::allocator_traits< Rebound >::pointer rebind_allocate( const Allocator& original,
                                                                    typename Rebound::size_type n ) {
    auto rebound = Rebound( original );
    return std::allocator_traits< Rebound >::allocate( rebound, n );
}

template < class Rebound, class Allocator >
void rebind_deallocate( const Allocator& original, typename Rebound::pointer where,
                        typename Rebound::size_type n ) {
    auto rebound = Rebound( original );
    std::allocator_traits< Rebound >::deallocate( rebound, where, n );
}

// Naming this type makes code read better, and also this struct has all trivial operations unlike
// std::pair.
inline constexpr size_type SENTINEL_INDEX = std::numeric_limits< size_type >::max();

struct BucketAndSlot {
    static_assert( sizeof( int ) >= sizeof( int16_t ), "We assume we have at least 16 bits to use in int" );
    size_type bucket_index;
    // slot_index is really a bitfield. No backend uses more than 32 slots so we only need 5
    // bits for the actual slot index. The 6th bit is used to signal that there is an existing
    // key in the map. The 7th bit signals that we are evicting an entry. Bits 8-15 are used to
    // store a probe length when inserting entries.
    int info;

    bool key_exists() const noexcept { return info & 0x20; }
    bool evict() const noexcept { return info & 0x40; }
    bool not_found() const noexcept { return bucket_index == SENTINEL_INDEX; }
    int slot_index() const noexcept { return info & 0x1f; }
    uint8_t probe_length() const noexcept { return info >> 7; }
};

inline constexpr BucketAndSlot NO_BUCKET = { SENTINEL_INDEX, 0 };

ALWAYS_INLINE BucketAndSlot found_empty_slot( size_type bi, int i, uint8_t probe_length ) {
    return { bi, i | ( int( probe_length ) << 7 ) };
}
ALWAYS_INLINE BucketAndSlot found_existing_slot( size_type bi, int i ) { return { bi, i | 32 }; }
ALWAYS_INLINE BucketAndSlot found_eviction( size_type bi, int i, uint8_t probe_length ) {
    return { bi, i | 64 | ( int( probe_length ) << 7 ) };
}

// Fibonacci hashing: multiply by 2^64 / phi, phi is the golden ratio. This distributes
// values ~equally in the range of 64 bit numbers.
inline uint64_t hash_mix( uint64_t hash ) { return hash * 11400714819323198485llu; }
inline uint32_t hash_mix( uint32_t hash ) { return hash * 2654435769u; }

template < class Hash >
struct known_good : std::false_type {};

template < class Hash, class T >
struct mixed_hash {
    size_type operator()( const T& x ) const {
        auto base_hash = Hash{}( x );
        if constexpr ( known_good< Hash >{} ) {
            return base_hash;
        } else if constexpr ( sizeof( base_hash ) <= sizeof( uint32_t ) ) {
            return hash_mix( uint32_t( x ) );
        } else {
            return hash_mix( uint64_t( x ) );
        }
    }
};

// Default assignment method - overridden for unordered_map.
template < class T >
struct default_assign {
    default_assign( T& x ) : m_ref( x ) {}

    const default_assign& operator=( const T& other ) const {
        m_ref = other;
        return *this;
    }

    const default_assign& operator=( T&& other ) const {
        m_ref = std::move( other );
        return *this;
    }

  private:
    T& m_ref;
};

/*
 * Policy classes: these are used internally to conditionally compile code for different scenarios
 * in the implementation.
 */
struct IsRehash : std::true_type {};
struct NotRehash : std::false_type {};

struct FindOrInsert : std::true_type {};
struct FindOnly : std::false_type {};

struct IsEviction : std::true_type {};
struct NotEviction : std::false_type {};

/*
 * Collect the main algorithms that can be abstracted away from the backend in a generic container.
 * These are given friend access to each backend so that the code does not need to be duplicated in
 * each. Only the methods that actually use architecture-specific code live in the backend.
 */
struct CoreAlgorithms {

    template < class Backend >
    static size_type bucket_index_from_hash( Backend& backend, size_type hash ) noexcept {
        return hash >> backend.m_bitshift;
    }

    template < class Backend, class S >
    static void do_insert( Backend& backend, S&& key, uint8_t low_bits, const BucketAndSlot& where ) {
        assert( !where.key_exists() );
        assert( !where.not_found() );
        assert( low_bits & 0x80 );

        int slot_index = where.slot_index();
        auto& slot = backend.m_slots[ where.bucket_index * Backend::bucket_type::NUM_SLOTS + slot_index ];
        if ( where.evict() ) {
            evict( backend, where );
            backend.assigner( slot.get() ) = std::forward< S >( key );
        } else {
            slot.emplace( std::forward< S >( key ) );
        }

        auto& bucket = backend.m_buckets[ where.bucket_index ];
        bucket.occupancy_and_hashes[ slot_index ] = low_bits;
        bucket.probe_lengths[ slot_index ] = where.probe_length();
        backend.m_num_occupied++;
    }

    template < class Backend >
    static void evict( Backend& backend, BucketAndSlot where ) {
        assert( !where.key_exists() );
        assert( !where.not_found() );
        assert( where.evict() );

        int slot_index = where.slot_index();
        auto bucket = backend.m_buckets + where.bucket_index;
        auto slot = backend.m_slots + Backend::bucket_type::NUM_SLOTS * where.bucket_index + slot_index;
        auto original_slot = slot;

        size_type next_bucket_index = ( where.bucket_index + 1 ) & ( SENTINEL_INDEX >> backend.m_bitshift );
        uint8_t probe_length = bucket->probe_lengths[ slot_index ];
        uint8_t low_bits = bucket->occupancy_and_hashes[ slot_index ];
        where = backend.template find_or_insert< IsEviction, IsRehash, FindOrInsert >(
            slot->get(), next_bucket_index, low_bits, probe_length + 1 );

        assert( !where.key_exists() );
        slot_index = where.slot_index();
        bucket = backend.m_buckets + where.bucket_index;
        probe_length = where.probe_length();
        slot = backend.m_slots + Backend::bucket_type::NUM_SLOTS * where.bucket_index + slot_index;

        while ( where.evict() ) {
            using std::swap;
            swap( original_slot->get(), slot->get() );
            swap( probe_length, bucket->probe_lengths[ slot_index ] );
            swap( low_bits, bucket->occupancy_and_hashes[ slot_index ] );
            next_bucket_index = ( where.bucket_index + 1 ) & ( SENTINEL_INDEX >> backend.m_bitshift );
            where = backend.template find_or_insert< IsEviction, IsRehash, FindOrInsert >(
                original_slot->get(), next_bucket_index, low_bits, probe_length + 1 );
            assert( !where.key_exists() );
            slot_index = where.slot_index();
            bucket = backend.m_buckets + where.bucket_index;
            probe_length = where.probe_length();
            slot = backend.m_slots + Backend::bucket_type::NUM_SLOTS * where.bucket_index + slot_index;
        }

        bucket->occupancy_and_hashes[ slot_index ] = low_bits;
        bucket->probe_lengths[ slot_index ] = probe_length;
        slot->emplace( std::move( original_slot->get() ) );
    }

    template < class Backend >
    static void resize( Backend& backend, size_type new_size, float load_factor ) {
        size_type num_buckets = backend.num_buckets();
        auto old_slots = backend.m_slots;
        auto old_buckets = backend.m_buckets;
        backend.m_slots = rebind_allocate< typename Backend::SlotAlloc >(
            backend.m_allocator, new_size * Backend::bucket_type::NUM_SLOTS );
        backend.m_buckets = rebind_allocate< typename Backend::BucketAlloc >( backend.m_allocator, new_size );
        std::memset( backend.m_buckets, 0, new_size * sizeof( typename Backend::bucket_type ) );
        std::uninitialized_default_construct_n( backend.m_buckets, new_size );
        backend.m_bitshift -= 1;
        backend.m_num_occupied = 0;
        backend.m_rehash = float( new_size * Backend::bucket_type::NUM_SLOTS ) * load_factor;

        backend.template visit_occupied_slots(
            old_buckets, old_slots, num_buckets, [ &backend ]( auto slot, int i ) {
                auto& key = slot->get();
                size_type hash = typename Backend::hash_type{}( slot->get() );
                size_type bucket_index = hash >> backend.m_bitshift;
                uint8_t low_bits = ( hash & 0x7f ) | 0x80;

                if ( backend.m_buckets[ bucket_index ].occupancy_and_hashes[ i ] == 0 ) {
                    backend.m_buckets[ bucket_index ].occupancy_and_hashes[ i ] = low_bits;
                    backend.m_slots[ bucket_index * Backend::bucket_type::NUM_SLOTS + i ].emplace(
                        std::move( key ) );
                    backend.m_num_occupied++;
                } else {
                    BucketAndSlot where =
                        backend.template find_or_insert< NotEviction, IsRehash, FindOrInsert >(
                            slot->get(), bucket_index, low_bits );

                    assert( !where.not_found() );
                    assert( !where.key_exists() );

                    do_insert( backend, std::move( key ), low_bits, where );
                }
            } );

        rebind_deallocate< typename Backend::SlotAlloc >( backend.m_allocator, old_slots,
                                                          num_buckets * Backend::bucket_type::NUM_SLOTS );
        rebind_deallocate< typename Backend::BucketAlloc >( backend.m_allocator, old_buckets, num_buckets );
    }
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_DETAIL_HPP

