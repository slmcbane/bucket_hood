#ifndef BUCKET_HOOD_DETAIL_HPP
#define BUCKET_HOOD_DETAIL_HPP

#include <algorithm>
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

inline constexpr float default_load_factor = 0.95;

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
inline uint64_t hash_mix( uint64_t hash ) {
    hash *= 0xbf58476d1ce4e5b9ull;
    hash ^= hash >> 32;
    hash *= 0x94d049bb133111ebull;
    hash ^= hash >> 32;
    hash *= 0x94d049bb133111ebull;
    return hash;
}

inline uint32_t hash_mix( uint32_t hash ) {
    hash ^= hash >> 17;
    hash *= 0xed5ad4bbU;
    hash ^= hash >> 11;
    hash *= 0xac4c1b51U;
    hash ^= hash >> 15;
    hash *= 0x31848babU;
    hash ^= hash >> 14;
    return hash;
}

template < class Hash >
struct known_good : std::false_type {};

template < class Hash, class T >
struct mixed_hash {
    size_type operator()( const T& x ) const {
        auto base_hash = Hash{}( x );
        if constexpr ( known_good< Hash >{} ) {
            return base_hash;
        } else if constexpr ( sizeof( base_hash ) <= sizeof( uint32_t ) ) {
            return hash_mix( uint32_t( base_hash ) );
        } else {
            return hash_mix( uint64_t( base_hash ) );
        }
    }
};

// Default assignment method - overridden for unordered_map.
template < class T >
struct default_assign {
    default_assign( T& x ) : m_ref( x ) {}

    void assign( const T& other ) const { m_ref = other; }

    void assign( T&& other ) const { m_ref = std::move( other ); }

    template < class... Args >
    void assign( Args&&... args ) const {
        m_ref = T( std::forward< Args >( args )... );
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
 * All the iterators we need can be built on the SetIterator defined below, which in
 * turn relies on SlotIterator for its implementation.
 */

struct EmptySlotTag {};
template < class Bucket, class T >
struct SlotIterator {
    SlotIterator() noexcept = default;

    explicit SlotIterator( const Bucket* bucket ) noexcept : m_bucket{ bucket } {
        if ( bucket ) {
            m_mask = Bucket::occupied_mask( *m_bucket );
        }
        scan_to_slot();
    }

    explicit SlotIterator( const Bucket* bucket, int slot_index ) noexcept : m_bucket{ bucket } {
        assert( m_bucket );
        m_mask = Bucket::occupied_mask( *m_bucket );
        assert( m_mask & ( 1 << slot_index ) );
        m_slot_index = slot_index;
        m_mask &= ( ~0u << m_slot_index );
    }

    explicit SlotIterator( const Bucket* bucket, int slot_index, EmptySlotTag ) noexcept : m_bucket{ bucket } {
        assert( m_bucket );
        m_mask = Bucket::occupied_mask( *m_bucket );
        m_mask &= ( ~0u << slot_index );
        scan_to_slot();
    }

    void increment() noexcept { scan_to_slot(); }

    const Slot< T >& dereference() const noexcept { return m_bucket->slots[ m_slot_index ]; }

    friend bool operator==( const SlotIterator& a, const SlotIterator& b ) {
        return a.m_bucket == b.m_bucket && a.m_slot_index == b.m_slot_index;
    }

    const Bucket* bucket() const noexcept { return m_bucket; }
    int slot_index() const noexcept { return m_slot_index; }

  private:
    const Bucket* m_bucket{ nullptr };
    int m_mask{ 0 };
    int m_slot_index{ 0 };

    void scan_to_slot() noexcept {
        while ( !m_mask ) {
            ++m_bucket;
            m_mask = Bucket::occupied_mask( *m_bucket );
        }
        m_slot_index = CTZ( m_mask );
        m_mask ^= ( 1 << m_slot_index );
    }
};

template < class Bucket, class T, bool is_const >
struct SetIterator {
    typedef T value_type;
    typedef std::conditional_t< is_const, const T*, T* > pointer;
    typedef std::conditional_t< is_const, const T&, T& > reference;
    typedef std::ptrdiff_t difference_type;
    typedef std::forward_iterator_tag iterator_category;

    SetIterator() noexcept = default;

    explicit SetIterator( const Bucket* bucket, int slot_index ) noexcept
        : m_slot_iterator( bucket, slot_index ) {}

    explicit SetIterator( const Bucket* bucket, int slot_index, EmptySlotTag ) noexcept
        : m_slot_iterator( bucket, slot_index, EmptySlotTag{} ) {}

    SetIterator& operator++() noexcept {
        m_slot_iterator.increment();
        return *this;
    }

    SetIterator operator++( int ) noexcept {
        auto out = *this;
        this->operator++();
        return out;
    }

    template < bool B >
    bool operator==( const SetIterator< Bucket, T, B >& other ) const noexcept {
        return m_slot_iterator == other.m_slot_iterator;
    }

    template < bool B >
    bool operator!=( const SetIterator< Bucket, T, B >& other ) const noexcept {
        return !( *this == other );
    }

    reference operator*() const noexcept { return m_slot_iterator.dereference().get(); }
    pointer operator->() const noexcept { return &reference(); }

    const Bucket* bucket() const noexcept { return m_slot_iterator.bucket(); }
    int slot_index() const noexcept { return m_slot_iterator.slot_index(); }

  private:
    SlotIterator< Bucket, T > m_slot_iterator;
};

/*
 * Collect the main algorithms that can be abstracted away from the backend in a generic container.
 * These are given friend access to each backend so that the code does not need to be duplicated in
 * each. Only the methods that actually use architecture-specific code live in the backend.
 */
struct CoreAlgorithms {

    template < class Backend >
    static size_type bucket_index_from_hash( const Backend& backend, size_type hash ) noexcept {
        return hash >> backend.m_bitshift;
    }

    template < class Backend >
    static size_type bucket_index_from_bucket( const Backend& backend,
                                               const typename Backend::bucket_type* bucket ) {
        return bucket - backend.m_buckets;
    }

    template < class Backend, class... Args >
    static void do_insert( Backend& backend, uint8_t low_bits, const BucketAndSlot& where, Args&&... args ) {
        assert( !where.key_exists() );
        assert( !where.not_found() );
        assert( low_bits & 0x80 );

        int slot_index = where.slot_index();
        auto& bucket = backend.m_buckets[ where.bucket_index ];
        if ( where.evict() ) {
            evict( backend, where );
            backend.assigner( bucket.slots[ slot_index ].get() ).assign( std::forward< Args >( args )... );
        } else {
            bucket.slots[ slot_index ].emplace( std::forward< Args >( args )... );
        }

        bucket.occupancy_and_hashes()[ slot_index ] = low_bits;
        bucket.probe_lengths()[ slot_index ] = where.probe_length();
        backend.m_num_occupied++;
    }

    template < class Backend >
    static void evict( Backend& backend, BucketAndSlot where ) {
        assert( !where.key_exists() );
        assert( !where.not_found() );
        assert( where.evict() );

        int slot_index = where.slot_index();
        auto* bucket = backend.m_buckets + where.bucket_index;
        auto* slot = bucket->slots + slot_index;
        auto* original_slot = slot;

        size_type next_bucket_index = ( where.bucket_index + 1 ) & ( SENTINEL_INDEX >> backend.m_bitshift );
        uint8_t probe_length = bucket->probe_lengths()[ slot_index ];
        uint8_t low_bits = bucket->occupancy_and_hashes()[ slot_index ];
        where = backend.template find_or_insert< IsEviction, IsRehash, FindOrInsert >(
            slot->get(), next_bucket_index, low_bits, probe_length + 1 );

        assert( !where.key_exists() );
        slot_index = where.slot_index();
        bucket = backend.m_buckets + where.bucket_index;
        probe_length = where.probe_length();
        slot = bucket->slots + slot_index;

        while ( where.evict() ) {
            using std::swap;
            swap( original_slot->get(), slot->get() );
            swap( probe_length, bucket->probe_lengths()[ slot_index ] );
            swap( low_bits, bucket->occupancy_and_hashes()[ slot_index ] );
            next_bucket_index = ( where.bucket_index + 1 ) & ( SENTINEL_INDEX >> backend.m_bitshift );
            where = backend.template find_or_insert< IsEviction, IsRehash, FindOrInsert >(
                original_slot->get(), next_bucket_index, low_bits, probe_length + 1 );
            assert( !where.key_exists() );
            slot_index = where.slot_index();
            bucket = backend.m_buckets + where.bucket_index;
            probe_length = where.probe_length();
            slot = bucket->slots + slot_index;
        }

        bucket->occupancy_and_hashes()[ slot_index ] = low_bits;
        bucket->probe_lengths()[ slot_index ] = probe_length;
        slot->emplace( std::move( original_slot->get() ) );
    }

    template < class Backend >
    static void resize( Backend& backend, size_type new_size, float load_factor ) {
        size_type num_buckets = backend.num_buckets();
        auto* old_buckets = backend.m_buckets;
        backend.m_buckets =
            rebind_allocate< typename Backend::BucketAlloc >( backend.allocator(), new_size + 1 );
        std::memset( backend.m_buckets, 0, sizeof( typename Backend::bucket_type ) * new_size );
        backend.m_buckets[ new_size ].setup_end_sentinel();
        backend.m_bitshift -= 1;
        backend.m_num_occupied = 0;
        backend.m_rehash = float( new_size * Backend::bucket_type::NUM_SLOTS ) * load_factor;

        backend.template visit_occupied_slots(
            old_buckets, num_buckets, [ &backend ]( auto& bucket, size_t, int slot_index ) {
                auto& key = bucket.slots[ slot_index ].get();
                size_type hash = typename Backend::hash_type{}( key );
                size_type bucket_index = hash >> backend.m_bitshift;
                uint8_t low_bits = ( hash & 0x7f ) | 0x80;

                if ( backend.m_buckets[ bucket_index ].occupancy_and_hashes()[ slot_index ] == 0 ) {
                    backend.m_buckets[ bucket_index ].occupancy_and_hashes()[ slot_index ] = low_bits;
                    backend.m_buckets[ bucket_index ].slots[ slot_index ].emplace( std::move( key ) );
                    backend.m_num_occupied++;
                } else {
                    BucketAndSlot where =
                        backend.template find_or_insert< NotEviction, IsRehash, FindOrInsert >(
                            key, bucket_index, low_bits );
                    assert( !where.not_found() );
                    assert( !where.key_exists() );
                    do_insert( backend, low_bits, where, std::move( key ) );
                }
            } );

        rebind_deallocate< typename Backend::BucketAlloc >( backend.allocator(), old_buckets, num_buckets + 1 );
    }

    template < class Backend >
    using value_type = typename Backend::value_type;

    template < class Backend >
    static auto make_const_iterator( const Backend& backend, const BucketAndSlot& where ) noexcept {
        assert( !where.not_found() );
        return SetIterator< typename Backend::bucket_type, value_type< Backend >, true >(
            backend.m_buckets + where.bucket_index, where.slot_index() );
    }

    template < class Backend >
    static auto const_end( const Backend& backend ) {
        return SetIterator< typename Backend::bucket_type, value_type< Backend >, true >(
            backend.m_buckets + backend.num_buckets(), 0 );
    }

    template < class Backend >
    static auto const_iterator_empty_slot( const Backend& backend, size_type bucket_index, int slot_index ) {
        return SetIterator< typename Backend::bucket_type, value_type< Backend >, true >(
            backend.m_buckets + bucket_index, slot_index, EmptySlotTag{} );
    }
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_DETAIL_HPP

