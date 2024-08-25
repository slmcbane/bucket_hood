#ifndef BUCKET_HOOD_TEST_UTILS_HPP
#define BUCKET_HOOD_TEST_UTILS_HPP

#include <cassert>
#include <chrono>
#include <cstdint>

#include "../bucket_hood.hpp"

inline uint64_t rotl( uint64_t x, int k ) { return ( x << k ) | ( x >> ( 64 - k ) ); }

class Splitmix64 {
    explicit Splitmix64( uint64_t state ) : m_state{ state } {}

    uint64_t operator()() {
        uint64_t out = m_state += 0x9E3779B97f4A7C15;
        out = ( out ^ ( out >> 30 ) ) * 0xBF58476D1CE4E5B9;
        out = ( out ^ ( out >> 27 ) ) * 0x94D049BB133111EB;
        return out ^ ( out >> 31 );
    }

  private:
    uint64_t m_state;
};

struct NoHash {
    template < class T >
    uint32_t operator()( const T& ) const noexcept {
        return -1;
    }
};

struct CountConstructions {
    int value;

    CountConstructions( int x ) : value{ x } { value_constructed += 1; }
    CountConstructions( const CountConstructions& other ) : value{ other.value } { copy_constructed += 1; }
    CountConstructions( CountConstructions&& other ) : value{ other.value } { move_constructed += 1; }
    ~CountConstructions() { destroyed += 1; }
    CountConstructions& operator=( CountConstructions&& other ) {
        value = other.value;
        return *this;
    }

    friend bool operator==( const CountConstructions& a, const CountConstructions& b ) {
        return a.value == b.value;
    }

    static void reset_counters() {
        value_constructed = 0;
        copy_constructed = 0;
        move_constructed = 0;
        destroyed = 0;
    }

    static size_t value_constructed;
    static size_t copy_constructed;
    static size_t move_constructed;
    static size_t destroyed;
};

namespace std {
template <>
struct hash< ::CountConstructions > {
    size_t operator()( const CountConstructions& a ) const { return std::hash< int >{}( a.value ); }
};

} // namespace std

// TODO: actually use this in tests.
struct NotMoveAssignable {};

struct AllocatorCounters {
    inline static size_t allocated = 0;
    inline static size_t deallocated = 0;
    inline static size_t peak = 0;
    inline static size_t constructed = 0;
    inline static size_t destroyed = 0;

    static void reset() {
        allocated = 0;
        deallocated = 0;
        peak = 0;
        constructed = 0;
        destroyed = 0;
    }
};

template < class T >
inline constexpr bool is_debug_bucket = false;

template < class T >
struct DebugBucket {
    typedef T value_type;
    typedef unsigned mask_type;
    static constexpr size_type num_slots = 8;
    static constexpr float default_load_factor = 0.9;

    uint8_t hash_bits[ 8 ]{ 0 };
    uint8_t probe_lengths[ 8 ]{ 0 };

    struct {
        alignas( T ) std::byte storage[ sizeof( T ) ];
    } slots[ 8 ];

    bool is_sentinel() const {
        uint64_t bitmask = std::bit_cast< uint64_t >( hash_bits );
        return bitmask == 0x7f7f7f7f7f7f7f7fUL;
    }

    constexpr DebugBucket() = default;

    DebugBucket( const DebugBucket& other, auto&& traits ) {
        assert( empty_slots() == 0xff );
        std::ranges::copy( other.hash_bits, std::begin( hash_bits ) );
        std::ranges::copy( other.probe_lengths, std::begin( probe_lengths ) );
        for ( int slot = 0; slot < 8; ++slot ) {
            if ( hash_bits[ slot ] ) {
                traits.construct_at( &get( slot ), other.get( slot ) );
            }
        }
    }

    DebugBucket( EndSentinelTag ) { std::ranges::fill( hash_bits, 0x7f ); }

    bool occupied( int slot ) const {
        assert( slot < 8 );
        return hash_bits[ slot ] >= 0x80;
    }

    T& get( int slot ) { return *std::launder( reinterpret_cast< T* >( slots[ slot ].storage ) ); }

    const T& get( int slot ) const {
        assert( occupied( slot ) );
        return *std::launder( reinterpret_cast< const T* >( slots[ slot ].storage ) );
    }

    T& emplace( int slot, auto&& val, size_type hash_val, int probe_length, auto& traits ) {
        assert( !occupied( slot ) && probe_length < 256 );
        hash_bits[ slot ] = get_check_bits( hash_val );
        probe_lengths[ slot ] = probe_length;
        T& result = get( slot );
        traits.construct_at( &result, std::forward< decltype( val ) >( val ) );
        return result;
    }

    void swap( int slot, T& x, uint8_t& hash_bits, uint8_t& probe_len ) {
        using std::swap;
        assert( hash_bits & 0x80 );
        swap( get( slot ), x );
        swap( this->hash_bits[ slot ], hash_bits );
        uint8_t tmp = probe_len;
        probe_len = probe_lengths[ slot ];
        probe_lengths[ slot ] = tmp;
    }

    auto extract( int slot, auto& traits ) {
        assert( slot >= 0 && slot < 8 );
        auto out = std::make_tuple( std::move( get( slot ) ), hash_bits[ slot ], probe_lengths[ slot ] );
        traits.destroy_at( &get( slot ) );
        // Reset hash_bits, too, in case a rehash is needed.
        hash_bits[ slot ] = 0;
        return out;
    }

    void destroy( int slot, auto& traits ) {
        assert( occupied( slot ) );
        traits.destroy_at( &get( slot ) );
        hash_bits[ slot ] = 0;
        probe_lengths[ slot ] = 0;
    }

    void steal_from( int my_slot, DebugBucket* other, int other_slot ) {
        assert( occupied( my_slot ) );
        assert( other->occupied( other_slot ) );
        assert( other->probe_lengths[ other_slot ] > 0 );
        get( my_slot ) = std::move( other->get( other_slot ) );
        hash_bits[ my_slot ] = other->hash_bits[ other_slot ];
        probe_lengths[ my_slot ] = other->probe_lengths[ other_slot ] - 1;
    }

    static uint8_t get_check_bits( size_type hash_val ) { return ( hash_val & 0xff ) | 0x80; }

    mask_type occupied_mask() const { return mask_type( 0xff ) & ~empty_slots(); }

    mask_type matching_slots( uint8_t check_bits ) const {
        mask_type out = 0;
        for ( int slot = 0; slot < 8; ++slot ) {
            if ( check_bits == hash_bits[ slot ] ) {
                out |= ( 1u << slot );
            }
        }
        return out;
    }

    mask_type empty_slots() const {
        mask_type out = 0;
        for ( int slot = 0; slot < 8; ++slot ) {
            if ( !occupied( slot ) ) {
                out |= ( 1u << slot );
            }
        }
        return out;
    }

    bool all_probe_lengths_shorter_than( int probe_length ) const {
        for ( int slot = 0; slot < 8; ++slot ) {
            if ( probe_lengths[ slot ] >= probe_length ) {
                return false;
            }
        }
        return true;
    }

    int min_probe_length_slot() const {
        int min = 256;
        int min_slot = -1;
        for ( int slot = 0; slot < 8; ++slot ) {
            if ( probe_lengths[ slot ] < min ) {
                min = probe_lengths[ slot ];
                min_slot = slot;
            }
        }
        assert( min_slot >= 0 && min_slot < 8 );
        return min_slot;
    }

    auto max_probe_length_slot() const {
        int max = 0;
        int max_slot = 0;
        for ( int slot = 0; slot < 8; ++slot ) {
            if ( probe_lengths[ slot ] > max ) {
                max = probe_lengths[ slot ];
                max_slot = slot;
            }
        }
        return std::make_pair( max_slot, max );
    }
};

template < class T >
inline constexpr bool is_debug_bucket< DebugBucket< T > > = true;

template < class T >
class DebugAllocator : private AllocatorCounters {
  public:
    typedef T value_type;
    typedef std::true_type is_always_equal;

    DebugAllocator() = default;

    template < class U >
    DebugAllocator( DebugAllocator< U >&& ) {}

    template < class U >
    DebugAllocator( const DebugAllocator< U >& ) {}

    T* allocate( std::size_t n ) {
        allocated += n * sizeof( T );
        size_t diff = allocated - deallocated;
        AllocatorCounters::peak = std::max( diff, AllocatorCounters::peak );
        return new T[ n ];
    }

    void deallocate( T* p, std::size_t n ) {
        deallocated += sizeof( T ) * n;
        delete[] p;
    }

    void construct( T* p, auto&&... args ) {
        if constexpr ( !is_debug_bucket< T > ) {
            constructed++;
        }
        std::construct_at( p, std::forward< decltype( args ) >( args )... );
    }

    void destroy( T* p ) {
        if constexpr ( !is_debug_bucket< T > ) {
            destroyed++;
        }
        p->~T();
    }
};

class Stopwatch {
  public:
    Stopwatch() : m_start( std::chrono::steady_clock::now() ) {}

    template < class Duration >
    auto elapsed() const {
        auto now = std::chrono::steady_clock::now();
        auto duration = now - m_start;
        return std::chrono::duration_cast< Duration >( duration ).count();
    }

    void reset() { m_start = std::chrono::steady_clock::now(); }

  private:
    std::chrono::time_point< std::chrono::steady_clock > m_start;
};

#endif // BUCKET_HOOD_TEST_UTILS_HPP
