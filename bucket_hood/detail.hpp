#ifndef BUCKET_HOOD_DETAIL_HPP
#define BUCKET_HOOD_DETAIL_HPP

#include <cstdint>
#include <iterator>
#include <type_traits>
#include <x86intrin.h>

#ifndef BUCKET_HOOD_64_BIT_SIZE
#define BUCKET_HOOD_SIZE_TYPE uint32_t
#else
#define BUCKET_HOOD_SIZE_TYPE uint64_t
#endif

namespace bucket_hood {

typedef BUCKET_HOOD_SIZE_TYPE size_type;

inline constexpr unsigned hash_bits = std::is_same_v< size_type, uint32_t > ? 32 : 64;
inline constexpr double default_load_factor = 0.95;

static_assert( default_load_factor < 1, "Load factor == 1 does not work" );

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

struct Bucket {
    static constexpr int NUM_SLOTS = 32;
    alignas( __m256i ) uint8_t occupancy_and_hashes[ 32 ]{ 0 };
    alignas( __m256i ) uint8_t probe_lengths[ 32 ]{ 0 };
};

// Naming this type makes code read better, and also this struct has all trivial operations unlike std::pair.
struct BucketAndSlot {
    size_type bucket_index;
    int slot_index;
};

// Fibonacci hashing: multiply by 2^64 / phi, phi is the golden ratio. This distributes
// values ~equally in the range of 64 bit numbers.
inline uint64_t hash_mix( uint64_t hash ) { return hash * 11400714819323198485llu; }
inline uint32_t hash_mix( uint32_t hash ) { return hash * 2654435769u; }

template < class T >
struct default_hash {
    size_type operator()( const T& x ) const noexcept {
        if constexpr ( sizeof( T ) <= sizeof( size_type ) ) {
            size_type base_hash = std::hash< T >{}( x );
            return hash_mix( base_hash );
        } else {
            return hash_mix( std::hash< T >{}( x ) );
        }
    }
};

// All compile builtins should be wrapped in macros below so that porting to another compiler
// is easily done by adding ifdefs.

#define LIKELY( x ) __builtin_expect( x, 1 )
#define UNLIKELY( x ) __builtin_expect( x, 0 )
#define CTZ( x ) __builtin_ctz( x )

// This one could be defined as nothing without breaking anything, but does provide the
// compiler with useful information.
#define UNREACHABLE() __builtin_unreachable()

// Can be undefined if desired.
#define ALWAYS_INLINE __attribute__( ( always_inline ) )

template < class T, bool is_const >
struct SlotIterator {
    typedef Slot< T > value_type;
    typedef std::conditional_t< is_const, const Slot< T >&, Slot< T >& > reference;
    typedef std::conditional_t< is_const, const Slot< T >*, Slot< T >* > pointer;
    typedef std::ptrdiff_t difference_type;
    typedef std::forward_iterator_tag iterator_category;

    SlotIterator() noexcept = default;

    SlotIterator( const Bucket* buckets_start, pointer slots_start, size_type num_buckets ) noexcept
        : m_first_bucket{ buckets_start }, m_first_slot{ slots_start }, m_bucket_index{ 0 }, m_num_buckets{
                                                                                                 num_buckets } {
        if ( num_buckets == 0 ) {
            return;
        }

        const Bucket& bucket = *m_first_bucket;
        __m256i slots = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
        m_mask = _mm256_movemask_epi8( slots );
        advance();
    }

    SlotIterator end() const noexcept {
        SlotIterator out = *this;
        out.m_bucket_index = m_num_buckets;
        out.m_slot_index = 0;
        return out;
    }

    template < bool A, bool B >
    friend bool operator==( const SlotIterator< T, A >& a, const SlotIterator< T, B >& b ) noexcept {
        return a.m_first_bucket == b.m_first_bucket && a.m_bucket_index == b.m_bucket_index &&
               a.m_slot_index == b.m_slot_index;
    }

    template < bool A, bool B >
    friend bool operator!=( const SlotIterator< T, A >& a, const SlotIterator< T, B >& b ) noexcept {
        return !( a == b );
    }

    SlotIterator& operator++() noexcept {
        advance();
        return *this;
    }

    SlotIterator operator++( int ) noexcept {
        SlotIterator out = *this;
        this->operator++();
        return out;
    }

    reference operator*() const noexcept { return *( m_first_slot + m_slot_index ); }
    pointer operator->() const noexcept { return m_first_slot + m_slot_index; }

  private:
    const Bucket* m_first_bucket{ nullptr };
    pointer m_first_slot{ nullptr }; // Points to the first slot *in this bucket*
    size_type m_bucket_index{ 0 };
    size_type m_num_buckets{ 0 };
    int m_mask{ 0 };       // Occupied slots in current bucket
    int m_slot_index{ 0 }; // Next occupied slot.

    void advance() noexcept {
        while ( !m_mask ) {
            ++m_bucket_index;
            m_first_slot += Bucket::NUM_SLOTS;
            if ( m_bucket_index == m_num_buckets ) {
                m_slot_index = 0;
                return;
            }
            const Bucket& bucket = *( m_first_bucket + m_bucket_index );
            __m256i slots = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
            m_mask = _mm256_movemask_epi8( slots );
        }
        m_slot_index = CTZ( m_mask );
        m_mask ^= ( 1 << m_slot_index );
    }
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_DETAIL_HPP

