#ifndef BUCKET_HOOD_AVX2_BACKEND_HPP
#define BUCKET_HOOD_AVX2_BACKEND_HPP

/*
 * Purpose: AVX2 backend for bucketed robin hood hash sets/maps. SetImpl classes _implement_ the
 * basic algorithms using different instruction sets to provide the most efficient implementation
 * on different architectures. Frontends are architecture agnostic.
 */

#include <cassert>
#include <cstdint>
#include <cstring>
#include <functional>
#include <memory>
#include <x86intrin.h>

#include "detail.hpp"

namespace bucket_hood {

namespace avx2 {

struct Bucket {
    static constexpr int NUM_SLOTS = 32;
    alignas( __m256i ) uint8_t occupancy_and_hashes[ 32 ];
    alignas( __m256i ) uint8_t probe_lengths[ 32 ];
};

template < class T, class Hash, class Compare, class Assign, class Allocator >
class SetImpl {
  private:
    Slot< T >* m_slots{ nullptr };
    Bucket* m_buckets{ nullptr };
    size_type m_num_occupied{ 0 };
    size_type m_rehash{ 0 };
    Allocator m_allocator;
    uint8_t m_bitshift{ hash_bits< Hash, T > };

    /*
     * For each occupied bucket in 'buckets', for each non-empty slot in the bucket, invoke F on a pointer to
     * the slot storing this entry and the index of the slot, that is, f( Slot<T>*, int i ) with i < 32.
     */
    template < class F >
    static ALWAYS_INLINE void visit_occupied_slots( const Bucket* buckets, Slot< T >* slots,
                                                    size_type num_buckets, F&& f ) {
        for ( size_type bi = 0; bi < num_buckets; ++bi ) {
            const Bucket& bucket = buckets[ bi ];
            __m256i occupancy_and_hashes = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
            int occupied_mask = _mm256_movemask_epi8( occupancy_and_hashes );
            Slot< T >* bucket_start = slots + Bucket::NUM_SLOTS * bi;

            while ( occupied_mask ) {
                int i = CTZ( occupied_mask );
                occupied_mask ^= ( 1 << i );
                std::invoke( f, bucket_start + i, i );
            }
        }
    }

  public:
    typedef typename std::allocator_traits< Allocator >::template rebind_alloc< Slot< T > > SlotAlloc;
    typedef typename std::allocator_traits< Allocator >::template rebind_alloc< Bucket > BucketAlloc;
    typedef std::invoke_result_t< Hash, T > hash_result;
    typedef Bucket bucket_type;
    typedef Hash hash_type;

    friend struct bucket_hood::CoreAlgorithms;

    static Assign assigner( T& dst ) { return Assign( dst ); }

    size_type num_buckets() const noexcept {
        if ( uninitialized() ) {
            return 0;
        }
        return size_type( 1 ) << ( hash_bits< Hash, T > - m_bitshift );
    }

    bool empty() const { return m_num_occupied == 0; }

    bool uninitialized() const noexcept { return m_bitshift == hash_bits< Hash, T >; }

    void initialize() {
        assert( uninitialized() );
        m_slots = rebind_allocate< SlotAlloc >( m_allocator, 2 * Bucket::NUM_SLOTS );
        m_buckets = rebind_allocate< BucketAlloc >( m_allocator, 2 );
        std::memset( m_buckets, 0, sizeof( Bucket ) * 2 );
        std::uninitialized_default_construct_n( m_buckets, 2 );
        m_rehash = 2 * Bucket::NUM_SLOTS * default_load_factor;
        m_bitshift -= 1;
    }

    SetImpl() = default;

    ~SetImpl() {
        if ( !m_slots )
            return;

        size_type num_buckets = size_type( 1 ) << ( hash_bits< Hash, T > - m_bitshift );
        if constexpr ( !std::is_trivially_destructible_v< T > ) {
            visit_occupied_slots( m_buckets, m_slots, num_buckets,
                                  []( Slot< T >* slot, int ) { slot->destroy(); } );
        }

        rebind_deallocate< SlotAlloc >( m_allocator, m_slots, Bucket::NUM_SLOTS * num_buckets );
        rebind_deallocate< BucketAlloc >( m_allocator, m_buckets, num_buckets );
    }

    /*
     * Core API for finding OR inserting elements. This function does not actually modify the container.
     * Instead, if an element is to be inserted, it returns the bucket and index where the insertion
     * happens (tagged to indicate if there is an eviction). A subsequent call to 'insert' with the key
     * supplied along with the returned BucketAndSlot does the actual insertion.
     *
     * If Insert is FindOrInsert, this function promises to return NO_BUCKET if a rehash is necessary.
     * If Insert is FindOnly, a return of NO_BUCKET means the key is not present.
     */
    template < class Evict, class Rehash, class Insert, class S >
    BucketAndSlot find_or_insert( const S& key, size_type bucket_index, uint8_t low_bits,
                                  uint8_t probe_length = 0 ) const {

        static_assert( Insert{} || !( Evict{} || Rehash{} ),
                       "Find-only and {Evict, Rehash} policies are mutually exclusive" );
        assert( !uninitialized() );

        if constexpr ( !Insert{} ) {
            if ( empty() ) {
                return NO_BUCKET;
            }
        }

        auto loop_condition = [ &probe_length ]() {
            if constexpr ( !Insert{} ) {
                return true;
            } else {
                bool out = ++probe_length;
                assert( out && "Overflowed 8-bit probe length!" );
                return out;
            }
        };

        do {
            const Bucket& bucket = m_buckets[ bucket_index ];
            const Slot< T >* bucket_start = m_slots + Bucket::NUM_SLOTS * bucket_index;
            __m256i slots = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );

            if constexpr ( !( Evict{} || Rehash{} ) ) {
                __m256i broadcasted_low_bits = _mm256_set1_epi8( low_bits );
                __m256i equal_slots = _mm256_cmpeq_epi8( broadcasted_low_bits, slots );
                int match_mask = _mm256_movemask_epi8( equal_slots );

                while ( match_mask ) {
                    int i = CTZ( match_mask );
                    if ( LIKELY( Compare{}( key, ( bucket_start + i )->get() ) ) ) {
                        return found_existing_slot( bucket_index, i );
                    }
                    match_mask ^= 1 << i;
                }
            }

            int empty_mask = ~_mm256_movemask_epi8( slots );
            if constexpr ( !Insert{} ) {
                if ( empty_mask ) {
                    return NO_BUCKET;
                }
            } else {
                // Optimistic case first - a slot is available.
                if ( LIKELY( empty_mask != 0 ) ) {
                    if constexpr ( !Rehash{} ) {
                        if ( UNLIKELY( m_num_occupied + 1 > m_rehash ) ) {
                            return NO_BUCKET;
                        }
                    }
                    int i = CTZ( empty_mask );
                    return found_empty_slot( bucket_index, i, probe_length );
                } else {
                    // First check if probe length is greater than all current probe lengths.
                    __m256i broadcasted_probe_length = _mm256_set1_epi8( probe_length );
                    __m256i probe_lengths = _mm256_load_si256( (const __m256i*)bucket.probe_lengths );
                    __m256i comparison = _mm256_cmpgt_epi8( broadcasted_probe_length, probe_lengths );
                    int gt_mask = _mm256_movemask_epi8( comparison );
                    // If gt_mask == ~0 then our probe length is longer than all probe lengths in
                    // the current bucket, so we evict the smallest probe length.
                    if ( gt_mask == ~0 ) {
                        if constexpr ( !Rehash{} ) {
                            if ( UNLIKELY( m_num_occupied + 1 > m_rehash ) ) {
                                return NO_BUCKET;
                            }
                        }
                        // Look for zeroes first.
                        __m256i zero_vector = _mm256_setzero_si256();
                        __m256i zero_pls = _mm256_cmpeq_epi8( zero_vector, probe_lengths );
                        int zero_mask = _mm256_movemask_epi8( zero_pls );
                        int min_slot = 0;
                        uint8_t min_probe_length = 0xff;
                        if ( LIKELY( zero_mask ) ) {
                            min_slot = CTZ( zero_mask );
                            min_probe_length = 0;
                        } else {
                            for ( int i = 0; i < 32; ++i ) {
                                if ( bucket.probe_lengths[ i ] < min_probe_length ) {
                                    min_probe_length = bucket.probe_lengths[ i ];
                                    min_slot = i;
                                }
                            }
                        }

                        return found_eviction( bucket_index, min_slot, probe_length );
                    }
                }
            }
            bucket_index = ( bucket_index + 1 ) & ( SENTINEL_INDEX >> m_bitshift );
        } while ( loop_condition() );
        UNREACHABLE();
    }
};

} // namespace avx2

} // namespace bucket_hood

#endif // BUCKET_HOOD_AVX2_BACKEND_HPP

