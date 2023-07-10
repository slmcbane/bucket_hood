#ifndef BUCKET_HOOD_SET_BASE_HPP
#define BUCKET_HOOD_SET_BASE_HPP

#include "detail.hpp"

#include <cassert>
#include <limits>
#include <memory>
#include <type_traits>
#include <utility>

namespace bucket_hood {

inline constexpr size_type SENTINEL_INDEX = std::numeric_limits< size_type >::max();
inline constexpr BucketAndSlot NO_BUCKET = { SENTINEL_INDEX, 0 };

template < class T, class Hash, class Compare, class Assign, class Allocator >
struct SetBase {
    virtual ~SetBase() {
        if ( !m_slots ) {
            return;
        }

        size_type num_buckets = size_type( 1 ) << ( hash_bits - m_bitshift );
        if constexpr ( !std::is_trivially_destructible_v< T > ) {
            if ( m_num_occupied ) {
                SlotIterator slot_iterator( m_buckets, m_slots, num_buckets );
                SlotIterator end_iterator = slot_iterator.end();
                for ( ; slot_iterator != end_iterator; ++slot_iterator ) {
                    slot_iterator->destroy();
                }
            }
        }

        auto slot_alloc = SlotAlloc( m_allocator );
        auto bucket_alloc = BucketAlloc( m_allocator );
        std::allocator_traits< SlotAlloc >::deallocate( slot_alloc, m_slots, num_buckets * Bucket::NUM_SLOTS );
        std::allocator_traits< BucketAlloc >::deallocate( bucket_alloc, m_buckets, num_buckets );
    }

  protected:
    SetBase() = default;

    // Client code is expected to provide the hash.
    template < class S >
    BucketAndSlot find_impl( const S& key ) const {
        if ( empty() ) {
            return NO_BUCKET;
        }
        size_type hash = Hash{}( key );
        size_type bucket_index = hash >> m_bitshift;

        return find_impl( key, bucket_index, hash & 0x7f );
    }

    template < class S >
    BucketAndSlot insert_impl( S&& key ) {
        if ( m_bitshift == hash_bits ) {
            initial_setup();
        }

        size_type hash = Hash{}( key );
        size_type bucket_index = hash >> m_bitshift;
        uint8_t low_bits = ( hash & 0x7f ) | 0x80;
        BucketAndSlot result =
            find_or_insert< true, false >( static_cast< S&& >( key ), bucket_index, low_bits, 0 );
        if ( UNLIKELY( result.bucket_index == SENTINEL_INDEX ) ) {
            rehash();
            bucket_index = hash >> m_bitshift;
            result = find_or_insert< true, false >( static_cast< S&& >( key ), bucket_index, low_bits, 0 );
            assert( result.bucket_index != SENTINEL_INDEX );
        }
        return result;
    }

    void erase_impl( Bucket* bucket, int index_in_bucket ) {
        size_type bucket_index = bucket - m_buckets;
        do {
            Slot< T >* bucket_start = m_slots + 32 * bucket_index;
            __m256i slots = _mm256_load_si256( (const __m256i*)bucket->occupancy_and_hashes );
            int occupied_mask = _mm256_movemask_epi8( slots );
            if ( LIKELY( occupied_mask != ~0 ) ) {
                ( bucket_start + index_in_bucket )->destroy();
                bucket->occupancy_and_hashes[ index_in_bucket ] = 0;
                bucket->probe_lengths[ index_in_bucket ] = 0;
                return;
            }

            // At this point, the current bucket is full so we need to shift a value backward from the next
            // bucket. We shift backward the value with longest probe length.
            bucket_index = ( bucket_index + 1 ) & ( SENTINEL_INDEX >> m_bitshift );
            Bucket* next_bucket = m_buckets + bucket_index;
            Slot< T >* next_bucket_start = m_slots + 32 * bucket_index;

            // Find slots in next bucket with non-zero probe length.
            __m256i probe_lengths = _mm256_load_si256( (const __m256i*)next_bucket->probe_lengths );
            __m256i zero_vec = _mm256_setzero_si256();
            __m256i nonzeros = _mm256_cmpgt_epi8( probe_lengths, zero_vec );
            int nzmask = _mm256_movemask_epi8( nonzeros );
            uint8_t max_probe_len = 0;
            int max_slot = 32;
            while ( nzmask ) {
                int i = CTZ( nzmask );
                uint8_t pl = next_bucket->probe_lengths[ i ];
                if ( pl > max_probe_len ) {
                    max_probe_len = pl;
                    max_slot = i;
                }
                nzmask ^= ( 1 << i );
            }
            if ( max_slot == 32 ) {
                // All probe lengths in next bucket were zero, so we're done.
                return;
            }
            ( bucket_start + index_in_bucket )->get() = std::move( ( next_bucket_start + max_slot )->get() );
            bucket = next_bucket;
            index_in_bucket = max_slot;
        } while ( true );
    }

  private:
    Slot< T >* m_slots{ nullptr };
    Bucket* m_buckets{ nullptr };
    size_type m_num_occupied{ 0 };
    size_type m_rehash{ 0 };
    Allocator m_allocator;
    uint8_t m_bitshift{ hash_bits };

    using SlotAlloc = typename std::allocator_traits< Allocator >::template rebind_alloc< Slot< T > >;
    using BucketAlloc = typename std::allocator_traits< Allocator >::template rebind_alloc< Bucket >;
    static_assert( std::is_trivially_default_constructible_v< Slot< T > > );

    // First allocations, etc. Uninitialized container owns no memory.
    void initial_setup() {
        auto slot_alloc = SlotAlloc( m_allocator );
        auto bucket_alloc = BucketAlloc( m_allocator );
        m_slots = std::allocator_traits< SlotAlloc >::allocate( slot_alloc, 2 * Bucket::NUM_SLOTS );
        m_buckets = std::allocator_traits< BucketAlloc >::allocate( bucket_alloc, 2 );
        std::uninitialized_default_construct_n( m_buckets, 2 );
        m_rehash = 2 * Bucket::NUM_SLOTS * default_load_factor;
        m_bitshift = hash_bits - 1;
    }

    template < bool top_level, bool is_rehash, class S >
    BucketAndSlot find_or_insert( S&& key, size_type bucket_index, uint8_t low_bits, uint8_t curr_probe_len ) {
        do {
            Bucket& bucket = m_buckets[ bucket_index ];
            Slot< T >* bucket_start = m_slots + 32 * bucket_index;

            __m256i slots = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
            if constexpr ( top_level && !is_rehash ) {
                __m256i broadcasted_low_bits = _mm256_set1_epi8( low_bits );
                __m256i equal_slots = _mm256_cmpeq_epi8( broadcasted_low_bits, slots );
                int match_mask = _mm256_movemask_epi8( equal_slots );

                while ( match_mask ) {
                    int i = CTZ( match_mask );
                    if ( Compare{}( key, ( bucket_start + i )->get() ) ) {
                        return { bucket_index, i | 32 };
                    }
                    match_mask ^= 1 << i;
                }
            }

            int empty_mask = ~_mm256_movemask_epi8( slots );
            // Optimistic case first - a slot is available.
            if ( LIKELY( empty_mask != 0 ) ) {
                if constexpr ( !is_rehash ) {
                    if ( UNLIKELY( m_num_occupied + 1 > m_rehash ) ) {
                        return NO_BUCKET;
                    }
                }
                int i = CTZ( empty_mask );
                ( bucket_start + i )->emplace( static_cast< S&& >( key ) );
                bucket.occupancy_and_hashes[ i ] = low_bits;
                bucket.probe_lengths[ i ] = curr_probe_len;
                m_num_occupied += 1;
                return { bucket_index, i };
            } else {
                // First check if probe length is greater than all current probe lengths.
                __m256i broadcasted_probe_length = _mm256_set1_epi8( curr_probe_len );
                __m256i probe_lengths = _mm256_load_si256( (const __m256i*)bucket.probe_lengths );
                __m256i comparison = _mm256_cmpgt_epi8( broadcasted_probe_length, probe_lengths );
                int gt_mask = _mm256_movemask_epi8( comparison );
                // If gt_mask == ~0 then our probe length is longer than all probe lengths in
                // the current bucket, so we evict the smallest probe length.
                if ( gt_mask == ~0 ) {
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

                    // If this is a top level call, then recurse. Otherwise, to keep from blowing
                    // the stack, we can swap the bucket's contained value and continue with our
                    // loop: we know that the passed argument is T&&.
                    if constexpr ( top_level ) {
                        auto [ final_bucket, _ ] = find_or_insert< false, is_rehash >(
                            std::move( ( bucket_start + min_slot )->get() ),
                            ( bucket_index + 1 ) & ( SENTINEL_INDEX >> m_bitshift ),
                            bucket.occupancy_and_hashes[ min_slot ], bucket.probe_lengths[ min_slot ] + 1 );
                        if constexpr ( !is_rehash ) {
                            if ( UNLIKELY( final_bucket == SENTINEL_INDEX ) ) {
                                return NO_BUCKET;
                            }
                        }
                        Assign{}( ( bucket_start + min_slot )->get(), static_cast< S&& >( key ) );
                        bucket.occupancy_and_hashes[ min_slot ] = low_bits;
                        bucket.probe_lengths[ min_slot ] = curr_probe_len;
                        return { bucket_index, min_slot };
                    } else {
                        static_assert( std::is_same_v< S, T > );
                        using std::swap;
                        swap( key, ( bucket_start + min_slot )->get() );
                        swap( low_bits, bucket.occupancy_and_hashes[ min_slot ] );
                        swap( curr_probe_len, bucket.probe_lengths[ min_slot ] );
                    }
                }
            }

            bucket_index = ( bucket_index + 1 ) & ( SENTINEL_INDEX >> m_bitshift );
        } while ( ++curr_probe_len );
        assert( false && "Overflowed 8-bit probe length" );
        UNREACHABLE();
    }

    template < class S >
    BucketAndSlot find_impl( const S& key, size_type bucket_index, uint8_t low_bits ) const {
        do {
            Bucket& bucket = m_buckets[ bucket_index ];
            const Slot< T >* bucket_start = m_slots + 32 * bucket_index;

            __m256i slots = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
            __m256i broadcasted_low_bits = _mm256_set1_epi8( low_bits | uint8_t( 0x80 ) );
            __m256i equal_slots = _mm256_cmpeq_epi8( broadcasted_low_bits, slots );
            int match_mask = _mm256_movemask_epi8( equal_slots );

            while ( match_mask ) {
                int i = CTZ( match_mask );
                if ( Compare{}( key, ( bucket_start + i )->get() ) ) {
                    return { bucket_index, i | 32 };
                }
                match_mask ^= 1 << i;
            }

            int occupied_mask = _mm256_movemask_epi8( slots );
            if ( occupied_mask != ~0 ) {
                return NO_BUCKET;
            }

            bucket_index = ( bucket_index + 1 ) & ( SENTINEL_INDEX >> m_bitshift );
        } while ( true );
        UNREACHABLE();
    }

  public:
    bool empty() const { return m_num_occupied == 0; }

    // Unconditionally doubles the number of slots.
    void rehash() {
        if ( m_bitshift == hash_bits ) {
            initial_setup();
            return;
        }

        size_type old_num_buckets = size_type( 1 ) << ( hash_bits - m_bitshift );
        size_type new_num_buckets = old_num_buckets * 2;
        auto slot_alloc = SlotAlloc( m_allocator );
        auto bucket_alloc = BucketAlloc( m_allocator );

        Bucket* old_buckets = m_buckets;
        Slot< T >* old_slots = m_slots;

        m_slots =
            std::allocator_traits< SlotAlloc >::allocate( slot_alloc, new_num_buckets * Bucket::NUM_SLOTS );
        m_buckets = std::allocator_traits< BucketAlloc >::allocate( bucket_alloc, new_num_buckets );
        std::uninitialized_default_construct_n( m_buckets, new_num_buckets );
        m_num_occupied = 0;
        m_rehash = double( new_num_buckets ) * Bucket::NUM_SLOTS * default_load_factor;
        m_bitshift -= 1;

        for ( size_type bi = 0; bi < old_num_buckets; ++bi ) {
            Bucket& bucket = old_buckets[ bi ];
            __m256i slots = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
            int occupied_mask = _mm256_movemask_epi8( slots );

            while ( occupied_mask ) {
                Slot< T >* bucket_start = old_slots + 32 * bi;
                int i = CTZ( occupied_mask );
                T& x = ( bucket_start + i )->get();
                size_type hash = Hash{}( x );
                size_type bucket_index = hash >> m_bitshift;
                uint8_t low_bits = ( hash & 0x7f ) | 0x80;
                Bucket& new_bucket = m_buckets[ bucket_index ];

                if ( new_bucket.occupancy_and_hashes[ i ] == 0 ) {
                    Slot< T >* new_bucket_start = m_slots + 32 * bucket_index;
                    new_bucket.occupancy_and_hashes[ i ] = low_bits;
                    ( new_bucket_start + i )->emplace( std::move( x ) );
                    m_num_occupied++;
                } else {
                    find_or_insert< true, true >( std::move( x ), bucket_index, low_bits, 0 );
                }
                ( bucket_start + i )->destroy();
                occupied_mask ^= ( 1 << i );
            }
        }

        std::allocator_traits< SlotAlloc >::deallocate( slot_alloc, old_slots,
                                                        old_num_buckets * Bucket::NUM_SLOTS );
        std::allocator_traits< BucketAlloc >::deallocate( bucket_alloc, old_buckets, old_num_buckets );
    }
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_SET_BASE_HPP

