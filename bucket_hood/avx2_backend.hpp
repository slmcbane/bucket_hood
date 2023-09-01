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
#include "traits.hpp"

namespace bucket_hood {

namespace avx2 {

struct Bucket {
    static constexpr int NUM_SLOTS = 32;
    alignas( __m256i ) uint8_t occupancy_and_hashes[ 32 ];
    alignas( __m256i ) uint8_t probe_lengths[ 32 ];

    static int occupied_mask( const Bucket& bucket ) {
        __m256i slots = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
        return _mm256_movemask_epi8( slots );
    }

    void setup_end_sentinel() noexcept {
        for ( int i = 0; i < 32; ++i ) {
            occupancy_and_hashes[ i ] = 0xff;
        }
    }
};

// Inherits from Allocator to take advantage of the empty base class optimization.
template < class T, class Hash, class Compare, class Assign, class Allocator >
class SetImpl : private Allocator {
  private:
    Slot< T >* m_slots{ nullptr };
    Bucket* m_buckets{ nullptr };
    size_type m_num_occupied{ 0 };
    size_type m_rehash{ 0 };
    uint8_t m_bitshift{ hash_bits< Hash, T > };

    /*
     * For each occupied bucket in 'buckets', for each non-empty slot in the bucket,
     * invoke F on a pointer to the slot storing this entry and the index of the
     * slot, that is, f( Slot<T>*, int i ) with i < 32.
     */
    template < class F >
    static ALWAYS_INLINE void visit_occupied_slots( Bucket* buckets, Slot< T >* slots, size_type num_buckets,
                                                    F&& f ) {
        static_assert( std::is_invocable_v< F, Bucket&, Slot< T >*, size_t, int >,
                       "The callable in visit_occupied_slots takes a pointer to Bucket, a pointer to Slot<T>, "
                       "the bucket index, and the slot index" );
        for ( size_type bi = 0; bi < num_buckets; ++bi ) {
            Bucket& bucket = buckets[ bi ];
            __m256i occupancy_and_hashes = _mm256_load_si256( (const __m256i*)bucket.occupancy_and_hashes );
            int occupied_mask = _mm256_movemask_epi8( occupancy_and_hashes );
            Slot< T >* bucket_start = slots + Bucket::NUM_SLOTS * bi;

            while ( occupied_mask ) {
                int i = CTZ( occupied_mask );
                occupied_mask ^= ( 1 << i );
                std::invoke( f, bucket, bucket_start + i, bi, i );
            }
        }
    }

    void copy_buckets_from( const SetImpl& other, size_t num_buckets ) {
        std::memset( m_buckets, 0, num_buckets * sizeof( Bucket ) );
        m_buckets[ num_buckets ].setup_end_sentinel();
        m_num_occupied = other.m_num_occupied;
        m_rehash = other.m_rehash;
        m_bitshift = other.m_bitshift;
        visit_occupied_slots(
            other.m_buckets, other.m_slots, num_buckets,
            [ this ]( auto& bucket, auto* slot, size_t bucket_index, int slot_index ) {
                m_buckets[ bucket_index ].occupancy_and_hashes[ slot_index ] =
                    bucket.occupancy_and_hashes[ slot_index ];
                m_buckets[ bucket_index ].probe_lengths[ slot_index ] = bucket.probe_lengths[ slot_index ];
                m_slots[ bucket_index * Bucket::NUM_SLOTS + slot_index ].emplace( slot->get() );
            } );
    }

    Allocator& allocator() { return static_cast< Allocator& >( *this ); }
    const Allocator& allocator() const { return static_cast< const Allocator& >( *this ); }

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

    size_type size() const { return m_num_occupied; }
    bool empty() const { return m_num_occupied == 0; }

    bool uninitialized() const noexcept { return m_bitshift == hash_bits< Hash, T >; }

    void initialize() {
        assert( uninitialized() );
        m_slots = rebind_allocate< SlotAlloc >( allocator(), 2 * Bucket::NUM_SLOTS );
        m_buckets = rebind_allocate< BucketAlloc >( allocator(), 3 );
        std::memset( m_buckets, 0, sizeof( Bucket ) * 2 );
        m_buckets[ 2 ].setup_end_sentinel();
        m_rehash = 2 * Bucket::NUM_SLOTS * default_load_factor;
        m_bitshift -= 1;
    }

    SetImpl() = default;

    SetImpl( const SetImpl& other )
        : Allocator(
              std::allocator_traits< Allocator >::select_on_container_copy_construction( other.allocator() ) ) {
        if ( other.empty() ) {
            return;
        }

        size_t nb = other.num_buckets();
        m_slots = rebind_allocate< SlotAlloc >( allocator(), nb * Bucket::NUM_SLOTS );
        m_buckets = rebind_allocate< BucketAlloc >( allocator(), nb + 1 );
        copy_buckets_from( other, nb );
    }

    ~SetImpl() {
        if ( !m_slots )
            return;

        if constexpr ( !std::is_trivially_destructible_v< T > ) {
            if ( m_num_occupied ) {
                visit_occupied_slots( m_buckets, m_slots, num_buckets(),
                                      []( Bucket&, Slot< T >* slot, size_t, int ) { slot->destroy(); } );
            }
        }

        rebind_deallocate< SlotAlloc >( allocator(), m_slots, Bucket::NUM_SLOTS * num_buckets() );
        rebind_deallocate< BucketAlloc >( allocator(), m_buckets, num_buckets() + 1 );
    }

    void clear() {
        if ( !m_slots || !m_num_occupied ) {
            return;
        }

        if constexpr ( !std::is_trivially_destructible_v< T > ) {
            visit_occupied_slots( m_buckets, m_slots, num_buckets(),
                                  []( Bucket& bucket, Slot< T >* slot, size_t, int slot_index ) {
                                      slot->destroy();
                                      bucket.occupancy_and_hashes[ slot_index ] = 0;
                                      bucket.probe_lengths[ slot_index ] = 0;
                                  } );
        } else {
            std::memset( m_buckets, 0, sizeof( Bucket ) * num_buckets() );
        }

        m_num_occupied = 0;
    }

    SetImpl& operator=( SetImpl&& other ) {
        if ( this == &other ) {
            return *this;
        }

        static_assert( std::allocator_traits< Allocator >::propagate_on_container_move_assignment::value ||
                           std::allocator_traits< Allocator >::is_always_equal::value,
                       "TODO: move assignment if allocator does not propagate" );

        if ( m_slots ) {
            size_type num_buckets = size_type( 1 ) << ( hash_bits< Hash, T > - m_bitshift );
            if constexpr ( !std::is_trivially_destructible_v< T > ) {
                visit_occupied_slots( m_buckets, m_slots, num_buckets,
                                      []( Bucket&, Slot< T >* slot, size_t, int ) { slot->destroy(); } );
            }

            rebind_deallocate< SlotAlloc >( allocator(), m_slots, Bucket::NUM_SLOTS * num_buckets );
            rebind_deallocate< BucketAlloc >( allocator(), m_buckets, num_buckets + 1 );
        }
        allocator() = std::move( other.allocator() );
        m_slots = other.m_slots;
        other.m_slots = nullptr;
        m_buckets = other.m_buckets;
        other.m_buckets = nullptr;
        m_num_occupied = other.m_num_occupied;
        other.m_num_occupied = 0;
        m_rehash = other.m_rehash;
        other.m_rehash = 0;
        m_bitshift = other.m_bitshift;
        other.m_bitshift = hash_bits< Hash, T >;
        return *this;
    }

    SetImpl& operator=( const SetImpl& other ) {
        if ( this == &other ) {
            return *this;
        } else if ( other.empty() ) {
            clear();
        }

        static_assert( is_always_equal< Allocator > || copy_assign_propagates< Allocator >,
                       "TODO: copy assignment if allocator does not propagate" );

        size_t new_num_buckets = other.num_buckets();
        if ( m_slots ) {
            if constexpr ( !std::is_trivially_destructible_v< T > ) {
                visit_occupied_slots( m_buckets, m_slots, num_buckets(),
                                      []( Bucket&, Slot< T >* slot, size_t, int ) { slot->destroy(); } );
            }

            // If my number of buckets is != other's number of buckets, we free our bucket and slot
            // allocations and allocate new ones. We do this even if we have more buckets, preferring
            // to be conservative with memory rather than retain a larger amount of storage.
            if ( m_bitshift != other.m_bitshift ) {
                rebind_deallocate< SlotAlloc >( allocator(), m_slots, Bucket::NUM_SLOTS * num_buckets() );
                rebind_deallocate< BucketAlloc >( allocator(), m_buckets, num_buckets() + 1 );
                m_slots = rebind_allocate< SlotAlloc >( allocator(), new_num_buckets * Bucket::NUM_SLOTS );
                m_buckets = rebind_allocate< BucketAlloc >( allocator(), new_num_buckets + 1 );
            }
        } else {
            m_slots = rebind_allocate< SlotAlloc >( allocator(), new_num_buckets * Bucket::NUM_SLOTS );
            m_buckets = rebind_allocate< BucketAlloc >( allocator(), new_num_buckets + 1 );
        }
        copy_buckets_from( other, new_num_buckets );
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

        auto loop_condition = [ & ]() {
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

    void erase( size_type bucket_index, int index_in_bucket ) {
        Bucket* bucket = m_buckets + bucket_index;
        do {
            Slot< T >* bucket_start = m_slots + 32 * bucket_index;
            __m256i slots = _mm256_load_si256( (const __m256i*)bucket->occupancy_and_hashes );
            int occupied_mask = _mm256_movemask_epi8( slots );
            if ( LIKELY( occupied_mask != ~0 ) ) {
            finished:
                ( bucket_start + index_in_bucket )->destroy();
                bucket->occupancy_and_hashes[ index_in_bucket ] = 0;
                bucket->probe_lengths[ index_in_bucket ] = 0;
                m_num_occupied--;
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
                goto finished;
            }
            ( bucket_start + index_in_bucket )->get() = std::move( ( next_bucket_start + max_slot )->get() );
            bucket->occupancy_and_hashes[ index_in_bucket ] = next_bucket->occupancy_and_hashes[ max_slot ];
            bucket->probe_lengths[ index_in_bucket ] = max_probe_len - 1;
            bucket = next_bucket;
            index_in_bucket = max_slot;
        } while ( true );
    }
};

} // namespace avx2

} // namespace bucket_hood

#endif // BUCKET_HOOD_AVX2_BACKEND_HPP

