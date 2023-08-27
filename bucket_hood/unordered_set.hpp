#ifndef BUCKET_HOOD_UNORDERED_SET_HPP
#define BUCKET_HOOD_UNORDERED_SET_HPP

#include "avx2_backend.hpp"
#include "detail.hpp"
#include "traits.hpp"

namespace bucket_hood {

template < class T, class Hash, class Compare, class Allocator >
struct SetBackendSelector {
#ifdef __AVX2__
    typedef avx2::SetImpl< T, Hash, Compare, default_assign< T >, Allocator > type;
#endif
};

template < class T, class Hash = std::hash< T >, class Compare = std::equal_to<>,
           class Allocator = std::allocator< T > >
class unordered_set {
  public:
    typedef mixed_hash< Hash, T > hasher;
    typedef typename SetBackendSelector< T, hasher, Compare, Allocator >::type backend_type;
    typedef typename backend_type::bucket_type Bucket;
    typedef SetIterator< Bucket, T, true > iterator;
    typedef iterator const_iterator;
    typedef T key_type;
    typedef T value_type;
    typedef bucket_hood::size_type size_type;
    typedef std::ptrdiff_t difference_type;
    typedef Allocator allocator_type;
    typedef value_type& reference;
    typedef value_type const& const_reference;
    typedef typename std::allocator_traits< Allocator >::pointer pointer;
    typedef typename std::allocator_traits< Allocator >::const_pointer const_pointer;

    std::pair< iterator, bool > insert( const T& key ) { return emplace( key ); }

    std::pair< iterator, bool > insert( T&& key ) { return emplace( std::move( key ) ); }

    template < class... Args >
    std::pair< iterator, bool > emplace( Args&&... args ) {
        constexpr bool transparent_comparison =
            is_transparent_comparison< Compare, std::decay_t< Args >... >::value;
        if constexpr ( hash_finder< Hash, T, Args... >::value && transparent_comparison ) {
            if ( UNLIKELY( m_impl.uninitialized() ) ) {
                m_impl.initialize();
            }
            using Hasher = mixed_hash< typename hash_finder< Hash, T, Args... >::hash, Args... >;
            const auto hash = Hasher{}( std::forward< Args >( args )... );
            size_type bucket_index = CoreAlgorithms::bucket_index_from_hash( m_impl, hash );
            const uint8_t low_bits = ( hash & 0x7f ) | 0x80;
            BucketAndSlot where = m_impl.template find_or_insert< NotEviction, NotRehash, FindOrInsert >(
                std::forward< Args >( args )..., bucket_index, low_bits );

            if ( where.not_found() ) {
                CoreAlgorithms::resize( m_impl, m_impl.num_buckets() * 2, m_load_factor );
                bucket_index = CoreAlgorithms::bucket_index_from_hash( m_impl, hash );
                where = m_impl.template find_or_insert< NotEviction, NotRehash, FindOrInsert >(
                    std::forward< Args >( args )..., bucket_index, low_bits );
            }

            if ( !where.key_exists() ) {
                CoreAlgorithms::do_insert( m_impl, low_bits, where, std::forward< Args >( args )... );
            }

            return { CoreAlgorithms::make_const_iterator( m_impl, where ), !where.key_exists() };
        } else {
            return insert( T( std::forward< Args >( args )... ) );
        }
    }

    iterator find( const T& key ) const {
        if ( m_impl.empty() ) {
            return CoreAlgorithms::const_end( m_impl );
        }

        const auto hash = hasher{}( key );
        const uint8_t low_bits = ( hash & 0x7f ) | 0x80;
        auto where = m_impl.template find_or_insert< NotEviction, NotRehash, FindOnly >(
            key, CoreAlgorithms::bucket_index_from_hash( m_impl, hash ), low_bits );

        if ( where.not_found() ) {
            return CoreAlgorithms::const_end( m_impl );
        } else {
            return CoreAlgorithms::make_const_iterator( m_impl, where );
        }
    }

    bool contains( const T& key ) const {
        if ( m_impl.empty() ) {
            return false;
        }
        const auto hash = hasher{}( key );
        const uint8_t low_bits = ( hash & 0x7f ) | 0x80;
        auto where = m_impl.template find_or_insert< NotEviction, NotRehash, FindOnly >(
            key, CoreAlgorithms::bucket_index_from_hash( m_impl, hash ), low_bits );
        return !where.not_found();
    }

    iterator begin() const noexcept {
        return m_impl.uninitialized() ? iterator() : CoreAlgorithms::const_iterator_empty_slot( m_impl, 0, 0 );
    }

    iterator end() const noexcept {
        return m_impl.uninitialized() ? iterator() : CoreAlgorithms::const_end( m_impl );
    }

    iterator erase( iterator pos ) {
        size_type bucket_index = CoreAlgorithms::bucket_index_from_bucket( m_impl, pos.bucket() );
        m_impl.erase( bucket_index, pos.slot_index() );
        return CoreAlgorithms::const_iterator_empty_slot( m_impl, bucket_index, pos.slot_index() );
    }

    bool erase( const T& key ) {
        if ( m_impl.empty() ) {
            return false;
        }

        const auto hash = hasher{}( key );
        const uint8_t low_bits = ( hash & 0x7f ) | 0x80;
        BucketAndSlot where = m_impl.template find_or_insert< NotEviction, NotRehash, FindOnly >(
            key, CoreAlgorithms::bucket_index_from_hash( m_impl, hash ), low_bits );

        if ( where.not_found() ) {
            return false;
        }

        m_impl.erase( where.bucket_index, where.slot_index() );
        return true;
    }

  private:
    backend_type m_impl;
    float m_load_factor{ default_load_factor };
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_UNORDERED_SET_HPP

