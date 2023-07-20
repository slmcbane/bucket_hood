#ifndef BUCKET_HOOD_UNORDERED_SET_HPP
#define BUCKET_HOOD_UNORDERED_SET_HPP

#include "avx2_backend.hpp"
#include "detail.hpp"

namespace bucket_hood {

template < class T, class Hash, class Compare, class Allocator >
struct SetBackendSelector {
#ifdef __AVX2__
    typedef avx2::SetImpl< T, Hash, Compare, default_assign< T >, Allocator > type;
#endif
};

template < class T, class Hash = std::hash< T >, class Compare = std::equal_to< T >,
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

    std::pair< iterator, bool > insert( const T& key ) {
        if ( UNLIKELY( m_impl.uninitialized() ) ) {
            m_impl.initialize();
        }

        const auto hash = hasher{}( key );
        size_type bucket_index = CoreAlgorithms::bucket_index_from_hash( m_impl, hash );
        const uint8_t low_bits = ( hash & 0x7f ) | 0x80;
        BucketAndSlot where = m_impl.template find_or_insert< NotEviction, NotRehash, FindOrInsert >(
            key, bucket_index, low_bits );

        if ( where.not_found() ) {
            CoreAlgorithms::resize( m_impl, m_impl.num_buckets() * 2, m_load_factor );
            bucket_index = CoreAlgorithms::bucket_index_from_hash( m_impl, hash );
            where = m_impl.template find_or_insert< NotEviction, NotRehash, FindOrInsert >( key, bucket_index,
                                                                                            low_bits );
        }

        if ( !where.key_exists() ) {
            CoreAlgorithms::do_insert( m_impl, key, low_bits, where );
        }

        return { CoreAlgorithms::make_const_iterator( m_impl, where ), !where.key_exists() };
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

    iterator end() const noexcept { return CoreAlgorithms::const_end( m_impl ); }

  private:
    backend_type m_impl;
    float m_load_factor{ default_load_factor };
};

} // namespace bucket_hood

#endif // BUCKET_HOOD_UNORDERED_SET_HPP

