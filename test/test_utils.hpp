/*
 * Author: Sean McBane, 2024
 *
 * This file and other files except for 'doctest.h' contained in this directory are released into the
 * public domain using the CC0 waiver, which you can read in CC0.txt or at
 * https://creativecommons.org/publicdomain/zero/1.0/legalcode.
 */

#ifndef BUCKET_HOOD_TEST_UTILS_HPP
#define BUCKET_HOOD_TEST_UTILS_HPP

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <unordered_map>

namespace bucket_hood {
struct EndSentinelTag;
} // namespace bucket_hood

class Splitmix64 {
  public:
    typedef uint64_t result_type;

    explicit Splitmix64( uint64_t state ) : m_state{ state } {}

    uint64_t operator()() {
        uint64_t out = m_state += 0x9E3779B97f4A7C15;
        out = ( out ^ ( out >> 30 ) ) * 0xBF58476D1CE4E5B9;
        out = ( out ^ ( out >> 27 ) ) * 0x94D049BB133111EB;
        return out ^ ( out >> 31 );
    }

    static constexpr uint64_t max() { return std::numeric_limits< uint64_t >::max(); }
    static constexpr uint64_t min() { return 0; }

  private:
    uint64_t m_state;
};

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

class NonTrivialInt {
    NonTrivialInt() : m_ptr( new uint64_t( 0 ) ) {}
    NonTrivialInt( NonTrivialInt&& ) = default;
    NonTrivialInt( const NonTrivialInt& other ) : m_ptr( new uint64_t( static_cast< uint64_t >( other ) ) ) {}
    NonTrivialInt& operator=( NonTrivialInt&& ) = default;
    NonTrivialInt& operator=( const NonTrivialInt& other ) {
        m_ptr.reset( new uint64_t( static_cast< uint64_t >( other ) ) );
        return *this;
    }

    operator uint64_t() const { return *m_ptr; }
    bool operator==( const NonTrivialInt& other ) const { return (uint64_t)( *this ) == (uint64_t)other; }

  private:
    std::unique_ptr< uint64_t > m_ptr;
};

// To stress test in testing I want to make containers work hard with a bad hash function.
// If it's completely trivial though, like mapping all entries to the same bucket, you will
// just get an infinite loop of rehashing.
// This is the infamously bad RANDU recurrence relation.
template < class T >
struct BadHash {
    uint64_t operator()( T x ) const { return ( (uint64_t)x * 65539ul ) & 0xfffffffful; }
};

template < class T >
struct DebugBucket;

template < class T >
inline constexpr bool is_debug_bucket = false;

template < class T >
inline constexpr bool is_debug_bucket< DebugBucket< T > > = true;

template < class T >
class DebugAllocator : private AllocatorCounters {
  public:
    typedef T value_type;
    typedef std::false_type is_always_equal;
    typedef std::true_type propagate_on_container_move_assignment;

    DebugAllocator() : m_allocations{ std::make_shared< std::unordered_map< void*, std::size_t > >() } {};

    template < class U >
    requires( !std::is_same_v< T, U > )
    DebugAllocator( const DebugAllocator< U >& other ) : m_allocations{ other.m_allocations } {}

    DebugAllocator( const DebugAllocator& ) = default;

    DebugAllocator& operator=( const DebugAllocator& other ) {
        m_allocations = other.m_allocations;
        return *this;
    }

    T* allocate( std::size_t n ) {
        allocated += n * sizeof( T );
        size_t diff = allocated - deallocated;
        AllocatorCounters::peak = std::max( diff, AllocatorCounters::peak );
        void* out = ::operator new[]( sizeof( T ) * n, std::align_val_t{ alignof( T ) } );
        m_allocations->insert( { out, n } );
        return reinterpret_cast< T* >( out );
    }

    void deallocate( T* p, std::size_t n ) {
        auto it = m_allocations->find( p );
        assert( it != m_allocations->end() );
        assert( it->second == n );
        m_allocations->erase( it );
        deallocated += sizeof( T ) * n;
        ::operator delete[]( p, sizeof( T ) * n, std::align_val_t{ alignof( T ) } );
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

    std::shared_ptr< std::unordered_map< void*, std::size_t > > m_allocations;
};

#include "../bucket_hood.hpp"
// Specialize known_good to prevent automatic hash mixing being applied.
template < class T >
struct bucket_hood::known_good< BadHash< T > > : std::true_type {};

template < class Set1, class Set2 >
bool compare_sets( const Set1& set1, const Set2& set2 ) {
    if ( set1.size() != set2.size() ) {
        return false;
    }
    for ( const auto& x : set1 ) {
        if ( !set2.contains( x ) ) {
            return false;
        }
    }
    return true;
}

#endif // BUCKET_HOOD_TEST_UTILS_HPP
