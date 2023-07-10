#include "bucket_hood/detail.hpp"
#include "bucket_hood/set_base.hpp"
#include "test/test_utils.hpp"

#include <boost/unordered/unordered_flat_set.hpp>

#include <chrono>
#include <functional>
#include <unordered_set>

struct Assigner {
    void operator()( int& a, const int& b ) const { a = b; }
};

struct NoHash {
    template < class T >
    uint32_t operator()( const T& ) const noexcept {
        return 0;
    }
};

struct TestHelper : public bucket_hood::SetBase< int, bucket_hood::default_hash< int >, std::equal_to< int >,
                                                 Assigner, std::allocator< int > > {
    bool try_insert( int x ) {
        auto [ bucket, index ] = insert_impl( x );
        return index < 32;
    }

    bool insert( int x ) { return try_insert( x ); }

    bool find( int x ) const { return find_impl( x ).bucket_index != bucket_hood::SENTINEL_INDEX; }
};

/*
 * Insert a bunch of values to a set< int > and make sure after each that it and the std::unordered_set
 * match. granularity controls how often we do a full comparison - granularity = 1 means compare after
 * each insertion.
 */
void test_insertion( xoshiro256ss& generator, int count, int granularity = 10 ) {
    assert( count > 0 );

    std::printf( "Begin insertion test\n" );

    TestHelper bset;
    std::unordered_set< int > stdset;

    for ( int i = 0; i < count; ++i ) {
        int to_insert = generator();
        bool inserted = stdset.insert( to_insert ).second;
        bool binserted = bset.try_insert( to_insert );

        if ( binserted ) {
            std::printf( "Inserted %d\n", to_insert );
        }

        assert( inserted == binserted );

        if ( ( i + 1 ) % granularity == 0 ) {
            for ( int x : stdset ) {
                assert( bset.find( x ) );
            }
        }
    }
}

template < class Set >
void benchmark_insertion( xoshiro256ss& generator, int count ) {
    auto start = std::chrono::high_resolution_clock::now();
    Set set;
    for ( int i = 0; i < count; ++i ) {
        set.insert( generator() );
    }
    auto end = std::chrono::high_resolution_clock::now();
    printf( "Elapsed: %ld microseconds\n",
            std::chrono::duration_cast< std::chrono::microseconds >( end - start ).count() );
}

int main( int argc, char* argv[] ) {
    int count = 10000;
    if ( argc > 1 ) {
        count = atoi( argv[ 1 ] );
        if ( count <= 0 ) {
            std::fprintf( stderr, "count should be > 0, got %d\n", count );
            return 1;
        }
    }

    xoshiro256ss generator;
    if ( argc == 6 ) {
        generator.set_state( std::atoll( argv[ 2 ] ), std::atoll( argv[ 3 ] ), std::atoll( argv[ 4 ] ),
                             std::atoll( argv[ 5 ] ) );
    }

    const auto& state = generator.state();
    std::printf( "Initial generator state: %ld %ld %ld %ld\n", state[ 0 ], state[ 1 ], state[ 2 ], state[ 3 ] );

    auto backup_generator = generator;
    benchmark_insertion< TestHelper >( generator, count );
    generator = backup_generator;
    benchmark_insertion< std::unordered_set< int, bucket_hood::default_hash< int > > >( generator, count );
    generator = backup_generator;
    benchmark_insertion< boost::unordered_flat_set< int, bucket_hood::default_hash< int > > >( generator,
                                                                                               count );
    generator = backup_generator;
    benchmark_insertion< TestHelper >( generator, count );
    generator = backup_generator;
    benchmark_insertion< std::unordered_set< int, bucket_hood::default_hash< int > > >( generator, count );
    generator = backup_generator;
    benchmark_insertion< boost::unordered_flat_set< int, bucket_hood::default_hash< int > > >( generator,
                                                                                               count );
    generator = backup_generator;
    benchmark_insertion< TestHelper >( generator, count );
    generator = backup_generator;
    benchmark_insertion< std::unordered_set< int, bucket_hood::default_hash< int > > >( generator, count );
    generator = backup_generator;
    benchmark_insertion< boost::unordered_flat_set< int, bucket_hood::default_hash< int > > >( generator,
                                                                                               count );
    generator = backup_generator;
    benchmark_insertion< TestHelper >( generator, count );
    generator = backup_generator;
    benchmark_insertion< std::unordered_set< int, bucket_hood::default_hash< int > > >( generator, count );
    generator = backup_generator;
    benchmark_insertion< boost::unordered_flat_set< int, bucket_hood::default_hash< int > > >( generator,
                                                                                               count );

    // test_insertion( generator, count, 10 );
}

