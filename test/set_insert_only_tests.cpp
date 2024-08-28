#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define BUCKET_HOOD_BUCKET_OVERRIDE DebugBucket
#include "doctest.h"

#include "test_utils.hpp"
#include <unordered_set>

TEST_CASE( "[small] simple deterministic test" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > set;
    for ( int i = 0; i < 200; ++i ) {
        bool inserted = static_cast< bool >( set.insert( i ) );
        REQUIRE_MESSAGE( inserted, i );
        for ( int j = 0; j < i; ++j ) {
            REQUIRE_MESSAGE( set.contains( j ), "Failed finding, ", j );
        }
    }
}

TEST_CASE( " [medium] Sequential insertions but more of them" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > set;
    for ( int i = 10000; i < 12000; ++i ) {
        bool inserted = static_cast< bool >( set.insert( i ) );
        REQUIRE_MESSAGE( inserted, i );
        for ( int j = 10000; j < i; ++j ) {
            if ( j % 2 == 0 ) {
                REQUIRE_MESSAGE( set.contains( j ), "Failed finding, ", j );
            }
        }
    }
    for ( int j = 0; j < 10000; ++j ) {
        REQUIRE( !set.contains( j ) );
    }
}

void test_insertion( Splitmix64& generator, int count, int granularity ) {
    assert( count > 0 );

    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    std::unordered_set< int > ref_set;

    REQUIRE( my_set.empty() );
    REQUIRE( my_set.size() == 0 );

    for ( int i = 0; i < count; ++i ) {
        int to_insert = generator();
        bool inserted = ref_set.insert( to_insert ).second;
        bool binserted = static_cast< bool >( my_set.insert( to_insert ) );
        REQUIRE_MESSAGE( inserted == binserted, "Insertion test failed for x = ", to_insert );
        if ( ( i + 1 ) % granularity == 0 ) {
            for ( int x : ref_set ) {
                REQUIRE_MESSAGE( my_set.contains( x ), "Failed finding ", x, " in my_set" );
            }
        }
    }
}

TEST_CASE( "[small][deterministic] Testing random insertions with fixed seed 1" ) {
    Splitmix64 generator( 0xdeadbeef );
    test_insertion( generator, 1000, 1 );
}

TEST_CASE( "[small][deterministic] Testing random insertions with fixed seed 2" ) {
    Splitmix64 generator( 0xcafebabe );
    test_insertion( generator, 1000, 1 );
}

TEST_CASE( "[small][deterministic] Testing random insertions with fixed seed 3" ) {
    Splitmix64 generator( 0x123456789abcde );
    test_insertion( generator, 1000, 1 );
}

TEST_CASE( "[small][insert-only] check resource management" ) {
    REQUIRE( AllocatorCounters::allocated );
    REQUIRE( AllocatorCounters::allocated == AllocatorCounters::deallocated );
    REQUIRE( AllocatorCounters::constructed );
    // This one should fail because of trivial destructibility
    // REQUIRE( AllocatorCounters::constructed == AllocatorCounters::destroyed );
}