#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define BUCKET_HOOD_BUCKET_OVERRIDE DebugBucket
#include "doctest.h"

#include "test_utils.hpp"
#include <unordered_set>

TEST_CASE( "[small] simple deterministic test" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    std::unordered_set< int > ref_set;

    for ( int i = 0; i < 100; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );

        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    for ( int i = 0; i < 50; ++i ) {
        my_set.erase( i );
        ref_set.erase( i );

        REQUIRE( compare_sets( my_set, ref_set ) );
    }
}

TEST_CASE( "[small] test that includes erasing values not in the map" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    std::unordered_set< int > ref_set;

    // Insert values
    for ( int i = 0; i < 10; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );
    }

    // Erase values that were inserted
    for ( int i = 0; i < 5; ++i ) {
        REQUIRE( my_set.erase( i ) );
        ref_set.erase( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Erase values that were not inserted
    for ( int i = 10; i < 15; ++i ) {
        REQUIRE_FALSE( my_set.erase( i ) );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Insert more values
    for ( int i = 10; i < 15; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Erase all values
    for ( int i = 5; i < 15; ++i ) {
        REQUIRE( my_set.erase( i ) );
        ref_set.erase( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Check that the set is empty
    REQUIRE( my_set.empty() );
    REQUIRE( my_set.size() == 0 );
}

TEST_CASE( "[small] test with find_iterator for erasing values" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    std::unordered_set< int > ref_set;

    // Insert values
    for ( int i = 0; i < 10; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );
    }

    // Erase values that were inserted
    for ( int i = 0; i < 5; ++i ) {
        auto it = my_set.find_iterator( i );
        REQUIRE( it != my_set.end() );
        REQUIRE( *it == i );
        it = my_set.erase( it );
        if ( it != my_set.end() ) {
            REQUIRE( ref_set.count( *it ) != 0 );
        }
        ref_set.erase( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }
}

TEST_CASE( "[small][insert-erase] check resource management" ) {
    REQUIRE( AllocatorCounters::allocated );
    REQUIRE( AllocatorCounters::allocated == AllocatorCounters::deallocated );
    REQUIRE( AllocatorCounters::constructed );
    // This one should fail because of trivial destructibility
    // REQUIRE( AllocatorCounters::constructed == AllocatorCounters::destroyed );
}