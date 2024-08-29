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
