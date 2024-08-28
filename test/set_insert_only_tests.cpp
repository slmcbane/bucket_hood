#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define BUCKET_HOOD_BUCKET_OVERRIDE DebugBucket
#include "doctest.h"

#include "test_utils.hpp"

TEST_CASE( "[small][deterministic] simple deterministic test" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > set;
    for ( int i = 0; i < 200; ++i ) {
        bool inserted = static_cast< bool >( set.insert( i ) );
        REQUIRE_MESSAGE( inserted, i );
        for ( int j = 0; j < i; ++j ) {
            REQUIRE_MESSAGE( set.contains( j ), "Failed finding, ", j );
        }
    }
}
