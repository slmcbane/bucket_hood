#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "use_debug_bucket.hpp"

std::string make_random_string( int len, Splitmix64& generator ) {
    std::uniform_int_distribution< char > dist( 'a', 'z' );
    std::string result;
    result.reserve( len );
    for ( int i = 0; i < len; ++i ) {
        result.push_back( dist( generator ) );
    }
    return result;
}

TEST_CASE( "[small] reserve ahead of time" ) {
    Splitmix64 generator( 0xa871875e );
    bh_set< std::string > set;
    set.set_max_load_factor( 0.75 );

    // There are 8 slots in a DebugBucket. At load factor of 0.75 we can store 384 elements
    // in 64 buckets.
    //
    // However, one element must remain empty - capacity() is at most num_buckets() * slots_per_bucket - 1
    set.rehash( 383 );
    REQUIRE( set.num_buckets() == 64 );
    REQUIRE( set.capacity() == 384 );
    REQUIRE( set.size() == 0 );

    for ( int i = 0; i < 384; ++i ) {
        REQUIRE( set.insert( make_random_string( 10, generator ) ) );
    }
    REQUIRE( set.size() == 384 );
    REQUIRE( set.num_buckets() == 64 );

    REQUIRE( set.insert( make_random_string( 10, generator ) ) );
    REQUIRE( set.size() == 385 );
    REQUIRE( set.num_buckets() == 128 );
    REQUIRE( set.capacity() == 768 );

    set.clear();
    REQUIRE( set.size() == 0 );
    REQUIRE( set.capacity() == 0 );

    set.rehash( 384 );
    REQUIRE( set.capacity() >= 384 );
    REQUIRE( set.num_buckets() == 64 );

    set.clear();
    set.rehash( 385 );
    REQUIRE( set.capacity() == 768 );

    set.clear();
    set.set_max_load_factor( 0.999999 );
    set.rehash( 524'287 );
    REQUIRE( set.capacity() == 524'287 );
} // TEST_CASE
