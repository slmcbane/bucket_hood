#include "use_debug_bucket.hpp"
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

TEST_CASE( "[small] copy constructing a set with trivially copyable elements" ) {
    using namespace bucket_hood;
    bh_set< int > set;
    for ( int i = 0; i < 100; ++i ) {
        set.insert( i );
    }

    auto copy = set;
    REQUIRE( copy == set );
    REQUIRE( copy.capacity() == set.capacity() );
}

TEST_CASE( " [small] copy assigning a set with trivially copyable elements" ) {
    bh_set< int > set;
    for ( int i = 0; i < 100; ++i ) {
        set.insert( i );
    }

    bh_set< int > copy;
    SUBCASE( "copy assigning to an empty set" ) {
        copy = set;
        REQUIRE( copy == set );
        REQUIRE( copy.capacity() == set.capacity() );
    }

    SUBCASE( "copy assigning with same size" ) {
        copy.rehash( 100 );
        for ( int i = 1000; i < 1100; ++i ) {
            copy.insert( i );
        }
        REQUIRE( copy.contains( 1050 ) );
        copy = set;
        REQUIRE( copy == set );
    }

    SUBCASE( "trivial copy to larger set" ) {
        copy.rehash( 200 );
        for ( int i = 100; i < 300; ++i ) {
            copy.insert( i );
        }
        REQUIRE( copy.size() == 200 );
        REQUIRE( copy.contains( 250 ) );
        copy = set;
        REQUIRE( copy == set );
    }

    SUBCASE( "trivial copy to smaller set" ) {
        for ( int i = 200; i < 300; ++i ) {
            set.insert( i );
        }
        REQUIRE( set.size() == 200 );
        for ( int i = 300; i < 350; ++i ) {
            copy.insert( i );
        }
        REQUIRE( copy.capacity() < set.capacity() );
        copy = set;
        REQUIRE( copy == set );
    }
}
