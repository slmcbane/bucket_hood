#ifndef SET_INSERT_ONLY_TESTS_HPP
#define SET_INSERT_ONLY_TESTS_HPP

#include <cassert>

#include "../bucket_hood.hpp"

#include "doctest.h"
#include "test_utils.hpp"
#include "unordered_dense.h"

template < class T >
using udset = ankerl::unordered_dense::set< T >;

template < class T, class Hash = std::hash< T > >
using bhset = bucket_hood::unordered_set< T, Hash >;

/*
 * Insert a bunch of values to a set< int > and make sure after each that it and the std::unordered_set
 * match. granularity controls how often we do a full comparison - granularity = 1 means compare after
 * each insertion.
 */
template < class Hash >
void test_insertion( xoshiro256ss& generator, int count, int granularity = 10 ) {
    assert( count > 0 );

    bhset< int, Hash > my_set;
    udset< int > ref_set;

    // REQUIRE( my_set.empty() );
    // REQUIRE( my_set.size() == 0 );

    for ( int i = 0; i < count; ++i ) {
        int to_insert = generator();
        bool inserted = ref_set.insert( to_insert ).second;
        bool binserted = my_set.insert( to_insert );
        REQUIRE_MESSAGE( inserted == binserted, "Insertion test failed for x = ", to_insert );
        if ( ( i + 1 ) % granularity == 0 ) {
            for ( int x : ref_set ) {
                REQUIRE_MESSAGE( my_set.contains( x ), "Failed finding ", x, " in my_set" );
            }
        }
    }
}

TEST_CASE( "Insert 100 values 1000 times with check after every value" ) {
    xoshiro256ss generator;

    for ( int i = 0; i < 1000; ++i ) {
        SUBCASE( "" ) {
            const auto state = generator.state();
            INFO( "Initial generator state: ", state[ 0 ], ", ", state[ 1 ], ", ", state[ 2 ], ", ",
                  state[ 3 ] );
            test_insertion< std::hash< int > >( generator, 100, 1 );
        }
    }
} // TEST_CASE

TEST_CASE( "[EXPENSIVE] Insert 1'000'000 values twice with check every 1000 values " ) {
    xoshiro256ss generator;

    for ( int i = 0; i < 2; ++i ) {
        SUBCASE( "" ) {
            const auto state = generator.state();
            INFO( "Initial generator state: ", state[ 0 ], ", ", state[ 1 ], ", ", state[ 2 ], ", ",
                  state[ 3 ] );
            test_insertion< std::hash< int > >( generator, 1'000'000, 1'000 );
        }
    }
} // TEST_CASE

#endif // SET_INSERT_ONLY_TESTS_HPP
