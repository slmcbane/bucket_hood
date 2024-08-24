#ifndef SET_INSERT_ERASE_HPP
#define SET_INSERT_ERASE_HPP

#include "doctest.h"
#include "test_utils.hpp"
#include "unordered_dense.h"
#include <random>

#include "set_insert_only_tests.hpp"

template < class Hash >
void test_insert_erase( xoshiro256ss& generator, int count, float insert_probability, int min, int max,
                        int granularity = 1 ) {
    assert( count > 0 );
    assert( min < max );

    bhset< int, Hash > my_set;
    udset< int > ref_set;

    std::uniform_real_distribution< float > insert_draw( 0, 1 );
    std::uniform_int_distribution< int > value_dist( min, max );

    for ( int i = 0; i < count; ++i ) {
        int value = value_dist( generator );
        bool insert = insert_draw( generator ) < insert_probability;

        if ( insert ) {
            bool inserted = ref_set.insert( value ).second;
            REQUIRE_MESSAGE( inserted == static_cast< bool >( my_set.insert( value ) ),
                             "Insertion test failed for x = ", value );
        } else {
            bool erased = ref_set.erase( value ) == 1;
            REQUIRE_MESSAGE( erased == my_set.erase( value ), "Erasure test failed for x = ", value );
        }

        if ( ( i + 1 ) % granularity == 0 ) {
            for ( int x : ref_set ) {
                REQUIRE_MESSAGE( my_set.contains( x ), "Failed finding ", x, " in my set" );
            }

            for ( int x : my_set ) {
                REQUIRE_MESSAGE( ref_set.find( x ) != ref_set.end(), "Failed finding ", x,
                                 " in reference set" );
            }
        }
    }
}

TEST_CASE( "[INSERT/ERASE] [TRIVIAL] { 1, 2, 3, 4 } \\ { 2, 3 }" ) {
    bucket_hood::unordered_set< int > set;
    REQUIRE( set.insert( 1 ) );
    REQUIRE( set.insert( 2 ) );
    REQUIRE( set.insert( 3 ) );
    REQUIRE( set.insert( 4 ) );

    REQUIRE( set.erase( 2 ) );
    REQUIRE( set.find_iterator( 1 ) != set.end() );
    REQUIRE( set.find_iterator( 2 ) == set.end() );
    REQUIRE( set.find_iterator( 3 ) != set.end() );
    REQUIRE( set.find_iterator( 4 ) != set.end() );
    REQUIRE( set.contains( 1 ) );
    REQUIRE( !set.contains( 2 ) );
    REQUIRE( set.contains( 3 ) );
    REQUIRE( set.contains( 4 ) );

    REQUIRE( set.erase( 3 ) );
    REQUIRE( set.find_iterator( 1 ) != set.end() );
    REQUIRE( set.find_iterator( 2 ) == set.end() );
    REQUIRE( set.find_iterator( 3 ) == set.end() );
    REQUIRE( set.find_iterator( 4 ) != set.end() );
    REQUIRE( set.contains( 1 ) );
    REQUIRE( !set.contains( 2 ) );
    REQUIRE( !set.contains( 3 ) );
    REQUIRE( set.contains( 4 ) );
} // TEST_CASE

TEST_CASE( "[INSERT/ERASE] [TRIVIAL] Very small case w iterators" ) {
    bucket_hood::unordered_set< int > set;
    REQUIRE( set.insert( 1 ) );
    REQUIRE( set.insert( 2 ) );
    REQUIRE( set.insert( 3 ) );
    REQUIRE( set.insert( 4 ) );

    auto it = set.find_iterator( 2 );
    it = set.erase( it );
    REQUIRE( !set.contains( 2 ) );
    std::vector< int > full_set( set.begin(), set.end() );
    std::vector< int > full_set2( set.begin(), it );
    full_set2.insert( full_set2.end(), it, set.end() );
    REQUIRE( full_set == full_set2 );

    it = set.find_iterator( 3 );
    REQUIRE( it != set.end() );
    it = set.erase( it );
    REQUIRE( !set.contains( 3 ) );
    REQUIRE( set.contains( 1 ) );
    REQUIRE( set.contains( 4 ) );
    full_set = std::vector< int >( set.begin(), set.end() );
    full_set2 = std::vector< int >( set.begin(), it );
    full_set2.insert( full_set2.end(), it, set.end() );
    REQUIRE( full_set == full_set2 );
} // TEST_CASE

TEST_CASE(
    "[INSERT/ERASE] [MEDIUM] 1000 random 3 insert to 1 erase from [0, 200] with trivial hash function" ) {
    xoshiro256ss generator;
    auto state = generator.state();

    INFO( "Initial generator state: ", state[ 0 ], ", ", state[ 1 ], ", ", state[ 2 ], ", ", state[ 3 ] );

    test_insert_erase< NoHash >( generator, 1000, 0.75, 0, 200 );
} // TEST_CASE

TEST_CASE( "[INSERT/ERASE] [MEDIUM] 1000 random 3 insert to 1 erase from [0, 200] with real hash function" ) {
    xoshiro256ss generator;
    auto state = generator.state();

    INFO( "Initial generator state: ", state[ 0 ], ", ", state[ 1 ], ", ", state[ 2 ], ", ", state[ 3 ] );

    test_insert_erase< std::hash< int > >( generator, 1000, 0.75, 0, 200 );
} // TEST_CASE

TEST_CASE( "[INSERT/ERASE] [LARGE] 100000 random 3 insert to 1 erase from [0, 500] (many collisions) with "
           "trivial hash function and check every 100" ) {
    xoshiro256ss generator;
    auto state = generator.state();
    INFO( "Initial generator state: ", state[ 0 ], ", ", state[ 1 ], ", ", state[ 2 ], ", ", state[ 3 ] );
    test_insert_erase< NoHash >( generator, 100000, 0.75, 0, 500 );
} // TEST_CASE

TEST_CASE(
    "[INSERT/ERASE] [LARGE] 100000 random 3 insert to 1 erase from [0, 5000] (not too many collisions) with "
    "real hash function and check every 100" ) {
    xoshiro256ss generator;
    auto state = generator.state();
    INFO( "Initial generator state: ", state[ 0 ], ", ", state[ 1 ], ", ", state[ 2 ], ", ", state[ 3 ] );
    test_insert_erase< std::hash< int > >( generator, 100000, 0.75, 0, 500 );
} // TEST_CASE
  //

TEST_CASE( "[INSERT/ERASE] [HUGE] 100'000'000 random 11 insert to 10 erase from [0, 1000000] with check every "
           "10000" ) {
    xoshiro256ss generator;
    auto state = generator.state();
    INFO( "Initial generator state: ", state[ 0 ], ", ", state[ 1 ], ", ", state[ 2 ], ", ", state[ 3 ] );
    test_insert_erase< std::hash< int > >( generator, 100000000, 0.55, 0, 1000000, 10000 );
}

#endif // SET_INSERT_ERASE_HPP
