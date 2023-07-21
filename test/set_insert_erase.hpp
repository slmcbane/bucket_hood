#ifndef SET_INSERT_ERASE_HPP
#define SET_INSERT_ERASE_HPP

#include "../bucket_hood/unordered_set.hpp"
#include "doctest.h"

TEST_CASE( "[INSERT/ERASE] [TRIVIAL] { 1, 2, 3, 4 } \\ { 2, 3 }" ) {
    bucket_hood::unordered_set< int > set;
    REQUIRE( set.insert( 1 ).second );
    REQUIRE( set.insert( 2 ).second );
    REQUIRE( set.insert( 3 ).second );
    REQUIRE( set.insert( 4 ).second );

    REQUIRE( set.erase( 2 ) );
    REQUIRE( set.find( 1 ) != set.end() );
    REQUIRE( set.find( 2 ) == set.end() );
    REQUIRE( set.find( 3 ) != set.end() );
    REQUIRE( set.find( 4 ) != set.end() );
    REQUIRE( set.contains( 1 ) );
    REQUIRE( !set.contains( 2 ) );
    REQUIRE( set.contains( 3 ) );
    REQUIRE( set.contains( 4 ) );

    REQUIRE( set.erase( 3 ) );
    REQUIRE( set.find( 1 ) != set.end() );
    REQUIRE( set.find( 2 ) == set.end() );
    REQUIRE( set.find( 3 ) == set.end() );
    REQUIRE( set.find( 4 ) != set.end() );
    REQUIRE( set.contains( 1 ) );
    REQUIRE( !set.contains( 2 ) );
    REQUIRE( !set.contains( 3 ) );
    REQUIRE( set.contains( 4 ) );
} // TEST_CASE

#endif // SET_INSERT_ERASE_HPP

