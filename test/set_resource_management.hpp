#ifndef SET_RESOURCE_MANAGEMENT_HPP
#define SET_RESOURCE_MANAGEMENT_HPP

#include "../bucket_hood/unordered_set.hpp"
#include "doctest.h"
#include "test_utils.hpp"

TEST_CASE( "[RESOURCE] [TRIVIAL] A set with only a few elements" ) {
    CountConstructions::reset_counters();
    bucket_hood::unordered_set< CountConstructions > set;
    set.emplace( 1 );
    set.emplace( 2 );
    set.emplace( 3 );
    set = bucket_hood::unordered_set< CountConstructions >();

    REQUIRE( CountConstructions::value_constructed == 3 );
    REQUIRE( CountConstructions::destroyed == 3 );
} // TEST_CASE

#endif // SET_RESOURCE_MANAGEMENT_HPP

