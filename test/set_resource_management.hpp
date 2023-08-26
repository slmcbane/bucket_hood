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

    /*
     * CountConstructions does not have hash marked as transparent. Therefore, for each of the
     * entries inserted above, we did:
     *  - 1 value construction to have an object we could hash.
     *  - 1 move construction in the set's storage, moving from the object just constructed.
     *  - 2 destructors (the temporary object constructed to hash, and the one in set storage).
     */
    REQUIRE( CountConstructions::value_constructed == 3 );
    REQUIRE( CountConstructions::move_constructed == 3 );
    REQUIRE( CountConstructions::copy_constructed == 0 );
    REQUIRE( CountConstructions::destroyed == 6 );
} // TEST_CASE

#endif // SET_RESOURCE_MANAGEMENT_HPP

