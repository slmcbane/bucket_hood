#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define BUCKET_HOOD_BUCKET_OVERRIDE DebugBucket
#include "doctest.h"

#include "test_utils.hpp"
#include <random>
#include <string>
#include <unordered_set>

template < class T >
using debug_set = bucket_hood::unordered_set< T, std::hash< T >, std::equal_to<>, DebugAllocator< T > >;

std::string make_random_string( int len, Splitmix64& generator ) {
    std::uniform_int_distribution< char > dist( 'a', 'z' );
    std::string result;
    result.reserve( len );
    for ( int i = 0; i < len; ++i ) {
        result.push_back( dist( generator ) );
    }
    return result;
}

template < class T >
debug_set< T > make_random_set( int count, Splitmix64& generator ) {
    debug_set< T > result;
    for ( int i = 0; i < count; ++i ) {
        result.insert( make_random_string( 5, generator ) );
    }
    return result;
}

TEST_CASE( "[small] test constructors for a standard type" ) {
    Splitmix64 generator( 0x1234567 );
    debug_set< std::string > my_set;
    auto set2 = make_random_set< std::string >( 100, generator );
    my_set = set2;
    auto set3 = set2;
    REQUIRE( my_set == set2 );
    my_set = std::move( set2 );
    REQUIRE( my_set == set3 );
    REQUIRE( set2.empty() );
    set2 = std::move( set3 );
    REQUIRE( my_set == set2 );
}
