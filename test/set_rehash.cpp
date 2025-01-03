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
        REQUIRE( set.insert( make_random_string( 100, generator ) ) );
    }
    REQUIRE( set.size() == 384 );
    REQUIRE( set.num_buckets() == 64 );

    REQUIRE( set.insert( make_random_string( 100, generator ) ) );
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

TEST_CASE( "[small] rehash while containing elements already" ) {
    Splitmix64 generator( 0xabcd87fe );
    bh_set< std::string > set;
    std::unordered_set< std::string > reference;
    set.set_max_load_factor( 0.75 );
    set.rehash( 100 );
    REQUIRE( set.capacity() == 192 );
    for ( int i = 0; i < 100; ++i ) {
        auto str = make_random_string( 100, generator );
        set.insert( str );
        reference.insert( std::move( str ) );
    }

    set.rehash( 200 );
    REQUIRE( compare_sets( set, reference ) );
    REQUIRE( set.capacity() == 384 );

    // This should be a no-op
    set.rehash( 10 );
    REQUIRE( set.capacity() == 384 );

    // Calling with 0 should unconditionally double capacity
    set.rehash( 0 );
    REQUIRE( set.capacity() == 768 );
} // TEST_CASE

TEST_CASE( "[small] change max load factor while containing elements" ) {
    Splitmix64 generator( 0x12345678 );
    bh_set< std::string > set;
    std::unordered_set< std::string > reference;
    set.set_max_load_factor( 0.95 );
    set.rehash( 100 );
    REQUIRE( set.capacity() == 121 );
    for ( int i = 0; i < 100; ++i ) {
        auto str = make_random_string( 100, generator );
        set.insert( str );
        reference.insert( std::move( str ) );
    }
    REQUIRE( compare_sets( set, reference ) );
    REQUIRE( set != bh_set< std::string >{} );

    // Reduce capacity
    set.set_max_load_factor( 0.5 );
    REQUIRE( set.capacity() == 64 );
    REQUIRE( set.size() == 100 );

    // Next insertion should trigger rehash
    set.insert( make_random_string( 100, generator ) );
    REQUIRE( set.capacity() == 128 );
    REQUIRE( set.size() == 101 );

    // Increase capacity
    set.set_max_load_factor( 0.75 );
    REQUIRE( set.capacity() == 192 );
    for ( int i = 0; i < 91; ++i ) {
        set.insert( make_random_string( 100, generator ) );
    }
    REQUIRE( set.size() == 192 );
    REQUIRE( set.capacity() == 192 );
    // Reduce capacity again and then rehash for new number of elements.
    set.set_max_load_factor( 0.5 );
    auto copy = set;
    set.rehash( 192 );
    REQUIRE( set.capacity() == 256 );
    REQUIRE( set.num_buckets() == 512 / 8 );
    REQUIRE( set == copy );
}
