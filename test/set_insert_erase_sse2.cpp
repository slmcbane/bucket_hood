#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define BUCKET_HOOD_BUCKET_OVERRIDE SSE2Bucket
#include "doctest.h"

#include "test_utils.hpp"

#include <random>
#include <unordered_set>

TEST_CASE( "[small] simple deterministic test" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    std::unordered_set< int > ref_set;

    for ( int i = 0; i < 100; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );

        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    for ( int i = 0; i < 50; ++i ) {
        my_set.erase( i );
        ref_set.erase( i );

        REQUIRE( compare_sets( my_set, ref_set ) );
    }
}

TEST_CASE( "[small] test that includes erasing values not in the map" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    std::unordered_set< int > ref_set;

    // Insert values
    for ( int i = 0; i < 10; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );
    }

    // Erase values that were inserted
    for ( int i = 0; i < 5; ++i ) {
        REQUIRE( my_set.erase( i ) );
        ref_set.erase( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Erase values that were not inserted
    for ( int i = 10; i < 15; ++i ) {
        REQUIRE_FALSE( my_set.erase( i ) );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Insert more values
    for ( int i = 10; i < 15; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Erase all values
    for ( int i = 5; i < 15; ++i ) {
        REQUIRE( my_set.erase( i ) );
        ref_set.erase( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }

    // Check that the set is empty
    REQUIRE( my_set.empty() );
    REQUIRE( my_set.size() == 0 );
}

TEST_CASE( "[small] test with find_iterator for erasing values" ) {
    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    std::unordered_set< int > ref_set;

    // Insert values
    for ( int i = 0; i < 10; ++i ) {
        my_set.insert( i );
        ref_set.insert( i );
    }

    // Erase values that were inserted
    for ( int i = 0; i < 5; ++i ) {
        auto it = my_set.find_iterator( i );
        REQUIRE( it != my_set.end() );
        REQUIRE( *it == i );
        it = my_set.erase( it );
        if ( it != my_set.end() ) {
            REQUIRE( ref_set.count( *it ) != 0 );
        }
        ref_set.erase( i );
        REQUIRE( compare_sets( my_set, ref_set ) );
    }
}

void test_insert_erase( Splitmix64& generator, int count, float insert_probability, int min, int max,
                        int granularity = 1 ) {
    assert( count > 0 );
    assert( min < max );

    bucket_hood::unordered_set< int, BadHash< int >, std::equal_to<>, DebugAllocator< int > > my_set;
    my_set.set_max_load_factor( 0.99 );
    std::unordered_set< int > ref_set;

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

TEST_CASE( "[medium] 1000 random 3 insert to 1 erase from [0, 200]" ) {
    Splitmix64 generator( 0xabababababababab );
    test_insert_erase( generator, 1000, 0.75, 0, 200 );
}

TEST_CASE( "[medium] 100000 random 3 insert to 1 erase from [0, 500] (many collisions)" ) {
    Splitmix64 generator( 0xdeadbeef );
    test_insert_erase( generator, 100000, 0.75, 0, 500, 100 );
}

TEST_CASE( "[large] 100000 random 3 insert to 1 erase from [0, 5000] (not too many collisions)" ) {
    Splitmix64 generator( 0xeeeeeeeeeeeeeeee );
    test_insert_erase( generator, 100000, 0.75, 0, 5000, 100 );
}

TEST_CASE( "[huge] 1'000'000 random 11 insert to 10 erase from [0, 10000] with check every 1000" ) {
    Splitmix64 generator( 0x123456789abcdef );
    test_insert_erase( generator, 1'000'000, 0.55, 0, 10'000, 1'000 );
}

TEST_CASE( "[small][insert-erase] check resource management" ) {
    REQUIRE( AllocatorCounters::allocated );
    REQUIRE( AllocatorCounters::allocated == AllocatorCounters::deallocated );
    REQUIRE( AllocatorCounters::constructed );
    // This one should fail because of trivial destructibility
    // REQUIRE( AllocatorCounters::constructed == AllocatorCounters::destroyed );
}
