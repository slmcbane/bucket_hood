#include "../bucket_hood/unordered_set.hpp"
#include "nanobench.h"
#include "robin_hood.h"
#include "test_utils.hpp"
#include "unordered_dense.h"

#include <boost/unordered/unordered_flat_set.hpp>

#include <iostream>
#include <random>

using namespace ankerl;

constexpr static int count = 50'000'000;

template < typename Set >
int bench( nanobench::Bench& bench, const char* name, xoshiro256ss& rng, int max ) {
    Set set;
    std::uniform_int_distribution< int > dist( 0, max );
    int checksum = 0;
    bench.run( name, [ & ]() {
        for ( int i = 0; i < count; ++i ) {
            auto [ it, inserted ] = set.insert( dist( rng ) );
            if ( inserted ) {
                checksum += *it;
            } else {
                checksum -= *it;
            }
        }
    } );
    return checksum;
}

int main( int argc, char* argv[] ) {
    assert( argc == 2 && "Need max int range specified" );
    int max = atoi( argv[ 1 ] );
    assert( max > 0 );
    xoshiro256ss rng;
    const auto [ a, b, c, d ] = rng.state();
    nanobench::Bench benchmarker;
    benchmarker.title( "Test inserting only into a hash set" )
        .relative( true )
        .performanceCounters( true )
        .warmup( 1 );

    int checksum = bench< bucket_hood::unordered_set< int > >( benchmarker, "bucket_hood", rng, max );
    std::cout << "checksum = " << checksum << '\n';
    rng.set_state( a, b, c, d );
    checksum = bench< unordered_dense::set< int > >( benchmarker, "ankerl", rng, max );
    std::cout << "checksum = " << checksum << '\n';
    rng.set_state( a, b, c, d );
    checksum = bench< robin_hood::unordered_flat_set< int > >( benchmarker, "robin_hood", rng, max );
    std::cout << "checksum = " << checksum << '\n';
    checksum = bench< boost::unordered::unordered_flat_set< int > >( benchmarker, "boost", rng, max );
}

