#include "../bucket_hood/unordered_set.hpp"
#include "nanobench.h"
#include "robin_hood.h"
#include "test_utils.hpp"
#include "unordered_dense.h"

using namespace ankerl;

template < typename Set >
void bench( nanobench::Bench& bench, const char* name, int count ) {
    bench.run( name, [ = ]() {
        Set set;
        xoshiro256ss rng;
        auto [ a, b, c, d ] = rng.state();
        typename Set::value_type result = 0;
        for ( int i = 0; i < count; ++i ) {
            set.insert( rng() );
            for ( auto x : set ) {
                result += x;
            }
        }

        rng.set_state( a, b, c, d );
        for ( int i = 0; i < count; ++i ) {
            set.erase( rng() );
            for ( auto x : set ) {
                result += x;
            }
        }
        return result;
    } );
}

int main( int argc, char* argv[] ) {
    if ( argc != 2 ) {
        printf( "Expected 1 argument - count\n" );
        exit( 1 );
    }

    int count = atoi( argv[ 1 ] );

    nanobench::Bench b;
    b.title( "Test iterating a hash set" ).relative( true ).performanceCounters( true ).warmup( 1 );
    b.minEpochIterations( 100 );
    bench< bucket_hood::unordered_set< uint64_t > >( b, "bucket_hood", count );
    bench< unordered_dense::set< uint64_t > >( b, "ankerl", count );
    bench< robin_hood::unordered_flat_set< uint64_t > >( b, "robin_hood", count );
}

