#include "../bucket_hood/unordered_set.hpp"
#include "nanobench.h"
#include "robin_hood.h"
#include "test_utils.hpp"
#include "unordered_dense.h"
#include <random>

using namespace ankerl;

template < typename Set >
void bench( nanobench::Bench& bench, const char* name, int count,
            int lower_bound = std::numeric_limits< int >::min(),
            int upper_bound = std::numeric_limits< int >::max() ) {
    bench.run( name, [ = ]() {
        Set set;
        xoshiro256ss rng;
        std::uniform_int_distribution< int > dist( lower_bound, upper_bound );
        for ( int i = 0; i < count; ++i ) {
            set.insert( dist( rng ) );
        }
    } );
}

int main( int argc, char* argv[] ) {
    if ( argc != 2 && argc != 4 ) {
        std::fprintf( stderr, "Needs either 'count min max' or just 'count' as arguments\n" );
        return 1;
    }

    int count = atoi( argv[ 1 ] );

    nanobench::Bench b;
    b.title( "Test inserting only into a hash set" ).relative( true ).performanceCounters( true ).warmup( 1 );
    b.minEpochIterations( 1000 );

    if ( argc == 4 ) {
        int min = atoi( argv[ 2 ] );
        int max = atoi( argv[ 3 ] );
        bench< bucket_hood::unordered_set< int > >( b, "bucket_hood", count, min, max );
        bench< unordered_dense::set< int > >( b, "ankerl", count, min, max );
        bench< robin_hood::unordered_flat_set< int > >( b, "robin_hood", count, min, max );
    } else {
        bench< bucket_hood::unordered_set< int > >( b, "bucket_hood", count );
        bench< unordered_dense::set< int > >( b, "ankerl", count );
        bench< robin_hood::unordered_flat_set< int > >( b, "robin_hood", count );
    }
}
