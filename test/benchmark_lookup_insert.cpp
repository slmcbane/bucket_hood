#include "../bucket_hood/unordered_set.hpp"
#include "nanobench.h"
#include "robin_hood.h"
#include "test_utils.hpp"
#include "unordered_dense.h"
#include <random>

using namespace ankerl;

#include <boost/unordered/unordered_flat_set.hpp>

template < typename Set >
void bench( nanobench::Bench& bench, const char* name, int insert_outer, int lookups_per_insert,
            int success_out_of_4, uint64_t mask ) {
    bench.run( name, [ = ]() {
        Set set;
        xoshiro256ss rng;
        xoshiro256ss rng_copy;
        xoshiro256ss shuf;
        auto [ a, b, c, d ] = rng.state();
        size_t lookups = 0;
        size_t lookups_succeeded = 0;
        for ( int i = 0; i < insert_outer; ++i ) {
            // Prepare random values to insert.
            uint64_t values_to_insert[ 4 ];
            for ( auto& x : values_to_insert ) {
                x = shuf() & mask;
            }
            switch ( success_out_of_4 ) {
            case 4:
                values_to_insert[ 0 ] = rng() & mask;
                [[fallthrough]];
            case 3:
                values_to_insert[ 1 ] = rng() & mask;
                [[fallthrough]];
            case 2:
                values_to_insert[ 2 ] = rng() & mask;
                [[fallthrough]];
            case 1:
                values_to_insert[ 3 ] = rng() & mask;
                [[fallthrough]];
            default:;
            }
            std::shuffle( std::begin( values_to_insert ), std::end( values_to_insert ), shuf );

            // Do insertion
            for ( auto x : values_to_insert ) {
                set.insert( x );
            }

            // Do lookups. Track successful lookup percentage.
            const size_t num_in_map = ( i + 1 ) * 4;
            const size_t iters = lookups_per_insert / num_in_map;
            for ( size_t j = 0; j < iters; ++j ) {
                rng_copy.set_state( a, b, c, d );
                for ( size_t k = 0; k < num_in_map; ++k ) {
                    uint64_t key = rng_copy() & mask;
                    lookups_succeeded += set.contains( key );
                }
                lookups += num_in_map;
            }
        }

        printf( "Successful lookup percentage: %f\n", 100 * (double)lookups_succeeded / lookups );
    } );
}

int main() {
    nanobench::Bench b;
    b.title( "Test lookups in a hash set" ).relative( true ).performanceCounters( true ).warmup( 1 );

    int lookups_per_insert = 5'000'000;
    int insert_outer = 50;
    int success_out_of_4 = 3;
    uint64_t mask = 0xffffffffffffffffUL;

    bench< bucket_hood::unordered_set< uint64_t > >( b, "bucket_hood", insert_outer, lookups_per_insert,
                                                     success_out_of_4, mask );
    bench< unordered_dense::set< uint64_t > >( b, "ankerl", insert_outer, lookups_per_insert, success_out_of_4,
                                               mask );
    bench< robin_hood::unordered_flat_set< uint64_t > >( b, "robin_hood", insert_outer, lookups_per_insert,
                                                         success_out_of_4, mask );
    bench< boost::unordered_flat_set< uint64_t > >( b, "boost", insert_outer, lookups_per_insert,
                                                    success_out_of_4, mask );
}

