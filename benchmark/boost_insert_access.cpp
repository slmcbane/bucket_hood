#include "nanobench.hpp"
#include "xorshiftstar.hpp"

#include <iostream>

#include <boost/unordered/unordered_flat_map.hpp>

static uint64_t insert_access_benchmark( Xorshift64Star& gen, int max_rng ) {
    boost::unordered_flat_map< int, int > map;
    map.rehash( max_rng );
    std::uniform_int_distribution< int > dist( 0, max_rng );
    uint64_t checksum = 0;

    for ( int i = 0; i < 50'000'000; ++i ) {
        checksum += ++map[ dist( gen ) ];
    }

    return checksum;
}

int main() {
    Xorshift64Star generator( 0xd00babadabadeeull );
    using namespace ankerl::nanobench;
    Bench().performanceCounters( true ).run( "bucket_hood insert_access max_rng = 250k", [ & ] {
        uint64_t checksum = insert_access_benchmark( generator, 250'000 );
        std::cout << "max_rng 250k checksum: " << checksum << '\n';
    } );
}
