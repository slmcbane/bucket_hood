#include "../test/use_debug_bucket.hpp"

#include "../bucket_hood.hpp"
#include "nanobench.hpp"
#include "xorshiftstar.hpp"

#include <iostream>

static uint64_t insert_access_benchmark( Xorshift64Star& gen, int max_rng ) {
    bucket_hood::unordered_map< int, int > map;
    map.rehash( max_rng );
    std::uniform_int_distribution< int > dist( 0, max_rng );
    uint64_t checksum = 0;

    for ( int i = 0; i < 50'000'000; ++i ) {
        auto [ value_ref, _ ] = map.find_or_insert( dist(gen), 0 );
        checksum += ++value_ref;
    }

    return checksum;
}

int main() {
    Xorshift64Star generator( 0xd00babadabadeeull );
    using namespace ankerl::nanobench;
    Bench().run( "bucket_hood insert_access max_rng = 250k", [ & ] {
        uint64_t checksum = insert_access_benchmark( generator, 250'000 );
        std::cout << "max_rng 250k checksum: " << checksum << '\n';
    } );
}
