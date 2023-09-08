#include "../bucket_hood/unordered_set.hpp"
#include "nanobench.h"
#include "robin_hood.h"
#include "test_utils.hpp"
#include "unordered_dense.h"

#include <iostream>
#include <random>

using namespace ankerl;

template < typename Set >
void bench( nanobench::Bench& bench, const char* name, int count ) {
    constexpr uint64_t bitmasks[ 6 ] = {
        0b1001000000000000000000000000000000000000000100000000000000001000ULL,
        0b1001000000000010001100000000000000000000000101000000000000001000ULL,
        0b1001000000000110001100000000000000010000000101100000000000001001ULL,
        0b1001000000000110001100000001000000010000000101110000000100101001ULL,
        0b1101100000000110001100001001000000010000000101110001000100101001ULL,
        0b1101100000001110001100001001001000010000100101110001000100101011ULL };

    bench.run( name, [ & ]() {
        Set set;
        xoshiro256ss rng;

        for ( int i = 0; i < 6; ++i ) {
            uint64_t mask = bitmasks[ i ];
            for ( int j = 0; j < count; ++j ) {
                set.insert( rng() & mask );
                set.erase( rng() & mask );
            }
        }
    } );
}

size_t AllocatorCounters::allocated = 0;
size_t AllocatorCounters::deallocated = 0;
size_t AllocatorCounters::peak = 0;

int main( int argc, char* argv[] ) {
    assert( argc == 2 && "Need 1 argument - count" );
    int count = atoi( argv[ 1 ] );
    nanobench::Bench b;
    b.title( "Test random insert and erase" ).relative( true ).performanceCounters( true ).warmup( 1 );

    bench<
        unordered_dense::set< uint64_t, std::hash< uint64_t >, std::equal_to<>, DebugAllocator< uint64_t > > >(
        b, "ankerl", count );
    std::cout << "unordered_dense allocated " << AllocatorCounters::allocated << " B\n";
    std::cout << "peak memory usage " << AllocatorCounters::peak << " B\n";
    AllocatorCounters::reset();
    bench< bucket_hood::unordered_set< uint64_t, std::hash< uint64_t >, std::equal_to<>,
                                       DebugAllocator< uint64_t > > >( b, "bucket_hood", count );
    assert( AllocatorCounters::allocated == AllocatorCounters::deallocated );
    std::cout << "bucket_hood allocated " << AllocatorCounters::allocated << " B\n";
    std::cout << "peak memory usage " << AllocatorCounters::peak << " B\n";
    AllocatorCounters::reset();
    bench< robin_hood::unordered_flat_set< uint64_t, std::hash< uint64_t >, std::equal_to<> > >(
        b, "robin_hood", count );
}

