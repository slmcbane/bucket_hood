#include "../bucket_hood/unordered_set.hpp"
#include "nanobench.h"
#include "robin_hood.h"
#include "test_utils.hpp"
#include "unordered_dense.h"

#include <iostream>
#include <random>

using namespace ankerl;

constexpr static int count = 100'000'000;

template < typename Set >
void bench( nanobench::Bench& bench, const char* name ) {
    std::array< size_t, 4 > elapsed_times = { 0, 0, 0, 0 };
    bench.run( name, [ & ]() {
        Set set;
        xoshiro256ss rng;
        const auto [ a, b, c, d ] = rng.state();

        Stopwatch stopwatch;
        for ( int i = 0; i < count; ++i ) {
            set.insert( rng() );
        }
        elapsed_times[ 0 ] = stopwatch.elapsed< std::chrono::nanoseconds >();
        stopwatch.reset();
        set.clear();
        elapsed_times[ 1 ] = stopwatch.elapsed< std::chrono::nanoseconds >();
        assert( set.size() == 0 );

        rng.set_state( a, b, c, d );
        stopwatch.reset();
        for ( int i = 0; i < count; ++i ) {
            set.insert( rng() );
        }
        elapsed_times[ 2 ] = stopwatch.elapsed< std::chrono::nanoseconds >();
        rng.set_state( a, b, c, d );
        stopwatch.reset();
        for ( int i = 0; i < count; ++i ) {
            set.erase( rng() );
        }
        elapsed_times[ 3 ] = stopwatch.elapsed< std::chrono::nanoseconds >();
        assert( set.size() == 0 );
    } );

    std::cout << name << " timing detail:\n"
              << "  First 100m insert: " << elapsed_times[ 0 ] << " ns\n"
              << "  Clear: " << elapsed_times[ 1 ] << " ns\n"
              << "  Second 100m insert: " << elapsed_times[ 2 ] << " ns\n"
              << "  100m erase: " << elapsed_times[ 3 ] << " ns\n";
}

size_t AllocatorCounters::allocated = 0;
size_t AllocatorCounters::deallocated = 0;
size_t AllocatorCounters::peak = 0;

int main() {
    nanobench::Bench b;
    b.title( "Test inserting only into a hash set" ).relative( true ).performanceCounters( true ).warmup( 1 );

    bench< unordered_dense::set< int, std::hash< int >, std::equal_to<>, DebugAllocator< int > > >( b,
                                                                                                    "ankerl" );
    std::cout << "unordered_dense allocated " << AllocatorCounters::allocated << " B\n";
    std::cout << "peak memory usage " << AllocatorCounters::peak << " B\n";
    AllocatorCounters::reset();
    bench< bucket_hood::unordered_set< int, std::hash< int >, std::equal_to<>, DebugAllocator< int > > >(
        b, "bucket_hood" );
    assert( AllocatorCounters::allocated == AllocatorCounters::deallocated );
    std::cout << "bucket_hood allocated " << AllocatorCounters::allocated << " B\n";
    std::cout << "peak memory usage " << AllocatorCounters::peak << " B\n";
    AllocatorCounters::reset();
    bench< robin_hood::unordered_flat_set< int, std::hash< int >, std::equal_to<> > >( b, "robin_hood" );
    std::cout << "robin_hood allocated " << AllocatorCounters::allocated << " B\n";
}

