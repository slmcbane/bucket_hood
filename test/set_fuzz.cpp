#include "use_debug_bucket.hpp"

#include <cstring>

static bh_set< long, BadHash< long > > set;
static bh_set< long > ref_set;

static_assert( sizeof( unsigned ) == 4 );

bool test_set( unsigned char* buf, int len ) {
    set.clear();
    set.set_max_load_factor( 0.995 );
    set.rehash( 10'000 );
    ref_set.clear();
    ref_set.set_max_load_factor( 0.995 );
    ref_set.rehash( 10'000 );
    // 10% chance we reset the size of our set (so rehashing is covered)
    if ( len < 8 ) {
        return true;
    }

    unsigned char local_buf[ 8 ];
    while ( len >= 8 ) {
        memcpy( local_buf, buf, 8 );
        buf += 8;
        len -= 8;
        // 10% chance to erase instead of insert
        constexpr unsigned char threshold = 0.9 * std::numeric_limits< unsigned char >::max();
        bool erase = local_buf[ 7 ] > threshold;
        local_buf[ 7 ] = 0;
        long next = std::bit_cast< long >( local_buf );
        if ( erase ) {
            set.erase( next );
            ref_set.erase( next );
        } else {
            set.insert( next );
            ref_set.insert( next );
        }
    }

    return compare_sets( set, ref_set );
}
