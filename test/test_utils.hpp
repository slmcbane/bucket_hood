#ifndef BUCKET_HOOD_TEST_UTILS_HPP
#define BUCKET_HOOD_TEST_UTILS_HPP

#include <cstdint>
#include <random>

inline uint64_t rotl( uint64_t x, int k ) { return ( x << k ) | ( x >> ( 64 - k ) ); }

struct xoshiro256ss {
    typedef uint64_t result_type;
    static constexpr auto min() { return std::numeric_limits< uint64_t >::min(); }
    static constexpr auto max() { return std::numeric_limits< uint64_t >::max(); }

    xoshiro256ss() {
        static_assert( sizeof( uint64_t ) == 2 * sizeof( unsigned ) ||
                       sizeof( uint64_t ) == sizeof( unsigned ) );
        std::random_device rd;
        for ( int i = 0; i < 4; ++i ) {
            m_state[ i ] = rd();
            if constexpr ( sizeof( uint64_t ) > sizeof( unsigned ) ) {
                m_state[ i ] += uint64_t( rd() ) << 32;
            }
        }
    }

    void set_state( uint64_t a, uint64_t b, uint64_t c, uint64_t d ) {
        m_state[ 0 ] = a;
        m_state[ 1 ] = b;
        m_state[ 2 ] = c;
        m_state[ 3 ] = d;
    }

    uint64_t operator()() {
        uint64_t result = rotl( m_state[ 1 ] * 5, 7 ) * 9;
        uint64_t t = m_state[ 1 ] << 17;

        m_state[ 2 ] ^= m_state[ 0 ];
        m_state[ 3 ] ^= m_state[ 1 ];
        m_state[ 1 ] ^= m_state[ 2 ];
        m_state[ 0 ] ^= m_state[ 3 ];
        m_state[ 2 ] ^= t;
        m_state[ 3 ] = rotl( m_state[ 3 ], 45 );

        return result;
    }

    using state_type = uint64_t[ 4 ];
    const state_type& state() const { return m_state; }

  private:
    uint64_t m_state[ 4 ];
};

struct NoHash {
    template < class T >
    uint32_t operator()( const T& ) const noexcept {
        return -1;
    }
};

#endif // BUCKET_HOOD_TEST_UTILS_HPP

