#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#define BUCKET_HOOD_BUCKET_OVERRIDE DebugBucket
#include "doctest.h"

#include "test_utils.hpp"
#include <random>
#include <string>
#include <unordered_set>

template < class T >
using debug_set = bucket_hood::unordered_set< T, std::hash< T >, std::equal_to<>, DebugAllocator< T > >;

class DebugString {
  public:
    operator std::string_view() const { return *m_string; }

    DebugString() : m_string( std::make_unique< std::string >() ) {}
    DebugString( const DebugString& other ) : m_string( std::make_unique< std::string >( *other.m_string ) ) {}
    DebugString( DebugString&& other ) noexcept : m_string( std::move( other.m_string ) ) {
        other.m_string = std::make_unique< std::string >();
    }
    DebugString& operator=( const DebugString& other ) {
        m_string = std::make_unique< std::string >( *other.m_string );
        return *this;
    }

    DebugString& operator=( DebugString&& other ) {
        m_string = std::move( other.m_string );
        other.m_string = std::make_unique< std::string >();
        return *this;
    }

    DebugString( const std::string& str ) : m_string( std::make_unique< std::string >( str ) ) {}

    bool operator==( const DebugString& other ) const { return *m_string == *other.m_string; }

  private:
    std::unique_ptr< std::string > m_string;
};

namespace std {
template <>
struct hash< DebugString > {
    size_t operator()( const DebugString& str ) const { return std::hash< std::string_view >{}( str ); }
};
} // namespace std

template <>
struct bucket_hood::is_transparent_hash< std::hash< DebugString >, std::string > : std::true_type {};

std::string make_random_string( int len, Splitmix64& generator ) {
    std::uniform_int_distribution< char > dist( 'a', 'z' );
    std::string result;
    result.reserve( len );
    for ( int i = 0; i < len; ++i ) {
        result.push_back( dist( generator ) );
    }
    return result;
}

static_assert( bucket_hood::is_transparent_hash< std::hash< DebugString >, std::string >{} );

template < class T >
debug_set< T > make_random_set( int count, Splitmix64& generator ) {
    debug_set< T > result;
    for ( int i = 0; i < count; ++i ) {
        result.insert( make_random_string( 5, generator ) );
    }
    return result;
}

TEST_CASE( "[small] test constructors for a standard type" ) {
    Splitmix64 generator( 0x1234567 );
    debug_set< DebugString > my_set;
    auto set2 = make_random_set< DebugString >( 100, generator );
    // Copy assign empty from non-empty.
    my_set = set2;
    // Copy construct
    auto set3 = set2;
    REQUIRE( my_set == set2 );
    // Move assign from non-empty with non-empty
    my_set = std::move( set2 );
    REQUIRE( my_set == set3 );
    REQUIRE( set2.empty() );
    // Move assign from non-empty with empty
    set2 = std::move( set3 );
    REQUIRE( my_set == set2 );
    REQUIRE( set3.empty() );
    // Move construct
    auto set4 = std::move( my_set );
    REQUIRE( my_set.empty() );
    REQUIRE( set4 == set2 );

    // Copy assign empty from empty.
    set4.clear();
    set4 = my_set;
    REQUIRE( set4.empty() );
    REQUIRE( set4 == my_set );
    // Move assign empty from empty.
    set4 = std::move( my_set );
    REQUIRE( set4.empty() );
    REQUIRE( set4 == my_set );

    // Copy construct from empty.
    auto set5 = my_set;
    REQUIRE( set5.empty() );
    REQUIRE( set5 == my_set );

    // Move construct from empty.
    auto set6 = std::move( my_set );
    REQUIRE( set6.empty() );
    REQUIRE( set6 == my_set );

    // Copy assign empty to non-empty
    set5 = set2;
    set2 = set6;
    REQUIRE( set2.empty() );
    REQUIRE( set2 == set6 );

    // Move assign empty to non-empty
    REQUIRE( !set5.empty() );
    set5 = std::move( set2 );
    REQUIRE( set5.empty() );
    REQUIRE( set5 == set2 );
}

TEST_CASE( "[small] Self-assignment" ) {
    Splitmix64 generator( 0x1234567 );
    debug_set< DebugString > my_set = make_random_set< DebugString >( 100, generator );
    auto set2 = my_set;
    my_set = my_set;
    REQUIRE( my_set == set2 );
    my_set = std::move( my_set );
    REQUIRE( my_set == set2 );

    my_set.clear();
    my_set = my_set;
    REQUIRE( my_set.empty() );
}
