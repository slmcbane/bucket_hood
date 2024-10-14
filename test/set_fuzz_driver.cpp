#include <cstdlib>
#include <unistd.h>

extern bool test_set( unsigned char* buf, int len );

#ifndef __AFL_FUZZ_TESTCASE_LEN

#include <cassert>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
#include <vector>

std::vector< unsigned char > slurp_file( const std::string& path ) {
    std::ifstream file( path, std::ios::binary );
    assert( file );
    file.ignore( std::numeric_limits< std::streamsize >::max() );
    std::streamsize length = file.gcount();
    file.clear(); //  Since ignore will have set eof.
    file.seekg( 0, std::ios_base::beg );
    std::vector< unsigned char > buffer( length );
    if ( !file.read( reinterpret_cast< char* >( buffer.data() ), length ) ) {
        std::cerr << "Failed to read file: " << path << std::endl;
        std::abort();
    }
    return buffer;
}

int main() {
    for ( const auto& entry : std::filesystem::directory_iterator( "set_fuzz_corpus_debug_bucket" ) ) {
        std::string path = entry.path();
        std::vector< unsigned char > buf = slurp_file( path );
        if ( !test_set( buf.data(), buf.size() ) ) {
            std::cerr << "Failed on: " << path << std::endl;
            return 1;
        }
    }
    return 0;
}

#else

__AFL_FUZZ_INIT();

#pragma clang optimize off

int main() {
    __AFL_INIT();
    unsigned char* buf = __AFL_FUZZ_TESTCASE_BUF;

    while ( __AFL_LOOP( 10000 ) ) {
        int len = __AFL_FUZZ_TESTCASE_LEN;
        if ( !test_set( buf, len ) ) {
            std::abort();
        }
    }

    return 0;
}

#endif
