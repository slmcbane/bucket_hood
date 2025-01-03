#include "use_debug_bucket.hpp"

#include <format>
#include <iostream>
#include <iterator>
#include <stack>

using slm::None;
using slm::Optional;
using slm::SomeRef;

class CollatzSequence {
  private:
    long m_head;
    long m_len;
    Optional< const CollatzSequence& > m_tail;

  public:
    constexpr CollatzSequence( long head ) : m_head{ head }, m_len{ 1 }, m_tail{ None } {}
    CollatzSequence( long head, const CollatzSequence& tail )
        : m_head{ head }, m_len{ 1 + tail.m_len }, m_tail{ SomeRef( tail ) } {}

    long size() const { return m_len; }

    void print() const {
        using std::swap;
        std::ostreambuf_iterator it( std::cout );
        Optional< const CollatzSequence& > cur = SomeRef( *this );
        Optional< const CollatzSequence& > next = m_tail;
        while ( next ) {
            std::format_to( it, "{} -> ", cur->m_head );
            swap( cur, next );
            next = cur->m_tail;
        }
        std::format_to( it, "{}\n", cur->m_head );
    }
};

using bh_map = bucket_hood::unordered_map< long, std::unique_ptr< CollatzSequence >, std::hash< long >,
                                           std::equal_to<>, DebugAllocator< long > >;

static const CollatzSequence& collatz( long n, bh_map& cache ) {
    static constexpr CollatzSequence one{ 1 };
    std::stack< long > stack;
    stack.push( n );
    Optional< const CollatzSequence& > next_seq = None;
    do {
        n = stack.top();
        if ( next_seq ) {
            stack.pop();
            next_seq = *cache.find_or_insert( n, std::make_unique< CollatzSequence >( n, *next_seq ) ).value;
        } else if ( n == 1 ) {
            stack.pop();
            next_seq = one;
        } else {
            long next = n % 2 == 0 ? n / 2 : 3 * n + 1;
            auto maybe_next = cache.find( next );
            if ( maybe_next ) {
                stack.pop();
                next_seq =
                    *cache.find_or_insert( n, std::make_unique< CollatzSequence >( n, **maybe_next ) ).value;
            } else {
                stack.push( next );
            }
        }
    } while ( !stack.empty() );
    return *next_seq;
}

int main() {
    bh_map cache;
    long max_len = 1;
    for ( int i = 10; i < 1'000'000; ++i ) {
        const auto& seq = collatz( i, cache );
        if ( seq.size() > max_len ) {
            std::format_to( std::ostreambuf_iterator( std::cout ), "{}: {}\n", i, seq.size() );
            max_len = seq.size();
        }
    }
    std::cout << "cache.size() = " << cache.size() << '\n';
    assert( max_len == 525 );
}
