#include <cstdint>
#include <limits>

class Xorshift64Star {
  public:
    typedef uint64_t result_type;
    Xorshift64Star( uint64_t seed ) : m_state{ seed } {}

    uint64_t operator()( void ) {
        m_state ^= m_state >> 12;
        m_state ^= m_state << 25;
        m_state ^= m_state >> 27;
        return m_state * 0x2545F4914F6CDD1DULL;
    }

    static constexpr uint64_t min() { return 1; }
    static constexpr uint64_t max() { return std::numeric_limits< uint64_t >::max(); }

  private:
    uint64_t m_state;
};
