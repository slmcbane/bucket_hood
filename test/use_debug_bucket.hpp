#define BUCKET_HOOD_BUCKET_OVERRIDE DebugBucket
#include "test_utils.hpp"

template < class... Args >
using bh_set = bucket_hood::unordered_set< Args... >;
