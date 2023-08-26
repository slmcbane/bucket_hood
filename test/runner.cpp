#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "set_insert_erase.hpp"
#include "set_insert_only_tests.hpp"
#include "set_resource_management.hpp"

size_t CountConstructions::value_constructed = 0;
size_t CountConstructions::copy_constructed = 0;
size_t CountConstructions::move_constructed = 0;
size_t CountConstructions::destroyed = 0;
