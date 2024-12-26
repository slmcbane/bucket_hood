set_insert_only = executable('set_insert_only', 'set_insert_only_tests.cpp')
test('Insert only tests for set', set_insert_only, timeout: 90)

set_insert_only_sse2 = executable('set_insert_only_sse2', 'set_insert_only_tests_sse2.cpp')
test('Insert only tests for set (SSE2)', set_insert_only_sse2, timeout: 100)

set_insert_erase = executable('set_insert_erase', 'set_insert_erase.cpp')
test('Insert and erase tests for set', set_insert_erase, timeout: 90)

set_insert_erase_sse2 = executable('set_insert_erase_sse2', 'set_insert_erase_sse2.cpp')
test('Insert and erase tests for set (SSE2)', set_insert_erase_sse2, timeout: 90)

set_resource_management = executable('set_resource_management', 'set_resource_management.cpp')
test('Resource management tests for set', set_resource_management)

set_fuzz = executable('set_fuzz', ['set_fuzz.cpp', 'set_fuzz_driver.cpp'])
test('Fuzz tests for set with DebugBucket', set_fuzz)

set_rehash = executable('set_rehash', 'set_rehash.cpp')
test('Rehashing tests for set', set_rehash)

set_trivial_copy = executable('set_trivial_copy', 'set_trivial_copy.cpp')
test('Trivial copy tests for set', set_trivial_copy)

collatz = executable('collatz', 'collatz.cpp')
test('Collatz sequence test for map', collatz)
