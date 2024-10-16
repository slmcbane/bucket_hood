set_insert_only = executable('set_insert_only', 'set_insert_only_tests.cpp')
test('Insert only tests for set', set_insert_only, timeout: 90)

set_insert_erase = executable('set_insert_erase', 'set_insert_erase.cpp')
test('Insert and erase tests for set', set_insert_erase, timeout: 90)

set_resource_management = executable('set_resource_management', 'set_resource_management.cpp')
test('Resource management tests for set', set_resource_management)

set_fuzz = executable('set_fuzz', ['set_fuzz.cpp', 'set_fuzz_driver.cpp'])
test('Fuzz tests for set with DebugBucket', set_fuzz)

set_rehash = executable('set_rehash', 'set_rehash.cpp')
test('Rehashing tests for set', set_rehash)
