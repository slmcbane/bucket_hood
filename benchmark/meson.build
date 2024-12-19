my_insert_access = executable('my_insert_access', ['my_insert_access.cpp', 'nanobench.cpp'])
std_insert_access = executable('std_insert_access', ['std_insert_access.cpp', 'nanobench.cpp'])

boost_path = get_option('boost_path')
if boost_path != ''
  boost_insert_access = executable('boost_insert_access',
    ['boost_insert_access.cpp', 'nanobench.cpp'],
    include_directories: include_directories(boost_path))
endif

