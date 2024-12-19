project('bucket_hood', ['cpp'], default_options: ['cpp_std=c++20'])

bucket_hood_dep = declare_dependency(include_directories: include_directories('.'))

if not meson.is_subproject()
    subdir('test')
    subdir('benchmark')
endif
