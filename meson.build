project('bucket_hood', ['cpp'])

bucket_hood_dep = declare_dependency(include_directories('.'))

if not meson.is_subproject()
    subdir('test')
    # subdir('benchmark')
endif
