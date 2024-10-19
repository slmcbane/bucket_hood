This repository contains a proof of concept implementation of a hash collision resolution
method devised by me that I have not found described elsewhere. It is an adaptation of the
well-known Robin Hood algorithm for hash tables to allow use of SIMD instructions to probe
multiple slots at once. I describe the algorithm further at the bottom of this file.

**STATUS** Updated 10/18/2024: Writing a last few tests using the unordered_set. I am pretty
confident that fuzz testing has covered the core algorithm thoroughly and that
test/set_resource_management.cpp has covered most of the rest of what's important. Once
I'm satisfied with the last few tests for unordered_set, I want to mock up an interface for
unordered_map (this will be subject to change, since I'm not going with the std interface)
and write a couple of test for that, including some interesting ones and a couple of benchmarks.
Once I have a couple of benchmarks available I will start actual use of SIMD.

The implementation is in the single header `bucket_hood.hpp`. It requires a compiler
supporting the C++20 standard. This header also contains inline my implementation of an
Optional type that supports optional references as well as the monadic operations added for
`std::optional` in C++23. This is added to serve as a return type for the `find` function;
I have modified the interface of my containers to differ from the standard library unordered
containers, mostly in line with Barry Revzin's article
[What's the right hash table API?](https://brevzin.github.io/c++/2023/05/23/map-api/).
The modified interface is documented below.

`bucket_hood::unordered_map` and `bucket_hood::unordered_set` should satisfy the requirements
of _AllocatorAwareContainer_. They support transparent hashing (e.g. for using `std::string_view`
to look up elements in a container keyed on `std::string`) by specializing
`bucket_hood::is_transparent_hash` for the type. If this is specialized, comparison is assumed
to be transparent.

The containers should not be treated as exception safe at this time; this has not been a design
goal in the proof of concept implementation.

Code under the 'bucket_hood' directory is unused currently and will be removed; it is preserved
only for reference at the moment.

### Interface
The size type for the containers is `uint32_t` by default. Define `BUCKET_HOOD_64_BIT_SIZE` to
override this default.

### License
The code in bucket_hood.hpp is Copyright 2024 Sean McBane under the terms of the MIT license,
which may be found in LICENSE.txt.

Code in the test directory is released into the public domain using the CC0 waiver, **except**
for doctest.h, which is Copyright 2016-2023 Viktor Kirilov, and included here as permitted by
its MIT license terms.

### Algorithm
TODO
