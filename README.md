This repository contains a proof of concept implementation of a hash collision resolution
method devised by me that I have not found described elsewhere. It is an adaptation of the
well-known Robin Hood algorithm for hash tables to allow use of SIMD instructions to probe
multiple slots at once. I describe the algorithm further at the bottom of this file.

**STATUS** Updated 11/4/2024: Determining the tentative interface for my unordered_map. I am
not sticking to the same API as the standard library. My next item to do is write a couple of
benchmarks to see what I think I might be missing from the unordered_map API, and once I have
a couple of benchmarks I will implement a SIMD backend and do some profiling. Currently, it is
uncertain if performance of this collision resolution algorithm will actually be competitive
with state of the art open addressing containers.

The implementation is in the single header `bucket_hood.hpp`. It requires a compiler
supporting the C++20 standard. This header also contains inline my implementation of an
Optional type that supports optional references as well as the monadic operations added for
`std::optional` in C++23. This is added to serve as a return type for the `find` function;
I have modified the interface of my containers to differ from the standard library unordered
containers.

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
Filling out this section is a to-do item. The idea is that instead of probing buckets containing
0 or 1 items as in standard Robin Hood hashing, we probe buckets with N slots, each of which can be
filled or empty and tracks its own probe length. When inserting, a new element is placed in a bucket
if any slot is available or if the current probe length is longer than the maximum probe length in
the bucket currently being probed. If the latter, the element with the minimum probe length in a bucket
is displaced. Probing 16 or 32 slots at a time using SIMD hopefully enables the table to perform well
at quite high load factors. There are 2 bytes of overhead per slot - one byte for probe length, and the
other to store 7 bits of hash value plus a flag, for fast probing.
