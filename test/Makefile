CXXFLAGS += -Wall -Wextra -pedantic -march=native -std=c++17

insert_access: benchmark_insert_access.o nanobench.o
	$(CXX) benchmark_insert_access.o nanobench.o -o insert_access

insert_erase: benchmark_insert_erase.o nanobench.o
	$(CXX) benchmark_insert_erase.o nanobench.o -o insert_erase

insertion: benchmark_insertion.o nanobench.o
	$(CXX) benchmark_insertion.o nanobench.o -o insertion

test: runner.o
	$(CXX) $(CXXFLAGS) runner.cpp -o test

benchmarks: insert_access insert_erase insertion 

clean:
	rm -f *.o insert_access insert_erase insertion test

.PHONY: clean benchmarks
