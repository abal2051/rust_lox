perf stat -B -e cache-references:u,cache-misses:u,cycles,instructions:u,branches,faults,migrations cargo run test_file2
