# WebGear Benchmarking
Some benchmarks comparing webgear against other Haskell web frameworks.

## Users benchmark

### Criterion
This benchmark runs a sequence of PUT, GET, and DELETE operations with criterion. This can be run with the following
commands:

```
stack build
stack exec bench-users -- --time-limit 15
```

Results are available [here](https://rkaippully.github.io/webgear/static/bench-criterion-users.html).

### ApacheBench
This benchmark runs a sequence of 50000 PUT operations with ApacheBench. This can be run with the following commands:

```
stack build

stack build; stack exec bench-users -- <library>   # library is one of webgear, servant, scotty
ab -k -c 3 -n 50000 -T application/json -u user.json http://localhost:3000/v1/users/1
```

Results are available [here](results/bench-ab-users.txt)

## Test environment
These benchmarks were run on a Thinkpad T450, Intel Core i5-5300U (2 core, 4 threads), 8 GB RAM running MX Linux 19.2
(Debian 10).
