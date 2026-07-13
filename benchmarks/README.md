# ThreadPool benchmark

Build and run the release-mode benchmark:

```bash
lazbuild --build-mode=Release benchmarks/ThreadPoolBenchmark.lpi
./benchmarks/ThreadPoolBenchmark
```

The same source compiles against v0.7.0 and v0.8.0. It reports:

- completion time for a 20,000-task burst through each pool;
- average queue-to-start latency after workers have been idle.

Run each version several times on the same otherwise-idle machine and compare
medians. Debug logging must be disabled in both versions so console I/O is not
included in the scheduler measurement.
