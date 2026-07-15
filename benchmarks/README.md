# ThreadPool benchmark

Build and run the release-mode benchmark:

```bash
lazbuild --build-mode=Release benchmarks/ThreadPoolBenchmark.lpi
./benchmarks/ThreadPoolBenchmark
```

The v0.9.0 benchmark reports:

- completion time for a 20,000-task legacy `Queue` burst through each pool;
- completion time for an equivalent tracked `Submit` burst;
- individual tracked submission versus chunked `SubmitRange` for 200,000
  indexed calls;
- average queue-to-start latency after workers have been idle.

For comparisons with v0.7.0 and v0.8.x, use the legacy queue and idle fields;
the tracked and range APIs did not exist in those releases.

Run each version several times on the same otherwise-idle machine and compare
medians. Debug logging must be disabled in both versions so console I/O is not
included in the scheduler measurement.

Do not enforce absolute millisecond thresholds on shared CI runners. Compare
five-run medians on the same otherwise-idle machine. The v0.9.0 release budget
allows at most a 10% regression in the legacy queue medians and expects the
Simple chunked range case to be at least 5x faster than individual tracked
indexed submissions.
