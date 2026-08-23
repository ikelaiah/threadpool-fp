# ThreadPool for Free Pascal — v0.8.0

> 0.8.0 makes threadpool-fp event-driven, deterministic under shutdown and
> failure, and measurably faster under burst and idle workloads.

## Highlights

- Both pools now have the same monotonic lifecycle and draining `Shutdown`.
- Idle workers block on events instead of polling with `Sleep`.
- The Simple pool uses a dynamically growing O(1) circular FIFO.
- The bounded pool signals not-empty and not-full transitions and only waits
  when its queue is actually full.
- `WaitForAll(TimeoutMS)` and `TryQueue(..., TimeoutMS)` make deadlines explicit.
- Exceptions raised by `OnError` are contained at the callback boundary.
- Existing `Queue(...)` source continues to compile unchanged.
- Debug logging is off by default.

## Lifecycle contract

`Shutdown` changes `State` from `tpsAccepting` to `tpsDraining`, prevents new
admission, waits for submissions already in progress, drains all accepted work,
wakes and joins workers, and publishes `tpsStopped`. Repeated calls are safe.
Queueing after shutdown raises `EThreadPoolShutdown`.

Task and `OnError` exceptions are contained, but callback execution is
synchronous and has no automatic deadline. A blocking callback can therefore
delay completion and `Shutdown`; operations that may block should implement an
application-level timeout or cancellation mechanism.

## Timeout contract

Timeout values are milliseconds:

- `0` performs an immediate attempt/check;
- finite values bound the call from entry;
- `THREADPOOL_INFINITE` waits without a deadline.

The Simple pool is unbounded, so `TryQueue` normally returns immediately. The
Producer-Consumer pool returns `False` when no slot becomes available before
the deadline.

## Verification

The v0.8.0 suite contains 58 tests covering both legacy behaviour and new
lifecycle, timeout, callback-failure, queue saturation, concurrent
admission/shutdown, and invalid-construction cases. On the development machine
it completes in roughly four seconds with zero heap leaks reported by `heaptrc`.

The benchmark in [`benchmarks/`](../../benchmarks/ThreadPoolBenchmark.lpr) was compiled unchanged against
v0.7.0 and v0.8.0 with FPC 3.2.2 `-O3`. Debug logging was disabled in both.
Median of three Windows runs:

| Workload | v0.7.0 | v0.8.0 | Change |
| --- | ---: | ---: | ---: |
| Simple, 20,000-task burst | 234 ms | 109 ms | 2.1× faster |
| Producer, 20,000-task burst | 3,953 ms | 172 ms | 23.0× faster |
| Simple idle queue-to-start | 7.9 ms | <1 ms | timer-resolution floor |
| Producer idle queue-to-start | 81.3 ms | <1 ms | timer-resolution floor |

## Upgrade notes

- Ordinary `Queue`/`WaitForAll` programs need no source changes.
- Code that submitted work after shutdown must now handle
  `EThreadPoolShutdown`; the old Simple pool silently discarded that work.
- `TBackpressureConfig` remains available, but threshold and low/medium-delay
  fields no longer sleep. Prefer `TryQueue(..., TimeoutMS)` in new code.

See [CHANGELOG.md](../../CHANGELOG.md) for the complete version history.
