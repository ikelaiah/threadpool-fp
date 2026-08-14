# ThreadPool for Free Pascal v0.9.1

v0.9.1 makes the project easier to enter and safer to monitor without changing
the established queueing, task, lifecycle, or error contracts.

## Highlights

- A shorter README now leads with what the library does, one default path, and
  a complete copy-paste example.
- The Starter example is a small queue-and-wait program without interactive
  pauses or outdated error guidance.
- A new examples index organizes all 15 samples by learning goal.
- Bounded pools expose `QueueCount`, `QueueCapacity`, and `QueueLoadFactor`
  directly, so monitoring does not require access to the mutable queue object.
- `BackpressureConfig` is available directly on the pool and is protected by
  the queue lock.
- Duplicate callback work-item logic now lives in one internal unit.
- Obsolete test code and a timing-dependent unit assertion were removed.

## Safe bounded-queue monitoring

```pascal
WriteLn('Queued: ', Pool.QueueCount, '/', Pool.QueueCapacity);
WriteLn('Load: ', Pool.QueueLoadFactor:0:2);

Config := Pool.BackpressureConfig;
Config.MaxAttempts := 3;
Config.HighLoadDelay := 200;
Pool.BackpressureConfig := Config;
```

The legacy `Pool.WorkQueue` property remains available so existing v0.x source
continues to compile. New code should not mutate it: direct enqueue, dequeue, or
clear operations bypass the pool's completion counter and can invalidate
`WaitForAll`.

## Documentation journey

The project documentation now has clearer roles:

- [`README.md`](../README.md) answers what the library does and gets a new user
  to a running program.
- [`examples/README.md`](../examples/README.md) selects the next sample by goal.
- [`CHEATSHEET.md`](CHEATSHEET.md) keeps operational rules on one page.
- The Simple, Producer-Consumer, and Tasks API documents remain the canonical
  detailed contracts.
- Technical guides and archived development plans serve maintainers without
  interrupting the beginner path.

## Maintenance

`ThreadPool.Internal.WorkItems` now owns the common untracked callback wrapper.
The existing `TSimpleWorkItem` and `TProducerConsumerWorkItem` names remain as
compatibility wrappers. Shared `ThreadCount` and `LastError` properties now
come directly from `TThreadPoolBase`, eliminating redundant overrides.

The inactive legacy `threadpooltests.pas` suite was removed. The wall-clock
parallel-scaling assertion was also removed from FPCUnit; performance remains
covered by the dedicated benchmark project.

## Compatibility

This is a source-compatible patch release. Existing programs can continue to
use:

- all `Queue`, `TryQueue`, `Submit`, `TrySubmit`, and `SubmitRange` overloads;
- `WaitForAll`, `Shutdown`, task handles, batches, cancellation, and errors;
- both pool constructors and `GlobalThreadPool`;
- the v0.8 `IThreadPool` interface and GUID; and
- legacy `WorkQueue` and `TBackpressureConfig` access.

## Verification

The release gate builds the Lazarus package, runs all 81 active tests, builds
the benchmark, and builds all 15 examples. CI covers Windows and Linux.
