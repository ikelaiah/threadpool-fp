# Contracts & Limitations

This page states the cross-cutting guarantees both pools honour, and the
boundaries the library deliberately does not cross. Use it as the court of
appeal when the guides and API pages disagree with something you observed.

## The two pools at a glance

| Property | `ThreadPool.Simple` | `ThreadPool.ProducerConsumer` |
| --- | --- | --- |
| Queue storage | Dynamically growing FIFO | Fixed-capacity circular FIFO |
| Queue capacity | Unbounded (limited by memory) | Fixed at construction; default 1024 |
| Admission deadline | None (queue never rejects for capacity) | `TryQueue`/`TrySubmit` deadlines; `Queue`/`Submit` raise on expiry |
| Default pool | `GlobalThreadPool` | — (constructor required) |
| Task surface | Same | Same |

## Timeout semantics (shared)

All timeout values are milliseconds:

| Value | Meaning |
| ---: | --- |
| `0` | Immediate attempt or state check |
| finite | Maximum wait measured from the start of the call |
| `THREADPOOL_INFINITE` | No deadline |

A timed `WaitForAll`, `Task.WaitFor`, or `Batch.WaitFor` uses **one overall
deadline** from call entry, never one fresh deadline per item.

## Worker-count rules

| Constructor argument | Result |
| --- | --- |
| `AThreadCount <= 0` | `TThread.ProcessorCount` |
| `< 4` | raised to 4 workers |
| `> 2 × ProcessorCount` | capped at `2 × ProcessorCount` |

Worker count is fixed after construction; neither pool scales dynamically.
`TThread.ProcessorCount` is read once at program start and may count logical
processors.

## Ordering guarantees

- The queue is FIFO at the moment of enqueue on **each** pool.
- Callbacks run concurrently; **completion order is unspecified**.
- `WaitForAll` returning does not imply any particular ordering of callbacks — only that the accepted set finished.

## WaitForAll guarantees

- Blocks until every callback accepted before the call returns has finished (completed, failed, or cancelled).
- Returns `False` only if a finite timeout expired with work still pending.
- It is **not an admission barrier**: concurrent producers may submit more work before or during the wait. Use `Shutdown` for an atomic barrier.

## Cancellation guarantees

- `Task.Cancel` wins only from `ttsPending`; exactly one of the worker's start transition and the caller's cancel transition wins.
- It never interrupts a running callback and never raises.
- Cancellation is not an error: it does not fire `OnError` and adds nothing to `LastError`/`Errors`.
- A cancelled entry remains as a tombstone until a worker reaches it. In the bounded pool it may briefly occupy its queue slot.
- Once a task is `ttsRunning`, there is **no** built-in way to stop it.

## Error containment

- Worker exceptions are captured, never propagated to the submitting thread, and always recorded in `LastError` plus the capped `Errors` collection; they do not stop the pool.
- `OnError` fires synchronously on the worker, outside the error lock. Handler exceptions are contained: they cannot terminate a worker or skip completion accounting.
- **Callbacks and handlers have no execution deadline.** A blocking callback or handler occupies a worker indefinitely and delays `WaitForAll` and `Shutdown`. Add application-level timeouts or cancellation to operations that may block.

## Lifecycle contract

```text
tpsAccepting -> tpsDraining -> tpsStopped
```

- `Shutdown` stops admission, drains accepted work, joins workers, then publishes `tpsStopped`. It is idempotent and never called from a worker (that raises `EThreadPoolShutdown`).
- After `Shutdown`, all submission methods raise `EThreadPoolShutdown`.
- Destruction calls `Shutdown` automatically.
- A callback waiting on its own task handle raises `EThreadPoolDeadlock`; the bounded pool rejects `SubmitRange` from its own workers with the same exception.

## Backpressure semantics (bounded pool)

- Producers wait only while the queue is actually full, using event signals (no polling, no load-threshold sleeps).
- `TBackpressureConfig` remains source-compatible but its threshold and low/medium-delay fields no longer introduce sleeps; only `MaxAttempts` and `HighLoadDelay` shape the legacy `Queue` deadline.
- Mutation of the legacy `WorkQueue` bypasses completion accounting and can invalidate `WaitForAll`; new code must not use it.
- A task that times out of admission was never accepted, so nothing will ever run it; the caller owns the decision of what to do with the dropped item.

## Thread-safety promises

- All submission, monitoring, and error-reading methods are safe from any thread.
- Multiple threads may wait on one task or batch.
- The library protects its own internals only; shared application data needs the usual Free Pascal synchronization.

## Explicit non-goals

The following are deliberately outside the library's contract:

- task priorities, dependence graphs, continuations, or work stealing;
- result values, futures, or generic typed tasks;
- forced cancellation or interruption of running callbacks;
- dynamic worker counts or load-based resizing;
- nested `SubmitRange` on the same bounded pool;
- immediate arbitrary removal of cancelled queue entries;
- real-time guarantees or UI-thread-safe callbacks (callbacks run on pool threads and can block).

## Compatibility stance

The v0.8 `IThreadPool` interface and GUID are unchanged. All `Queue`,
`TryQueue`, `WaitForAll`, lifecycle, error, constructor, `GlobalThreadPool`,
`WorkQueue`, and `TBackpressureConfig` symbols from earlier v0.x releases
remain source-compatible. Removal or visibility-breaking changes are reserved
for a major release.

## Related

- [Thread Safety](../guides/thread-safety.md)
- [Lifecycle & Shutdown](../guides/lifecycle-and-shutdown.md)
- [Types & Interfaces](types-and-interfaces.md)
