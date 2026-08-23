# Thread Safety

ThreadPool-FP makes specific concurrency guarantees. This page states them
exactly, because an inaccurate concurrency contract is worse than none.

## What is safe to do from any thread

- Call every `Queue`, `TryQueue`, `Submit`, `TrySubmit`, and `SubmitRange` overload concurrently from any number of producer threads.
- Read `LastError`, `Errors`, `ErrorCount`, `ThreadCount`, `State`, `QueueCount`, `QueueCapacity`, `QueueLoadFactor`, and `BackpressureConfig` from any thread.
- Wait on the same task or batch from multiple threads.
- Call `WaitForAll` from a different thread than the one submitting work.
- Assign and read `OnError` (protected by the error lock).

## What is NOT safe

- **Freeing a callback target before the callback finished.** A queued method reference keeps using the object; freeing it is a use-after-free. Wait for completion first.
- **Freeing `GlobalThreadPool`.** The unit owns it and frees it at exit.
- **Calling `Shutdown` from the pool's own worker.** The library raises `EThreadPoolShutdown` instead.
- **Calling `SubmitRange` from a bounded pool worker.** Raises `EThreadPoolDeadlock`.
- **A callback waiting on its own task.** Raises `EThreadPoolDeadlock`.
- **Mutating `WorkQueue` directly.** Bypasses completion accounting and can break `WaitForAll`.
- **Assuming shared state is protected.** The pool protects its own internals, not your data. Objects touched by more than one callback need their own synchronization (critical sections, `TThreadList`, atomics, or the RTL's thread helper units).

## Execution order

Queued callbacks run **concurrently**. The FIFO governs *dequeue order*, not
completion order: a later callback can finish before an earlier one. Never
derive "ran before" conclusions from the order callbacks were submitted, and
never write results assuming a callback has completed until `WaitForAll`
returns.

## What WaitForAll guarantees

- Every callback **accepted before the call returns** has finished when a no-deadline `WaitForAll` returns.
- `WaitForAll(TimeoutMS)` returns `True` only if that entire set finished within the deadline from call entry (a single overall deadline, not one per item).
- Completed, failed, and cancelled entries all count as finished.

What it does **not** guarantee:

- It is not an admission barrier. Other producer threads may queue more work before or during the wait, which `WaitForAll` never counted.
- It does not give a "commit point" against concurrent producers; use `Shutdown` if you need that.

## Worker exceptions

- An exception inside a callback is caught by the worker.
- It is recorded in `LastError` (most recent) and `Errors` (oldest-first, capped at `MAX_STORED_ERRORS = 1000`), and reported through `OnError`.
- The pool continues processing remaining work; no exception propagates to the submitting thread.
- For submitted tasks, the message is also stored in `Task.ErrorMessage`.
- `OnError` runs synchronously on the worker that caught the failure, outside the pool's error lock. Handler exceptions are contained: they cannot terminate a worker or skip completion accounting.
- **There is no execution deadline.** A callback (or `OnError` handler) that blocks keeps its worker occupied and delays `WaitForAll`/`Shutdown`.

## Task state transitions

Each task transition (`pending -> running -> completed/failed`, or
`pending -> cancelled`) happens under a task-level lock, so exactly one of the
worker's start transition and a caller's cancel transition wins.

- `Cancel` succeeds only from `ttsPending`.
- A task can never be both run and cancelled; the loser sees `False`/skip.
- Cancelled callbacks never start; running callbacks are never interrupted.

## Submission and shutdown race

Admission and shutdown are serialized: `BeginQueue`/`EndQueue` and
`BeginShutdown`/`FinishShutdown` share a lifecycle lock. Either a submission
completes and is drained, or shutdown wins and the submission raises
`EThreadPoolShutdown`. There is no lost accounting between the two paths.

## Callbacks submitting more work

- **Simple pool:** allowed. Its queue never blocks, so a worker cannot deadlock itself submitting. Watch out for unbounded growth.
- **Bounded pool:** submitting from one of its own workers is allowed for `Queue`/`Submit`, but if the queue is full the worker waits for space — while every other worker may be doing the same — so it can stall with no deadline. Prefer coordinating from another thread, and remember ranges are rejected outright (see [Producer-Consumer](producer-consumer.md)).

## Timeout semantics

All timeouts are milliseconds measured from the start of the call:

| Value | Meaning |
| ---: | --- |
| `0` | Immediate attempt or state check |
| finite | Maximum wait from call entry |
| `THREADPOOL_INFINITE` | No deadline |

`THREADPOOL_INFINITE = High(Cardinal)` (see
[Types & Interfaces](../reference/types-and-interfaces.md)).

## Thread-count reality

`TThread.ProcessorCount` is read once at startup, may count logical cores, and
does not change at run time. Treat the resulting worker count as approximate
guidance for `QueueCount` budgets and chunk sizing, not a precise parallel
budget.

## Related

- [Lifecycle & Shutdown](lifecycle-and-shutdown.md)
- [Cancellation](cancellation.md)
- [Contracts & Limitations](../reference/contracts-and-limitations.md)
