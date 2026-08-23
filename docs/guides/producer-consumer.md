# Producer-Consumer Queue

`ThreadPool.ProducerConsumer` is the bounded pool. It is backed by a
fixed-capacity circular FIFO queue, so producer threads cannot outrun consumer
threads without slowing down or being told the queue is full.

Use it when task production can outpace consumption and you need predictable
memory usage and overflow control.

```pascal
uses
  ThreadPool.ProducerConsumer;

Pool := TProducerConsumerThreadPool.Create(4, 1024);
```

The constructor takes the worker count and the queue capacity, defaults
`(0, 1024)`: `0` selects `ProcessorCount` and the queue holds up to 1024
callbacks.

## Queuing with and without a deadline

All four callback forms support plain `Queue` and timeout-aware `TryQueue`.

`Queue` **waits while the queue is full**, up to a compatibility deadline, then
raises `EQueueFullException`:

```pascal
Pool.Queue(@DoWork);          // may block briefly, then raise if still full
```

`TryQueue` never raises for capacity; it returns `False` when its deadline
expires:

```pascal
if not Pool.TryQueue(@DoWork, 50) then
  WriteLn('No queue space became available within 50 ms');
```

Because `Queue` waits only while the queue is actually full, it does not sleep
at low load thresholds. New code should state its deadline explicitly with
`TryQueue`.

## Submission timeouts

`Submit`, `TrySubmit`, and `SubmitRange` observe the same rules:

- `Submit` uses the compatibility timeout and raises `EQueueFullException` on expiry.
- `TrySubmit(..., TimeoutMS, Task)` returns `False` with `Task = nil` on a full queue.
- `SubmitRange` runs from a coordinating thread in one admission pass and waits for queue space for its (small) set of chunks.

## WaitForAll

`WaitForAll` blocks until every accepted callback has finished:

```pascal
Pool.WaitForAll;

if Pool.WaitForAll(250) then
  WriteLn('All finished within 250 ms');
```

As with the Simple pool, it does not close admission; coordinate producers or
use `Shutdown` first. See [Lifecycle & Shutdown](lifecycle-and-shutdown.md).

## Errors in the two surfaces

Handle both independently:

- catch `EQueueFullException` around each `Queue` call;
- read `LastError`/`Errors`/`OnError` after work runs.

See [Error Handling](error-handling.md).

## Knowing how full the queue is

The pool exposes read-only monitoring:

```pascal
WriteLn('Queued: ', Pool.QueueCount, '/', Pool.QueueCapacity);
WriteLn('Load: ', Pool.QueueLoadFactor:0:2);
```

- `QueueCount` is the number of callbacks currently in the queue.
- `QueueCapacity` is the configured queue size.
- `QueueLoadFactor` is `QueueCount / QueueCapacity` in `[0, 1]`.

These are thread-safe snapshots; a count can change immediately after you read
it.

## Legacy WorkQueue

`WorkQueue` remains public so older v0.x programs compile, but **do not mutate
it in new code**. Direct `TryEnqueue`/`TryDequeue`/`Clear` through the queue
bypasses the pool's completion accounting and can break `WaitForAll` and
`Shutdown`. Use the pool-level metrics and submission methods instead.

## Rules for the bounded pool

1. Prefer `TryQueue`/`TrySubmit` with an explicit deadline over `Queue`.
2. Never call `SubmitRange` from one of the pool's own workers (`EThreadPoolDeadlock`); always coordinate from another thread.
3. Avoid submitting work from a worker callback to **the same bounded pool**: a full queue can block the worker's own submission indefinitely while every worker is blocked trying to submit. The Simple pool's queue never blocks, so this hazard does not apply there.
4. A cancelled task leaves a tombstone that may occupy its queue slot until it reaches a worker.

## Related

- [Choose a Pool](../getting-started/choosing-a-pool.md)
- [Backpressure](backpressure.md)
- [Producer-Consumer API](../reference/producer-consumer-api.md)
- [Producer-Consumer Internals](../internals/producer-consumer-internals.md)
