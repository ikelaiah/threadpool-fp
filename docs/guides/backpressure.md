# Backpressure

Backpressure is how a bounded pool tells its producers to slow down. Because
the queue has a fixed capacity, a producer that outruns the workers must either
wait for space, or be told there is no space within its deadline.

Two features implement this: **submission deadlines** and **queue
monitoring**.

## The default deadline behaviour

The bounded pool (`ThreadPool.ProducerConsumer`) waits for queue space while it
is full:

- `Queue(...)` waits up to a compatibility deadline, then raises `EQueueFullException`.
- `TryQueue(..., TimeoutMS)` returns `False` when its deadline expires, without raising.
- `Submit(...)` behaves like `Queue`; `TrySubmit(..., TimeoutMS, Task)` like `TryQueue`.

Waits are **event-driven**: a producer blocks only while the queue is actually
full, and is woken by a free slot. There are no load-threshold polling sleeps.

### Choosing a deadline

```pascal
if not Pool.TryQueue(@DoWork, 50) then
  WriteLn('Full for 50 ms');
```

Timeout values are milliseconds:

| Value | Meaning |
| ---: | --- |
| `0` | Immediate attempt; return `False` if already full |
| finite | Maximum wait measured from call entry |
| `THREADPOOL_INFINITE` | Wait for space without a deadline |

## The backpressure compatibility record

`TBackpressureConfig` remains source-compatible. Its threshold and low/medium
delay fields no longer introduce sleeps; only `MaxAttempts` and
`HighLoadDelay` shape the compatibility deadline used by the legacy `Queue`
overloads.

```pascal
var
  Config: TBackpressureConfig;
begin
  Config := Pool.BackpressureConfig;
  Config.MaxAttempts   := 3;
  Config.HighLoadDelay := 200;
  Pool.BackpressureConfig := Config;
end;
```

Defaults: thresholds `0.5 / 0.7 / 0.9`, delays `10 / 50 / 100` ms,
`MaxAttempts = 5`. The compatibility deadline is approximately
`MaxAttempts * HighLoadDelay + (MaxAttempts - 1) * 10` ms.

New code should express its deadline directly with `TryQueue`/`TrySubmit`
instead of tuning these fields.

## Monitoring queue utilisation

Three pool-level, lock-protected metrics tell you how close the queue is to
full:

```pascal
WriteLn('Queued: ', Pool.QueueCount, '/', Pool.QueueCapacity);
WriteLn('Load: ', Pool.QueueLoadFactor:0:2);

if Pool.QueueLoadFactor > 0.9 then
  SlowDownProducerRate;
```

- `QueueCount` — callbacks waiting for a worker right now.
- `QueueCapacity` — the constructor's queue size.
- `QueueLoadFactor` — `QueueCount / QueueCapacity`, a `Double` in `[0, 1]`.

A count read is a snapshot: the value changes the moment a worker dequeues.

## Producer-side patterns

### Back off and retry

```pascal
if not Pool.TryQueue(@DoWork, 25) then
begin
  TrySleep(5);            // leave room for workers to drain a slot
  if not Pool.TryQueue(@DoWork, 100) then
    HandleDroppedWork(@DoWork);
end;
```

### Drain before retrying a batch

Queue a bounded number, wait for them to finish, then continue:

```pascal
for I := 1 to BatchSize do
  Pool.Queue(@ProcessItem, I);   // bounded, one batch at a time
Pool.WaitForAll;
```

### Resize expectations, not the queue

The queue does not resize. If you need to absorb a larger burst, create the
pool with a larger capacity, or accept that `TryQueue` will report `False`.

## Rules that keep backpressure honest

1. Prefer `TryQueue`/`TrySubmit` over `Queue` so saturation is a returned value, not an exception (except where `Queue` semantics fit).
2. Never mutate `WorkQueue` directly; pool completion accounting depends on the pool's own submission methods.
3. Do not call `SubmitRange` from a worker of the same bounded pool.
4. A task timed out of admission is never queued, so its result is simply absent; you are responsible for deciding what to do with the dropped item.

## Related

- [Producer-Consumer Queue](producer-consumer.md)
- [Producer-Consumer API](../reference/producer-consumer-api.md)
- The [Simple thread pool](simple-thread-pool.md) is unbounded and has no backpressure; see [Choose a Pool](../getting-started/choosing-a-pool.md).
