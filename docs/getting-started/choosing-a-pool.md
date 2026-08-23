# Choose a Pool

ThreadPool-FP ships two pool implementations with the same callback forms,
error API, and lifecycle contract. Choose between them on one factor: **do you
need to bound the number of queued callbacks?**

| If you need... | Start with... |
| --- | --- |
| Ordinary parallel work with the least setup | `ThreadPool.Simple` |
| A ready-to-use process-wide pool | `GlobalThreadPool` from `ThreadPool.Simple` |
| A private pool with a controlled worker count | `TSimpleThreadPool.Create` |
| A fixed queue capacity and submission deadlines | `ThreadPool.ProducerConsumer` |
| Fire-and-forget work only | `Queue` on either pool |
| A handle to wait for or cancel one task | `Submit` plus `IThreadPoolTask` |
| Bounded memory under a flood of work | `ThreadPool.ProducerConsumer` + `TryQueue` |

## The two pools

### ThreadPool.Simple — unbounded queue

- Dynamically growing FIFO: the queue never rejects work for capacity reasons.
- `Queue` and `TryQueue` accept immediately (unless shutdown has begun).
- Suited to short bursts, job lists, range processing, and most applications.
- Memory grows with the backlog; there is no admission deadline.

### ThreadPool.ProducerConsumer — bounded queue

- Fixed-capacity circular queue (default 1024 callbacks).
- `Queue` waits while the queue is full and raises `EQueueFullException` if its compatibility deadline expires.
- `TryQueue(..., TimeoutMS)` returns `False` on deadline instead of raising.
- Suited to producers that can outrun consumers, streaming workloads, and anything where a bounded backlog matters.

## Decision guide

1. **Start with `GlobalThreadPool`.** It is process-wide, already exists, and covers most realistic workloads.
2. Choose `ThreadPool.ProducerConsumer` **only** when queue growth must be controlled or producers need a submission deadline.
3. Choose a private `TSimpleThreadPool.Create(N)` pool when you need a fixed worker count or a lifetime independent of the global pool.

```pascal
uses
  ThreadPool.Simple, ThreadPool.Tasks;

begin
  // A private unbounded pool with an explicit worker count.
  SimplePool := TSimpleThreadPool.Create(4);
  try
    SimplePool.Queue(@DoWork);
    SimplePool.WaitForAll;
  finally
    SimplePool.Free;
  end;

  // A bounded pool: 4 workers, queue capacity 1024.
  BoundedPool := TProducerConsumerThreadPool.Create(4, 1024);
  try
    if not BoundedPool.TryQueue(@DoWork, 50) then
      WriteLn('No queue space within 50 ms');
  finally
    BoundedPool.Free;
  end;
end;
```

> [!TIP]
> The two pools are interchangeable behind the `IThreadPool` interface for the
> `Queue`/`WaitForAll`/`Shutdown` surface. Task handling (`Submit`,
> `SubmitRange`) is available on both concrete classes and through
> `IThreadPoolTaskSource`.

## Thread-count rules (both pools)

| Constructor argument | Result |
| --- | --- |
| `0` (default) | `TThread.ProcessorCount` |
| below 4 | raised to 4 workers |
| above `2 × ProcessorCount` | capped at `2 × ProcessorCount` |

Worker count is fixed after construction; neither pool scales dynamically.

## Not a fit here?

- ThreadPool-FP is not a task-scheduling framework: there are no priorities, futures, continuations, or automatic load balancing.
- Callbacks have no execution deadline and no forced interruption.
- For per-thread UI work or real-time guarantees, look elsewhere — worker callbacks run on pool threads and can block without a timeout.

See [Contracts & Limitations](../reference/contracts-and-limitations.md) for
the precise boundaries.

## Related

- [Installation & Quick Start](installation.md)
- [Simple Thread Pool](../guides/simple-thread-pool.md)
- [Producer-Consumer Queue](../guides/producer-consumer.md)
- [Cheat Sheet](cheat-sheet.md)
