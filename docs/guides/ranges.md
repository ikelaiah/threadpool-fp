# Parallel Ranges

`SubmitRange` processes an **inclusive** integer range in parallel. Instead of
queuing one callback per index, it splits the range into chunks and queues one
task per chunk, so the worker count (not the index count) drives the queue.

```pascal
Batch := GlobalThreadPool.SubmitRange(@ProcessItem, 0, High(Items));
Batch.WaitFor;
```

Bounds follow Pascal's `for I := First to Last` convention: both `AFirstIndex`
and `ALastIndex` are included. Object-method indexed callbacks work the same
way:

```pascal
Batch := Pool.SubmitRange(@Worker.ProcessItem, FirstIndex, LastIndex);
```

## Chunking

| Argument | Behaviour |
| --- | --- |
| `AChunkSize = 0` (default) | Automatic: at most `ThreadCount * 4` chunks total |
| `AChunkSize > 0` | That many indexes per queued task |
| negative chunk size | Raises `EArgumentOutOfRangeException` |
| `AFirstIndex > ALastIndex` | Returns an empty, already-finished batch |

In automatic mode the library computes
`ceil(ItemCount / (ThreadCount * 4))`, so each worker gets roughly four jobs.
This keeps overhead low for huge ranges — a 200,000-index range queues fewer
than a few hundred tasks instead of 200,000.

## Each chunk is one task

A returned batch has one entry per chunk:

- Cancelling a **pending** chunk skips every index in that chunk.
- A **running** chunk finishes; you cannot interrupt it.
- Smaller explicit chunks give finer cancellation and load balance but add queue overhead.
- If one index raises, that chunk stops and becomes `ttsFailed`; other chunks continue running. A callback that must attempt every index should catch its own per-index exceptions.

```pascal
// Explicit chunk size: 250 indexes per queued task.
Batch := Pool.SubmitRange(@ProcessItem, 0, 99_999, 250);
```

## Deadlock protection on the bounded pool

The bounded pool rejects `SubmitRange` from one of its own workers with
`EThreadPoolDeadlock`. A fixed worker set that blocks feeding its own full
queue could otherwise starve the pool. Always submit bounded ranges from a
coordinating thread, and keep the chunk count smaller than you expect to fit
in the queue.

## Alternatives

- A plain `for` loop of `Queue(@ProcessItem, I)` works and is simpler for small ranges; see [Callback Forms](callback-forms.md).
- Individual `Submit` calls return per-index handles when you need the fine granularity; see [Tasks & Batches](tasks-and-batches.md).

## Related

- [Cancellation](cancellation.md)
- [Thread Safety](thread-safety.md)
- [Tasks API](../reference/tasks-api.md)
