# Cancellation

Cancellation in ThreadPool-FP is **best-effort and pending-only**. It removes
work that has not started; it never interrupts work that is running.

```pascal
if Task.Cancel then
  WriteLn('The callback will not run')
else
  WriteLn('The task had already started or finished');
```

`Cancel` is a single-winner state transition:

```text
pending -> running -> completed or failed
   |
   +-----> cancelled
```

## The contract

- `Cancel` returns `True` only for `pending -> cancelled`, exactly the transition that prevents the callback from running.
- It is safe to call repeatedly; later calls return `False`.
- It never terminates a worker and never interrupts a running callback.
- Cancellation is **not an error**: it does not fire `OnError`, and it does not add a message to `LastError` or `Errors`.
- The task handle becomes terminal immediately, so `IsFinished` is `True` and `WaitFor` returns immediately.

There is no way to force-stop a running callback in Free Pascal at the pool
level. Design blocking or long operations so they can stop cooperatively, or
size chunks small enough that cancellation granularity is acceptable.

## Queued work can be skipped later

A cancelled queue entry remains in the queue as a cheap **tombstone** until a
worker reaches it; the worker then skips the callback. In the bounded pool the
tombstone may briefly occupy its slot. The library deliberately avoids an
expensive queue-wide removal scan on the submit/dequeue path.

## Cancelling pending work in a batch

`IThreadPoolTaskBatch.CancelPending` attempts every cancellation and returns
the number that won:

```pascal
N := Batch.CancelPending;
WriteLn(N, ' pending task(s) cancelled');
```

## Object lifetime note

If a submitted object method cannot be cancelled because it is running, keep
the callback target alive until the task finishes. A successful `Cancel`
guarantees the callback will never be invoked.

## Cancelling several chunks of a range

Each range chunk is a task in the returned batch, so `CancelPending` skips the
whole chunk while already-running chunks finish. See
[Parallel Ranges](ranges.md).

## Related

- [Tasks & Batches](tasks-and-batches.md)
- [Thread Safety](thread-safety.md)
- [Tasks API](../reference/tasks-api.md)
