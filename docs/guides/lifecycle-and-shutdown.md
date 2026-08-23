# Lifecycle & Shutdown

Both pools share one lifecycle contract. The pool state machine moves
monotonically from accepting, to draining, to stopped:

```text
tpsAccepting -> tpsDraining -> tpsStopped
```

`State` is read-only and observable from any thread.

## Waiting

`WaitForAll` blocks until every accepted callback has finished.

```pascal
Pool.WaitForAll;                  // no deadline
Ready := Pool.WaitForAll(250);    // deadline in milliseconds
```

This is the gate before reading results, checking errors, or freeing callback
targets.

> [!IMPORTANT]
> `WaitForAll` does **not** close admission. Producers on other threads can
> keep submitting while you wait, and new work may arrive after the wait
> returns. To coordinate a known set of producers with a pool-wide barrier,
> use `Shutdown`, or synchronize your producers yourself.

## Shut down

`Shutdown` performs a controlled stop:

1. Stops new admission atomically.
2. Waits for submissions already passing admission to finish enqueueing.
3. Drains every accepted task to completion.
4. Sets `Terminated` on each worker, wakes, joins, and frees them.
5. Publishes `tpsStopped` and wakes concurrent shutdown callers.

After `Shutdown`, `Queue`, `TryQueue`, `Submit`, and `TrySubmit` raise
`EThreadPoolShutdown`. The pool is permanently stopped; a later call to
`Shutdown` is safe and a no-op.

```pascal
Pool.Shutdown;            // stop admission, drain, join workers
WriteLn(Pool.State = tpsStopped);
```

Destruction calls `Shutdown` automatically, so the `try/finally Free` pattern
drains accepted work even if you forget the explicit call.

## Rights and restrictions

- `Shutdown` is **idempotent** and never blocks a second caller once the first finishes.
- `Shutdown` must **not** be called from one of the pool's own worker threads; the library raises `EThreadPoolShutdown` rather than deadlock the worker.
- Waiting for a pool never reduces its own admission: `WaitForAll` and `Shutdown` are the only lifecycle operations.

## Why not call Free early?

`Destroy` = `Shutdown` + resource release. It joins every worker, so freeing a
pool while callbacks are still running is safe from the pool's perspective.
The danger is your own objects: free callback targets only after `WaitForAll`
returns, otherwise a still-running method dereferences a freed object.

## Worker-thread hazards

- Calling `Shutdown` from a callback running on that pool raises.
- A callback waiting on its **own** task handle raises `EThreadPoolDeadlock` (see [Tasks & Batches](tasks-and-batches.md)).
- On the Simple pool, callbacks may queue more work (the queue never blocks). On the bounded pool, a worker submitting to its own full queue can block without a bound; avoid it (see [Producer-Consumer](producer-consumer.md)).

## Putting it together

```pascal
Pool := TProducerConsumerThreadPool.Create(4, 1024);
try
  // Producers finish while admission is open.
  for I := 1 to NumberOfJobs do
    Pool.Queue(@DoWork);

  // Barrier: nothing is accepted after this call starts passing.
  Pool.Shutdown;
finally
  Pool.Free; // already stopped; resource release only
end;
// Safe to read shared results here: Shutdown drained everything.
```

Using `Shutdown` as the final barrier removes the "another producer still
submitting" ambiguity.

## Related

- [Thread Safety](thread-safety.md)
- [Simple Thread Pool](simple-thread-pool.md)
- [Contracts & Limitations](../reference/contracts-and-limitations.md)
