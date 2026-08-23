# Error Handling

Worker exceptions never propagate to the caller's thread. The pool captures
them, keeps running, and lets you inspect them after the fact or react as they
happen.

There are two distinct error surfaces:

1. **Submission errors** raised synchronously on the calling thread (`EQueueFullException`, `EThreadPoolShutdown`).
2. **Worker errors** collected asynchronously (`LastError`, `Errors`, `OnError`), plus per-task `ErrorMessage` for submitted tasks.

## Inspecting failures after WaitForAll

The simplest pattern polls the pool after work completes:

```pascal
Pool.ClearErrors;
Pool.Queue(@RiskyWork);
Pool.WaitForAll;

if Pool.LastError <> '' then
  WriteLn('The most recent failure was: ', Pool.LastError);

for Msg in Pool.Errors do
  WriteLn('All failures, oldest first: ', Msg);
```

| Member | Meaning |
| --- | --- |
| `LastError` | The most recent captured message; empty if none |
| `Errors` | Oldest-first snapshot of every captured message |
| `ErrorCount` | Number of messages currently in `Errors` |
| `ClearErrors` | Clears `Errors` and resets `LastError` |
| `ClearLastError` | Legacy single-value reset (same effect as above) |

`Errors` is capped at `MAX_STORED_ERRORS = 1000`; the oldest entries are
dropped so a flood of failures cannot exhaust memory. `LastError` always
reflects the most recent failure regardless of the cap.

## Reacting as failures happen

Assign `OnError` to be notified on the worker thread the moment a task fails:

```pascal
Pool.OnError := @Handler.OnTaskError;
```

The handler runs **synchronously on the worker that caught the exception**.
Keep it short, bounded, and thread-safe; synchronize if it touches the UI or
shared state.

> [!WARNING]
> `OnError` has no execution deadline and runs on a worker. A blocking handler
> delays `WaitForAll` and `Shutdown` and can occupy a worker indefinitely.

### Handler exceptions are contained

An exception raised inside your `OnError` handler is caught by the pool. It
cannot terminate a worker or prevent task completion accounting, and the
original task error is already recorded.

## Per-task error messages

A task submitted with `Submit` stores its failure in `Task.ErrorMessage`,
independently of the pool collection:

```pascal
Task := Pool.Submit(@RiskyWork);
Task.WaitFor;
if Task.State = ttsFailed then
  WriteLn('That task failed: ', Task.ErrorMessage);
```

See [Tasks & Batches](tasks-and-batches.md).

## Queue-full errors (bounded pool only)

`Queue` on `ThreadPool.ProducerConsumer` raises `EQueueFullException` when the
queue stays full until the compatibility deadline expires. Catch it **around
each `Queue` call**, never around `WaitForAll`:

```pascal
try
  Pool.Queue(@MyProcedure);
except
  on E: EQueueFullException do
    WriteLn('Queue is saturated: ', E.Message);
end;
Pool.WaitForAll;
```

Catch by exception **type**, never by message string.

Prefer `TryQueue(..., TimeoutMS)` when saturation is expected; it returns
`False` instead of raising. See [Backpressure](backpressure.md).

## Shutdown errors

Once `Shutdown` begins, `Queue`, `TryQueue`, `Submit`, and `TrySubmit` raise
`EThreadPoolShutdown`. See [Lifecycle & Shutdown](lifecycle-and-shutdown.md).

## Rules that prevent most surprises

1. Check `LastError` or `Errors` **after** `WaitForAll`, and clear them before reusing the pool so stale messages do not linger.
2. Give blocking operations an application-level timeout or cancellation; the pool cannot time a callback out.
3. Do not place `WaitForAll` inside a `EQueueFullException` handler — drain and retry separately.
4. Keep `OnError` handlers non-blocking and thread-safe.

## Related

- [Common Recipes](recipes.md) includes a compiled failure-reporting program.
- [Simple API](../reference/simple-api.md)
- [Producer-Consumer API](../reference/producer-consumer-api.md)
