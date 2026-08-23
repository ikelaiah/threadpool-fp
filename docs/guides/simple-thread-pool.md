# Simple Thread Pool

`ThreadPool.Simple` provides the unbounded, dynamically-growing FIFO pool and
is the default starting point for most programs.

It exposes two uses: the process-wide `GlobalThreadPool` and private
`TSimpleThreadPool` instances.

## Using GlobalThreadPool

`GlobalThreadPool` is created when the unit initializes and freed when your
program exits. It needs no construction and no cleanup.

```pascal
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  ThreadPool.Simple;

GlobalThreadPool.Queue(@DoWork);
GlobalThreadPool.WaitForAll;
```

- Do **not** call `GlobalThreadPool.Free`; the unit manages its lifetime.
- Its worker count is `TThread.ProcessorCount`, minimum four.
- It stays alive for the whole process, so it is safe to use from any unit.

If your program uses several pools, `GlobalThreadPool` is still useful as the
process-wide default and for fire-and-forget work.

## Private pools

Create a private pool when you need a specific worker count or a lifetime you
control:

```pascal
var
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    for I := 0 to High(Items) do
      Pool.Queue(@ProcessItem, I);
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end;
```

`Pool.Free` calls `Shutdown`, which drains accepted work and joins the worker
threads; a `finally` destructor is still the correct pattern because it runs
after your `WaitForAll` point.

## Queue vs Submit

`Queue` is the lightest path: fire-and-forget, no handle. `Submit` returns an
`IThreadPoolTask` you can wait on, inspect, or cancel while pending.

```pascal
// Fire-and-forget.
Pool.Queue(@DoWork);

// Observable.
Task := Pool.Submit(@DoWork);
if Task.WaitFor(250) and (Task.State = ttsFailed) then
  WriteLn(Task.ErrorMessage);
```

`Queue` callers who never need a handle do not pay for task state or
completion events.

## Workers and order

- Worker count is fixed at construction: `0` selects `ProcessorCount`, the minimum is four, and requests above `2 × ProcessorCount` are capped.
- The queue is FIFO at the moment of enqueue, but **callbacks run concurrently**, so their completion order is unspecified.
- Idle workers block without polling and are woken on submission.

## Waiting and readiness

`WaitForAll` blocks until every accepted callback has finished. Reading shared
results, checking errors, or freeing callback targets must happen after this
point.

```pascal
if not Pool.WaitForAll(250) then
  WriteLn('Work remains after 250 ms');
```

> [!WARNING]
> `WaitForAll` is not an admission barrier. Another producer thread can keep
> queueing after your `WaitForAll` starts, so the wait may return while new
> work is already in the queue. Coordinate your producers, or call `Shutdown`
> to close admission before draining (see
> [Lifecycle & Shutdown](lifecycle-and-shutdown.md)).

## The queue is unbounded

`TryQueue(..., TimeoutMS)` exists for API symmetry with the bounded pool; its
timeout normally never expires because the Simple queue grows to fit. Capacity
is limited only by available memory. If a runaway producer matters to you,
consider the bounded pool instead ([Backpressure](backpressure.md)).

## Errors

Worker exceptions are captured in `LastError` and `Errors`; they do not stop
the pool. See [Error Handling](error-handling.md).

## Concurrency contracts

- All `Queue`, `TryQueue`, and `Submit` overloads are thread-safe and may be called from any thread.
- Queueing from inside a callback is allowed on the Simple pool: the queue never blocks, so a worker cannot deadlock itself while submitting.
- `Shutdown` must not be called from one of the pool's own workers.
- See [Thread Safety](thread-safety.md) for the full list.

## Related

- [Callback Forms](callback-forms.md)
- [Tasks & Batches](tasks-and-batches.md)
- [Simple API](../reference/simple-api.md)
- [Simple Pool Internals](../internals/simple-internals.md)
