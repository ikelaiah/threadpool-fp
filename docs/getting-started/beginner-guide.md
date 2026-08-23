# Beginner Guide

ThreadPool-FP lets you run ordinary Pascal procedures on a fixed set of worker
threads. You queue work, the pool runs it, and you wait. This guide walks
through the ideas in the order you will meet them.

Work through it with the the [`Starter`](../../examples/Starter/Starter.lpr) and
[`SimpleDemo`](../../examples/SimpleDemo/SimpleDemo.lpr) examples open beside you.

## 1. One queued callback

The pool runs a procedure on one of its worker threads. Everything you queue
goes into a FIFO, and whichever worker is free takes the next entry.

```pascal
GlobalThreadPool.Queue(@ProcessItem, 5);
GlobalThreadPool.WaitForAll;
```

`@ProcessItem` is a pointer to your procedure. The second argument to the
**indexed** overload is passed to your callback as its `Index` parameter.

Three rules apply from the start:

- Callbacks run concurrently, so their order is not guaranteed.
- Do not free an object whose method is queued until `WaitForAll` returns.
- On Linux and macOS, `cthreads` must be the first unit in `uses`.

The [Callback Forms](../guides/callback-forms.md) guide describes all four
callback signatures.

## 2. The global pool

`GlobalThreadPool` is a ready-to-use `TSimpleThreadPool` created for you when
your program starts. Use it for everything that needs a pool until you want a
private one.

- Do **not** call `GlobalThreadPool.Free` — the unit owns it.
- Its worker count is `ProcessorCount`, with a minimum of four workers.

## 3. Waiting

`WaitForAll` blocks the calling thread until **every accepted callback** has
finished.

- Always call it before reading results, checking errors, or freeing callback targets.
- A timed `WaitForAll(TimeoutMS)` returns `False` if work remains.
- `WaitForAll` does not stop other producer threads from queuing more work; that is a separate admission concern (see [Lifecycle & Shutdown](../guides/lifecycle-and-shutdown.md)).

## 4. A private pool

Use `TSimpleThreadPool.Create(N)` when you want a fixed worker count or a pool
that lives independently of `GlobalThreadPool`.

```pascal
Pool := TSimpleThreadPool.Create(4);
try
  Pool.Queue(@DoWork);
  Pool.WaitForAll;
finally
  Pool.Free; // drains accepted work and joins workers
end;
```

`Pool.Free` calls `Shutdown` internally, so it drains accepted work before
returning; you still need `WaitForAll` *before* that point if you free objects
used by callbacks.

## 5. Observing a single task

`Queue` is fire-and-forget. When you need a handle to wait for, inspect, or
cancel a single piece of work, use `Submit`; it returns an `IThreadPoolTask`.

```pascal
uses
  ThreadPool.Simple, ThreadPool.Tasks;

var
  Task: IThreadPoolTask;
begin
  Task := GlobalThreadPool.Submit(@DoWork);
  if not Task.WaitFor(250) then
    WriteLn('Still working after 250 ms')
  else if Task.State = ttsFailed then
    WriteLn('Failed: ', Task.ErrorMessage);
end;
```

A task moves through `ttsPending`, `ttsRunning`, and one terminal state:
`ttsCompleted`, `ttsFailed`, or `ttsCancelled`. See
[Tasks & Batches](../guides/tasks-and-batches.md).

## 6. Coordinating several tasks

Put handles into an `IThreadPoolTaskBatch` to wait for them as a group, or
process an integer range with `SubmitRange`:

```pascal
Batch := GlobalThreadPool.SubmitRange(@ProcessItem, 0, High(Items));
Batch.WaitFor;
```

`SubmitRange` splits the inclusive range into a small number of chunks, so you
get parallelism without one queue entry per index. See
[Parallel Ranges](../guides/ranges.md).

## 7. Cancelling work that has not started

`Task.Cancel` can win a race against a worker **only while the task is
pending**. It never interrupts running code, and it does not raise an error.

See [Cancellation](../guides/cancellation.md) for the exact contract.

## 8. What happens when work raises?

Worker exceptions are captured, not propagated. Read `LastError` and `Errors`
after `WaitForAll`, or assign `OnError` to react as failures happen. A single
failed task does not stop the pool. See
[Error Handling](../guides/error-handling.md).

## 9. Shutting a pool down

`Shutdown` closes admission, drains every accepted task, and joins the worker
threads. Queueing after `Shutdown` raises `EThreadPoolShutdown`. See
[Lifecycle & Shutdown](../guides/lifecycle-and-shutdown.md).

## When to switch to the bounded pool

The Simple pool's queue never runs out of room. When producers can outrun
consumers and memory must stay predictable, switch to
`ThreadPool.ProducerConsumer`, which bounds the queue and lets `TryQueue`
express a submission deadline. The decision table lives in
[Choose a Pool](choosing-a-pool.md), and the mechanics in
[Backpressure](../guides/backpressure.md).

## Learn by example

| Example | Teaches |
| --- | --- |
| [`Starter`](../../examples/Starter/Starter.lpr) | Smallest complete queue-and-wait program |
| [`SimpleDemo`](../../examples/SimpleDemo/SimpleDemo.lpr) | All four callback forms |
| [`TaskCoordination`](../../examples/TaskCoordination/TaskCoordination.lpr) | Tasks, batches, ranges, cancellation |
| [`ProdConSimpleDemo`](../../examples/ProdConSimpleDemo/ProdConSimpleDemo.lpr) | Owning a bounded pool |
| [`CoordinatedFileBackup`](../../examples/CoordinatedFileBackup/CoordinatedFileBackup.lpr) | A complete failure-aware workflow |

Next: the [Cheat Sheet](cheat-sheet.md) summarises the operational rules once
you know the basics.
