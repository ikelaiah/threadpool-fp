# Tasks & Batches

`ThreadPool.Tasks` adds observable task handles and thread-safe batches to both
pools. Use `Submit` when one piece of work needs a handle; use `Queue` for the
smallest fire-and-forget path.

```pascal
uses
  ThreadPool.Tasks, ThreadPool.Simple;
```

## Submitting a task

Both pools provide four `Submit` overloads matching the callback forms:

```pascal
Task := Pool.Submit(@DoWork);                 // procedure
Task := Pool.Submit(@Worker.DoWork);          // object method
Task := Pool.Submit(@ProcessItem, I);         // indexed procedure
Task := Pool.Submit(@Worker.ProcessItem, I);  // indexed object method
```

`Submit` returns a non-nil `IThreadPoolTask` for accepted work. On the bounded
pool it uses the same compatibility timeout as `Queue` and raises
`EQueueFullException` if that deadline expires. The handle:

- manages its own lifetime;
- does not retain the pool; and
- stays readable after the pool has been shut down and freed.

## Task states

| State | Meaning |
| --- | --- |
| `ttsPending` | Accepted but no worker has claimed it |
| `ttsRunning` | A worker won the start transition |
| `ttsCompleted` | Callback returned normally |
| `ttsFailed` | Callback raised an exception |
| `ttsCancelled` | Cancellation won before worker start |

`IsFinished` is `True` for completed, failed, or cancelled tasks.

## Waiting on a task

```pascal
Task.WaitFor;                         // no deadline
Finished := Task.WaitFor(100);        // milliseconds
Finished := Task.WaitFor(0);          // immediate check
Finished := Task.WaitFor(THREADPOOL_INFINITE);
```

- A `True` result means the task reached a terminal state. Inspect `State` and `ErrorMessage` to distinguish completion, failure, and cancellation.
- Waiting never re-raises a worker exception.
- Multiple threads may wait on the same task.
- A callback cannot wait for its own handle: the library raises `EThreadPoolDeadlock` rather than letting a worker block forever. The same guard protects an `OnError` handler that waits on the failing task.

## Failure reporting

A failed task stores its message in both the task's `ErrorMessage` and the
existing pool `LastError`/`Errors`/`OnError` surface. `OnError` still runs
synchronously on the worker before the task becomes terminal.

## Timeout-aware submission

Matching `TrySubmit` overloads return `False`, without raising, when bounded
admission reaches its deadline; the output task is then `nil`:

```pascal
if not Pool.TrySubmit(@DoWork, 50, Task) then
  WriteLn('No queue space became available within 50 ms');
```

For indexed callbacks the order is callback, index, timeout, output task:

```pascal
Accepted := Pool.TrySubmit(@ProcessItem, I, 50, Task);
```

The Simple pool is unbounded, so `TrySubmit` normally returns `True` and its
timeout exists for `IThreadPoolTaskSource` symmetry. Both pools raise
`EThreadPoolShutdown` once shutdown has begun.

## Batches

`IThreadPoolTaskBatch` groups handles that belong together.

```pascal
Batch := NewThreadPoolTaskBatch;
for I := 0 to High(Items) do
  Batch.Add(Pool.Submit(@ProcessItem, I));

if not Batch.WaitFor(1000) then
  WriteLn('Some tasks remain');
```

| Member | Meaning |
| --- | --- |
| `Add(Task)` | Add one non-nil handle |
| `WaitFor` | Wait indefinitely for the current snapshot |
| `WaitFor(TimeoutMS)` | Wait using one overall deadline |
| `CancelPending` | Cancel every still-pending entry; returns wins |
| `Count` | Number of entries |
| `FinishedCount` | Completed, failed, or cancelled entries |
| `FailedCount` | Failed entries |
| `CancelledCount` | Cancelled entries |
| `Tasks[Index]` | Read an individual handle |

Batch rules worth knowing:

- An empty batch is already finished.
- `WaitFor` snapshots the entries **when the call starts**, so a producer may keep adding tasks for a later wait.
- Adding the same handle twice creates two entries; `Add(nil)` raises an argument exception.
- Count properties are thread-safe snapshots, but tasks can change state immediately after you read a count.

## Recipe: one task, wait, inspect

```pascal
Task := GlobalThreadPool.Submit(@DoWork);
if Task.WaitFor(250) and (Task.State = ttsFailed) then
  WriteLn(Task.ErrorMessage);
```

See the [`TaskCoordination`](../../examples/TaskCoordination/TaskCoordination.lpr) example for the
same ideas in a complete program, and [Common Recipes](recipes.md) for
task-oriented recipes.

## Related

- [Parallel Ranges](ranges.md) — `SubmitRange` returns a populated batch.
- [Cancellation](cancellation.md)
- [Tasks API](../reference/tasks-api.md)
