# ThreadPool.Tasks API

`ThreadPool.Tasks` adds observable task handles, batches, chunked ranges, and
pending-work cancellation to both thread pools. It requires Free Pascal 3.2.2
or later and uses only FCL units.

On Unix-like systems, `cthreads` must remain the first unit in the program's
`uses` clause.

```pascal
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  ThreadPool.Tasks, ThreadPool.Simple;
```

The existing v0.8 `Queue` API remains available. Use `Submit` when one piece of
work needs a handle; use `Queue` for the smallest fire-and-forget path.

## Individual tasks

Both `TSimpleThreadPool` and `TProducerConsumerThreadPool` provide four
`Submit` overloads matching the existing callback forms:

```pascal
function Submit(AProcedure: TThreadProcedure): IThreadPoolTask;
function Submit(AMethod: TThreadMethod): IThreadPoolTask;
function Submit(AProcedure: TThreadProcedureIndex;
  AIndex: Integer): IThreadPoolTask;
function Submit(AMethod: TThreadMethodIndex;
  AIndex: Integer): IThreadPoolTask;
```

The returned interface manages its own lifetime. It does not own or retain the
pool and remains readable after the pool has been shut down and freed.

```pascal
Task := Pool.Submit(@DoWork);

if not Task.WaitFor(250) then
  WriteLn('Task has not finished')
else
  case Task.State of
    ttsCompleted: WriteLn('Done');
    ttsFailed: WriteLn('Failed: ', Task.ErrorMessage);
    ttsCancelled: WriteLn('Cancelled');
  end;
```

### Task states

| State | Meaning |
| --- | --- |
| `ttsPending` | Accepted but no worker has claimed it |
| `ttsRunning` | A worker won the start transition |
| `ttsCompleted` | Callback returned normally |
| `ttsFailed` | Callback raised an exception |
| `ttsCancelled` | Cancellation won before worker start |

`IsFinished` is true for completed, failed, and cancelled tasks.

### Waiting

```pascal
Task.WaitFor;                         // no deadline
Finished := Task.WaitFor(100);        // milliseconds
Finished := Task.WaitFor(0);          // immediate check
Finished := Task.WaitFor(THREADPOOL_INFINITE);
```

A `True` result means that the task reached any terminal state. Waiting does
not re-raise a worker exception; inspect `State` and `ErrorMessage`.

Multiple threads may observe or wait for the same task. Completion events are
created lazily only when unfinished work is actually waited on.

A callback cannot wait for its own task handle. That would prevent its terminal
transition, so the library raises `EThreadPoolDeadlock` instead. The same guard
also protects an `OnError` handler that tries to wait for the failing task.

### Failure reporting

A failed submitted task reports its message in two places:

- the task's `ErrorMessage`; and
- the existing pool-level `LastError`, `Errors`, and `OnError` API.

`OnError` still runs synchronously on the worker. A task becomes terminal after
the error callback returns, preserving the existing completion behaviour.

## Timeout-aware submission

Each callback form also has a `TrySubmit` overload:

```pascal
if not Pool.TrySubmit(@DoWork, 50, Task) then
  WriteLn('Queue remained full for 50 ms');
```

For indexed callbacks, the order is callback, index, timeout, output task:

```pascal
Accepted := Pool.TrySubmit(@ProcessItem, I, 50, Task);
```

On the bounded Producer-Consumer pool, `False` means that admission reached its
deadline, and the output task is `nil`. `Submit` uses the same bounded
compatibility timeout as `Queue` and raises `EQueueFullException` on expiry.

The Simple pool is unbounded, so `TrySubmit` normally returns `True` and its
timeout exists for API symmetry. Both pools raise `EThreadPoolShutdown` if
shutdown has begun.

## Cancelling pending work

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

- It returns `True` only for `pending -> cancelled`.
- It is safe to call repeatedly.
- It never terminates a worker or interrupts a running callback.
- Cancellation is not an error and does not fire `OnError`.

A cancelled item remains as a tombstone until a worker reaches its queue
position. Its handle is terminal immediately and its callback is skipped. In a
bounded queue, the tombstone can occupy its slot briefly; v0.9 deliberately
avoids an expensive queue-wide removal scan.

If a submitted object method cannot be cancelled because it is running, keep
the callback target alive until the task finishes. A successful cancellation
guarantees that the callback will not be invoked.

## Batches

Create a batch and add handles from either pool:

```pascal
Batch := NewThreadPoolTaskBatch;
for I := 0 to High(Items) do
  Batch.Add(Pool.Submit(@ProcessItem, I));

if not Batch.WaitFor(1000) then
  WriteLn('Some tasks remain');
```

`IThreadPoolTaskBatch` provides:

| Member | Meaning |
| --- | --- |
| `Add(Task)` | Add one non-nil task handle |
| `WaitFor` | Wait indefinitely for the current snapshot |
| `WaitFor(TimeoutMS)` | Wait using one overall deadline |
| `CancelPending` | Attempt every pending cancellation; return wins |
| `Count` | Number of entries |
| `FinishedCount` | Completed, failed, or cancelled entries |
| `FailedCount` | Failed entries |
| `CancelledCount` | Cancelled entries |
| `Tasks[Index]` | Read an individual handle |

An empty batch is already finished. `WaitFor` snapshots the entries at call
entry; tasks added concurrently are included in a later wait. Adding the same
handle twice creates two entries. Count properties are thread-safe snapshots,
but tasks can change state immediately after a count is read.

## Chunked ranges

`SubmitRange` submits an inclusive integer range and returns a populated batch:

```pascal
RangeTasks := Pool.SubmitRange(@ProcessItem, 0, High(Items));
RangeTasks.WaitFor;
```

Procedure and object-method indexed callbacks are supported. Bounds follow
Pascal's `for I := First to Last` convention:

- `First > Last` returns an empty batch;
- `AChunkSize = 0` selects automatic chunking;
- `AChunkSize > 0` selects that many indexes per queued task; and
- a negative chunk size raises `EArgumentOutOfRangeException`.

Automatic mode creates at most `ThreadCount * 4` chunks. Range arithmetic uses
64-bit intermediates, so valid `Integer` bounds do not overflow while chunks
are calculated.

One batch entry represents one chunk. Cancelling a pending entry skips the
whole chunk; a running chunk finishes. Smaller explicit chunks trade queue
overhead for finer cancellation and load balancing.

If an index callback raises, that chunk stops and becomes failed while other
chunks continue. Catch exceptions inside the callback when every index must be
attempted.

The bounded pool rejects `SubmitRange` from one of its own workers with
`EThreadPoolDeadlock`. Blocking a fixed worker set while it tries to feed its
own full queue can otherwise starve the pool. Submit bounded ranges from a
coordinating thread. The coordinating call may wait for queue space until all
of its small set of chunks has been accepted.

## Capability interface

New interface-oriented code can depend on `IThreadPoolTaskSource`. It describes
the common `Submit`, `TrySubmit`, and `SubmitRange` surface implemented by both
pools.

The v0.8 `IThreadPool` interface and GUID are unchanged. This is intentional:
third-party `IThreadPool` implementations continue to compile without adding
v0.9 methods.

## Examples

See [`examples/TaskCoordination`](../../examples/TaskCoordination/TaskCoordination.lpr) for individual
waiting, batches, ranges, state inspection, and best-effort pending
cancellation in one small program.

Two production-shaped examples show how those pieces fit into complete
workflows:

- [`CoordinatedFileBackup`](../../examples/CoordinatedFileBackup/CoordinatedFileBackup.lpr) submits one observable task per file, reports batch progress, reacts to a critical failure, cancels only work that has not started, and waits before releasing callback-owned state.
- [`ParallelLogAnalyzer`](../../examples/ParallelLogAnalyzer/ParallelLogAnalyzer.lpr) analyzes 100,000 records with automatic range chunking, uses the range batch as a phase barrier, then coordinates independent report sections as a second batch.
