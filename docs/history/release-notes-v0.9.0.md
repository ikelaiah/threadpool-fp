# ThreadPool for Free Pascal v0.9.0

v0.9.0 adds observable work and coordination while keeping the v0.8 queueing
surface source-compatible.

## Highlights

- `Submit` returns an `IThreadPoolTask` that can be observed and waited.
- `TrySubmit` combines a task handle with bounded admission deadlines.
- `Cancel` prevents a pending callback from starting without terminating a
  worker thread.
- `IThreadPoolTaskBatch` coordinates related handles and cancels their pending
  work as a group.
- `SubmitRange` divides inclusive integer ranges into a small number of chunks.
- Both Simple and Producer-Consumer pools implement the same task semantics.

## Quick start

```pascal
uses
  ThreadPool.Tasks, ThreadPool.Simple;

var
  Task: IThreadPoolTask;
  Batch: IThreadPoolTaskBatch;
begin
  Task := GlobalThreadPool.Submit(@DoWork);
  Task.WaitFor;

  Batch := GlobalThreadPool.SubmitRange(@ProcessItem, 0, 999);
  Batch.WaitFor;
end;
```

See the [task API](../reference/tasks-api.md) and
[`TaskCoordination`](../../examples/TaskCoordination/TaskCoordination.lpr) example for the complete
contract.

For real workflow patterns, see
[`CoordinatedFileBackup`](../../examples/CoordinatedFileBackup/CoordinatedFileBackup.lpr) for observation,
failure policy, and pending cancellation, and
[`ParallelLogAnalyzer`](../../examples/ParallelLogAnalyzer/ParallelLogAnalyzer.lpr) for efficient range
chunking and phase coordination.

## Cancellation contract

Cancellation is intentionally narrow and safe. `Task.Cancel` succeeds only
while the task is pending. Once a worker transitions it to running,
cancellation returns `False` and the callback finishes normally.

Waiting for the current callback's own task would deadlock its terminal
transition, so it is rejected with `EThreadPoolDeadlock`.

Cancelled entries are skipped when dequeued. A cancelled handle becomes
terminal immediately, although the queue entry remains as a tombstone until a
worker reaches it. This keeps handles independent from pool lifetime and avoids
queue-wide removal scans.

## Range contract

Range bounds are inclusive. Automatic chunking creates at most four chunks per
worker; a positive explicit chunk size selects the number of indexes per task.
A range returns a batch whose entries represent chunks.

One index failure stops its chunk and marks that task failed. Other chunks
continue. A pending chunk cancellation skips all of its indexes, while a
running chunk finishes.

The bounded pool rejects nested `SubmitRange` from one of its own workers.
Blocking a fixed worker set while it submits to its own full queue can
otherwise deadlock through starvation.

## Compatibility

The existing `IThreadPool` interface and GUID are unchanged. The new
`IThreadPoolTaskSource` capability keeps third-party v0.8 implementations from
having to add methods.

Existing programs can continue using:

- all four `Queue` and `TryQueue` forms;
- pool-wide `WaitForAll` and draining `Shutdown`;
- `LastError`, `Errors`, and `OnError`;
- the existing constructors and managed `GlobalThreadPool`.

The test suite includes an independent v0.8-style `IThreadPool`
implementation as a compile sentinel.

## Performance and verification

Legacy `Queue` remains untracked. New task state is allocated only for
`Submit`, and its completion event is lazy.

The release benchmark now separates legacy queue, tracked submit, and range
paths. On the Windows/FPC 3.2.2 orientation run, the legacy 20,000-task median
remained within the 10% v0.8.5 budget, and chunking a 200,000-index Simple range
was over 60x faster than submitting each index as a tracked task.

The v0.9 suite contains 81 tests and reports no leaks under heap tracing.
