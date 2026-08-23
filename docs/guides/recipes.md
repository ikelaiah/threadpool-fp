# Common Recipes

Each recipe below is a **complete, compiled program**. The code blocks are
the exact sources under `examples/documentation/`,
compiled with FPC 3.2.2 and checked against the stated output by
`tools/test_docs_examples.py`. Copy the whole program, or the relevant lines,
into your own project.

All programs follow the same shape: create the pool (or use `GlobalThreadPool`),
queue work, **wait**, then read results. Study the [Beginner Guide]
(../getting-started/beginner-guide.md) if the pieces are unfamiliar.

## Queue an indexed procedure over a range

**Problem:** Process every index in an array with one indexed callback.

**Recommended API:** `GlobalThreadPool.Queue(@Procedure, Index)` — the `TThreadProcedureIndex` callback form.

```pascal
program ParallelIndexProcessing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  ThreadPool.Simple;

const
  N = 100;

var
  Results: array[0..N - 1] of Integer;
  I, Total: Integer;

procedure Compute(Index: Integer);
begin
  Results[Index] := Index * Index;
end;

begin
  for I := 0 to N - 1 do
    GlobalThreadPool.Queue(@Compute, I);

  GlobalThreadPool.WaitForAll;

  Total := 0;
  for I := 0 to N - 1 do
    Inc(Total, Results[I]);

  WriteLn('Squares total: ', Total);
end.```

**Expected output:**

`Squares total: 328350`

**Caveat:** each callback writes a **distinct** `Results[Index]` slot, which is safe without locking; callbacks that touch shared fields or counters need their own synchronization (see the object-method recipe). The submission order does not predict completion order.

[Source program](../../examples/documentation/01_parallel_index_processing.pas)

## Use an object method as the callback

**Problem:** Have callbacks update object state instead of free-standing procedures.

**Recommended API:** `Queue(@Object.Method, Amount)` — the `TThreadMethodIndex` callback form.

```pascal
program ObjectMethodCallback;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils, SyncObjs,
  ThreadPool.Simple;

type
  TCounter = class
  private
    FLock: TCriticalSection;
    FValue: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Amount: Integer);
    function GetValue: Integer;
  end;

constructor TCounter.Create;
begin
  FLock := TCriticalSection.Create;
  FValue := 0;
end;

destructor TCounter.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TCounter.Add(Amount: Integer);
begin
  FLock.Enter;
  try
    Inc(FValue, Amount);
  finally
    FLock.Leave;
  end;
end;

function TCounter.GetValue: Integer;
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

var
  Counter: TCounter;
  I: Integer;
begin
  Counter := TCounter.Create;
  try
    for I := 1 to 10 do
      GlobalThreadPool.Queue(@Counter.Add, 1);

    GlobalThreadPool.WaitForAll;

    WriteLn('Counted ', Counter.GetValue, ' items');
  finally
    Counter.Free;
  end;
end.```

**Expected output:**

`Counted 10 items`

**Caveat:** the queued method holds a live reference to `Counter`. `WaitForAll` must return before `Counter.Free`, or a still-running method dereferences a freed object.

[Source program](../../examples/documentation/02_object_method_callback.pas)

## Wait for a task with a deadline

**Problem:** Wait for one piece of work, but give up after a fixed number of milliseconds.

**Recommended API:** `Submit` then `Task.WaitFor(TimeoutMS)`.

```pascal
program WaitTimeout;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple, ThreadPool.Tasks;

procedure Pause;
begin
  Sleep(20);
end;

var
  Task: IThreadPoolTask;
begin
  Task := GlobalThreadPool.Submit(@Pause);

  if Task.WaitFor(2000) then
    WriteLn('Finished within 2 s')
  else
    WriteLn('Still busy');
end.```

**Expected output:**

`Finished within 2 s`

**Caveat:** `WaitFor(TimeoutMS)` returns `True` for any terminal state (completed, failed, or cancelled). Inspect `Task.State` to distinguish them.

[Source program](../../examples/documentation/03_wait_timeout.pas)

## Submit a task and inspect its state

**Problem:** Get a handle to one job so you can inspect whether it completed.

**Recommended API:** `Submit` plus `IThreadPoolTask.State`.

```pascal
program ObserveOneTask;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple, ThreadPool.Tasks;

procedure DoSomething;
begin
  Sleep(5);
end;

var
  Task: IThreadPoolTask;
begin
  Task := GlobalThreadPool.Submit(@DoSomething);

  if Task.WaitFor(1000) and (Task.State = ttsCompleted) then
    WriteLn('Task completed')
  else
    WriteLn('Task did not complete: ', Task.ErrorMessage);
end.```

**Expected output:**

`Task completed`

**Caveat:** `WaitFor(0)` is an immediate check and may return `False` while the task is still pending or running.

[Source program](../../examples/documentation/04_observe_one_task.pas)

## Coordinate several tasks in a batch

**Problem:** Wait for several jobs as one group and report how many finished.

**Recommended API:** `NewThreadPoolTaskBatch`, `Batch.Add`, `Batch.WaitFor`, and the count properties.

```pascal
program BatchTasks;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple, ThreadPool.Tasks;

procedure Nop;
begin
  Sleep(1);
end;

var
  Batch: IThreadPoolTaskBatch;
  I: Integer;
begin
  Batch := NewThreadPoolTaskBatch;
  for I := 1 to 5 do
    Batch.Add(GlobalThreadPool.Submit(@Nop));

  Batch.WaitFor;

  WriteLn(Batch.Count, ' tasks, ',
          Batch.FinishedCount, ' finished, ',
          Batch.FailedCount, ' failed');
end.```

**Expected output:**

`5 tasks, 5 finished, 0 failed`

**Caveat:** `WaitFor` waits on the entries present when the call starts. Count properties are thread-safe snapshots; a task can change state immediately after you read one.

[Source program](../../examples/documentation/05_batch_tasks.pas)

## Process an inclusive index range in parallel

**Problem:** Parallelize a large fixed index range without submitting one task per index.

**Recommended API:** `GlobalThreadPool.SubmitRange(@Procedure, First, Last)`.

```pascal
program SubmitRangeChunking;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  ThreadPool.Simple, ThreadPool.Tasks;

const
  N = 200;

var
  Results: array[0..N - 1] of Integer;
  I, Total: Integer;
  RangeTasks: IThreadPoolTaskBatch;

procedure Compute(Index: Integer);
begin
  Results[Index] := Index * 2;
end;

begin
  RangeTasks := GlobalThreadPool.SubmitRange(@Compute, 0, N - 1);
  RangeTasks.WaitFor;

  Total := 0;
  for I := 0 to N - 1 do
    Inc(Total, Results[I]);

  WriteLn('Range total: ', Total);
end.```

**Expected output:**

`Range total: 39800`

**Caveat:** bounds are inclusive. Automatic chunking queues at most `ThreadCount * 4` tasks, each handling a contiguous range. `RangeTasks.WaitFor` blocks until every chunk finished.

[Source program](../../examples/documentation/06_submit_range.pas)

## Cancel a task that has not started

**Problem:** Revoke one queued job before a worker picks it up.

**Recommended API:** `IThreadPoolTask.Cancel`.

```pascal
program CancelPendingTask;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SyncObjs,
  ThreadPool.Simple, ThreadPool.Tasks, ThreadPool.ProducerConsumer;

var
  Gate: TEvent;

procedure HoldCallbacks;
begin
  Gate.WaitFor(INFINITE);
end;

procedure Unused;
begin
end;

var
  Pool: TProducerConsumerThreadPool;
  Tasks: array[1..4] of IThreadPoolTask;
  Task: IThreadPoolTask;
  I: Integer;
begin
  Gate := TEvent.Create(nil, True, False, '');
  Pool := TProducerConsumerThreadPool.Create(4, 16);
  try
    { Occupy every worker so the task below stays pending. }
    for I := 1 to 4 do
      Tasks[I] := Pool.Submit(@HoldCallbacks);

    Task := Pool.Submit(@Unused);

    if Task.Cancel then
      WriteLn('Cancelled while pending')
    else
      WriteLn('Could not cancel');

    Gate.SetEvent;
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end.```

**Expected output:**

`Cancelled while pending`

**Caveat:** `Cancel` wins only while the task is `ttsPending`; the four holders below keep every worker busy so this stays deterministic. It never interrupts a running callback, and it is not an error (no `OnError`, no `LastError`).

[Source program](../../examples/documentation/07_cancel_pending.pas)

## Collect every worker failure

**Problem:** See the details of every task that raised, not just the last one.

**Recommended API:** `Errors`, `ErrorCount`, and `LastError` after `WaitForAll`.

```pascal
program CollectErrors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple;

procedure Fail;
begin
  raise Exception.Create('boom');
end;

var
  I: Integer;
begin
  GlobalThreadPool.ClearErrors;

  for I := 1 to 3 do
    GlobalThreadPool.Queue(@Fail);

  GlobalThreadPool.WaitForAll;

  WriteLn(GlobalThreadPool.ErrorCount, ' error(s) recorded');
  if GlobalThreadPool.ErrorCount > 0 then
    WriteLn(GlobalThreadPool.LastError);
end.```

**Expected output:**

```text
3 error(s) recorded
boom
```

**Caveat:** `Errors` is capped at `MAX_STORED_ERRORS = 1000` (oldest entries dropped). Clear with `ClearErrors` before reusing the pool so stale messages do not linger.

[Source program](../../examples/documentation/08_collect_errors.pas)

## React to failures as they happen

**Problem:** Be notified the moment a task fails instead of polling after `WaitForAll`.

**Recommended API:** assign `Pool.OnError`.

```pascal
program OnErrorCallback;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils, SyncObjs,
  ThreadPool.Simple;

type
  TErrorRecorder = class
  private
    FLock: TCriticalSection;
    FCount: Integer;
  public
    constructor Create;
    procedure Handle(const AMessage: string);
    function GetCount: Integer;
  end;

constructor TErrorRecorder.Create;
begin
  FLock := TCriticalSection.Create;
  FCount := 0;
end;

procedure TErrorRecorder.Handle(const AMessage: string);
begin
  FLock.Enter;
  try
    Inc(FCount);
  finally
    FLock.Leave;
  end;
end;

function TErrorRecorder.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

procedure Fail;
begin
  raise Exception.Create('worker failure');
end;

var
  Recorder: TErrorRecorder;
  I: Integer;
begin
  Recorder := TErrorRecorder.Create;
  try
    GlobalThreadPool.OnError := @Recorder.Handle;
    GlobalThreadPool.ClearErrors;

    for I := 1 to 3 do
      GlobalThreadPool.Queue(@Fail);

    GlobalThreadPool.WaitForAll;

    WriteLn('OnError fired ', Recorder.GetCount, ' time(s)');
  finally
    GlobalThreadPool.OnError := nil;
    Recorder.Free;
  end;
end.```

**Expected output:**

`OnError fired 3 time(s)`

**Caveat:** the handler runs **synchronously on the worker thread** that caught the error. Keep it short, bounded, and thread-safe; the pool contains handler exceptions, but a blocking handler delays `WaitForAll` and `Shutdown`.

[Source program](../../examples/documentation/09_onerror_callback.pas)

## Bound a producer with TryQueue

**Problem:** Submit work to a bounded pool but stop cleanly when the queue is full.

**Recommended API:** `TProducerConsumerThreadPool.TryQueue(..., TimeoutMS)`.

```pascal
program BoundedTryQueue;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils, SyncObjs,
  ThreadPool.Types, ThreadPool.Simple, ThreadPool.Tasks, ThreadPool.ProducerConsumer;

var
  Gate: TEvent;

procedure HoldCallbacks;
begin
  Gate.WaitFor(INFINITE);
end;

procedure Unused;
begin
end;

var
  Pool: TProducerConsumerThreadPool;
  I, Accepted: Integer;
begin
  Gate := TEvent.Create(nil, True, False, '');
  Pool := TProducerConsumerThreadPool.Create(4, 8);
  try
    { Occupy every worker so the queue fills deterministically. }
    for I := 1 to 4 do
      Pool.TryQueue(@HoldCallbacks, THREADPOOL_INFINITE);

    { Wait until the blockers are all held by workers. }
    while Pool.QueueCount > 0 do
      Sleep(1);

    Accepted := 0;
    for I := 1 to 100 do
    begin
      if Pool.TryQueue(@Unused, 0) then
        Inc(Accepted)
      else
        Break;
    end;

    WriteLn('Accepted ', Accepted, ' queued items');

    Gate.SetEvent;
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end.```

**Expected output:**

`Accepted 8 queued items`

**Caveat:** `TryQueue` returns `False` instead of raising `EQueueFullException`. The four holders block every worker, so 8 (the full capacity) deterministic items are accepted before the queue reports full.

[Source program](../../examples/documentation/10_bounded_tryqueue.pas)

## Monitor bounded-queue utilisation

**Problem:** Watch how full a bounded queue is while producers run.

**Recommended API:** `QueueCount`, `QueueCapacity`, and `QueueLoadFactor`.

```pascal
program QueueMetrics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils, SyncObjs,
  ThreadPool.Types, ThreadPool.Simple, ThreadPool.Tasks, ThreadPool.ProducerConsumer;

var
  Gate: TEvent;

procedure HoldCallbacks;
begin
  Gate.WaitFor(INFINITE);
end;

procedure Unused;
begin
end;

var
  Pool: TProducerConsumerThreadPool;
  I: Integer;
begin
  Gate := TEvent.Create(nil, True, False, '');
  Pool := TProducerConsumerThreadPool.Create(4, 32);
  try
    { Occupy every worker so nothing drains while we inspect the queue. }
    for I := 1 to 4 do
      Pool.TryQueue(@HoldCallbacks, THREADPOOL_INFINITE);

    { Wait until the blockers are all held by workers. }
    while Pool.QueueCount > 0 do
      Sleep(1);

    for I := 1 to 3 do
      Pool.TryQueue(@Unused, THREADPOOL_INFINITE);

    WriteLn('Capacity: ', Pool.QueueCapacity);
    WriteLn('Queued: ', Pool.QueueCount);
    WriteLn('Load: ', Pool.QueueLoadFactor:0:2);

    Gate.SetEvent;
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end.```

**Expected output:**

```text
Capacity: 32
Queued: 3
Load: 0.09
```

**Caveat:** all three are read-only snapshots; the values change the moment a worker dequeues or a producer enqueues. Use them for monitoring and coarse decisions, not precise accounting.

[Source program](../../examples/documentation/11_queue_metrics.pas)

## Drain and stop a pool with Shutdown

**Problem:** Finish everything accepted so far, then stop admitting new work.

**Recommended API:** `Shutdown` (called explicitly, or implicitly by the destructor).

```pascal
program ShutdownDrain;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SyncObjs,
  ThreadPool.Types, ThreadPool.Simple;

type
  TCounter = class
  private
    FLock: TCriticalSection;
    FValue: Integer;
  public
    constructor Create;
    procedure Add(Amount: Integer);
    function GetValue: Integer;
  end;

constructor TCounter.Create;
begin
  FLock := TCriticalSection.Create;
  FValue := 0;
end;

procedure TCounter.Add(Amount: Integer);
begin
  FLock.Enter;
  try
    Inc(FValue, Amount);
  finally
    FLock.Leave;
  end;
end;

function TCounter.GetValue: Integer;
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

var
  Counter: TCounter;
  I: Integer;
begin
  Counter := TCounter.Create;
  try
    for I := 1 to 100 do
      GlobalThreadPool.Queue(@Counter.Add, 1);

    GlobalThreadPool.Shutdown;

    WriteLn('Counted: ', Counter.GetValue);
    WriteLn('Stopped: ', GlobalThreadPool.State = tpsStopped);
  finally
    Counter.Free;
  end;
end.```

**Expected output:**

```text
Counted: 100
Stopped: TRUE
```

**Caveat:** once `Shutdown` begins, further `Queue`, `TryQueue`, `Submit`, and `TrySubmit` calls raise `EThreadPoolShutdown`. A pool cannot be restarted. Do not call `Shutdown` from one of the pool's own workers.

[Source program](../../examples/documentation/12_shutdown_drain.pas)

