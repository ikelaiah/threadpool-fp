# ThreadPool.Simple API Documentation

> [!IMPORTANT]
> On Linux and macOS, `cthreads` must be the **first** unit in your program's
> `uses` clause, or creating the pool will raise a runtime access violation
> (exit code 217). Windows does not need it.

```pascal
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}  // must be first on Unix-like systems
  ThreadPool.Simple;
```

See the [official FPC documentation on `cthreads`](https://www.freepascal.org/docs-html/rtl/cthreads/index.html).

v0.9 observable `Submit` tasks, batches, ranges, and pending cancellation are
documented in the [ThreadPool.Tasks API](tasks-api.md). The queueing
surface below remains unchanged.

## Thread Pool Types

### GlobalThreadPool

A ready-to-use singleton instance declared in the `ThreadPool.Simple` unit.

- Created automatically at program startup — do **not** call `GlobalThreadPool.Free`
- Uses the default thread count (`ProcessorCount`, minimum 4)
- Suitable for most applications

```pascal
uses ThreadPool.Simple;

GlobalThreadPool.Queue(@MyProcedure);
GlobalThreadPool.WaitForAll;
```

The timeout overload returns `False` if work remains after the requested number
of milliseconds:

```pascal
if not GlobalThreadPool.WaitForAll(250) then
  WriteLn('Still running');
```

`0` is an immediate check. `THREADPOOL_INFINITE` waits indefinitely.

`WaitForAll` does not close admission. Coordinate concurrent producers first,
or use `Shutdown` to atomically stop admission before draining.

### TSimpleThreadPool

A manually managed pool for when you need explicit control over thread count or lifetime.

- Create with `TSimpleThreadPool.Create(AThreadCount)`
- Thread count is clamped: minimum 4, maximum 2× `ProcessorCount`
- Multiple independent pools can coexist
- Must be freed by the caller

```pascal
uses ThreadPool.Simple;

var
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    Pool.Queue(@MyProcedure);
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end;
```

---

## Queue Overloads

All four `Queue` overloads are thread-safe and can be called from any thread.

```pascal
// 1. Plain procedure — no shared state needed
GlobalThreadPool.Queue(@MyProcedure);

// 2. Object method — task needs access to object fields
GlobalThreadPool.Queue(@MyObject.MyMethod);

// 3. Indexed procedure — loop parallelism
GlobalThreadPool.Queue(@MyIndexedProcedure, 42);

// 4. Indexed method — loop parallelism + object state
GlobalThreadPool.Queue(@MyObject.MyIndexedMethod, 42);
```

### Type signatures (from `ThreadPool.Types`)

```pascal
TThreadProcedure      = procedure;
TThreadMethod         = procedure of object;
TThreadProcedureIndex = procedure(index: Integer);
TThreadMethodIndex    = procedure(index: Integer) of object;
```

---

## WaitForAll

Blocks the calling thread until every queued task has finished executing.

```pascal
GlobalThreadPool.WaitForAll;
```

Always call `WaitForAll` before:

- Reading results written by worker tasks
- Freeing objects whose methods were queued
- Calling `LastError`

---

## Error Handling

Worker thread exceptions are caught automatically and stored in `LastError`. The pool
continues processing remaining tasks after an exception.

```pascal
var
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    Pool.Queue(@RiskyProcedure);
    Pool.WaitForAll;

    if Pool.LastError <> '' then
    begin
      // LastError holds the raw exception message of the most recent failure.
      WriteLn('Error: ', Pool.LastError);
      Pool.ClearLastError;  // Reset before reusing the pool
    end;
  finally
    Pool.Free;
  end;
end;
```

### Collecting all errors (v0.7.0)

`LastError` only holds the most recent failure. To inspect **every** task error,
use the `Errors` collection (oldest first, capped at `MAX_STORED_ERRORS = 1000`):

```pascal
var
  Msg: string;
begin
  Pool.ClearErrors;
  for i := 0 to N - 1 do
    Pool.Queue(@RiskyProcedure);
  Pool.WaitForAll;

  WriteLn(Pool.ErrorCount, ' task(s) failed:');
  for Msg in Pool.Errors do
    WriteLn('  - ', Msg);

  Pool.ClearErrors;  // resets the collection and LastError
end;
```

### Reacting to errors as they happen (v0.7.0)

Assign `OnError` to be notified the moment a task fails, instead of polling after
`WaitForAll`:

```pascal
type
  TMyHandler = class
    procedure OnTaskError(const AMessage: string);
  end;

procedure TMyHandler.OnTaskError(const AMessage: string);
begin
  // NOTE: called synchronously from a worker thread. Keep it short, bounded,
  // and thread-safe; synchronize if you touch the UI or shared state.
  Log('task failed: ' + AMessage);
end;

Pool.OnError := @Handler.OnTaskError;
```

Exceptions raised by the handler are contained by the pool. They cannot
terminate a worker or prevent task completion accounting.

Containment does not limit callback execution time. A task or `OnError` handler
that blocks continues to occupy its worker, and can delay `WaitForAll` and
`Shutdown`. Apply an application-level timeout or cancellation mechanism to
operations that may block.

## Lifecycle and submission timeouts (v0.8.0)

`TSimpleThreadPool` is unbounded, so `TryQueue` normally succeeds immediately;
the timeout argument exists so code can use either pool through `IThreadPool`.
After shutdown begins, `Queue` and `TryQueue` raise `EThreadPoolShutdown`.

```pascal
Pool.TryQueue(@MyProcedure, 0);
Pool.Shutdown; // stop admission, drain accepted work, join workers
```

`State` progresses once from `tpsAccepting` to `tpsDraining` to `tpsStopped`.
`Shutdown` is idempotent and destruction calls it automatically.

## Properties

```pascal
property LastError: string;            // Raw message of the most recent worker exception
property Errors: TStringArray;         // All captured messages (oldest first, capped)
property ErrorCount: Integer;          // Number of messages currently in Errors
property OnError: TThreadPoolErrorEvent; // Fired (on a worker thread) per failed task
property ThreadCount: Integer;         // Number of worker threads (read-only)
property State: TThreadPoolState;       // Accepting, draining, or stopped
```

## Methods

```pascal
procedure Queue(AProcedure: TThreadProcedure);
procedure Queue(AMethod: TThreadMethod);
procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
function TryQueue(...; ATimeoutMS: Cardinal): Boolean; // four matching overloads
procedure WaitForAll; overload;
function WaitForAll(ATimeoutMS: Cardinal): Boolean; overload;
procedure Shutdown;
procedure ClearLastError;
procedure ClearErrors;  // clears both Errors and LastError
```

---

## Usage Examples

### 1. Simple procedure

```pascal
procedure PrintHello;
begin
  WriteLn('Hello from thread ', GetCurrentThreadId);
end;

begin
  GlobalThreadPool.Queue(@PrintHello);
  GlobalThreadPool.WaitForAll;
end;
```

### 2. Object method

> **Warning:** do not free `MyObject` until after `WaitForAll` returns. Worker
> threads hold a reference to the object's method and will crash if the object
> is freed while they are still running.

```pascal
type
  TMyClass = class
    procedure ProcessData;
  end;

procedure TMyClass.ProcessData;
begin
  WriteLn('Processing in thread ', GetCurrentThreadId);
end;

var
  MyObject: TMyClass;
begin
  MyObject := TMyClass.Create;
  try
    GlobalThreadPool.Queue(@MyObject.ProcessData);
    GlobalThreadPool.WaitForAll;  // must finish before Free below
  finally
    MyObject.Free;
  end;
end;
```

### 3. Indexed procedure (loop parallelism)

```pascal
procedure ProcessItem(index: Integer);
begin
  WriteLn('Item ', index, ' in thread ', GetCurrentThreadId);
end;

var
  i: Integer;
begin
  for i := 0 to 9 do
    GlobalThreadPool.Queue(@ProcessItem, i);

  GlobalThreadPool.WaitForAll;
end;
```

### 4. Custom pool with error check

```pascal
var
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    Pool.Queue(@RiskyProcedure);
    Pool.WaitForAll;

    if Pool.LastError <> '' then
      WriteLn('Task failed: ', Pool.LastError);
  finally
    Pool.Free;
  end;
end;
```

---

## Common Pitfalls

### Freeing an object before WaitForAll

```pascal
// WRONG — worker thread may still be calling MyObject.ProcessData
MyObject := TMyClass.Create;
GlobalThreadPool.Queue(@MyObject.ProcessData);
MyObject.Free;  // access violation risk

// CORRECT
MyObject := TMyClass.Create;
try
  GlobalThreadPool.Queue(@MyObject.ProcessData);
  GlobalThreadPool.WaitForAll;
finally
  MyObject.Free;
end;
```

### Calling Free on GlobalThreadPool

```pascal
// WRONG — the unit's finalization block already does this
GlobalThreadPool.Free;  // double-free at program exit

// CORRECT — just use it; no manual cleanup needed
GlobalThreadPool.Queue(@MyProcedure);
GlobalThreadPool.WaitForAll;
```

### Forgetting WaitForAll

```pascal
// WRONG — program may exit before tasks finish
for i := 0 to 99 do
  GlobalThreadPool.Queue(@ProcessItem, i);
// results are incomplete or uninitialized here

// CORRECT
for i := 0 to 99 do
  GlobalThreadPool.Queue(@ProcessItem, i);
GlobalThreadPool.WaitForAll;
// results are now complete
```
