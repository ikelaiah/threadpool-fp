# ThreadPool.ProducerConsumer API Documentation

## Overview

`ThreadPool.ProducerConsumer` implements a thread pool backed by a fixed-size
circular queue with built-in backpressure. Use it when task production can
outpace consumption and you need predictable memory usage and overflow control.

For simpler use cases see `ThreadPool.Simple`.

> **Linux/macOS:** your program must list `cthreads` as the **first** unit in its
> `uses` clause, or creating the pool will raise a runtime access violation
> (exit code 217). Windows does not need it.
>
> ```pascal
> uses
>   {$IFDEF UNIX}cthreads,{$ENDIF}  // must be first on Unix-like systems
>   ThreadPool.ProducerConsumer;
> ```
>
> See the [official FPC documentation on `cthreads`](https://www.freepascal.org/docs-html/rtl/cthreads/index.html).

---

## Constructor

```pascal
constructor Create(AThreadCount: Integer = 0; AQueueSize: Integer = 1024);
```

| Parameter | Default | Description |
| --- | --- | --- |
| `AThreadCount` | `0` (uses CPU count) | Worker threads. Clamped: min 4, max 2× `ProcessorCount` |
| `AQueueSize` | `1024` | Circular queue capacity in work items |

```pascal
// Default: CPU-count threads, 1024-item queue
Pool := TProducerConsumerThreadPool.Create;

// Custom: 4 threads, 512-item queue
Pool := TProducerConsumerThreadPool.Create(4, 512);
```

---

## Queue Methods

All four overloads are thread-safe. A call waits only while the bounded queue is
actually full. If its compatibility timeout expires, `EQueueFullException` is
raised.

```pascal
procedure Queue(AProcedure: TThreadProcedure);
procedure Queue(AMethod: TThreadMethod);
procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
```

```pascal
Pool.Queue(@MyProcedure);            // plain procedure
Pool.Queue(@MyObject.MyMethod);      // object method
Pool.Queue(@MyIndexedProc, 42);      // indexed procedure
Pool.Queue(@MyObject.MyMethod, 42);  // indexed method
```

Use the matching `TryQueue` overloads when queue saturation is expected. They
return `False` instead of raising:

```pascal
if not Pool.TryQueue(@MyProcedure, 50) then
  WriteLn('No queue space became available within 50 ms');
```

Timeouts are milliseconds. `0` does not wait and `THREADPOOL_INFINITE` waits
without a deadline.

### Task type signatures (from `ThreadPool.Types`)

```pascal
TThreadProcedure      = procedure;
TThreadMethod         = procedure of object;
TThreadProcedureIndex = procedure(index: Integer);
TThreadMethodIndex    = procedure(index: Integer) of object;
```

---

## WaitForAll

Blocks until every queued task has finished executing.

```pascal
Pool.WaitForAll;
```

```pascal
if not Pool.WaitForAll(250) then
  WriteLn('Tasks are still running');
```

`WaitForAll` does not close admission. Coordinate concurrent producers first,
or use `Shutdown` to atomically stop admission before draining.

Always call before freeing the pool or any objects whose methods were queued.

---

## Error Handling

Two distinct error paths exist — handle both:

### 1. Queue-full errors (raised by `Queue`)

`EQueueFullException` is raised synchronously on the calling thread when the
queue stays full until the compatibility timeout expires. Catch it **around each `Queue` call**,
not around `WaitForAll`.

```pascal
try
  Pool.Queue(@MyProcedure);
except
  on E: EQueueFullException do
  begin
    // Queue is saturated. Options:
    // - wait and retry: Pool.WaitForAll; Pool.Queue(@MyProcedure);
    // - log and skip
    WriteLn('Queue full: ', E.Message);
  end;
end;
```

Always catch by **type** (`EQueueFullException`), not by message string.

### 2. Worker execution errors (read from `LastError`)

Exceptions that occur *inside* a task are caught by the worker thread and stored
in `LastError`. Check after `WaitForAll`.

```pascal
Pool.WaitForAll;

if Pool.LastError <> '' then
begin
  // LastError holds the raw message of the most recent worker exception.
  WriteLn('Task error: ', Pool.LastError);
  Pool.ClearLastError;  // reset before reuse
end;
```

To inspect **every** task failure (not just the last), use the `Errors`
collection added in v0.7.0 — oldest first, capped at `MAX_STORED_ERRORS = 1000`:

```pascal
var
  Msg: string;
begin
  Pool.ClearErrors;
  // ... queue tasks ...
  Pool.WaitForAll;

  WriteLn(Pool.ErrorCount, ' task(s) failed:');
  for Msg in Pool.Errors do
    WriteLn('  - ', Msg);
  Pool.ClearErrors;  // resets the collection and LastError
end;
```

Or assign `OnError` to react the moment a task fails, instead of polling:

```pascal
// Called from a worker thread — keep it short and thread-safe.
Pool.OnError := @Handler.OnTaskError;
```

Exceptions raised by the handler are contained by the pool. They cannot
terminate a worker or prevent task completion accounting.

### Full pattern

```pascal
var
  Pool: TProducerConsumerThreadPool;
begin
  Pool := TProducerConsumerThreadPool.Create;
  try
    try
      Pool.Queue(@RiskyOperation);
    except
      on E: EQueueFullException do
        WriteLn('Queue full: ', E.Message);
    end;

    Pool.WaitForAll;

    if Pool.LastError <> '' then
      WriteLn('Task failed: ', Pool.LastError);
  finally
    Pool.Free;
  end;
end;
```

---

## Properties and Methods

```pascal
property ThreadCount: Integer;        // read-only; number of worker threads
property LastError: string;           // read-only; last worker exception message
property Errors: TStringArray;        // read-only; all captured messages (capped)
property ErrorCount: Integer;         // read-only; count of messages in Errors
property OnError: TThreadPoolErrorEvent; // fired (on a worker thread) per failed task
property WorkQueue: TThreadSafeQueue; // access to queue for monitoring/config
property State: TThreadPoolState;      // accepting, draining, or stopped

function TryQueue(...; ATimeoutMS: Cardinal): Boolean; // four matching overloads
procedure WaitForAll; overload;
function WaitForAll(ATimeoutMS: Cardinal): Boolean; overload;
procedure Shutdown;
procedure ClearLastError;
procedure ClearErrors;                // clears both Errors and LastError
```

---

## Backpressure Configuration compatibility

v0.8.0 replaces load-threshold sleeps with event-driven not-full signalling.
`TBackpressureConfig` remains available so existing source continues to compile.
`MaxAttempts` and `HighLoadDelay` determine the compatibility wait used by the
legacy `Queue` overloads; new code should express its deadline directly with
`TryQueue(..., TimeoutMS)`. Threshold and low/medium-delay fields are retained
for source compatibility and no longer introduce sleeps.

```pascal
type
  TBackpressureConfig = record
    LowLoadThreshold:    Double;   // Default: 0.5  — delay starts at 50% capacity
    MediumLoadThreshold: Double;   // Default: 0.7  — medium delay at 70%
    HighLoadThreshold:   Double;   // Default: 0.9  — max delay at 90%
    LowLoadDelay:        Integer;  // Default: 10 ms
    MediumLoadDelay:     Integer;  // Default: 50 ms
    HighLoadDelay:       Integer;  // Default: 100 ms
    MaxAttempts:         Integer;  // Default: 5 — raises EQueueFullException after this
  end;
```

Read and write the config through `WorkQueue.BackpressureConfig`:

```pascal
var
  Config: TBackpressureConfig;
begin
  Config := Pool.WorkQueue.BackpressureConfig;
  Config.MaxAttempts   := 3;
  Config.HighLoadDelay := 200;
  Pool.WorkQueue.BackpressureConfig := Config;
end;
```

---

## Debug Logging

The `DEBUG_LOG` variable controls verbose output:

```pascal
DEBUG_LOG := True; // opt in while diagnosing scheduler behaviour
```

Debug logging is disabled by default so normal workloads and benchmarks do not
pay for synchronized console output.

---

## Lifecycle (v0.8.0)

`Shutdown` atomically stops admission, lets submissions already in progress
finish enqueueing, drains all accepted tasks, wakes and joins every worker, and
sets `State` to `tpsStopped`. It is idempotent and is called automatically by
the destructor. Queueing after shutdown raises `EThreadPoolShutdown`.

---

## Thread Count Rules

| Condition | Result |
| --- | --- |
| `AThreadCount <= 0` | Uses `TThread.ProcessorCount` |
| `AThreadCount < 4` | Raised to 4 (minimum enforced) |
| `AThreadCount > 2 × ProcessorCount` | Clamped to `2 × ProcessorCount` |

Thread count is fixed after construction.

---

## Common Pitfalls

### Catching EQueueFullException by message string

```pascal
// WRONG — the message includes dynamic content and will never match literally
on E: Exception do
  if E.Message = 'Queue is full' then ...

// CORRECT — catch by exception type
on E: EQueueFullException do ...
```

### Freeing an object before WaitForAll

```pascal
// WRONG — worker thread may still be calling MyObject.MyMethod
Pool.Queue(@MyObject.MyMethod);
MyObject.Free;

// CORRECT
try
  Pool.Queue(@MyObject.MyMethod);
  Pool.WaitForAll;
finally
  MyObject.Free;
end;
```

### Placing WaitForAll inside the EQueueFullException handler

```pascal
// WRONG — if Queue raises, WaitForAll is never reached, pool is freed while busy
try
  Pool.Queue(@MyProc);
  Pool.WaitForAll;   // skipped on exception
except
  on E: EQueueFullException do ...
end;
Pool.Free;

// CORRECT — separate concerns
try
  Pool.Queue(@MyProc);
except
  on E: EQueueFullException do
    WriteLn('Queue full: ', E.Message);
end;
Pool.WaitForAll;  // always reached
Pool.Free;
```

---

## Advanced: Queue Management

Queue multiple items and handle overflow per-item:

```pascal
var
  Pool: TProducerConsumerThreadPool;
  i: Integer;
begin
  Pool := TProducerConsumerThreadPool.Create(4, 512);
  try
    for i := 1 to 2000 do
    begin
      try
        Pool.Queue(@MyProc);
      except
        on E: EQueueFullException do
        begin
          Pool.WaitForAll;   // let the queue drain
          Pool.Queue(@MyProc);  // retry
        end;
      end;
    end;

    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end;
```

---

## Limitations

- Fixed queue capacity — no dynamic resizing
- `LastError` holds only the most recent worker exception (use `Errors` to collect all; capped at `MAX_STORED_ERRORS`)
- No task priority or cancellation support
- No dynamic thread scaling after construction
- Not suitable for real-time or UI-thread work
