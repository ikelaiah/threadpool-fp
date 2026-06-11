# ThreadPool.Simple API Documentation

> **Linux/macOS:** your program must list `cthreads` as the **first** unit in its
> `uses` clause, or creating the pool will raise a runtime access violation
> (exit code 217). Windows does not need it.
>
> ```pascal
> uses
>   {$IFDEF UNIX}cthreads,{$ENDIF}  // must be first on Unix-like systems
>   ThreadPool.Simple;
> ```

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
      // If multiple tasks fail, only the last exception is stored.
      WriteLn('Error: ', Pool.LastError);
      Pool.ClearLastError;  // Reset before reusing the pool
    end;
  finally
    Pool.Free;
  end;
end;
```

### Properties

```pascal
property LastError: string;   // Raw message of the most recent worker exception
property ThreadCount: Integer; // Number of worker threads (read-only)
```

### Methods

```pascal
procedure Queue(AProcedure: TThreadProcedure);
procedure Queue(AMethod: TThreadMethod);
procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
procedure WaitForAll;
procedure ClearLastError;
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
