# ThreadPool for Free Pascal — cheat sheet

Quick reference for v0.8.5. For full contracts, use the
[Simple API](ThreadPool.Simple-API.md) or
[Producer-Consumer API](ThreadPool.ProducerConsumer-API.md).

## Choose a pool

| Need | Use |
| --- | --- |
| Straightforward fire-and-forget work and an unbounded FIFO | `ThreadPool.Simple` |
| A ready-to-use process-wide instance | `GlobalThreadPool` from `ThreadPool.Simple` |
| Bounded memory and explicit queue saturation handling | `ThreadPool.ProducerConsumer` |
| Producer backpressure with a submission deadline | `TProducerConsumerThreadPool.TryQueue` |

## Import correctly

On Linux and macOS, `cthreads` must be the first unit in the program's `uses`
clause. Windows does not need it.

```pascal
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  ThreadPool.Simple;  // or ThreadPool.ProducerConsumer
```

## Create a pool

```pascal
// Managed singleton; do not free it yourself.
GlobalThreadPool.Queue(@DoWork);

// Private unbounded pool.
SimplePool := TSimpleThreadPool.Create(4);

// Private bounded pool: worker count, queue capacity.
BoundedPool := TProducerConsumerThreadPool.Create(4, 1024);
```

Pass `0` as the worker count to use `TThread.ProcessorCount`. The library always
creates at least four workers; positive requests are capped at
`2 * TThread.ProcessorCount` before that minimum is applied.

## Queue work

| Task form | Call |
| --- | --- |
| Procedure | `Pool.Queue(@DoWork)` |
| Object method | `Pool.Queue(@Worker.DoWork)` |
| Indexed procedure | `Pool.Queue(@ProcessItem, Index)` |
| Indexed object method | `Pool.Queue(@Worker.ProcessItem, Index)` |

The indexed callback signatures are:

```pascal
procedure ProcessItem(Index: Integer);
procedure TWorker.ProcessItem(Index: Integer);
```

## Handle bounded-queue saturation

Prefer `TryQueue` when using the Producer-Consumer pool:

```pascal
if not Pool.TryQueue(@DoWork, 50) then
  WriteLn('Queue remained full for 50 ms');
```

`Queue(...)` remains available, but it uses a bounded compatibility wait and
raises `EQueueFullException` if that wait expires. The Simple pool is unbounded,
so its `TryQueue` timeout exists for API symmetry and capacity does not time out.

## Wait and shut down

```pascal
Pool.WaitForAll;                 // wait indefinitely

if not Pool.WaitForAll(250) then
  WriteLn('Work remains after 250 ms');

Pool.Shutdown;                   // stop admission, drain, join workers
```

Timeouts are milliseconds:

| Value | Meaning |
| ---: | --- |
| `0` | Immediate attempt or state check |
| finite value | Maximum wait from call entry |
| `THREADPOOL_INFINITE` | No deadline |

After shutdown begins, `Queue` and `TryQueue` raise `EThreadPoolShutdown`.
Repeated `Shutdown` calls are safe. Do not call `Shutdown` from one of the
pool's own worker tasks.

## Read task errors

Worker exceptions are captured; they do not terminate the pool.

```pascal
var
  MessageText: string;
begin
  Pool.ClearErrors;
  Pool.Queue(@RiskyWork);
  Pool.WaitForAll;

  for MessageText in Pool.Errors do
    WriteLn(MessageText);
end;
```

- `LastError` is only the most recent message.
- `Errors` is an oldest-first snapshot, capped at `MAX_STORED_ERRORS = 1000`.
- `ErrorCount` reports the stored count.
- `ClearErrors` clears both the collection and `LastError`.
- `OnError` runs synchronously on the worker that caught the exception; keep the
  handler short, bounded, and thread-safe.

## Keep objects alive

```pascal
Worker := TWorker.Create;
try
  Pool.Queue(@Worker.DoWork);
  Pool.WaitForAll;  // wait before freeing the callback target
finally
  Worker.Free;
end;
```

Also remember:

- Do not free `GlobalThreadPool`; its unit owns it.
- `WaitForAll` is not an admission barrier for unrelated producers. Coordinate
  producers first, or use `Shutdown` to close admission before draining.
- Tasks and `OnError` callbacks have no automatic execution deadline. Add
  cancellation or application-level timeouts to operations that may block.

## Build and test

```bash
lazbuild package/lazarus/threadpool_fp.lpk
lazbuild tests/TestRunner.lpi
./tests/TestRunner -a -p --format=plain
```

On Windows, run `tests/TestRunner.exe` in the final command.

## More detail

- [README](../README.md)
- [Examples](../examples/)
- [v0.8.5 release notes](release-notes-v0.8.5.md)
- [Changelog](../CHANGELOG.md)
