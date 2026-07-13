<p align="center">
  <img src="docs/assets/threadpool-banner.png" alt="ThreadPool for Free Pascal — concurrent task queue splitting across parallel worker threads" width="100%">
</p>

# ThreadPool for Free Pascal

[![Version](https://img.shields.io/badge/version-0.8.5-8B5CF6.svg)](CHANGELOG.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](LICENSE.md)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-60A5FA.svg)](https://www.lazarus-ide.org/)
[![CI](https://github.com/ikelaiah/threadpool-fp/actions/workflows/ci.yml/badge.svg)](https://github.com/ikelaiah/threadpool-fp/actions/workflows/ci.yml)
![Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
![No dependencies](https://img.shields.io/badge/dependencies-none-10B981.svg)

A lightweight, dependency-free thread pool library for Free Pascal. It provides
an unbounded pool for straightforward parallel work and a bounded pool for
producer-consumer workloads that need backpressure.

[Quick start](#quick-start) · [Cheat sheet](docs/CHEATSHEET.md) ·
[API documentation](#documentation) · [Examples](examples/) ·
[v0.8.5 release notes](docs/release-notes-v0.8.5.md)

> [!TIP]
> ✨ **New in v0.8.5:** refreshed project identity, a shorter README, and a new
> [cheat sheet](docs/CHEATSHEET.md). Runtime behavior is unchanged from v0.8.0.

> [!NOTE]
> This library is designed for simple parallel processing and learning-friendly
> integration. It is not intended to replace high-load, production-scale
> frameworks such as [mORMot2](https://github.com/synopse/mORMot2),
> [ezthreads](https://github.com/mr-highball/ezthreads), or
> [OmniThreadLibrary](https://github.com/gabr42/OmniThreadLibrary).

## Choose a pool

| | `ThreadPool.Simple` | `ThreadPool.ProducerConsumer` |
| --- | --- | --- |
| Queue | Dynamically growing FIFO | Fixed-size circular FIFO |
| Capacity | Unbounded | Configurable; 1024 by default |
| Submission | Immediate | Timeout-aware backpressure |
| Convenience | Managed `GlobalThreadPool` | Create a pool instance |
| Best for | Predictable, moderate fire-and-forget work | Producers that may outpace consumers |

Both implementations provide:

- event-driven workers with no polling sleeps;
- four task forms: procedures, methods, and indexed variants;
- timeout-aware `TryQueue` and `WaitForAll` overloads;
- deterministic, draining `Shutdown`;
- captured worker exceptions through `LastError`, `Errors`, and `OnError`; and
- automatic worker-count selection with safety limits.

## Quick start

> [!IMPORTANT]
> 🧵 **Unix thread setup:** On Linux and macOS, `cthreads` must be the first unit
> in the program's `uses` clause. Without it, Free Pascal can compile
> successfully but fail at runtime when the pool creates worker threads. Windows
> does not need `cthreads`.

### Simple pool

Use the managed global pool when you only need to submit work and wait for it:

```pascal
program SimplePoolDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  ThreadPool.Simple;

procedure ProcessItem(Index: Integer);
begin
  WriteLn('Processing item ', Index);
end;

var
  I: Integer;
begin
  for I := 1 to 5 do
    GlobalThreadPool.Queue(@ProcessItem, I);

  GlobalThreadPool.WaitForAll;
end.
```

`GlobalThreadPool` is created and destroyed by the unit. Do not free it
yourself.

### Producer-consumer pool

Use a bounded pool when submission may need to wait for queue space:

```pascal
program BoundedPoolDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  ThreadPool.ProducerConsumer;

procedure DoWork;
begin
  WriteLn('Working');
end;

var
  Pool: TProducerConsumerThreadPool;
begin
  Pool := TProducerConsumerThreadPool.Create(0, 1024);
  try
    if not Pool.TryQueue(@DoWork, 50) then
      WriteLn('Queue remained full for 50 ms');

    Pool.Shutdown; // close admission, drain accepted work, join workers
  finally
    Pool.Free;
  end;
end.
```

The first constructor argument is the worker count; `0` selects
`TThread.ProcessorCount`. The second is queue capacity.

## Lifecycle and timeouts

Both pools follow one monotonic lifecycle:

```text
tpsAccepting -> tpsDraining -> tpsStopped
```

`Shutdown` stops new admission, waits for submissions already entering the
pool, drains every accepted task, wakes workers, and joins them. It is safe to
call more than once. Queueing after shutdown begins raises
`EThreadPoolShutdown`.

```pascal
Pool.WaitForAll;                    // wait indefinitely

if not Pool.WaitForAll(250) then    // milliseconds
  WriteLn('Work remains');

Pool.Shutdown;
```

Timeout values use these rules:

| Value | Meaning |
| ---: | --- |
| `0` | Immediate attempt or check |
| finite value | Maximum wait from call entry, in milliseconds |
| `THREADPOOL_INFINITE` | No deadline |

The Simple queue is unbounded, so its `TryQueue` timeout is present for API
symmetry and capacity cannot time out. For the bounded pool, `TryQueue` returns
`False` if no slot becomes available before the deadline. The legacy `Queue`
calls use a bounded compatibility wait and raise `EQueueFullException` when it
expires.

> [!WARNING]
> ⏱️ **Coordination matters:** `WaitForAll` does not stop unrelated producer
> threads from submitting more work. Coordinate producers first, or call
> `Shutdown` to close admission before draining. Tasks and `OnError` callbacks
> also have no automatic execution deadline; add cancellation or
> application-level timeouts where needed.

## Error handling

Task exceptions are caught so a worker failure does not terminate the pool.

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

- `LastError` is the most recent message.
- `Errors` is an oldest-first snapshot capped at 1000 entries.
- `ErrorCount` reports the stored count.
- `ClearErrors` clears the collection and `LastError`.
- `OnError` fires synchronously on the worker that caught the exception. Keep
  callbacks short, bounded, and thread-safe.

## Supported task forms

| Form | Example |
| --- | --- |
| Procedure | `Pool.Queue(@DoWork)` |
| Object method | `Pool.Queue(@Worker.DoWork)` |
| Indexed procedure | `Pool.Queue(@ProcessItem, I)` |
| Indexed object method | `Pool.Queue(@Worker.ProcessItem, I)` |

Objects must remain alive until all queued methods that reference them have
finished. Call `WaitForAll` before freeing a callback target.

## Installation

The library has no external dependencies.

1. Add [`src`](src/) to the project's unit search path, or install
   [`package/lazarus/threadpool_fp.lpk`](package/lazarus/threadpool_fp.lpk) in
   Lazarus.
2. Add `ThreadPool.Simple` or `ThreadPool.ProducerConsumer` to the `uses` clause.
3. On Unix-like systems, put `cthreads` first as shown in the quick start.

Requirements:

- Free Pascal 3.2.2 or later
- Lazarus 4.0 or later when using the package or project files

## Examples

| Start with | Demonstrates |
| --- | --- |
| [`Starter`](examples/Starter/) | Smallest compilable program with explanatory comments |
| [`SimpleDemo`](examples/SimpleDemo/) | Procedures, methods, indexes, and the global pool |
| [`ProdConSimpleDemo`](examples/ProdConSimpleDemo/) | Basic bounded-pool ownership and queueing |
| [`SimpleErrorHandlingBasic`](examples/SimpleErrorHandlingBasic/) | Reading captured errors after completion |

More focused samples cover:

- computation: [`SimpleSquareNumbers`](examples/SimpleSquareNumbers/) and
  [`ProdConSquareNumbers`](examples/ProdConSquareNumbers/);
- stateful processing: [`SimpleThreadpoolDemo`](examples/SimpleThreadpoolDemo/),
  [`SimpleWordCounter`](examples/SimpleWordCounter/), and
  [`ProdConMessageProcessor`](examples/ProdConMessageProcessor/);
- advanced callbacks: [`SimpleErrorHandling`](examples/SimpleErrorHandling/);
- real I/O: [`ParallelFileHasher`](examples/ParallelFileHasher/) and
  [`ParallelUrlFetcher`](examples/ParallelUrlFetcher/).

## Documentation

| Document | Purpose |
| --- | --- |
| [Cheat sheet](docs/CHEATSHEET.md) | Calls and safety rules at a glance |
| [Simple API](docs/ThreadPool.Simple-API.md) | Complete unbounded-pool reference |
| [Producer-Consumer API](docs/ThreadPool.ProducerConsumer-API.md) | Complete bounded-pool reference |
| [Simple technical guide](docs/ThreadPool.Simple-Technical.md) | Internal design and synchronization |
| [Producer-Consumer technical guide](docs/ThreadPool.ProducerConsumer-Technical.md) | Queue and backpressure internals |
| [v0.8.5 release notes](docs/release-notes-v0.8.5.md) | Current release scope and compatibility |
| [Changelog](CHANGELOG.md) | Full version history |

The banner's editable source is
[`docs/assets/threadpool-banner.svg`](docs/assets/threadpool-banner.svg); the
README displays its synchronized PNG render.

## Build and test

```bash
lazbuild package/lazarus/threadpool_fp.lpk
lazbuild tests/TestRunner.lpi
./tests/TestRunner -a -p --format=plain
```

On Windows, run `tests/TestRunner.exe` in the final command. CI builds the
package, tests, benchmark, and every example on Windows and Linux.

## Contributing

Bug reports, documentation improvements, examples, and code contributions are
welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for the build, style, and pull
request workflow.

## License

ThreadPool for Free Pascal is available under the [MIT License](LICENSE.md).
