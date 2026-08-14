<p align="center">
  <img src="docs/assets/threadpool-banner.png" alt="ThreadPool for Free Pascal — concurrent task queue splitting across parallel worker threads" width="100%">
</p>

# ThreadPool for Free Pascal

[![Version](https://img.shields.io/badge/version-0.9.1-8B5CF6.svg)](CHANGELOG.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](LICENSE.md)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-60A5FA.svg)](https://www.lazarus-ide.org/)
[![CI](https://github.com/ikelaiah/threadpool-fp/actions/workflows/ci.yml/badge.svg)](https://github.com/ikelaiah/threadpool-fp/actions/workflows/ci.yml)
![Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
![No dependencies](https://img.shields.io/badge/dependencies-none-10B981.svg)

Run ordinary Pascal procedures concurrently without managing worker threads
yourself. Threadpool-fp is small, dependency-free, and designed for applications
that need straightforward parallel work or a bounded producer-consumer queue.

Use it to:

- process files, records, or array indexes in parallel;
- run independent background jobs and wait for them as a group;
- observe failures or cancel work that has not started; and
- limit queued work when producers can outrun consumers.

Most applications should start with `ThreadPool.Simple`.

[Quick start](#quick-start) · [Examples](examples/) ·
[Cheat sheet](docs/CHEATSHEET.md) · [API reference](#documentation) ·
[v0.9.1 release notes](docs/release-notes-v0.9.1.md)

## Quick start

```pascal
program HelloThreadPool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  ThreadPool.Simple;

procedure ProcessItem(Index: Integer);
begin
  WriteLn('Processed item ', Index);
end;

var
  I: Integer;
begin
  for I := 1 to 5 do
    GlobalThreadPool.Queue(@ProcessItem, I);

  GlobalThreadPool.WaitForAll;
end.
```

The five callbacks run across the pool's workers, so their output order may
change between runs. `GlobalThreadPool` is managed by the unit; do not free it.

> [!IMPORTANT]
> On Linux and macOS, `cthreads` must be the first unit in the program's `uses`
> clause. A program can compile without it and still fail when threads start.

## Installation

The library has no external dependencies.

1. Add [`src`](src/) to your project's unit search path, or install
   [`package/lazarus/threadpool_fp.lpk`](package/lazarus/threadpool_fp.lpk) in
   Lazarus.
2. Add `ThreadPool.Simple` to your `uses` clause.
3. Add `ThreadPool.Tasks` when you declare task or batch interfaces.

Requirements are Free Pascal 3.2.2 or later, plus Lazarus 4.0 or later when
using the package or project files.

## Choose a pool

| If you need... | Start with... |
| --- | --- |
| Ordinary parallel work with the least setup | `ThreadPool.Simple` |
| A ready-to-use process-wide pool | `GlobalThreadPool` |
| A private unbounded pool | `TSimpleThreadPool.Create` |
| A fixed queue capacity and submission backpressure | `ThreadPool.ProducerConsumer` |

The Simple pool uses a dynamically growing queue. Choose the bounded pool only
when queue growth must be controlled or producers need a submission deadline.

## Common recipes

### Observe one task

Use `Submit` instead of `Queue` when you need a handle for waiting, failure
inspection, or best-effort cancellation:

```pascal
uses
  ThreadPool.Tasks, ThreadPool.Simple;

Task := GlobalThreadPool.Submit(@DoWork);
if Task.WaitFor(250) and (Task.State = ttsFailed) then
  WriteLn(Task.ErrorMessage);
```

`Task.Cancel` succeeds only while work is pending. It never interrupts a
running callback.

### Process a range

```pascal
Batch := GlobalThreadPool.SubmitRange(@ProcessItem, 0, High(Items));
Batch.WaitFor;
```

The bounds are inclusive. Automatic mode creates a small number of chunks
instead of one queue entry per index.

### Use a bounded queue

```pascal
Pool := TProducerConsumerThreadPool.Create(0, 1024);
try
  if not Pool.TryQueue(@DoWork, 50) then
    WriteLn('Queue remained full for 50 ms');
finally
  Pool.Free; // drains accepted work and joins workers
end;
```

Import `ThreadPool.ProducerConsumer` for this example. The first constructor
argument is the requested worker count. `0` selects the processor count, with
the library's minimum of four workers; positive requests are capped at twice
the processor count before that minimum is applied.

## Five rules worth knowing

- Call `WaitForAll` before freeing objects referenced by queued methods.
- `WaitForAll` does not stop other producer threads from submitting more work.
- `Shutdown` closes admission, drains accepted work, and joins the workers.
- Worker exceptions are captured in `LastError` and `Errors`; they do not stop
  the pool.
- Callbacks have no automatic execution deadline. Add application-level
  cancellation or timeouts to operations that may block.

The [cheat sheet](docs/CHEATSHEET.md) covers callback forms, timeout values,
error handling, cancellation, and lifecycle rules on one page.

## Examples

Start with these:

| Example | What it teaches |
| --- | --- |
| [`Starter`](examples/Starter/) | The smallest complete program |
| [`SimpleDemo`](examples/SimpleDemo/) | Procedures, methods, and indexed callbacks |
| [`ProdConSimpleDemo`](examples/ProdConSimpleDemo/) | Owning and using a bounded pool |
| [`TaskCoordination`](examples/TaskCoordination/) | Tasks, batches, ranges, and cancellation |
| [`CoordinatedFileBackup`](examples/CoordinatedFileBackup/) | A complete failure-aware workflow |

The [examples guide](examples/README.md) organizes all samples by learning goal.

Build every example in Release mode from the repository root:

```powershell
.\build-examples.ps1
```

```sh
sh ./build-examples.sh
```

Executables are written to the ignored `example-bin/` directory.

## Documentation

| Document | Use it for |
| --- | --- |
| [Cheat sheet](docs/CHEATSHEET.md) | Calls and safety rules at a glance |
| [Simple API](docs/ThreadPool.Simple-API.md) | Complete unbounded-pool contract |
| [Producer-Consumer API](docs/ThreadPool.ProducerConsumer-API.md) | Bounded queues and backpressure |
| [Tasks API](docs/ThreadPool.Tasks-API.md) | Task handles, batches, ranges, and cancellation |
| [Simple internals](docs/ThreadPool.Simple-Technical.md) | Unbounded-pool implementation |
| [Producer-Consumer internals](docs/ThreadPool.ProducerConsumer-Technical.md) | Bounded-queue implementation |
| [Changelog](CHANGELOG.md) | Version history |

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
welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for the workflow.

## License

ThreadPool for Free Pascal is available under the [MIT License](LICENSE.md).
