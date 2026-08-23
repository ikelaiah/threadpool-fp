# Installation & Quick Start

ThreadPool-FP has **no external dependencies**. It uses only the Free Pascal
run-time library and FCL, so installation is a search path, not a package
hunt.

## Requirements

| Tool | Minimum |
| --- | --- |
| Free Pascal | 3.2.2 or later |
| Lazarus | 4.0 or later (only when you use the package or project files) |

The library is CI-tested on Windows and Linux and follows Free Pascal's
thread model, so it also works on macOS when `cthreads` is enabled (see
[Platform Requirements](../guides/runtime-requirements.md)). There are no external package
dependencies.

> [!IMPORTANT]
> On Linux and macOS the `cthreads` unit **must be the first unit** in your
> program's `uses` clause. A program can compile without it and still crash at
> run time when the first worker thread starts, usually with exit code 217.
> Windows does not need it.

## Install

1. Add the unit search path to [`src/`](../../src/ThreadPool.Simple.pas) (the library source), or install [`threadpool_fp.lpk`](../../package/lazarus/threadpool_fp.lpk) in Lazarus.
2. Add `ThreadPool.Simple` to your `uses` clause.
3. Add `ThreadPool.Tasks` when you declare task or batch interface variables.

In Lazarus, open `package/lazarus/threadpool_fp.lpk` from the package manager
and click **Install**. The package adds the library units to the compiler path
automatically.

## Quick start

The smallest complete program:

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

Save this as `HelloThreadPool.pas` and compile with FPC only:

```bash
fpc -Fu./src HelloThreadPool.pas
```

Run it and you will see five lines such as:

```text
Processed item 3
Processed item 1
Processed item 5
Processed item 2
Processed item 4
```

The five callbacks run across the pool's workers, so **their output order may
change between runs**. That is expected and is the point: the callbacks run
concurrently.

## What just happened?

- `GlobalThreadPool` is a ready-to-use process-wide pool owned by the unit. Do not free it; the unit manages its lifetime.
- `Queue(@ProcessItem, I)` enqueues one call that will run on a worker thread.
- `GlobalThreadPool.WaitForAll` blocks until every accepted callback has finished, so the program does not exit while work is still running.

## Next steps

- [Beginner Guide](beginner-guide.md) — the same ideas in a longer sequence.
- [Choose a Pool](choosing-a-pool.md) — when to use the bounded pool instead.
- [Common Recipes](../guides/recipes.md) — compiled, tested programs.

## Build and test

Build the package, tests, and run the suite:

```bash
lazbuild package/lazarus/threadpool_fp.lpk
lazbuild tests/TestRunner.lpi
./tests/TestRunner -a -p --format=plain
```

On Windows, run `tests/TestRunner.exe` in the final command.

The library and every example also build through the example build scripts
([PowerShell](../../build-examples.ps1) on Windows, `build-examples.sh` on
Linux) from the repository root:

```powershell
.\build-examples.ps1
```

```sh
sh ./build-examples.sh
```

Executables are written to the ignored `example-bin/` directory.

## Sources of examples

Copy the fully worked, compiled programs under [Common
Recipes](../guides/recipes.md), or browse the [examples guide](../../examples/README.md)
directory and the [library README](../../README.md).
