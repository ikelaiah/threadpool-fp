# Platform Requirements

ThreadPool-FP is a dependency-free Free Pascal library. Its platform contract
is deliberately small but must be respected.

## Supported toolchains

| Tool | Minimum | Notes |
| --- | --- | --- |
| Free Pascal | 3.2.2 or later | Uses only the RTL and FCL (`Classes`, `SysUtils`, `SyncObjs`, `Math`) |
| Lazarus | 4.0 or later | Only required for the package and project files; FPC-only users do not need it |

The library has **no external dependencies**. `ThreadPool.Tasks` requires
3.2.2+; earlier FPC versions cannot compile the task unit.

## Operating systems

- CI tests the full suite and all examples on **Windows** and **Linux**.
- **macOS** follows the same Free Pascal thread model as Linux: it requires `cthreads` and is expected to work, but it is not part of the CI matrix.
- Unit files obey the `{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}` interface calling-convention rule so `IWorkerThread` compiles on non-Windows targets.

## cthreads is mandatory on Unix-like systems

On Linux and macOS, `cthreads` **must be the first unit** in your program's
`uses` clause. Without it a program can compile, then crash at run time with an
access violation (exit code 217) when the first worker thread initializes.

```pascal
uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  ThreadPool.Simple;
```

Windows does not need `cthreads`.

## ProcessorCount caveats

- `TThread.ProcessorCount` is the default worker count and is read once at program start.
- It counts logical CPUs (including hyper-threads) on most systems, not physical cores.
- It does not reflect later changes (CPU affinity, power states, containers).
- Treat the computed worker count as approximate guidance.

## Worker-count rules

| Argument | Result |
| --- | --- |
| `AThreadCount <= 0` | `TThread.ProcessorCount` |
| `< 4` | raised to 4 |
| `> 2 × ProcessorCount` | capped at `2 × ProcessorCount` |

## Building from source

Compile a program with just the source path:

```bash
fpc -Fu./src MyProgram.pas
```

Build the package, tests, benchmark, and examples with Lazarus; the repository
README describes the commands and the CI pipeline runs the same steps on both
operating systems.

## Concurrency runtime notes

- Objects created per callback on the worker thread need no extra setup.
- Any shared object the callbacks touch must use Free Pascal's usual thread helpers (`TCriticalSection`, `TThreadList`, atomic operations from the RTL).
- Never assume the Unix or Windows scheduler measures are identical; workers map to OS threads, and thread count fixes the parallelism independent of how many cores are busy right now.

## Related

- [Installation & Quick Start](../getting-started/installation.md)
- [Thread Safety](thread-safety.md)
- [Contracts & Limitations](../reference/contracts-and-limitations.md)
