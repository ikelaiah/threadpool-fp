# ThreadPool for Free Pascal — v0.6.5

A small, documentation-focused release. **There are no code or API changes** —
existing programs compile and run exactly as before. The goal is to stop new
Linux/macOS users from hitting a confusing runtime crash.

## Why this release exists

On Unix-like systems (Linux, macOS), Free Pascal does **not** install a
threading manager by default. Any program that creates threads must include the
`cthreads` unit as the **first** unit in its `uses` clause. Without it, creating
the thread pool fails at runtime with an access violation (exit code 217) —
**not** a compile error, so the build succeeds and the crash only appears when
the program runs.

This caught us during CI setup, and it will catch anyone building their own
program against this library. v0.6.5 documents the requirement everywhere a new
user is likely to look.

```pascal
program MyApp;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,            // MUST be first on Linux/macOS
  {$ENDIF}
  ThreadPool.Simple;   // or ThreadPool.ProducerConsumer
```

Windows does not need `cthreads` and is unaffected.

## What changed

- **README** — a prominent note at the top of Quick Start explaining the
  `cthreads` requirement, plus the `{$IFDEF UNIX}cthreads{$ENDIF}` guard added
  to every Quick Start and Installation snippet, and a reminder in the
  compilation Tip.
- **API docs** — a platform note at the top of both
  `ThreadPool.Simple-API.md` and `ThreadPool.ProducerConsumer-API.md`.
- **Roadmap** — "Planned/In Progress" dropped adaptive thread adjustment (it
  conflicts with the library's intentionally simple, fixed-count design) and
  added richer error handling, planned for 0.7.0.

> The examples in this repository already include the `cthreads` guard (added in
> v0.6.0), so they build and run correctly on both platforms — see
> `examples/Starter/Starter.lpr`.

## Upgrade notes

- **Nothing to change in your code.** If your program already runs correctly on
  Linux/macOS, it already has `cthreads` and is fine.
- If you are *new* to the library on Linux/macOS, add the `cthreads` guard shown
  above as the first unit in your program.

## Full changelog

See [CHANGELOG.md](../CHANGELOG.md) for the complete version history.
