# Contributing to ThreadPool for Free Pascal

Thanks for your interest in improving this library! Contributions of all
sizes are welcome — bug reports, documentation fixes, new examples, and code.

## Ways to contribute

- 🐛 **Report a bug** — open an [issue](https://github.com/ikelaiah/threadpool-fp/issues) using the Bug Report template.
- 💡 **Request a feature** — open an issue using the Feature Request template.
- 📝 **Improve docs or examples** — these are especially appreciated and easy to start with.
- 🔧 **Fix a bug or add a feature** — see the workflow below.

## Prerequisites

- Free Pascal 3.2.2 or later
- Lazarus 3.6.0 or later (provides `lazbuild`)
- No external dependencies

## Building

Build the package:

```bash
lazbuild package/lazarus/threadpool_fp.lpk
```

Or compile a single example directly with FPC:

```bash
fpc -Fu./src examples/SimpleDemo/SimpleDemo.lpr
```

## Running the tests

```bash
lazbuild tests/TestRunner.lpi
# Windows
./tests/TestRunner.exe -a -p --format=plain
# Linux / macOS
./tests/TestRunner -a -p --format=plain
```

All tests should pass before you submit a change. The full suite can take a
few minutes to run because some tests exercise high task volumes.

CI runs the package build, the test suite, and all examples on Linux and
Windows for every pull request — please make sure your branch is green.

## Code style

- Use `{$mode objfpc}{$H+}{$J-}` at the top of new units and programs.
- Match the existing formatting: two-space indentation, `PascalCase` for
  types/methods, and a descriptive comment for non-obvious logic.
- **Filenames must match the `unit` identifier's casing exactly**
  (e.g. unit `ThreadPool.Simple` lives in `ThreadPool.Simple.pas`). This keeps
  the library compiling on case-sensitive filesystems like Linux.
- Keep the library dependency-free (FCL only).

## Pull request workflow

1. Fork the repo and create a branch off `main`
   (e.g. `feat/adaptive-threads` or `fix/queue-overflow`).
2. Make your change, including tests and/or an example where it makes sense.
3. Run the test suite locally and make sure it passes.
4. Update `CHANGELOG.md` and any relevant docs in `docs/`.
5. Open a pull request describing **what** changed and **why**.

## Reporting security issues

For anything sensitive, please email the maintainer rather than opening a
public issue.

Thank you for helping make this library better! 🙏
