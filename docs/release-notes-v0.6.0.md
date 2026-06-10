# ThreadPool for Free Pascal — v0.6.0

This release focuses on **developer experience and cross-platform reliability**.
There are no public API changes — existing code continues to compile and run
unchanged.

## What's new

### Continuous Integration

- **GitHub Actions CI** (`.github/workflows/ci.yml`) — every push and pull
  request now builds the package, runs the full FPCUnit test suite, and builds
  all examples on both **Linux and Windows**.
- **Live CI badge** — the README's hardcoded test-count badge has been replaced
  with a real build-status badge, so the README always reflects the current
  state of the `main` branch.

### Easier contributing

- **`CONTRIBUTING.md`** — build and test instructions, code style guide
  (including the unit-filename casing rule), and the pull-request workflow.
- **Issue templates** (`.github/ISSUE_TEMPLATE/`) — structured forms for bug
  reports and feature requests, plus a link to Discussions for questions.
- **Pull request template** (`.github/PULL_REQUEST_TEMPLATE.md`) — a checklist
  covering tests, changelog, docs, and the unit-casing convention.
- **README: Contributing section** linking to `CONTRIBUTING.md`.

## Cross-platform fix

Unit filenames now match their `unit` identifier casing exactly, so the library
compiles on case-sensitive filesystems such as Linux. On Windows this was
masked by the case-insensitive filesystem.

| Before | After |
| --- | --- |
| `src/threadpool.simple.pas` | `src/ThreadPool.Simple.pas` |
| `src/threadpool.producerconsumer.pas` | `src/ThreadPool.ProducerConsumer.pas` |
| `tests/threadpool.producerconsumer.tests.pas` | `tests/ThreadPool.ProducerConsumer.Tests.pas` |

The unit reference in `tests/TestRunner.lpr` was also normalized
(`Threadpool.` → `ThreadPool.`).

## Upgrade notes

- **No code changes required.** `uses ThreadPool.Simple;`,
  `uses ThreadPool.ProducerConsumer;`, etc. are unchanged.
- If you reference the source files by literal path (rare), update to the new
  capitalized filenames listed above.

## Verification

- `lazbuild tests/TestRunner.lpi` builds clean (0 errors) after the renames.

## Full changelog

See [CHANGELOG.md](../CHANGELOG.md) for the complete version history.
