# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [0.6.0] - 2026-06-10

### Added

- GitHub Actions CI (`.github/workflows/ci.yml`) — builds the package, runs the FPCUnit test suite, and builds all examples on both Linux and Windows for every push and pull request
- `CONTRIBUTING.md` — build/test instructions, code style guide (including the unit-filename casing rule), and pull-request workflow
- Issue templates (`.github/ISSUE_TEMPLATE/`) for bug reports and feature requests, plus a discussions contact link
- Pull request template (`.github/PULL_REQUEST_TEMPLATE.md`)
- README: Contributing section linking to `CONTRIBUTING.md`

### Changed

- Renamed source files to match their `unit` identifier casing so the library compiles on case-sensitive filesystems (Linux): `threadpool.simple.pas` → `ThreadPool.Simple.pas`, `threadpool.producerconsumer.pas` → `ThreadPool.ProducerConsumer.pas`, and `tests/threadpool.producerconsumer.tests.pas` → `tests/ThreadPool.ProducerConsumer.Tests.pas`
- Normalized the unit reference in `tests/TestRunner.lpr` (`Threadpool.` → `ThreadPool.`)
- README: replaced the hardcoded test-count badge with a live CI status badge

### Fixed

- `ThreadPool.Simple` now compiles on non-Windows targets: the `IWorkerThread` methods (`QueryInterface`/`_AddRef`/`_Release`) hardcoded the `stdcall` calling convention, which only matches `IInterface` on Windows — FPC uses `cdecl` elsewhere. Guarded with `{$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}`
- Worker-thread double-free that caused an `EAccessViolation` on Linux during unit finalization: `IWorkerThread` is now non-ref-counted (the pool owns worker lifetime explicitly via `ClearThreads`), so an interface assignment can no longer free a live worker. Removed the now-unused `FRefCount` field
- Example projects (all except `Starter`) only had the `src` search path in their `Release` build mode, so `lazbuild` (which builds the `Default` mode) could not find the units. Added the search path to each project's global compiler options

## [0.5.0] - 2026-04-13

### Added

- `examples/Starter/Starter.lpr` — a heavily-commented "Hello, ThreadPool" entry point for new users; includes compile instructions and expected output in the file header
- README: decision flowchart for choosing `ThreadPool.Simple` vs `ThreadPool.ProducerConsumer`
- README: Queue overload reference table documenting all 4 `Queue` signatures side-by-side with use-cases and examples
- README: **Common Mistakes** section covering the four most frequent pitfalls (free-before-WaitForAll, missing WaitForAll, LastError overwrites, manual free of GlobalThreadPool)
- README: compilation sanity-check section with `fpc` and `lazbuild` one-liners, expected output, and `{$mode objfpc}{$H+}` reminder
- README: `LastError` overwrite warning — clarifies only the last exception per pool cycle is stored
- `{$REGION}` / `{$ENDREGION}` grouping in `ThreadPool.Simple.pas` (Public API, Internal Worker Thread, Internal Work Item, Global instance)
- `{$REGION}` / `{$ENDREGION}` grouping in `ThreadPool.ProducerConsumer.pas` (Public API, Internal Worker Thread, Internal Work Item, Internal Queue, DebugLog)
- Object lifetime `WARNING` comments in `SimpleDemo.lpr` and `ProdConSimpleDemo.lpr` explaining why objects must not be freed before `WaitForAll`

### Fixed

- `Test07_ParallelExecution`: removed `LogTest` calls from `IncrementCounter` — 2 000 serialised `WriteLn` calls across 1 000 tasks were the bottleneck, consistently exceeding the timing assertion; these calls were present since the test was first written
- `Test08_QueueFullBehavior`: replaced `SlowTask` (250 ms) with `LongTask` (5 000 ms) so the single worker cannot drain the 2-slot queue before the third enqueue — the race condition was present since the test was first written
- `Test12_LoadFactorCalculation`: increased queued task count from 2 to 50 — `LoadFactor` drops to 0 the moment `TryDequeue` succeeds (not when execution finishes), so 4+ workers racing to dequeue 2 tasks left the queue empty before the assertion ran; the flaw was present since the test was first written

## [0.4.0] - 2024-xx-xx

### Added

- `ThreadPool.ProducerConsumer` — second thread pool implementation with fixed-size circular queue (default 1 024 items) and built-in backpressure
- Configurable `TBackpressureConfig` record (load thresholds, adaptive delays, max retry attempts)
- `EQueueFullException` raised when queue remains full after all retry attempts
- `TThreadSafeQueue` with `LoadFactor`, `TryEnqueue`, `TryDequeue`
- `WorkQueue` property on `TProducerConsumerThreadPool` for monitoring and configuration
- 3 new Producer-Consumer examples: `ProdConSimpleDemo`, `ProdConSquareNumbers`, `ProdConMessageProcessor`
- API and technical documentation for `ThreadPool.ProducerConsumer`
- Documentation: Interface Reference Counting and Object Lifetime Management in FPC
- Lazarus package (`package/lazarus/threadpool_fp.lpk`)

### Fixed

- Incorrect freeing of work items in `ThreadPool.ProducerConsumer`
- Double decrement of work item counter
- Race condition in exception propagation from worker threads

## [0.3.0] - 2024-xx-xx

### Added

- `ThreadPool.Types` unit — shared interfaces (`IThreadPool`, `IWorkItem`, `IWorkerThread`) and base class `TThreadPoolBase`
- Thread count safety limits: minimum 4 threads, maximum 2× `ProcessorCount`
- `ClearLastError` method on all pool implementations
- Comprehensive unit tests for `ThreadPool.Simple`

### Changed

- Refactored `ThreadPool.Simple` onto the new `TThreadPoolBase` class
- Improved method names in unit tests

### Fixed

- Race condition in error handling in `ThreadPool.Simple`

## [0.2.0] - 2024-xx-xx

### Added

- Thread count safety: enforced minimum and maximum bounds
- Technical documentation and sequence diagrams
- `SimpleWordCounter` and `SimpleSquareNumbers` examples

### Changed

- Improved comments in the interface section of the unit

## [0.1.0] - 2024-xx-xx

### Added

- Initial implementation of `ThreadPool.Simple` with global `GlobalThreadPool` singleton
- 4 `Queue` overloads: plain procedure, object method, indexed procedure, indexed method
- `WaitForAll` and `LastError` / `ClearLastError`
- `SimpleDemo` and `SimpleThreadpoolDemo` examples
- MIT licence
