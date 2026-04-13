## ThreadPool for Free Pascal — v0.5.0

This release focuses on making the library easier to pick up for new Free Pascal
developers, tightens up the documentation, and fixes three long-standing test
failures.

### What's new

#### Easier onboarding

- **New `examples/Starter/Starter.lpr`** — a single, heavily-commented file that
  compiles and runs in under a minute. Includes `fpc`/`lazbuild` one-liners and
  expected output directly in the header.
- **README: decision flowchart** — a quick text tree to choose between
  `ThreadPool.Simple` and `ThreadPool.ProducerConsumer`.
- **README: Queue overload reference table** — all 4 `Queue` signatures
  side-by-side with use-cases and examples.
- **README: Common Mistakes section** — annotated before/after code pairs for the
  four most frequent pitfalls:
  - Freeing an object before `WaitForAll`
  - Forgetting `WaitForAll` entirely
  - `LastError` only storing the last exception
  - Manually calling `Free` on `GlobalThreadPool`
- **README: compilation sanity-check** — `fpc` and `lazbuild` one-liners with
  expected output, plus a reminder about `{$mode objfpc}{$H+}`.

#### Source readability

- `{$REGION}` / `{$ENDREGION}` grouping added to `ThreadPool.Simple.pas` and
  `ThreadPool.ProducerConsumer.pas` — public API, internal worker thread, work
  item, and queue sections are now collapsible in Lazarus and VS Code.
- Object lifetime `WARNING` comments added inline to `SimpleDemo.lpr` and
  `ProdConSimpleDemo.lpr`.

#### Documentation corrections

- `ThreadPool.Simple-API.md`: full rewrite for clarity and structure; corrected
  `LastError` description — stores the raw exception message only, no thread-ID
  prefix.
- `ThreadPool.Simple-Technical.md`: removed incorrect `GlobalThreadPool.Free`
  from example; corrected "initialized on first use" (it is initialized at unit
  startup); removed duplicate section heading; replaced bullet lists with tables.
- `ThreadPool.ProducerConsumer-API.md`: full rewrite for clarity; removed
  duplicate Constructor section; fixed error handling example (`WaitForAll` was
  incorrectly inside the `EQueueFullException` handler); added missing `WorkQueue`
  property; fixed two examples that caught queue-full by message string equality —
  now catch `EQueueFullException` by type.
- `ThreadPool.ProducerConsumer-Technical.md`: replaced simplified `TryEnqueue`
  snippet (missing retry loop) with the actual implementation; replaced
  pseudo-code `Execute` snippet with real code; added thread safety summary table.
- `Interface-Reference-Counting` doc: updated test example to use `LongTask`
  instead of `SlowTask`, matching the fix to Test08.

### Bug fixes (tests)

Three test failures were present since the tests were first written — they were
never passing, not regressions introduced in this release.

| Test | Root cause | Fix |
| --- | --- | --- |
| `Test07_ParallelExecution` | `IncrementCounter` contained two `LogTest` (`WriteLn`) calls; 2 000 serialised console writes across 1 000 tasks consistently exceeded the timing assertion | Removed `LogTest` calls from `IncrementCounter` |
| `Test08_QueueFullBehavior` | `SlowTask` (250 ms) was short enough for the single worker to drain the 2-slot queue before the 3rd enqueue, so `EQueueFullException` was never raised | Replaced with `LongTask` (5 000 ms) to keep the queue full long enough |
| `Test12_LoadFactorCalculation` | `LoadFactor` is `FCount / FCapacity` — it drops to 0 the moment a worker calls `TryDequeue`, not when the task finishes. With only 2 tasks and 4+ workers, both items were dequeued before the assertion ran | Increased queued task count to 50 so the queue backlog outlasts the dequeue race |

**All 35 tests pass, 0 errors, 0 failures.**

### Full changelog

See [CHANGELOG.md](CHANGELOG.md) for the complete version history.
