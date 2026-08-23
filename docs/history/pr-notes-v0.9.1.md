# PR: Prepare the v0.9.1 usability and maintenance release

## Summary

Make threadpool-fp easier to approach, safer to monitor, and less costly to
maintain while preserving the v0.9 public surface.

## Why

The project had accumulated strong reference documentation but no longer gave
new users a short, obvious path from “what is this?” to a working program.
Implementation types labelled internal were also duplicated or exposed for
compatibility, and direct `WorkQueue` mutation could bypass completion
accounting.

## Changes

- Rewrite README as a concise, value-first landing page.
- Reduce Starter to one complete queue-and-wait example.
- Add a curated index for all examples.
- Add safe pool-level queue metrics and lock-protected backpressure config.
- Document `WorkQueue` as legacy compatibility access and warn against mutation.
- Consolidate duplicate callback work-item implementations.
- Remove unused private state, pass-through accessors, an inactive legacy test
  suite, and a timing-dependent unit assertion.
- Archive the completed v0.9.0 plan and refresh affected API/technical docs.
- Advance package and project documentation to v0.9.1.

## Compatibility

No established v0.9 APIs are removed. Compatibility work-item classes,
`WorkQueue`, `TBackpressureConfig`, the v0.8 `IThreadPool` interface, and all
queue/task overloads remain available.

## Verification

- [x] Lazarus package builds
- [x] 81 active tests pass
- [x] Benchmark builds and smoke test runs
- [x] All 15 examples build in Release mode
- [x] Relative Markdown links resolve
- [x] `git diff --check` passes

## Reviewer focus

- Confirm the new pool-level metrics cannot mutate queue state.
- Confirm backpressure configuration access is lock-protected.
- Confirm the internal work-item consolidation preserves all callback forms.
- Review the README as a first-time Free Pascal user's path, not as an API
  reference.
