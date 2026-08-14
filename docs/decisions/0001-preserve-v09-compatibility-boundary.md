# ADR-001: Preserve the v0.9 compatibility boundary during internal cleanup

## Status

Accepted

## Date

2026-08-14

## Context

Several queue, worker, and work-item implementation types are visible from unit
interfaces. The bounded pool also exposes its mutable queue through `WorkQueue`.
Removing those symbols would simplify the library but would break source that
named them directly, which is not appropriate for the v0.9.1 patch release.

Direct `WorkQueue` mutation is unsafe because the queue does not own the pool's
completion counter. Enqueuing, dequeuing, or clearing through the queue can make
`WaitForAll` observe the wrong number of accepted tasks.

## Decision

- Preserve existing visible implementation and compatibility symbols in v0.9.1.
- Add safe pool-level queue metrics and backpressure configuration access.
- Document `WorkQueue` as legacy compatibility access and warn against mutation.
- Consolidate duplicated callback behavior in an internal-named unit while
  retaining the existing work-item classes as thin wrappers.
- Reserve removal or visibility-breaking changes for a major release.

## Alternatives considered

### Remove `WorkQueue` and internal classes immediately

This gives the cleanest surface but creates an avoidable patch-release source
break for callers and tests that name those symbols.

### Make direct queue mutation update pool accounting

This couples the queue to pool lifecycle and makes externally removed work
ambiguous: a dequeued item might be executed elsewhere, cancelled, or dropped.
Preventing new direct use is simpler and safer.

### Leave the API unchanged and document nothing

This preserves compatibility but leaves a publicly advertised path that can
invalidate completion accounting.

## Consequences

- Existing v0.x source continues to compile.
- New code has a safe monitoring and configuration path.
- Some compatibility-only symbols remain visible until a major release.
- A future major release can move queue, worker, work-item, and task-factory
  machinery fully behind internal units.
