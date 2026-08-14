# Implementation Plan: v0.9.1 usability and maintenance release

## Overview

Prepare a compatibility-safe v0.9.1 release that makes threadpool-fp easier to
approach and maintain. The release will protect producer-consumer completion
accounting, reduce implementation leakage where a patch release permits it,
remove stale test and source artifacts, and reorganize documentation around a
short beginner journey.

## Architecture Decisions

- Preserve v0.9 source compatibility. Existing `WorkQueue` and backpressure
  symbols remain available but are deprecated; new pool-level monitoring
  properties provide the safe replacement.
- Keep the two pool implementations. Their different capacity semantics are
  useful; v0.9.1 will clarify the default choice instead of combining them.
- Move only non-public shared implementation into internal units. Compatibility
  types that existing callers may name remain available until a major release.
- Treat README as a landing page, not the canonical contract. Detailed lifecycle,
  timeout, error, and compatibility rules remain in focused documentation.

## Task List

### Phase 1: Protect the queue boundary

- [ ] Add failing tests for safe pool-level queue monitoring.
- [ ] Add read-only queue count, capacity, and load-factor properties.
- [ ] Deprecate direct `WorkQueue` access and document its mutation hazard.

### Checkpoint: Queue boundary

- [ ] Focused producer-consumer tests pass.
- [ ] Package builds without errors.

### Phase 2: Reduce maintenance debt

- [ ] Move duplicated callback work-item behavior into a shared internal unit.
- [ ] Remove unused private fields, pass-through overrides, and no-op code where
      compatibility does not require them.
- [ ] Remove the obsolete test suite from the repository and Lazarus project.
- [ ] Replace the timing-based unit assertion with deterministic coverage or
      leave throughput measurement solely in benchmarks.

### Checkpoint: Maintenance

- [ ] Full active test suite passes.
- [ ] Package and benchmark build.

### Phase 3: Improve onboarding and release documentation

- [ ] Rewrite README as a concise value-first landing page.
- [ ] Simplify the Starter example and update stale error guidance.
- [ ] Add a curated examples index.
- [ ] Correct documentation inconsistencies and broken relative links.
- [ ] Archive the completed v0.9.0 development plan.
- [ ] Add v0.9.1 release notes, PR notes, and changelog entry.

### Checkpoint: Complete

- [ ] Package, tests, benchmark, and all examples build.
- [ ] Markdown relative links resolve.
- [ ] Git diff passes whitespace checks.
- [ ] Final multi-axis code review has no required findings.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| --- | --- | --- |
| Removing exported implementation types breaks existing callers | High | Keep compatibility symbols in v0.9.1 and deprecate unsafe entry points |
| Queue monitoring introduces new races | Medium | Expose values through the queue's existing lock-protected getters |
| Documentation becomes shorter but omits safety information | Medium | Keep canonical contracts in API guides and link them prominently |
| Test cleanup hides behavior | Medium | Remove only the suite absent from `TestRunner`; keep compatibility sentinel |

## Open Questions

- None. The requested v0.9.1 patch scope implies source compatibility.
