# Parallel log analyzer

This example models a two-phase access-log reporting pipeline over 100,000
deterministic records:

1. `SubmitRange` validates and classifies entries using a small number of
   automatically sized chunks.
2. The completed range batch acts as a phase barrier. Independent endpoint
   summaries and malformed-record counting are then submitted as one batch.

Callbacks write separate result slots, and the second phase reads only data
made immutable by the barrier, so the workflow needs no application-level
locks. One malformed record is retained as a validation result instead of
raising from its callback and discarding the rest of that chunk.

Build with Lazarus or run:

```text
lazbuild ParallelLogAnalyzer.lpi
```
