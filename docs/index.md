# ThreadPool-FP

ThreadPool-FP is a small, dependency-free thread pool library for Free Pascal
and Lazarus. It runs ordinary procedures and object methods across a managed
set of worker threads, so you can parallelise work without writing thread
runbooks by hand.

It is practical software: four callback forms, two pool styles (unbounded, and
a bounded producer-consumer queue), task handles with pending cancellation,
worker-error capture, and a clear lifecycle contract.

## Getting started

- [Installation & Quick Start](getting-started/installation.md) — add the units to your project and run the smallest complete program
- [Beginner Guide](getting-started/beginner-guide.md) — learn the core ideas in order, from one queued callback to observable tasks and batches
- [Choose a Pool](getting-started/choosing-a-pool.md) — decide between the unbounded `ThreadPool.Simple` pool and the bounded `ThreadPool.ProducerConsumer` pool
- [Cheat Sheet](getting-started/cheat-sheet.md) — the operational rules on one page

## Guides

- [Callback Forms](guides/callback-forms.md) — the four callback signatures
- [Simple Thread Pool](guides/simple-thread-pool.md) — `GlobalThreadPool`, `TSimpleThreadPool`, and the fire-and-forget `Queue` workflow
- [Tasks & Batches](guides/tasks-and-batches.md) — submit observable work, inspect states, and coordinate groups of handles
- [Parallel Ranges](guides/ranges.md) — process an inclusive integer range efficiently with `SubmitRange`
- [Cancellation](guides/cancellation.md) — what cancellation can and cannot do
- [Error Handling](guides/error-handling.md) — worker failures, `LastError`, `Errors`, and `OnError`
- [Producer-Consumer Queue](guides/producer-consumer.md) — bounded work queues
- [Backpressure](guides/backpressure.md) — submission deadlines and queue monitoring
- [Lifecycle & Shutdown](guides/lifecycle-and-shutdown.md) — waiting, draining, and stopping a pool cleanly
- [Thread Safety](guides/thread-safety.md) — the concurrency contracts you can rely on, and the hazards to avoid
- [Platform Requirements](guides/runtime-requirements.md) — `cthreads`, FPC and Lazarus versions, and platform notes
- [Common Recipes](guides/recipes.md) — compiled programs for real tasks

## Reference

- [Simple API](reference/simple-api.md)
- [Tasks API](reference/tasks-api.md)
- [Producer-Consumer API](reference/producer-consumer-api.md)
- [Types & Interfaces](reference/types-and-interfaces.md)
- [Contracts & Limitations](reference/contracts-and-limitations.md)

## Internals

- [Simple Pool Internals](internals/simple-internals.md)
- [Producer-Consumer Internals](internals/producer-consumer-internals.md)
- [Interface Reference Counting in FPC](internals/interface-reference-counting.md)

## Project

- [Changelog](../CHANGELOG.md)
- [Contributing](../CONTRIBUTING.md)

This online documentation is versioned. The top-level site opens the current
release under `/0.9.1/`, and the version selector in the header switches
between published documentation releases.