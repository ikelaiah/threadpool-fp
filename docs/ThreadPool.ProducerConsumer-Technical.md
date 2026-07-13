# ThreadPool.ProducerConsumer — Technical Documentation

## Overview

`ThreadPool.ProducerConsumer` uses a fixed-capacity circular FIFO buffer. Calls
to `Queue` or `TryQueue` produce work, and a fixed set of workers consume it.
v0.8.0 makes both sides event-driven: workers wait for a not-empty signal and
bounded producers wait for a not-full signal.

## Components

### TProducerConsumerThreadPool

- Owns the queue, worker threads, completion event, and pending-work counter.
- Applies the lifecycle contract implemented by `TThreadPoolBase`.
- Wraps all four existing task signatures in reference-counted work items.
- Counts a task before it becomes visible to workers and decrements it in a
  worker `finally` block.

### TThreadSafeQueue

- Stores `IWorkItem` values in a fixed-size circular array.
- Uses head/tail indices for O(1) enqueue and dequeue.
- Protects array state with one critical section.
- Maintains manual-reset not-empty and not-full events under the same lock, so
  wakeups cannot be lost between a state check and an event reset.
- Rejects capacities less than one.

### TProducerConsumerWorkerThread

- Blocks indefinitely on the queue's not-empty event while idle.
- Drains available work after being woken.
- Captures task exceptions through `TThreadPoolBase.SetLastError`.
- Releases the work item and updates completion state in `finally`, independent
  of task or `OnError` callback failures.

## Event-driven queue algorithm

`TryEnqueue(Item, TimeoutMS)` attempts an enqueue under the queue lock. If the
buffer is full, it waits on the not-full event and recalculates the remaining
deadline after every wake. `TryDequeue` signals not-full whenever it frees a
slot, and resets not-empty when the last item is removed.

The inverse applies to consumers: enqueue signals not-empty, while the worker
waits without polling. Shutdown explicitly wakes all waiters after setting each
worker's `Terminated` flag.

Timeout rules are shared with the Simple pool:

- `0`: immediate attempt/check;
- finite value: milliseconds from the start of the call;
- `THREADPOOL_INFINITE`: no deadline.

The legacy `TBackpressureConfig` record remains source-compatible. Threshold
and low/medium delay values no longer cause sleeps. New code should express its
deadline directly with `TryQueue`.

## Lifecycle

Both pools use the same monotonic state machine:

```text
tpsAccepting -> tpsDraining -> tpsStopped
```

`Shutdown` performs these steps:

1. Atomically stop new admission.
2. Wait for submissions that already passed admission to finish enqueueing.
3. Wait for all accepted tasks to finish.
4. Set `Terminated`, wake, join, and free every worker.
5. Publish `tpsStopped` and wake concurrent shutdown callers.

Queueing after step 1 raises `EThreadPoolShutdown`. `Shutdown` is idempotent,
and invoking it from one of the pool's own workers raises instead of deadlocking
that worker on itself.

## Error containment

Task exceptions remain asynchronous and are stored in `LastError` plus the
bounded `Errors` collection. `OnError` runs on the worker that observed the
failure. The base class catches exceptions raised by the handler so user
diagnostic code cannot terminate a worker or skip completion accounting.

## Synchronization summary

| Mechanism | Protects or signals |
| --- | --- |
| Queue critical section | Circular buffer, head, tail, and count |
| Queue not-empty event | Sleeping consumers |
| Queue not-full event | Producers waiting for bounded capacity |
| Work-item critical section | Pending count and completion transition |
| Completion event | `WaitForAll`, including timeout overload |
| Base error lock | `LastError`, `Errors`, and `OnError` |
| Base lifecycle lock/events | Admission, active submitters, draining, stopped |

## Performance characteristics

- O(1) queue insertion and removal.
- No worker polling sleeps.
- No artificial delay while the queue still has space.
- Debug logging is disabled by default.
- A bounded queue still serializes its short state updates through one critical
  section; batching very small tasks may improve throughput further.

## Limitations

- Queue capacity and worker count are fixed after construction.
- No task priorities, result-bearing futures, or task cancellation yet.
- `OnError` executes on a worker and should remain short and thread-safe.
