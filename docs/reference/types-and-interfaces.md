# Types & Interfaces

ThreadPool-FP is organized into small units. This page is the map of the
public surface and the shared declarations behind the API reference pages.

## Units

| Unit | Level | Purpose |
| --- | --- | --- |
| `ThreadPool.Types` | Shared | Callback types, `IThreadPool`, `TThreadPoolBase`, lifecycle and error declarations, constants |
| `ThreadPool.Simple` | Public | `TSimpleThreadPool`, `GlobalThreadPool` |
| `ThreadPool.Tasks` | Public | `IThreadPoolTask`, `IThreadPoolTaskBatch`, `SubmitRange` machinery, `IThreadPoolTaskSource` |
| `ThreadPool.ProducerConsumer` | Public | `TProducerConsumerThreadPool`, bounded queue, `TBackpressureConfig` |
| `ThreadPool.Internal.WorkItems` | Internal | Shared callback work item used by both pools |

Family users typically `uses ThreadPool.Simple` (and `ThreadPool.Tasks` when
they declare task or batch variables). `ThreadPool.ProducerConsumer` is added
only for bounded pools. `ThreadPool.Internal.WorkItems` is implementation
support; applications should not construct work items directly.

## Callback types

```pascal
TThreadProcedure      = procedure;
TThreadMethod         = procedure of object;
TThreadProcedureIndex = procedure(Index: Integer);
TThreadMethodIndex    = procedure(Index: Integer) of object;
```

See [Callback Forms](../guides/callback-forms.md).

## Error callback

```pascal
TThreadPoolErrorEvent = procedure(const AMessage: string) of object;
```

Fired on a worker thread each time a queued task raises. Handlers must be
thread-safe and should not block.

## State enums

```pascal
TThreadPoolState = (
  tpsAccepting,   // the pool accepts new work
  tpsDraining,    // Shutdown began; leaving admission, draining accepted work
  tpsStopped      // permanently stopped; submissions raise EThreadPoolShutdown
);
```

```pascal
TThreadPoolTaskState = (
  ttsPending,    // accepted but no worker claimed it
  ttsRunning,    // a worker won the start transition
  ttsCompleted,  // callback returned normally
  ttsFailed,     // callback raised
  ttsCancelled   // cancellation won before worker start
);
```

## Constants

| Constant | Value | Meaning |
| --- | --- | --- |
| `THREADPOOL_INFINITE` | `High(Cardinal)` | No deadline for waits and timeouts |
| `MAX_STORED_ERRORS` | `1000` | Cap on `Errors`; oldest entries are dropped |

## Core interface: IThreadPool

```pascal
IThreadPool = interface
  // all four callback forms
  procedure Queue(AProcedure: TThreadProcedure); overload;
  procedure Queue(AMethod: TThreadMethod); overload;
  procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); overload;
  procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); overload;
  function TryQueue(AProcedure: TThreadProcedure; ATimeoutMS: Cardinal): Boolean; overload;
  // ... one overload per callback form ...
  procedure WaitForAll; overload;
  function WaitForAll(ATimeoutMS: Cardinal): Boolean; overload;
  procedure Shutdown;
  procedure ClearLastError;
  procedure ClearErrors;
  function GetLastError: string;
  function GetThreadCount: Integer;
  function GetErrors: TStringArray;
  function GetErrorCount: Integer;
  function GetOnError: TThreadPoolErrorEvent;
  procedure SetOnError(AValue: TThreadPoolErrorEvent);
  function GetState: TThreadPoolState;
  property LastError: string;
  property ThreadCount: Integer;
  property Errors: TStringArray;
  property ErrorCount: Integer;
  property OnError: TThreadPoolErrorEvent;
  property State: TThreadPoolState;
end;
```

This GUID and contract are v0.8-stable and unchanged, so third-party classes
implementing `IThreadPool` keep compiling.

> [!TIP]
> The canonical declarations live in `ThreadPool.Types`; the extract above
> omits method bodies and the property getters for readability.

## Capability interface: IThreadPoolTaskSource

The v0.9 task surface is described by a separate optional interface so that
`IThreadPool` itself did not change:

```pascal
IThreadPoolTaskSource = interface
  function Submit(...): IThreadPoolTask; overload;        // 4 callback forms
  function TrySubmit(...; ATimeoutMS: Cardinal;
    out ATask: IThreadPoolTask): Boolean; overload;       // 4 callback forms
  function SubmitRange(ACallback: TThreadProcedureIndex;
    AFirstIndex, ALastIndex: Integer;
    AChunkSize: Integer = 0): IThreadPoolTaskBatch; overload;
  function SubmitRange(ACallback: TThreadMethodIndex;
    AFirstIndex, ALastIndex: Integer;
    AChunkSize: Integer = 0): IThreadPoolTaskBatch; overload;
end;
```

Both concrete pools implement it, and `GlobalThreadPool` exposes the methods
directly.

## Task and batch interfaces

```pascal
IThreadPoolTask = interface
  procedure WaitFor; overload;
  function WaitFor(ATimeoutMS: Cardinal): Boolean; overload;
  function Cancel: Boolean;
  property State: TThreadPoolTaskState;
  property ErrorMessage: string;
  property IsFinished: Boolean;
end;
```

```pascal
IThreadPoolTaskBatch = interface
  procedure Add(const ATask: IThreadPoolTask);
  procedure WaitFor; overload;
  function WaitFor(ATimeoutMS: Cardinal): Boolean; overload;
  function CancelPending: Integer;
  property Count: Integer;
  property FinishedCount: Integer;
  property FailedCount: Integer;
  property CancelledCount: Integer;
  property Tasks[AIndex: Integer]: IThreadPoolTask; default;
end;
```

`TThreadPoolTaskArray = array of IThreadPoolTask`. `NewThreadPoolTaskBatch`
builds an empty batch.

See [Tasks & Batches](../guides/tasks-and-batches.md) and
[Parallel Ranges](../guides/ranges.md).

## Queue and worker interfaces (internal-facing)

`IWorkItem`, `IWorkQueue`, and `IWorkerThread` describe the internal machinery.
They are declared in `ThreadPool.Types` and implemented by both pools; return
to them through [Internals](../internals/simple-internals.md), not through
application code.

## Exceptions

| Exception | Raised when |
| --- | --- |
| `EThreadPoolShutdown` | Submitting after `Shutdown` began; `Shutdown` from a pool worker |
| `EQueueFullException` | Bounded `Queue`/`Submit` waiting past their deadline |
| `EThreadPoolDeadlock` | A callback waits on its own task; bounded `SubmitRange` from a pool worker |
| `EThreadPoolTaskSubmission` | A `SubmitRange` chunk was not accepted |
| `EArgumentOutOfRangeException` | Negative range chunk size; invalid queue capacity; invalid task index |

## Related

- [Simple API](simple-api.md)
- [Tasks API](tasks-api.md)
- [Producer-Consumer API](producer-consumer-api.md)
- [Contracts & Limitations](contracts-and-limitations.md)