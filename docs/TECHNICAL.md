# ThreadPool-FP Technical Documentation

## Architecture Overview


```mermaid
graph TD
    A1[Application] --> G[GlobalThreadPool]
    A2[Application] --> C1[Custom ThreadPool]
    
    subgraph "Thread Pool"
        G & C1 --> |owns| TL[TThreadList]
        G & C1 --> |owns| WQ[TThreadList<br>Work Queue]
        G & C1 --> |owns| CS[TCriticalSection<br>Work Item Count]
        G & C1 --> |owns| EV[TEvent<br>Completion Signal]
        
        TL --> |contains| W1[Worker Thread 1]
        TL --> |contains| W2[Worker Thread 2]
        TL --> |contains| Wn[Worker Thread n]
        
        W1 & W2 & Wn -->|pop & execute| WQ
    end
    
    subgraph "Work Items"
        P1[TThreadProcedure] --> |wrapped as| WI[TWorkItem]
        P2[TThreadMethod] --> |wrapped as| WI
        P3[TThreadProcedureIndex] --> |wrapped as| WI
        P4[TThreadMethodIndex] --> |wrapped as| WI
        WI -->|queued in| WQ
    end
```

## Core Components

### TThreadPool

Main thread pool manager that:

- Maintains worker threads using `TThreadList`
- Manages work item queue using `TThreadList`
- Tracks work item count with `TCriticalSection`
- Signals completion using `TEvent`
- Provides thread-safe queueing methods
- Handles shutdown and cleanup

### TWorkerThread

Worker thread implementation that:

- Inherits from `TThread`
- Runs suspended until explicitly started
- Continuously polls for work items
- Executes work items when available
- Sleeps when idle to prevent busy waiting
- Terminates cleanly on pool shutdown

### TWorkItem

Task wrapper that:

- Encapsulates four types of work:
  - Simple procedures (`TThreadProcedure`)
  - Object methods (`TThreadMethod`)
  - Indexed procedures (`TThreadProcedureIndex`)
  - Indexed methods (`TThreadMethodIndex`)
- Manages task execution
- Updates completion status


## Usage Patterns

### 1. Simple Procedure

```pascal
GlobalThreadPool.Queue(@SimpleProcedure);
```

### 2. Method of Object

```pascal
GlobalThreadPool.Queue(@SimpleProcedure);
```


### 3. Indexed Operations


```pascal
GlobalThreadPool.Queue(@ProcessItem, itemIndex);
```

## Example Implementation

See examples in the [examples](../examples) directory.

## Thread Safety

The implementation uses several synchronization mechanisms:

- `TThreadList` for thread and work item collections
- `TCriticalSection` for work item count
- `TEvent` for completion signaling
- Thread-safe queue operations

## Performance Considerations

- Thread count is based on `TThread.ProcessorCount` (see [limitations](#system-processor-count-detection-limitations) below)
- Sleep(1) prevents busy waiting in worker threads
- Batch processing recommended for small tasks
- Consider task granularity to avoid overhead

## Thread Management

### Thread Count Initialization

The default thread count uses `TThread.ProcessorCount`, but this has important limitations:

```pascal
constructor TThreadPool.Create(AThreadCount: Integer = 0);
begin
  inherited Create;
  
  // Note: ProcessorCount limitations:
  // - May be cores or CPUs (unspecified)
  // - Set at program start
  // - May not reflect runtime changes
  if AThreadCount <= 0 then
    AThreadCount := TThread.ProcessorCount;
    
  FThreadCount := AThreadCount;
end;
```

### System Processor Count Detection Limitations

- `TThread.ProcessorCount` is set once at program start
- No guarantee whether it counts cores or CPUs
- May not reflect dynamic system changes
- Should be treated as approximate guidance only

### Performance Considerations
1. **Sleep Prevention**: Worker threads use `Sleep(1)` when idle
2. **Task Granularity**: Consider overhead vs task size
3. **Thread Count**: Default uses system processor count
4. **Shutdown**: Clean termination of all threads
5. **Resource Management**: Proper cleanup in destructor
