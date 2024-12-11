# 📚 ThreadPool.ProducerConsumer API

## 🔧 Core Components

### Thread Pool Types

1. **TProducerConsumerThreadPool**
   - Custom instance creation
   - Control over thread count
   - Fixed-size work queue (1024 items)
   - Thread-safe operation
   - Error handling support

### Core Functions

#### Queue Operations

```pascal
// Queue a simple procedure
Pool.Queue(@MyProcedure);

// Queue a method of an object
Pool.Queue(@MyObject.MyMethod);

// Queue a procedure with an index
Pool.Queue(@MyIndexedProcedure, 42);

// Queue a method with an index
Pool.Queue(@MyObject.MyIndexedMethod, 42);
```

#### Error Handling
```pascal
var
  Pool: TProducerConsumerThreadPool;
begin
  Pool := TProducerConsumerThreadPool.Create;
  try
    Pool.ClearLastError;  // Clear any previous errors
    Pool.Queue(@RiskyOperation);
    Pool.WaitForAll;
    
    // Check for errors after completion
    if Pool.LastError <> '' then
      WriteLn('Error occurred: ', Pool.LastError);
  finally
    Pool.Free;
  end;
end;
```

#### Synchronization
```pascal
// Wait for all queued tasks to complete
Pool.WaitForAll;
```

### 📋 API Reference

#### Constructor
```pascal
constructor Create(AThreadCount: Integer = 0);
```
Creates a new thread pool instance.
- `AThreadCount`: Number of worker threads. If 0 or negative, uses CPU count.

#### Queue Methods
```pascal
procedure Queue(AProcedure: TThreadProcedure);
procedure Queue(AMethod: TThreadMethod);
procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
```
All Queue methods:
- Are thread-safe
- Raise Exception if queue is full
- Support different task types

#### Control Methods
```pascal
procedure WaitForAll;    // Wait for completion
procedure ClearLastError;  // Clear error state
```

#### Properties
```pascal
property ThreadCount: Integer;  // Number of worker threads
property LastError: string;     // Last error message
```

### 🚨 Important Notes

1. **Thread Safety**
   - All operations are thread-safe
   - Queue has fixed capacity (1024 items)
   - Queue full raises exception
   - Error handling is thread-safe

2. **Object Lifetime**
   - Keep objects alive until their methods complete
   - Wait for tasks before freeing objects
   - Use try-finally blocks for cleanup

3. **Best Practices**
   - Check queue capacity limits
   - Handle queue full exceptions
   - Clear errors before reuse
   - Always wait for completion

### ⚠️ Common Pitfalls

```pascal
// DON'T DO THIS - Queue might be full
try
  for i := 1 to 2000 do  // Too many items!
    Pool.Queue(@MyProc);
except
  // Queue full exception
end;

// DO THIS INSTEAD
for i := 1 to 2000 do
begin
  try
    Pool.Queue(@MyProc);
  except
    on E: Exception do
      if E.Message = 'Queue is full' then
      begin
        Pool.WaitForAll;  // Wait for queue to clear
        Pool.Queue(@MyProc);  // Try again
      end;
  end;
end;

// DON'T DO THIS - Object freed too early
MyObject := TMyClass.Create;
Pool.Queue(@MyObject.MyMethod);
MyObject.Free;  // Wrong!

// DO THIS INSTEAD
MyObject := TMyClass.Create;
try
  Pool.Queue(@MyObject.MyMethod);
  Pool.WaitForAll;  // Wait for completion
finally
  MyObject.Free;  // Safe now
end;
```

### 🔧 Advanced Usage

#### Queue Management
```pascal
var
  Pool: TProducerConsumerThreadPool;
begin
  // Create pool with specific thread count
  Pool := TProducerConsumerThreadPool.Create(4);
  try
    // Queue until full
    while True do
    try
      Pool.Queue(@MyProc);
    except
      on E: Exception do
        if E.Message = 'Queue is full' then
          Break;
    end;
    
    // Process queue
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end;
```

### 📝 Type Definitions

```pascal
type
  TThreadProcedure = procedure;
  TThreadMethod = procedure of object;
  TThreadProcedureIndex = procedure(Index: Integer);
  TThreadMethodIndex = procedure(Index: Integer) of object;
```

### 🔍 Thread Management

1. **Thread Count Rules**
   - Minimum: Uses CPU count if AThreadCount ≤ 0
   - Maximum: No enforced limit
   - Default: CPU count when not specified
   - Thread creation: All threads created at startup

2. **Performance Tuning**
   - Worker threads sleep 100ms when idle
   - Fixed queue size affects memory usage
   - Consider queue capacity when designing tasks
   - Balance thread count with system resources

### 🛡️ Thread Safety Guarantees

1. **Queue Operations**
   - Thread-safe work item queueing
   - Protected work item count
   - Safe error state management
   - Synchronized completion tracking

2. **Resource Management**
   - Safe worker thread termination
   - Protected queue access
   - Thread-safe error handling
   - Proper cleanup in destructor

### ⚡ Performance Tips

1. **Queue Management**
   - Monitor queue capacity (1024 items)
   - Handle queue full conditions
   - Consider batching small tasks
   - Watch for queue saturation

2. **Task Design**
   - Keep tasks reasonably sized
   - Avoid very short tasks
   - Handle long-running tasks appropriately
   - Consider task dependencies

### 🚫 Limitations

1. **Queue Constraints**
   - Fixed capacity (1024 items)
   - No dynamic resizing
   - Blocking on queue full
   - No priority queueing

2. **Error Handling**
   - Single error storage
   - Last error overwrites previous
   - No error event mechanism
   - No error queueing

### 💡 Usage Recommendations

1. **Optimal Use Cases**
   - Batch processing tasks
   - Parallel computations
   - I/O operations
   - CPU-intensive work

2. **Less Suitable For**
   - Very short tasks (overhead)
   - Real-time operations
   - UI thread work
   - Critical timing requirements