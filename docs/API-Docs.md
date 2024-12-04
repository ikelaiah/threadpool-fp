## 📚 API Documentation

### Core Functions

#### Queue Operations

```pascal
// Basic Queue Operations
GlobalThreadPool.Queue(@MyProcedure);                    // Simple procedure
GlobalThreadPool.Queue(@MyObject.MyMethod);              // Object method
GlobalThreadPool.Queue(@MyIndexedProcedure, 42);         // Indexed procedure
GlobalThreadPool.Queue(@MyObject.MyIndexedMethod, 42);   // Indexed method

// Priority Queue Operations
GlobalThreadPool.QueueWithPriority(@MyProcedure, tpHigh);  // High priority task
GlobalThreadPool.QueueWithPriority(@MyProcedure);          // Normal priority (default)
```

#### Task Management
```pascal
// Create dependent tasks
var
  Task1, Task2: TWorkItem;
begin
  Task1 := GlobalThreadPool.Queue(@FirstProcedure);
  Task2 := GlobalThreadPool.Queue(@SecondProcedure);
  GlobalThreadPool.AddDependency(Task2, Task1); // Task2 waits for Task1
end;

// Check task status
if Task1.Status = tsCompleted then
  WriteLn('Task completed successfully');
```

#### Thread Pool Configuration and Scaling
```pascal
// Basic thread count configuration
GlobalThreadPool.MinThreads := 4;              // Minimum number of threads
GlobalThreadPool.MaxThreads := 16;             // Maximum number of threads
GlobalThreadPool.LoadCheckInterval := 500;     // Check workload every 500ms
```

### Dynamic Thread Scaling

The thread pool automatically adjusts its thread count based on workload:

1. **Scaling Parameters**
```pascal
type
  TThreadPool = class
  public
    property MinThreads: Integer;          // Minimum threads (default: max(4, CPU count))
    property MaxThreads: Integer;          // Maximum threads (default: CPU count × 2)
    property LoadCheckInterval: Integer;    // Milliseconds between checks (default: 1000)
  end;
```

2. **How Scaling Works**
- A dedicated monitor thread checks workload every `LoadCheckInterval` milliseconds
- Threads are added when utilization is high (more tasks than optimal per thread)
- Threads are removed when utilization is low (fewer tasks than optimal)
- Thread count always stays between `MinThreads` and `MaxThreads`

3. **Default Settings**
```pascal
// These are set automatically in TThreadPool.Create
MinThreads := Max(4, TThread.ProcessorCount);
MaxThreads := TThread.ProcessorCount * 2;
LoadCheckInterval := 1000;  // 1 second
TargetQueueLength := 4;     // Aim for 4 tasks per thread
```

### Best Practices for Scaling

1. **Setting Thread Limits**
```pascal
// For CPU-intensive tasks
ThreadPool.MinThreads := TThread.ProcessorCount;
ThreadPool.MaxThreads := TThread.ProcessorCount;

// For I/O-heavy tasks
ThreadPool.MinThreads := 8;
ThreadPool.MaxThreads := 32;
```

2. **Adjusting Check Interval**
```pascal
// More responsive scaling (but more overhead)
ThreadPool.LoadCheckInterval := 250;  // 250ms

// Less overhead (but slower to adapt)
ThreadPool.LoadCheckInterval := 2000; // 2 seconds
```

### Task Priorities

```pascal
type
  TTaskPriority = (
    tpLow,      // Background tasks
    tpNormal,   // Default priority
    tpHigh,     // Important tasks
    tpCritical  // Urgent tasks
  );
```

### Task Status

```pascal
type
  TTaskStatus = (
    tsQueued,     // Task is queued but not started
    tsExecuting,  // Task is currently executing
    tsCompleted,  // Task completed successfully
    tsFailed      // Task failed with error
  );
```

### Usage Examples

#### 1. Priority-based Tasks
```pascal
procedure ProcessImportantData;
begin
  WriteLn('Processing high priority data...');
end;

begin
  GlobalThreadPool.QueueWithPriority(@ProcessImportantData, tpHigh);
  GlobalThreadPool.WaitForAll;
end;
```

#### 2. Dependent Tasks
```pascal
var
  PrepTask, ProcessTask: TWorkItem;
begin
  PrepTask := GlobalThreadPool.Queue(@PrepareData);
  ProcessTask := GlobalThreadPool.Queue(@ProcessData);
  GlobalThreadPool.AddDependency(ProcessTask, PrepTask);
  GlobalThreadPool.WaitForAll;
end;
```

### 🚨 Important Notes

1. **Thread Safety**
   - Tasks are executed in priority order (Critical → High → Normal → Low)
   - Thread count automatically adjusts based on workload
   - Dependencies are checked before task execution

2. **Dynamic Thread Scaling**
   - Pool automatically adjusts thread count based on workload
   - Respects MinThreads and MaxThreads limits
   - Adjustments occur at LoadCheckInterval intervals

3. **Best Practices**
   - Use appropriate task priorities
   - Set reasonable thread count limits
   - Consider dependencies when ordering tasks
   - Monitor task status for error handling

### ⚠️ Common Pitfalls

```pascal
// DON'T DO THIS - Priority abuse
// Setting everything to Critical defeats the purpose
GlobalThreadPool.QueueWithPriority(@Task1, tpCritical);
GlobalThreadPool.QueueWithPriority(@Task2, tpCritical);
GlobalThreadPool.QueueWithPriority(@Task3, tpCritical);

// DO THIS INSTEAD - Use priorities appropriately
GlobalThreadPool.QueueWithPriority(@UrgentTask, tpCritical);
GlobalThreadPool.QueueWithPriority(@ImportantTask, tpHigh);
GlobalThreadPool.QueueWithPriority(@NormalTask, tpNormal);
GlobalThreadPool.QueueWithPriority(@BackgroundTask, tpLow);
```

### 🔧 Advanced Usage

#### Custom Thread Pool with Dynamic Scaling
```pascal
var
  CustomPool: TThreadPool;
begin
  CustomPool := TThreadPool.Create(4);  // Initial 4 threads
  try
    CustomPool.MinThreads := 2;
    CustomPool.MaxThreads := 8;
    CustomPool.LoadCheckInterval := 250; // More frequent checks
    
    // Queue some work
    CustomPool.QueueWithPriority(@ImportantTask, tpHigh);
    CustomPool.Queue(@NormalTask);
    
    CustomPool.WaitForAll;
  finally
    CustomPool.Free;
  end;
end;
```
