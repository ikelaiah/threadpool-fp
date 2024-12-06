## 📚 API Documentation

### Thread Pool Types

1. **GlobalThreadPool**
   - Singleton instance for simple usage
   - Automatically initialized
   - Suitable for most applications
   - Uses default thread count

2. **TSimpleThreadPool**
   - Custom instance creation
   - Control over thread count
   - Multiple pools possible
   - More control over lifetime


### Core Functions

#### Queue Operations

```pascal
// Queue a simple procedure
GlobalThreadPool.Queue(@MyProcedure);

// Queue a method of an object
GlobalThreadPool.Queue(@MyObject.MyMethod);

// Queue a procedure with an index
GlobalThreadPool.Queue(@MyIndexedProcedure, 42);

// Queue a method with an index
GlobalThreadPool.Queue(@MyObject.MyIndexedMethod, 42);
```

#### Error Handling

See `Test16_ExceptionHandling` and `Test18_ExceptionMessage`.

```pascal
var
  Pool: TSimpleThreadPool; 
begin
  Pool := TSimpleThreadPool.Create;
  try
    Pool.Queue(@RiskyProcedure);
    Pool.WaitForAll;
    
    // Check for errors after completion
    if Pool.LastError <> '' then
    begin
      WriteLn('Error occurred: ', Pool.LastError);
      // Error messages include thread ID and exception message
      // Example: "[Thread 1234] Test exception message"
    end;
    
    // Clear error state if needed for pool reuse
    Pool.ClearLastError;
  finally
    Pool.Free;
  end;
end;
```

#### Synchronization
```pascal
// Wait for all queued tasks to complete
GlobalThreadPool.WaitForAll;
```

### Usage Examples

#### 1. Simple Procedure
```pascal
procedure PrintHello;
begin
  WriteLn('Hello from thread!');
end;

begin
  GlobalThreadPool.Queue(@PrintHello);
  GlobalThreadPool.WaitForAll;
end;
```

#### 2. Object Method
```pascal
type
  TMyClass = class
    procedure ProcessData;
  end;

procedure TMyClass.ProcessData;
begin
  WriteLn('Processing in thread...');
end;

var
  MyObject: TMyClass;
begin
  MyObject := TMyClass.Create;
  try
    GlobalThreadPool.Queue(@MyObject.ProcessData);
    GlobalThreadPool.WaitForAll;
  finally
    MyObject.Free;
  end;
end;
```

#### 3. Indexed Operations
```pascal
procedure ProcessItem(index: Integer);
begin
  WriteLn('Processing item: ', index);
end;

var
  i: Integer;
begin
  // Process items 0 to 9 in parallel
  for i := 0 to 9 do
    GlobalThreadPool.Queue(@ProcessItem, i);
    
  GlobalThreadPool.WaitForAll;
end;
```

### 🚨 Important Notes

1. **Thread Safety**
   - Critical sections protect shared counters (Test13_ConcurrentQueueAccess)
   - Multiple WaitForAll calls are safe (Test16_MultipleWaits)
   - Queue operations are thread-safe (Test15_QueueAfterWait)
   - Object lifetime is properly managed (Test17_ObjectLifetime)

1. **Object Lifetime**
   - Keep objects alive until their queued methods complete
   - Wait for tasks to finish before freeing objects
   - Use `try-finally` blocks for proper cleanup

2. **Best Practices**
   - Don't queue too many small tasks
   - Consider batching small operations
   - Use indexed operations for better task distribution
   - Always call `WaitForAll` before accessing results

### ⚠️ Common Pitfalls

```pascal
// DON'T DO THIS - Object might be freed before method executes
var
  MyObject: TMyClass;
begin
  MyObject := TMyClass.Create;
  GlobalThreadPool.Queue(@MyObject.ProcessData);
  MyObject.Free;  // Wrong! Object freed too early
end;

// DO THIS INSTEAD
var
  MyObject: TMyClass;
begin
  MyObject := TMyClass.Create;
  try
    GlobalThreadPool.Queue(@MyObject.ProcessData);
    GlobalThreadPool.WaitForAll;  // Wait for completion
  finally
    MyObject.Free;  // Safe to free now
  end;
end;
```

### 🔧 Advanced Usage

#### Custom Thread Pool
```pascal
var
  CustomPool: TSimpleThreadPool;
begin
  // Thread count is automatically adjusted:
  // - Minimum: 4 threads
  // - Maximum: 2× ProcessorCount
  // - Default: ProcessorCount (when count = 0)
  CustomPool := TSimpleThreadPool.Create(4);
  try
    CustomPool.Queue(@MyProcedure);
    CustomPool.WaitForAll;
  finally
    CustomPool.Free;
  end;
end;
```
