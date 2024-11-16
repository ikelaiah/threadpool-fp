## 📚 API Documentation

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
   - Ensure shared resources are protected
   - Use `TCriticalSection` for thread-safe operations
   - Avoid writing to the same variables from multiple threads

2. **Object Lifetime**
   - Keep objects alive until their queued methods complete
   - Wait for tasks to finish before freeing objects
   - Use `try-finally` blocks for proper cleanup

3. **Best Practices**
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
  CustomPool: TThreadPool;
begin
  // Create pool with specific thread count
  CustomPool := TThreadPool.Create(4);  // 4 threads
  try
    CustomPool.Queue(@MyProcedure);
    CustomPool.WaitForAll;
  finally
    CustomPool.Free;
  end;
end;
```
