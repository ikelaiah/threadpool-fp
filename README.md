# 🚀 ThreadPool for Free Pascal

A lightweight, easy-to-use thread pool implementation for Free Pascal. Simplify parallel processing for simple tasks! ⚡

> [!IMPORTANT]
>
> Parallel processing can improve performance for CPU-intensive tasks that can be executed independently. However, not all tasks benefit from parallelization. See [Thread Management](#-thread-management) for important considerations.

> [!NOTE]
> This library is an experimental project, as I was exploring the concept of thread pools and how to implement them in Free Pascal.
> 
> Hence, this library is **not suitable** for high-load applications. It is designed for simple parallel processing tasks that can be executed in parallel.

> [!TIP]
> 
> If you are looking for performant and battle-tested threading libraries, please check out these alternatives:
> 
> - [Mormot2 Threading Library](https://github.com/synopse/mORMot2) by [@synopse](https://github.com/synopse)
> - [ezthreads](https://github.com/mr-highball/ezthreads) by [@mr-highball](https://github.com/mr-highball)
> - [OmniThreadLibrary](https://github.com/gabr42/OmniThreadLibrary) by [@gabr42](https://github.com/gabr42) (Delphi-only)
> - Or use threading in other languages via DLL/EXE calls;
>    - Go lang with [Goroutines](https://go.dev/tour/concurrency/1)
>    - Python with [concurrent.futures](https://docs.python.org/3/library/concurrent.futures.html)
>    - Rust with [threadpool](https://github.com/lifthrasiir/threadpool)
>    - Any other language that supports modern threading

## ✨ Features & Implementations

This library provides two thread pool implementations, each with its own strengths:

### 1. 🚀 Simple Thread Pool (ThreadPool.Simple)
```pascal
uses ThreadPool.Simple;
```
- Global singleton instance for quick use
- Direct task execution
- Automatic thread count management
- Best for simple parallel tasks
- Lower memory overhead

### 2. 🏭 Producer-Consumer Thread Pool (ThreadPool.ProducerConsumer)
```pascal
uses ThreadPool.ProducerConsumer;
```
- Queue-based task processing (1024 items)
- Better for high volume tasks
- Handles queue full conditions
- More sophisticated error handling
- Full control over execution

The **Producer-Consumer Thread Pool** utilizes a fixed-size circular buffer combined with a simple fail-fast strategy fortask management:

- **Fixed-Size Circular Buffer**
  - **Capacity:** The task queue is limited to 1024 items to ensure predictable memory usage.
  - **Circular Nature:** Efficiently reuses buffer space without the need for dynamic resizing.

- **Fail-Fast Approach**
  - **Immediate Feedback:** When the queue reaches its maximum capacity, any attempt to enqueue additional tasks will fail instantly.
  - **Manual Handling Required:** Users must implement their own logic to handle scenarios where the task queue is full, such as retry mechanisms, task prioritization, or dropping tasks as necessary.

> [!WARNING]
> 
> Since the queue does not dynamically expand, it is crucial to manage task production rates to prevent queue saturation.Implement appropriate error handling to manage cases when the queue is full.


### 🎯 Shared Features

- **Thread Count Management**
  - Minimum 4 threads for optimal parallelism
  - Maximum 2× ProcessorCount to prevent overload
  - Fixed count after initialization
  
- **Task Types Support**
  - Simple procedures: `Pool.Queue(@MyProc)`
  - Object methods: `Pool.Queue(@MyObject.MyMethod)`
  - Indexed variants: `Pool.Queue(@MyProc, Index)`
  
- **Thread Safety**
  - Built-in synchronization
  - Safe resource sharing
  - Protected error handling
  
- **Error Management**
  - Thread-specific error capture
  - Error messages with thread IDs
  - Continuous operation after exceptions

### 🔄 Choosing an Implementation

**Use Simple Thread Pool when:**
- Quick, direct task execution needed
- Task count is moderate
- Memory overhead is a concern
- Global instance convenience desired

**Use Producer-Consumer Pool when:**
- High task volume expected
- Queue management needed
- Task buffering required
- Full execution control needed

> [!NOTE]
> Thread count is determined by `TThread.ProcessorCount` at startup and remains fixed. See [Thread Management](#-thread-management) for details.

### Example Comparison

**Simple Thread Pool:**
```pascal
uses ThreadPool.Simple;

begin
  // Use global instance
  GlobalThreadPool.Queue(@MyTask);
  GlobalThreadPool.WaitForAll;
end;
```

**Producer-Consumer Thread Pool:**
```pascal
uses ThreadPool.ProducerConsumer;

var
  Pool: TProducerConsumerThreadPool;
begin
  Pool := TProducerConsumerThreadPool.Create;
  try
    // Handle queue full condition
    try
      Pool.Queue(@MyTask);
    except
      on E: Exception do
        if E.Message = 'Queue is full' then
          // Handle full queue
    end;
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end;
```


## 🏃 Quick Start

```pascal
program QuickStart;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool.Simple;  // Note the .Simple unit suffix

// Simple parallel processing
procedure ProcessItem(index: Integer);
begin
  WriteLn('Processing item: ', index);
end;

var
  i: Integer;
begin
  // Queue multiple items for parallel processing
  for i := 1 to 5 do
    GlobalThreadPool.Queue(@ProcessItem, i);
    
  // Wait for all tasks to complete
  GlobalThreadPool.WaitForAll;
end.
```

## ⚠️ Error Handling

```pascal
program ErrorHandling;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool.Simple;

procedure RiskyProcedure;
begin
  raise Exception.Create('Something went wrong!');
end;

var
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4); // Create with 4 threads
  try
    Pool.Queue(@RiskyProcedure);
    Pool.WaitForAll;
    
    // Check for errors after completion
    if Pool.LastError <> '' then
    begin
      WriteLn('An error occurred: ', Pool.LastError);
      Pool.ClearLastError;  // Clear for reuse if needed
    end;
  finally
    Pool.Free;
  end;
end.
```

### 💡 Tips

> [!NOTE]
> - 🛡️ Exceptions in worker threads are caught and stored
> - 🔍 Error messages include thread IDs for debugging
> - ⚡ The pool continues operating after exceptions
> - 🔄 Error state can be cleared for reuse


### 🛠️ Custom Thread Pool

```pascal
var
  CustomPool: TSimpleThreadPool;
begin
  CustomPool := TSimpleThreadPool.Create(4);  // Minimum allowed threads
  try
    CustomPool.Queue(@MyProcedure);
    CustomPool.WaitForAll;
  finally
    CustomPool.Free;
  end;
end;
```

### 🛠️ When to Use Each 
    
**GlobalThreadPool**
- Simple parallel tasks
- One-off parallel operations
- When default thread count is sufficient
    
**TSimpleThreadPool**
- Custom thread count needs
- Multiple independent thread pools
- Advanced error handling needs
- When you need control over pool lifetime


## 📚 Examples

### Simple Thread Pool Examples

1. 🎓 **Simple Demo** (`examples/SimpleDemo/SimpleDemo.lpr`)
   - Basic usage with GlobalThreadPool
   - Demonstrates procedures and methods
   - Shows proper object lifetime

2. 🔢 **Thread Pool Demo** (`examples/SimpleThreadpoolDemo/SimpleThreadpoolDemo.lpr`)
   - Custom thread pool management
   - Thread-safe operations
   - Error handling patterns

3. 📝 **Word Counter** (`examples/SimpleWordCounter/SimpleWordCounter.lpr`)
   - Queue-based task processing
   - Thread-safe counters
   - File I/O with queue management

4. 🔢 **Square Numbers** (`examples/SimpleSquareNumbers/SimpleSquareNumbers.lpr`)
   - High volume task processing
   - Queue full handling
   - Performance comparison

### Producer-Consumer Examples

5. 🔢 **Square Numbers** (`examples/ProdConSquareNumbers/ProdConSquareNumbers.lpr`)
   - High volume task processing
   - Queue full handling

6. 📝 **Message Processor** (`examples/ProdConMessageProcessor/ProdConMessageProcessor.lpr`)
   - Queue-based task processing
   - Thread-safe message handling
   - Graceful shutdown


## 🛠️ Installation

1. Add the `src` directory to your project's search path
2. Choose your implementation:
   
   For Simple Thread Pool:
   ```pascal
   uses ThreadPool.Simple;
   ```
   
   For Producer-Consumer Thread Pool:
   ```pascal
   uses ThreadPool.ProducerConsumer;
   ```

3. Start using:
   - Simple: Use `GlobalThreadPool` or create `TSimpleThreadPool`
   - Producer-Consumer: Create `TProducerConsumerThreadPool`

## ⚙️ Requirements

- 💻 Free Pascal 3.2.2 or later
- 📦 Lazarus 3.6.0 or later
- 🆓 No external dependencies


## 📚 Documentation

- [ThreadPool.Simple API Documentation](docs/ThreadPool.Simple-API.md)
- [ThreadPool.Simple Technical Details](docs/ThreadPool.Simple-Technical.md)
- [ThreadPool.ProducerConsumer API Documentation](docs/ThreadPool.ProducerConsumer-API.md)
- [ThreadPool.ProducerConsumer Technical Details](docs/ThreadPool.ProducerConsumer-Technical.md)

## 🧪 Testing

1. Go to the `tests/` directory
2. Open `TestRunner.lpi` in Lazarus IDE and compile
3. Run `./TestRunner.exe -a -p --format=plain` to see the test results.
4. Ensure all tests pass to verify the library's functionality

May take up to 5 mins to run all tests.

## 🧵 Thread Management

### Simple Thread Pool
- **Thread Creation**
  - Automatic thread count adjustment
  - Minimum 4 threads enforced
  - Maximum 2× ProcessorCount
  - Created and started immediately

- **Thread Behavior**
  - Direct task execution
  - No queuing overhead
  - Continuous task processing
  - Clean shutdown handling

### Producer-Consumer Thread Pool
- **Thread Creation**
  - Uses CPU count by default
  - No enforced minimum/maximum
  - Created in suspended state
  - Explicitly started after creation

- **Thread Behavior**
  - Queue-based task processing
  - Sleep when queue empty (100ms)
  - Handles queue full conditions
  - Graceful termination support

### Common Thread Management
- Thread count fixed after creation
- No dynamic thread scaling
- Thread-safe operation
- Proper resource cleanup

> [!IMPORTANT]
> Both implementations:
> - Use `TThread.ProcessorCount` for defaults
> - Create threads at startup only
> - Maintain thread safety
> - Handle clean shutdown


## 👏 Acknowledgments

Special thanks to the Free Pascal and Lazarus communities and the creators of the threading libraries mentioned above for inspiration!
- [Mormot2 Threading Library](https://github.com/synopse/mORMot2)
- [ezthreads](https://github.com/mr-highball/ezthreads)
- [OmniThreadLibrary](https://github.com/gabr42/OmniThreadLibrary)


## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

---

💡 **More Tip**: Check out the examples directory for more usage patterns!

