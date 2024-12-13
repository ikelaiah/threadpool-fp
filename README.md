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


## 📑 Table of Contents
- [🚀 ThreadPool for Free Pascal](#-threadpool-for-free-pascal)
  - [📑 Table of Contents](#-table-of-contents)
  - [✨ Features](#-features)
    - [ThreadPool Implementations](#threadpool-implementations)
      - [1. Simple Thread Pool (ThreadPool.Simple)](#1-simple-thread-pool-threadpoolsimple)
      - [2. Producer-Consumer Thread Pool (ThreadPool.ProducerConsumer)](#2-producer-consumer-thread-pool-threadpoolproducerconsumer)
    - [Shared Features](#shared-features)
    - [Choosing an Implementation](#choosing-an-implementation)
  - [🏃 Quick Start](#-quick-start)
    - [Simple Thread Pool](#simple-thread-pool)
    - [Producer-Consumer Thread Pool](#producer-consumer-thread-pool)
    - [Error Handling Simple Thread Pool](#error-handling-simple-thread-pool)
    - [Error Handling Producer-Consumer Thread Pool](#error-handling-producer-consumer-thread-pool)
    - [Tips](#tips)
  - [📚 Examples](#-examples)
    - [Simple Thread Pool Examples](#simple-thread-pool-examples)
    - [Producer-Consumer Examples](#producer-consumer-examples)
  - [🛠️ Installation](#️-installation)
  - [⚙️ Requirements](#️-requirements)
  - [📚 Documentation](#-documentation)
  - [🧪 Testing](#-testing)
  - [🧵 Thread Management](#-thread-management)
    - [Thread Count Rules](#thread-count-rules)
    - [Implementation Characteristics](#implementation-characteristics)
  - [🚧 Planned/In Progress](#-plannedin-progress)
  - [👏 Acknowledgments](#-acknowledgments)
  - [📄 License](#-license)

## ✨ Features

This library provides two thread pool implementations, each with its own strengths:

### ThreadPool Implementations

#### 1. Simple Thread Pool (ThreadPool.Simple)
```pascal
uses ThreadPool.Simple;
```
- Global singleton instance for quick use
- Direct task execution
- Automatic thread count management
- Best for simple parallel tasks
- Lower memory overhead

#### 2. Producer-Consumer Thread Pool (ThreadPool.ProducerConsumer)
```pascal
uses ThreadPool.ProducerConsumer;
```

A thread pool with fixed-size circular buffer (1024 items) and built-in backpressure handling:

- **Queue Management**
  - Fixed-size circular buffer for predictable memory usage
  - Efficient space reuse without dynamic resizing
  - Configurable capacity (default: 1024 items)

- **Backpressure Handling**
  - Load-based adaptive delays (10ms to 100ms)
  - Automatic retry mechanism (up to 5 attempts)
  - Throws EQueueFullException when retries exhausted

- **Monitoring & Debug**
  - Thread-safe error capture with thread IDs
  - Detailed debug logging (can be disabled)

Best for:
- High-volume task processing with rate control
- Systems needing graceful overflow handling
- Applications requiring execution monitoring

> [!WARNING]
> 
> While the system includes automatic retry mechanisms, it's recommended that users implement their own error handling strategies for scenarios where the queue remains full after all retry attempts.

### Shared Features

- **Thread Count Management**
  - Minimum 4 threads for optimal parallelism
  - Maximum 2× `ProcessorCount` to prevent overload
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

### Choosing an Implementation

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

## 🏃 Quick Start

### Simple Thread Pool
```pascal
uses ThreadPool.Simple;

// Simple parallel processing
procedure ProcessItem(index: Integer);
begin
  WriteLn('Processing item: ', index);
end;

begin
  // Queue multiple items
  for i := 1 to 5 do
    GlobalThreadPool.Queue(@ProcessItem, i);
    
  GlobalThreadPool.WaitForAll;
end;
```

### Producer-Consumer Thread Pool
```pascal
uses ThreadPool.ProducerConsumer;

procedure DoWork;
begin
  WriteLn('Working in thread: ', GetCurrentThreadId);
end;

var
  Pool: TProducerConsumerThreadPool;
begin
  Pool := TProducerConsumerThreadPool.Create;
  try
    Pool.Queue(@DoWork);
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end;
```

### Error Handling Simple Thread Pool

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

### Error Handling Producer-Consumer Thread Pool

```pascal
program ErrorHandling;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool.ProducerConsumer;

procedure RiskyProcedure;
begin
  raise Exception.Create('Something went wrong!');
end;

var
  Pool: TProducerConsumerThreadPool;
begin
  Pool := TProducerConsumerThreadPool.Create;
  try
    try
      Pool.Queue(@RiskyProcedure);
    except
      on E: EQueueFullException do
        WriteLn('Queue is full after retries: ', E.Message);
    end;
    
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

### Tips

> [!NOTE]
> **Error Handling**
> - 🛡️ Exceptions are caught and stored with thread IDs
> - ⚡ Pool continues operating after exceptions
> - 🔄 Use ClearLastError to reset error state
>
> **Debugging**
> - 🔍 Error messages contain thread identification
> - 📝 Debug logging enabled by default (configurable)
> - 📊 Queue capacity monitoring available


When to use each implementation?
    
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

5. 🎓 **Simple Demo** (`examples/ProdConSimpleDemo/ProdConSimpleDemo.lpr`)
   - Basic usage with ProducerConsumerThreadPool
   - Demonstrates procedures and methods
   - Shows proper object lifetime
   
6. 🔢 **Square Numbers** (`examples/ProdConSquareNumbers/ProdConSquareNumbers.lpr`)
   - High volume task processing
   - Queue full handling
   - Backpressure demonstration
   - Performance monitoring

7. 📝 **Message Processor** (`examples/ProdConMessageProcessor/ProdConMessageProcessor.lpr`)
   - Queue-based task processing
   - Thread-safe message handling
   - Graceful shutdown
   - Error handling patterns


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

### Thread Count Rules
- Default: Uses ProcessorCount when thread count ≤ 0
- Minimum: 4 threads enforced
- Maximum: 2× ProcessorCount
- Fixed after creation (no dynamic scaling)

### Implementation Characteristics

**Simple Thread Pool**
- Direct task execution without queuing
- Continuous task processing
- Clean shutdown handling

**Producer-Consumer Thread Pool**
- Fixed-size circular queue (1024 items by default, configurable)
- Backpressure handling with adaptive delays
- Graceful overflow management


## 🚧 Planned/In Progress
- Adaptive thread adjustment based on a load factor
- Support for `procedure Queue(AMethod: TProc; AArgs: array of Const);`
- More comprehensive tests
- More examples

## 👏 Acknowledgments

Special thanks to the Free Pascal and Lazarus communities and the creators of the threading libraries mentioned above for inspiration!
- [Mormot2 Threading Library](https://github.com/synopse/mORMot2)
- [ezthreads](https://github.com/mr-highball/ezthreads)
- [OmniThreadLibrary](https://github.com/gabr42/OmniThreadLibrary)


## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

---

💡 **More Tip**: Check out the examples directory for more usage patterns!

