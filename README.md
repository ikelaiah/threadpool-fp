# 🚀 ThreadPool for Free Pascal

[![Version](https://img.shields.io/badge/version-0.6.5-8B5CF6.svg)](CHANGELOG.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0+-60A5FA.svg)](https://www.lazarus-ide.org/)
![Supports Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Supports Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
![No Dependencies](https://img.shields.io/badge/dependencies-none-10B981.svg)
[![CI](https://github.com/ikelaiah/threadpool-fp/actions/workflows/ci.yml/badge.svg)](https://github.com/ikelaiah/threadpool-fp/actions/workflows/ci.yml)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Status](https://img.shields.io/badge/Status-Stable-10B981.svg)](README.md)

A lightweight, easy-to-use thread pool implementation for Free Pascal. Simplify parallel processing for simple tasks! ⚡

> [!IMPORTANT]
>
> Parallel processing can improve performance for CPU-intensive tasks that can be executed independently. However, not all tasks benefit from parallelization. See [Thread Management](#-thread-management) for important considerations.

> [!NOTE]
> This library was originally written to explore the concept of thread pools in Free Pascal. It has since grown into a stable, tested implementation suitable for simple parallel processing tasks.
>
> It is **not designed** for high-load or production-scale applications. For those use cases, see the alternatives below.

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
      - [1. Simple Thread Pool (`ThreadPool.Simple`)](#1-simple-thread-pool-threadpoolsimple)
      - [2. Producer-Consumer Thread Pool (`ThreadPool.ProducerConsumer`)](#2-producer-consumer-thread-pool-threadpoolproducerconsumer)
    - [Shared Features](#shared-features)
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
  - [📋 Changelog](CHANGELOG.md)

## ✨ Features

This library provides two thread pool implementations, each with its own strengths:

### ThreadPool Implementations

#### 1. Simple Thread Pool (`ThreadPool.Simple`)
```pascal
uses ThreadPool.Simple;
```
- Global singleton instance for quick use
- Direct task execution
- Automatic thread count management
- Best for simple parallel tasks
- Lower memory overhead

#### 2. Producer-Consumer Thread Pool (`ThreadPool.ProducerConsumer`)
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

> [!NOTE]
> Thread count is determined by `TThread.ProcessorCount` at startup and remains fixed. See [Thread Management](#-thread-management) for details.

## 🏃 Quick Start

> [!IMPORTANT]
> **On Linux/macOS, your program must use the `cthreads` unit — and it must be the *first* unit in your program's `uses` clause.**
>
> Free Pascal does not install a threading manager by default on Unix-like systems. Without `cthreads`, creating the thread pool fails at runtime with an access violation (exit code 217). Windows does not need it.
>
> ```pascal
> program MyApp;
> {$mode objfpc}{$H+}
> uses
>   {$IFDEF UNIX}
>   cthreads,            // MUST be first on Linux/macOS
>   {$ENDIF}
>   ThreadPool.Simple;   // or ThreadPool.ProducerConsumer
> ```
>
> The examples in this repository already include this guard — see `examples/Starter/Starter.lpr`.
>
> From the [official FPC documentation](https://www.freepascal.org/docs-html/rtl/cthreads/index.html): *"The cthreads unit simply needs to be included in the uses clause of the program, preferably the very first unit, and the initialization section of the unit will do all the work."*

### Simple Thread Pool
```pascal
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}  // see the note above — required on Linux/macOS
  ThreadPool.Simple;

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
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}  // see the note above — required on Linux/macOS
  ThreadPool.ProducerConsumer;

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


### Which implementation should I use?

```text
Need a thread pool?
├─ Tasks are fire-and-forget, count is predictable, low overhead wanted?
│  └─ → Use ThreadPool.Simple (or the GlobalThreadPool singleton)
└─ Producer can outpace consumers, or you need queue overflow control?
   └─ → Use ThreadPool.ProducerConsumer
```

**Use Simple Thread Pool when:**
- Direct task execution without queuing needed
- Task count is predictable and moderate
- Low memory overhead is important
- Global instance (GlobalThreadPool) convenience desired
- Simple error handling is sufficient

**Use Producer-Consumer Pool when:**
- High volume of tasks with rate control needed
- Backpressure handling required
- Queue overflow protection important
- Need detailed execution monitoring
- Want configurable retry mechanisms

### Queue overload reference

All four `Queue` overloads share the same pattern — pick the one that fits your task:

| Overload | Signature | Use when | Example |
| --- | --- | --- | --- |
| Plain procedure | `Queue(@MyProc)` | Standalone procedure, no shared state needed | File I/O, independent calculations |
| Object method | `Queue(@MyObj.MyMethod)` | Task needs access to object fields/state | Counter objects, result accumulators |
| Indexed procedure | `Queue(@MyProc, i)` | Loop parallelism over an array/range | `for i := 0 to N-1 do Queue(@Proc, i)` |
| Indexed method | `Queue(@MyObj.MyMethod, i)` | Loop parallelism + object state | Parallel array transform on an object |

> [!NOTE]
> `LastError` is **overwritten** (not appended) each time a task raises an exception. If you queue multiple tasks, only the last error is stored. Check `LastError` immediately after `WaitForAll` and call `ClearLastError` before reusing the pool.


## 📚 Examples

### Getting Started

1. 👋 **Starter** (`examples/Starter/Starter.lpr`)
   - The absolute minimum to compile and run
   - Heavily commented — every line explained
   - Best first file to read before the other examples

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
   uses
     {$IFDEF UNIX}cthreads,{$ENDIF}  // required on Linux/macOS (must be first)
     ThreadPool.Simple;
   ```
   
   For Producer-Consumer Thread Pool:
   ```pascal
   uses
     {$IFDEF UNIX}cthreads,{$ENDIF}  // required on Linux/macOS (must be first)
     ThreadPool.ProducerConsumer;
   ```

3. Start using:
   - Simple: Use `GlobalThreadPool` or create `TSimpleThreadPool`
   - Producer-Consumer: Create `TProducerConsumerThreadPool`

### Verify your setup

Compile and run the simplest demo from the command line to confirm everything is wired up correctly:

```bash
# Using the Free Pascal compiler directly
fpc -Fu./src examples/SimpleDemo/SimpleDemo.lpr && ./SimpleDemo

# Or build with Lazarus from the command line
lazbuild examples/SimpleDemo/SimpleDemo.lpi && ./SimpleDemo
```

Expected output (order may vary — tasks run in parallel):

```text
Demo of ThreadPool functionality:
--------------------------------
1. Queueing simple procedure
2. Queueing method of a class
3. Queueing indexed procedure
4. Queueing method with index of a class
--------------------------------
Waiting for all tasks to complete...
Simple procedure executed
Method executed
Indexed procedure executed with index: 1
Method with index executed: 2
--------------------------------
All tasks completed successfully!
```

> [!TIP]
> Make sure your source file starts with `{$mode objfpc}{$H+}`. Without this, Free Pascal defaults to TP/Delphi-7 mode and some syntax will not compile.
>
> On Linux/macOS, also ensure `{$IFDEF UNIX}cthreads{$ENDIF}` is the **first** unit in your program's `uses` clause (see the [Quick Start](#-quick-start) note). Forgetting it causes a runtime access violation, not a compile error — so the build succeeds but the program crashes when it creates the pool.

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


## ⚠️ Common Mistakes

### 1. Freeing an object before `WaitForAll`

```pascal
// WRONG — MyObject may be freed while worker threads are still calling its methods
MyObject := TMyClass.Create;
GlobalThreadPool.Queue(@MyObject.DoWork);
MyObject.Free;          // freed too early!
GlobalThreadPool.WaitForAll;

// CORRECT — always wait before freeing
MyObject := TMyClass.Create;
try
  GlobalThreadPool.Queue(@MyObject.DoWork);
  GlobalThreadPool.WaitForAll;  // wait first
finally
  MyObject.Free;        // safe to free now
end;
```

### 2. Forgetting `WaitForAll`

Without `WaitForAll`, your program may exit (and destroy the pool) while tasks are still running, causing access violations or silent data loss.

```pascal
// WRONG
for i := 0 to 99 do
  GlobalThreadPool.Queue(@ProcessItem, i);
// program exits here, tasks may never finish

// CORRECT
for i := 0 to 99 do
  GlobalThreadPool.Queue(@ProcessItem, i);
GlobalThreadPool.WaitForAll;
```

### 3. Only the last error is kept

`LastError` is overwritten on every exception — not appended. If multiple tasks fail, you only see the last one.

```pascal
// Queue several tasks that might fail
for i := 0 to 9 do
  Pool.Queue(@RiskyProc, i);
Pool.WaitForAll;

// Only the LAST exception is in LastError
if Pool.LastError <> '' then
  WriteLn('At least one task failed: ', Pool.LastError);
Pool.ClearLastError;
```

### 4. Freeing the global pool manually

`GlobalThreadPool` is managed by the unit's `initialization`/`finalization` blocks. Do **not** call `GlobalThreadPool.Free` — let the runtime clean it up.

```pascal
// WRONG
GlobalThreadPool.Free;  // double-free at program exit!

// CORRECT — just use it; finalization handles cleanup
GlobalThreadPool.Queue(@MyProc);
GlobalThreadPool.WaitForAll;
```

## 🚧 Planned/In Progress

- Richer error handling — collect all task errors (not just the last) and an optional `OnError` callback (planned for 0.7.0)
- Support for `procedure Queue(AMethod: TProc; AArgs: array of Const);`
- More comprehensive tests
- More examples

## 🤝 Contributing

Contributions are welcome — bug reports, docs, examples, and code. See
[CONTRIBUTING.md](CONTRIBUTING.md) for how to build, test, and submit changes.

## 👏 Acknowledgments

Special thanks to the Free Pascal and Lazarus communities and the creators of the threading libraries mentioned above for inspiration!
- [Mormot2 Threading Library](https://github.com/synopse/mORMot2)
- [ezthreads](https://github.com/mr-highball/ezthreads)
- [OmniThreadLibrary](https://github.com/gabr42/OmniThreadLibrary)


## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## 📋 Changelog

See [CHANGELOG.md](CHANGELOG.md) for the full version history.

---

💡 **More Tip**: Check out the examples directory for more usage patterns!

