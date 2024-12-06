# 🚀 ThreadPool for Free Pascal

A lightweight, easy-to-use thread pool implementation for Free Pascal. Simplify parallel processing for simple tasks! ⚡

> [!IMPORTANT]
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

## ✨ Features

- 🔄 Automatic thread count management
  - Minimum 4 threads for optimal parallelism
  - Maximum 2× ProcessorCount to prevent overload
  - Automatic adjustment to safe limits
  
- 🎯 Multiple task types support
  - Simple procedures: `GlobalThreadPool.Queue(@MyProc)`
  - Object methods: `GlobalThreadPool.Queue(@MyObject.MyMethod)`
  - Indexed variants: `GlobalThreadPool.Queue(@MyProc, Index)`
  
- 🔒 Thread-safe operation
  - Built-in synchronization
  - Safe resource sharing
  - Protected error handling
  
- ⚠️ Exception handling
  - Thread-specific error capture
  - Error messages with thread IDs
  - Pool continues after exceptions
  
- 🌍 Convenience
  - Global pool instance
  - Custom pool creation
  - Simple API

> [!NOTE]
> The thread count is determined by `TThread.ProcessorCount` at startup and remains fixed. See [Thread Management](#-thread-management) for important details and limitations.

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

1. 🎓 **Simple Demo** (`examples/SimpleDemo/SimpleDemo.lpr`)
   - Basic usage patterns with GlobalThreadPool
   - Demonstrates procedures, methods, and indexed variants
   - Shows proper object lifetime management

2. 🔢 **Thread Pool Demo** (`examples/SimpleThreadpoolDemo/SimpleThreadpoolDemo.lpr`)
   - Thread management with custom pools
   - Thread-safe counter operations
   - Error handling patterns
   - Resource cleanup

3. 📝 **Simple Word Counter** (`examples/SimpleWordCounter/SimpleWordCounter.lpr`)
   - Parallel text file processing
   - Thread-safe counter operations
   - File I/O with multiple threads


4. 🔢 **Square Numbers** (`examples/SimpleSquareNumbers/SimpleSquareNumbers.lpr`)
   - Basic number crunching
   - Array processing in parallel
   - Performance comparison


## 🛠️ Installation

1. Add the `src` directory to your project's search path
2. Add `ThreadPool.Simple` to your uses clause
3. Start using the `GlobalThreadPool` instance or create `TSimpleThreadPool` instances

## ⚙️ Requirements

- 💻 Free Pascal 3.2.2 or later
- 📦 Lazarus 3.6.0 or later
- 🆓 No external dependencies


## 📚 Documentation

- [API Documentation](docs/API-Docs.md)
- [Technical Details](docs/TECHNICAL.md)

## 🧪 Testing

1. Go to the `tests/` directory
2. Open `TestRunner.lpi` in Lazarus IDE and compile
3. Run `./TestRunner.exe -a -p --format=plain` to see the test results.
4. Ensure all tests pass to verify the library's functionality

## 🧵 Thread Management

### Default Behavior
The `GlobalThreadPool` creates threads with these safety limits:
- ⬇️ Values below 4 are increased to 4
- ⬆️ Values above 2× ProcessorCount are reduced
- 🎯 Invalid values (≤ 0) default to [TThread.ProcessorCount](https://www.freepascal.org/docs-html/rtl/classes/tthread.processorcount.html)

> [!IMPORTANT]
> Thread count is automatically adjusted to safe limits.

> [!TIP]
> The thread pool automatically manages safe thread counts:
> - Prevents too few threads (< 4) that could limit parallelism
> - Prevents too many threads that could overwhelm the system
> - Maintains reasonable scaling with available processors
> - No need to manually calculate safe limits


## 👏 Acknowledgments

Special thanks to the Free Pascal and Lazarus communities and the creators of the threading libraries mentioned above for inspiration!
- [Mormot2 Threading Library](https://github.com/synopse/mORMot2)
- [ezthreads](https://github.com/mr-highball/ezthreads)
- [OmniThreadLibrary](https://github.com/gabr42/OmniThreadLibrary)


## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

---

💡 **More Tip**: Check out the examples directory for more usage patterns!

