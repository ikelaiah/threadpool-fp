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
- 🎯 Multiple task types support (procedures, methods, indexed variants)
- 🔒 Thread-safe operation
- ⚠️ Exception handling and error reporting
- 🌍 Global pool instance for convenience

> [!NOTE]
> The thread count is determined by `TThread.ProcessorCount` at startup and remains fixed. See [Thread Management](#-thread-management) for important details and limitations.

## 🏃 Quick Start

```pascal
uses
  ThreadPool;

// Simple parallel processing
procedure ProcessItem(index: Integer);
begin
  WriteLn('Processing item: ', index);
end;

begin
  // Queue multiple items for parallel processing
  for i := 1 to 5 do
    GlobalThreadPool.Queue(@ProcessItem, i);
    
  // Wait for all tasks to complete
  GlobalThreadPool.WaitForAll;
end;
```

## ⚠️ Error Handling


```pascal
uses
  ThreadPool;

var
  Pool: TThreadPool;

begin
  Pool := TThreadPool.Create(4); // Create with 4 threads
  try
    Pool.Queue(@MyProcedure);
    Pool.WaitForAll;
    // Check for errors
    if Pool.LastError <> '' then
      WriteLn('An error occurred: ', Pool.LastError);
  finally
    Pool.Free;
  end;
end;
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
  CustomPool: TThreadPool;
begin
  CustomPool := TThreadPool.Create(4);  // Minimum allowed threads
  try
    CustomPool.Queue(@MyProcedure);
    CustomPool.WaitForAll;
  finally
    CustomPool.Free;
  end;
end;
```

## 📚 Examples

1. 📝 **Word Counter** - Parallel text file processing
2. 🔢 **Square Numbers** - Basic number crunching
3. 🎓 **Simple Demo** - Various usage patterns

## 🛠️ Installation

1. Add the `src` directory to your project's search path
2. Add `ThreadPool` to your uses clause
3. Start using the `GlobalThreadPool` instance

## ⚙️ Requirements

- 💻 Free Pascal 3.2.2 or later
- 📦 Lazarus 3.6.0 or later
- 🆓 No external dependencies


## 📚 Documentation

- [API Documentation](docs/API-Docs.md)
- [Technical Details](docs/TECHNICAL.md)

## 🧪 Testing

1. Go to `tests/` directory
2. Open `TestRunner.lpi` in Lazarus IDE and compile
3. Run `./TestRunner.exe -a -p --format=plain` to see the test results.

## 🧵 Thread Management

### Default Behavior
The `GlobalThreadPool` creates threads with these safety limits:
- ⬇️ Values below 4 are increased to 4
- ⬆️ Values above 2× ProcessorCount are reduced
- 🎯 Invalid values (≤ 0) default to ProcessorCount

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


## 📄 License
MIT License


---

💡 **More Tip**: Check out the examples directory for more advanced usage patterns!

