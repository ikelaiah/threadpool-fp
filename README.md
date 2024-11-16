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

- 🎯 Simple API for parallel processing
- 🔄 Support for procedures and methods
- 🔒 Thread-safe operations
- 🌍 Global thread pool instance (fixed thread count based on `TThread.ProcessorCount`)
- 📑 Indexed task support

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
The `GlobalThreadPool` creates threads based on `TThread.ProcessorCount`. However, please note:

> [!IMPORTANT]
> `TThread.ProcessorCount` has some limitations:
> - May return cores OR CPUs (deliberately unspecified)
> - Value is set at program start and may not reflect runtime changes
> - Should only be used as a rough indication of parallel execution capacity
> - No guarantee of actual available processing units

### Custom Thread Pool
```pascal
var
  CustomPool: TThreadPool;
begin
  // Create pool with specific thread count
  CustomPool := TThreadPool.Create(4);  // Force 4 threads
  try
    CustomPool.Queue(@MyProcedure);
    CustomPool.WaitForAll;
  finally
    CustomPool.Free;
  end;
end;
```

> [!TIP]
> Consider your specific use case when deciding thread count:
> - Too many threads can waste resources
> - Too few threads may not fully utilize the system
> - Monitor and test with your target systems
> - Consider making thread count configurable in production


## 👏 Acknowledgments

Special thanks to the Free Pascal and Lazarus communities and the creators of the threading libraries mentioned above for inspiration!


## 📄 License
MIT License


---

💡 **More Tip**: Check out the examples directory for more advanced usage patterns!

