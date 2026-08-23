# Threadpool-fp examples

Start with [`Starter`](Starter/). It is the smallest complete program and shows
the default `GlobalThreadPool.Queue` plus `WaitForAll` workflow.

## Learn the core API

| Example | What it demonstrates |
| --- | --- |
| [`Starter`](Starter/) | Smallest complete Simple-pool program |
| [`SimpleDemo`](SimpleDemo/) | Procedures, object methods, and indexed callbacks |
| [`ProdConSimpleDemo`](ProdConSimpleDemo/) | Creating, using, and freeing a bounded pool |
| [`TaskCoordination`](TaskCoordination/) | Task handles, batches, ranges, and cancellation |

## Focused recipes

| Goal | Examples |
| --- | --- |
| Parallel calculations | [`SimpleSquareNumbers`](SimpleSquareNumbers/), [`ProdConSquareNumbers`](ProdConSquareNumbers/) |
| Stateful callback objects | [`SimpleThreadpoolDemo`](SimpleThreadpoolDemo/), [`SimpleWordCounter`](SimpleWordCounter/) |
| Captured worker errors | [`SimpleErrorHandlingBasic`](SimpleErrorHandlingBasic/), [`SimpleErrorHandling`](SimpleErrorHandling/) |
| Bounded message processing | [`ProdConMessageProcessor`](ProdConMessageProcessor/) |

## Complete workflows

| Example | What it demonstrates |
| --- | --- |
| [`ParallelFileHasher`](ParallelFileHasher/) | Parallel local file I/O |
| [`ParallelUrlFetcher`](ParallelUrlFetcher/) | Bounded concurrent network requests |
| [`CoordinatedFileBackup`](CoordinatedFileBackup/) | Progress, failure policy, and pending cancellation |
| [`ParallelLogAnalyzer`](ParallelLogAnalyzer/) | Chunked analysis followed by a second parallel phase |

## Build all examples

Run one command from the repository root:

```powershell
.\build-examples.ps1
```

```sh
sh ./build-examples.sh
```

Executables are written to the ignored root-level `example-bin/` directory.
Pass `Default` to select the default build mode, or use `-Rebuild` in
PowerShell / `--rebuild` in the shell script to force a complete rebuild.

For guides, recipes, and the full API reference, see the
[online documentation](https://ikelaiah.github.io/threadpool-fp/).
