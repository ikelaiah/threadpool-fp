# Callback Forms

ThreadPool-FP accepts four callback signatures. You pass each one with the
`@` address operator, just as you would to `TThread.Create` or a synchronous
dispatch loop.

Everything queues through the same four forms; only the payload differs.

## The four signatures

From `ThreadPool.Types`:

```pascal
TThreadProcedure      = procedure;
TThreadMethod         = procedure of object;
TThreadProcedureIndex = procedure(Index: Integer);
TThreadMethodIndex    = procedure(Index: Integer) of object;
```

| Form | Queue call | Index? | Object state? |
| --- | --- | --- | --- |
| Plain procedure | `Pool.Queue(@DoWork)` | no | no |
| Object method | `Pool.Queue(@Worker.DoWork)` | no | yes |
| Indexed procedure | `Pool.Queue(@ProcessItem, Index)` | yes | no |
| Indexed object method | `Pool.Queue(@Worker.ProcessItem, Index)` | yes | yes |

`Submit`, `TrySubmit`, and `SubmitRange` mirror the same four forms; see the
[reference API](../reference/simple-api.md) and
[Tasks & Batches](tasks-and-batches.md).

## Untyped positional callbacks

All four forms declare a parameter of the matching corner in the types above.
Plain procedures take **no** argument; indexed callbacks take exactly one
`Integer`. Indexed procedures and methods are convenient for processing
array indexes, record positions, file numbers, or any numbered item.

## Object methods keep a live reference

An object method callback captures the **object itself**, so the pool holds a
reference to its instance data. You are responsible for keeping that object
alive until the callback has finished running.

```pascal
Worker := TWorker.Create;
try
  GlobalThreadPool.Queue(@Worker.DoWork);
  GlobalThreadPool.WaitForAll; // wait before freeing the callback target
  Readln(Worker.ResultData);
finally
  Worker.Free;
end;
```

Freeing the object before `WaitForAll` is an access-violation hazard, because
a worker may call the method after the object is gone. The same rule applies
to `Submit` and to method targets used by an `OnError` handler.

## Objects you create inside a callback

If only the callback itself creates and destroys state, no cross-thread
lifetime rule applies: the callback owns that object and the callback runs on
a worker thread. Still keep any shared state thread-safe (see
[Thread Safety](thread-safety.md)).

## Indexed callbacks and loop parallelism

Indexed callbacks turn a `for` loop into parallel work:

```pascal
for I := 0 to High(Items) do
  GlobalThreadPool.Queue(@ProcessItem, I);
GlobalThreadPool.WaitForAll;
```

Each `ProcessItem(I)` runs on a worker, in an unspecified order and with up to
`ThreadCount` of them running at once. For very large ranges prefer
[`SubmitRange`](ranges.md), which bundles indexes into chunks and queues far
fewer entries.

## Passing data beyond an `Index`

An `Integer` index is intentionally narrow. To pass structured data:

- index into a pre-filled array or record list you own, and keep it alive until `WaitForAll`;
- give the indexed callback a numeric handle into a lookup table that the callback resolves under the same thread-safety rules as any shared state; or
- partition the work so one index selects one unit of work.

The library deliberately stays signature-based: it avoids generics, closures,
and per-task captive state to keep the public surface small.

## Compile-time summary

```pascal
procedure FreeProcedure;                                             // 1
procedure TWorker.Method;                                            // 2
procedure IndexedFree(Index: Integer);                               // 3
procedure TWorker.IndexedMethod(Index: Integer);                     // 4
```

All four are interchangeable on both pool classes and on
`IThreadPoolTaskSource`. See [Types & Interfaces](../reference/types-and-interfaces.md)
for the declarations and overload lists.

## Related

- [Simple Thread Pool](simple-thread-pool.md)
- [Tasks & Batches](tasks-and-batches.md)
- [Common Recipes](recipes.md) contains compiled, tested callback programs.
