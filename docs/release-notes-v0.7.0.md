# ThreadPool for Free Pascal — v0.7.0

This release adds **richer error handling**. Both thread pools can now report
*every* task that failed — not just the most recent one — and notify you the
moment a task raises. Existing code keeps working unchanged: `LastError` behaves
exactly as before.

## What's new

### Collect every task error

Previously, `LastError` held only the most recent failure — if several tasks
failed, earlier errors were lost. The new `Errors` collection keeps them all
(oldest first):

```pascal
var
  Msg: string;
begin
  Pool.ClearErrors;
  for i := 0 to 9 do
    Pool.Queue(@RiskyProc, i);
  Pool.WaitForAll;

  WriteLn(Pool.ErrorCount, ' task(s) failed:');
  for Msg in Pool.Errors do
    WriteLn('  - ', Msg);

  Pool.ClearErrors;  // resets the collection and LastError
end;
```

The collection is capped at `MAX_STORED_ERRORS` (1000); beyond that the oldest
messages are dropped, so a flood of failing tasks can't exhaust memory.

### React to failures as they happen

Assign an `OnError` callback to be notified immediately, instead of polling after
`WaitForAll`:

```pascal
// Called from a worker thread — keep it short and thread-safe; synchronize if
// it touches the UI or shared state.
Pool.OnError := @MyHandler.OnTaskError;
```

### New API (both pools)

| Member | Description |
| --- | --- |
| `Errors: TStringArray` | All captured task error messages, oldest first |
| `ErrorCount: Integer` | Number of messages currently held |
| `OnError: TThreadPoolErrorEvent` | Fired (on a worker thread) per failed task |
| `ClearErrors` | Clears both `Errors` and `LastError` |

The API lives in `TThreadPoolBase`, so `TSimpleThreadPool` and
`TProducerConsumerThreadPool` both get it.

## Under the hood

- Removed the now-redundant per-pool error lock; error capture is serialized by
  the base class, which fires `OnError` outside its lock to avoid deadlocks.
- Documentation corrected: task error messages are raw exception messages (an
  earlier doc note incorrectly claimed they included thread IDs).

## Upgrade notes

- **No breaking changes.** `LastError` and `ClearLastError` behave exactly as
  before; the new members are purely additive.
- `OnError` handlers run on a worker thread — synchronize any access to shared or
  UI state.

## Verification

- Test suite: **43 tests, 0 errors, 0 failures, 0 unfreed memory blocks**
  (8 new tests cover the error-collection API on both pools).
- Package and all 8 examples build cleanly.

## Full changelog

See [CHANGELOG.md](../CHANGELOG.md) for the complete version history.
