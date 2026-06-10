program Starter;

{ Hello, ThreadPool!
  ==================
  The simplest possible introduction to threadpool-fp.

  HOW TO COMPILE AND RUN
  ----------------------
  Option A — Free Pascal compiler:
    fpc -Fu../../src Starter.lpr && ./Starter

  Option B — Lazarus IDE:
    File > Open > Starter.lpi, then Run > Run (F9)

  Option C — lazbuild (command line):
    lazbuild Starter.lpi && ./Starter

  WHAT TO EXPECT
  --------------
  Five items are processed in parallel. Because threads run concurrently,
  the output lines may appear in a different order each run — that is normal
  and is exactly the point of a thread pool.

  NEXT STEPS
  ----------
  1. Change @ProcessItem to @MyObject.MyMethod to use an object method.
  2. Swap GlobalThreadPool for a custom TSimpleThreadPool(N) to control
     the number of threads.
  3. Browse the other examples/ folders for real-world patterns.
}

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,  // MUST be first: enables threading support on Unix/Linux
  {$ENDIF}
  Classes, SysUtils, ThreadPool.Simple;

// ---------------------------------------------------------------------------
// The work: a plain indexed procedure — the most common starting point.
// Each call receives its own index so it knows which item to process.
// ---------------------------------------------------------------------------
procedure ProcessItem(index: Integer);
begin
  // Simulate a small amount of work
  Sleep(10);
  WriteLn(Format('  Item %d processed in thread %d', [index, GetCurrentThreadId]));
end;

// ---------------------------------------------------------------------------
// Main program
// ---------------------------------------------------------------------------
const
  ITEM_COUNT = 5;

var
  i: Integer;
begin
  WriteLn('=== Hello, ThreadPool! ===');
  WriteLn(Format('Queuing %d items across %d worker threads...',
    [ITEM_COUNT, GlobalThreadPool.ThreadCount]));
  WriteLn;

  // Queue all items — they will run in parallel on worker threads
  for i := 1 to ITEM_COUNT do
    GlobalThreadPool.Queue(@ProcessItem, i);

  // Block until every queued item has finished
  // WARNING: Never free objects used by queued tasks before this call returns.
  GlobalThreadPool.WaitForAll;

  WriteLn;
  WriteLn('All items done!');

  // Optional: check whether any task raised an exception.
  // Only the last exception is stored — call ClearLastError to reset.
  if GlobalThreadPool.LastError <> '' then
  begin
    WriteLn('At least one task failed: ', GlobalThreadPool.LastError);
    GlobalThreadPool.ClearLastError;
  end;

  WriteLn;
  WriteLn('Press Enter to quit...');
  ReadLn;
end.
