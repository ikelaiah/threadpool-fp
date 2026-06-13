program SimpleErrorHandlingBasic;

{ Error handling the EASY way (v0.7.0)
  =====================================
  The simplest possible way to handle task errors: queue your work, call
  WaitForAll, then read the results. No callback, no custom class, no locking.

    * LastError   — the most recent task error (a single string)
    * ErrorCount  — how many tasks failed
    * Errors      — the message of EVERY failed task
    * ClearErrors — reset before reusing the pool

  Reading these AFTER WaitForAll is completely safe — the pool handles all the
  threading for you. You only need locks/callbacks if you want to react to
  failures *while tasks are still running* (see the SimpleErrorHandling example
  for that more advanced pattern).

  HOW TO COMPILE AND RUN
  ----------------------
  Option A — Free Pascal compiler:
    fpc -Fu../../src SimpleErrorHandlingBasic.lpr && ./SimpleErrorHandlingBasic

  Option B — Lazarus IDE / lazbuild:
    lazbuild SimpleErrorHandlingBasic.lpi && ./SimpleErrorHandlingBasic }

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,  // MUST be first: enables threading support on Unix/Linux
  {$ENDIF}
  Classes, SysUtils, ThreadPool.Simple;

{ A task that fails for some inputs and succeeds for others. }
procedure ProcessItem(index: Integer);
begin
  if (index mod 3) = 0 then
    raise Exception.CreateFmt('could not process item %d', [index]);
  // Successful items just do their work here (nothing, for the demo).
end;

var
  Msg: string;
  i: Integer;
begin
  WriteLn('=== Easy error handling (v0.7.0) ===');
  WriteLn;

  // 1) Queue some work. Items where index is divisible by 3 will fail.
  WriteLn('Queueing 10 tasks (items divisible by 3 will fail)...');
  for i := 0 to 9 do
    GlobalThreadPool.Queue(@ProcessItem, i);

  // 2) Wait for everything to finish.
  GlobalThreadPool.WaitForAll;

  // 3) Now it is safe to read the results — no locking needed.
  WriteLn;
  if GlobalThreadPool.ErrorCount = 0 then
    WriteLn('All tasks succeeded!')
  else
  begin
    WriteLn(GlobalThreadPool.ErrorCount, ' task(s) failed:');
    for Msg in GlobalThreadPool.Errors do
      WriteLn('  - ', Msg);

    WriteLn;
    WriteLn('Most recent error (LastError): ', GlobalThreadPool.LastError);
  end;

  // 4) Clear before reusing the global pool elsewhere in your program.
  GlobalThreadPool.ClearErrors;

  WriteLn;
  WriteLn('Press enter to quit ...');
  ReadLn;
end.
