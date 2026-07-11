program SimpleErrorHandling;

{ Error handling with ThreadPool (v0.7.0)
  ========================================
  Demonstrates the richer error-handling API added in v0.7.0:

    * LastError   — the most recent task error (unchanged from earlier versions)
    * Errors      — EVERY failed task's message, not just the last
    * ErrorCount  — how many tasks have failed
    * OnError     — a callback fired the moment a task fails
    * ClearErrors — reset the collection (and LastError) before reusing the pool

  HOW TO COMPILE AND RUN
  ----------------------
  Option A — Free Pascal compiler:
    fpc -Fu../../src SimpleErrorHandling.lpr && ./SimpleErrorHandling

  Option B — Lazarus IDE / lazbuild:
    lazbuild SimpleErrorHandling.lpi && ./SimpleErrorHandling

  Works the same with TProducerConsumerThreadPool — the error API lives on the
  shared base class. }

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,  // MUST be first: enables threading support on Unix/Linux
  {$ENDIF}
  Classes, SysUtils, SyncObjs, ThreadPool.Simple;

type
  { Collects OnError notifications. The handler runs on a worker thread, so it
    must be thread-safe — here a critical section guards the counter. }
  TErrorWatcher = class
  private
    FLock: TCriticalSection;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleError(const AMessage: string);  // assigned to Pool.OnError
    property Count: Integer read FCount;
  end;

constructor TErrorWatcher.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCount := 0;
end;

destructor TErrorWatcher.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TErrorWatcher.HandleError(const AMessage: string);
begin
  // Called from a worker thread — keep it short and synchronized.
  FLock.Enter;
  try
    Inc(FCount);
    WriteLn(Format('  [OnError] task failed: %s', [AMessage]));
  finally
    FLock.Leave;
  end;
end;

{ Some tasks succeed, some fail. The index decides which. }
procedure MaybeFail(index: Integer);
begin
  if (index mod 3) = 0 then
    raise Exception.CreateFmt('task %d hit a problem', [index]);
  // Successful tasks simply do nothing here.
end;

var
  Pool: TSimpleThreadPool;
  Watcher: TErrorWatcher;
  Msg: string;
  i: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  Watcher := TErrorWatcher.Create;
  try
    WriteLn('=== ThreadPool error handling (v0.7.0) ===');
    WriteLn;

    // 1) React to failures as they happen via OnError.
    Pool.OnError := @Watcher.HandleError;

    // 2) Queue 10 tasks; indices divisible by 3 (0,3,6,9) will fail.
    WriteLn('Queueing 10 tasks (indices divisible by 3 will fail)...');
    Pool.ClearErrors;  // start from a clean slate
    for i := 0 to 9 do
      Pool.Queue(@MaybeFail, i);

    Pool.WaitForAll;
    WriteLn;

    // 3) After WaitForAll, inspect the collected errors.
    WriteLn(Format('ErrorCount: %d task(s) failed', [Pool.ErrorCount]));
    WriteLn(Format('OnError fired %d time(s)', [Watcher.Count]));
    WriteLn(Format('LastError (most recent only): "%s"', [Pool.LastError]));
    WriteLn;

    WriteLn('All failures (from Pool.Errors):');
    for Msg in Pool.Errors do
      WriteLn('  - ', Msg);
    WriteLn;

    // 4) Reset and reuse the pool.
    Pool.ClearErrors;
    WriteLn(Format('After ClearErrors: ErrorCount = %d, LastError = "%s"',
      [Pool.ErrorCount, Pool.LastError]));

  finally
    Watcher.Free;
    Pool.Free;
  end;

  WriteLn;
  WriteLn('Press enter to quit ...');
  ReadLn;
end.
