program BoundedTryQueue;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils, SyncObjs,
  ThreadPool.Types, ThreadPool.Simple, ThreadPool.Tasks, ThreadPool.ProducerConsumer;

var
  Gate: TEvent;

procedure HoldCallbacks;
begin
  Gate.WaitFor(INFINITE);
end;

procedure Unused;
begin
end;

var
  Pool: TProducerConsumerThreadPool;
  I, Accepted: Integer;
begin
  Gate := TEvent.Create(nil, True, False, '');
  Pool := TProducerConsumerThreadPool.Create(4, 8);
  try
    { Occupy every worker so the queue fills deterministically. }
    for I := 1 to 4 do
      Pool.TryQueue(@HoldCallbacks, THREADPOOL_INFINITE);

    { Wait until the blockers are all held by workers. }
    while Pool.QueueCount > 0 do
      Sleep(1);

    Accepted := 0;
    for I := 1 to 100 do
    begin
      if Pool.TryQueue(@Unused, 0) then
        Inc(Accepted)
      else
        Break;
    end;

    WriteLn('Accepted ', Accepted, ' queued items');

    Gate.SetEvent;
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end.