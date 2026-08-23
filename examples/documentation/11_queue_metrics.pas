program QueueMetrics;

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
  I: Integer;
begin
  Gate := TEvent.Create(nil, True, False, '');
  Pool := TProducerConsumerThreadPool.Create(4, 32);
  try
    { Occupy every worker so nothing drains while we inspect the queue. }
    for I := 1 to 4 do
      Pool.TryQueue(@HoldCallbacks, THREADPOOL_INFINITE);

    { Wait until the blockers are all held by workers. }
    while Pool.QueueCount > 0 do
      Sleep(1);

    for I := 1 to 3 do
      Pool.TryQueue(@Unused, THREADPOOL_INFINITE);

    WriteLn('Capacity: ', Pool.QueueCapacity);
    WriteLn('Queued: ', Pool.QueueCount);
    WriteLn('Load: ', Pool.QueueLoadFactor:0:2);

    Gate.SetEvent;
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end.