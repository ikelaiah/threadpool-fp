program CancelPendingTask;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SyncObjs,
  ThreadPool.Simple, ThreadPool.Tasks, ThreadPool.ProducerConsumer;

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
  Tasks: array[1..4] of IThreadPoolTask;
  Task: IThreadPoolTask;
  I: Integer;
begin
  Gate := TEvent.Create(nil, True, False, '');
  Pool := TProducerConsumerThreadPool.Create(4, 16);
  try
    { Occupy every worker so the task below stays pending. }
    for I := 1 to 4 do
      Tasks[I] := Pool.Submit(@HoldCallbacks);

    Task := Pool.Submit(@Unused);

    if Task.Cancel then
      WriteLn('Cancelled while pending')
    else
      WriteLn('Could not cancel');

    Gate.SetEvent;
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;
end.