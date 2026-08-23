program BatchTasks;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple, ThreadPool.Tasks;

procedure Nop;
begin
  Sleep(1);
end;

var
  Batch: IThreadPoolTaskBatch;
  I: Integer;
begin
  Batch := NewThreadPoolTaskBatch;
  for I := 1 to 5 do
    Batch.Add(GlobalThreadPool.Submit(@Nop));

  Batch.WaitFor;

  WriteLn(Batch.Count, ' tasks, ',
          Batch.FinishedCount, ' finished, ',
          Batch.FailedCount, ' failed');
end.