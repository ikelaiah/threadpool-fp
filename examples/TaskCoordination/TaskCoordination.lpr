program TaskCoordination;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, ThreadPool.Tasks, ThreadPool.Simple;

const
  ITEM_COUNT = 20;

var
  Results: array[0..ITEM_COUNT - 1] of Integer;

procedure DoOneTask;
begin
  Sleep(10);
end;

procedure FailTask;
begin
  raise Exception.Create('demonstration failure');
end;

procedure ProcessItem(AIndex: Integer);
begin
  Results[AIndex] := AIndex * AIndex;
end;

function TaskStateName(AState: TThreadPoolTaskState): string;
begin
  case AState of
    ttsPending: Result := 'pending';
    ttsRunning: Result := 'running';
    ttsCompleted: Result := 'completed';
    ttsFailed: Result := 'failed';
    ttsCancelled: Result := 'cancelled';
  end;
end;

var
  Task: IThreadPoolTask;
  Batch, RangeTasks: IThreadPoolTaskBatch;
  I: Integer;
begin
  { Submit returns a handle for observing and waiting for one task. }
  Task := GlobalThreadPool.Submit(@DoOneTask);
  if Task.WaitFor(1000) then
    WriteLn('Individual task: ', TaskStateName(Task.State));

  Task := GlobalThreadPool.Submit(@FailTask);
  Task.WaitFor;
  if Task.State = ttsFailed then
    WriteLn('Failed task: ', Task.ErrorMessage);

  { A batch coordinates any collection of task handles. }
  Batch := NewThreadPoolTaskBatch;
  for I := 1 to 3 do
    Batch.Add(GlobalThreadPool.Submit(@DoOneTask));
  Batch.WaitFor;
  WriteLn('Batch finished: ', Batch.FinishedCount, '/', Batch.Count);

  { SubmitRange queues chunks rather than one task per index. Bounds are
    inclusive, and automatic chunking is selected by the default zero size. }
  RangeTasks := GlobalThreadPool.SubmitRange(
    @ProcessItem, 0, High(Results));
  RangeTasks.WaitFor;
  WriteLn('Range chunks: ', RangeTasks.Count);
  WriteLn('Last square: ', Results[High(Results)]);

  { Cancel succeeds only while a task is still pending. It never terminates a
    callback that has started running. }
  Task := GlobalThreadPool.Submit(@DoOneTask);
  if Task.Cancel then
    WriteLn('Optional task cancelled before it started')
  else
  begin
    Task.WaitFor;
    WriteLn('Optional task had already started: ', TaskStateName(Task.State));
  end;
end.
