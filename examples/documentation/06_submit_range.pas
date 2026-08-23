program SubmitRangeChunking;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  ThreadPool.Simple, ThreadPool.Tasks;

const
  N = 200;

var
  Results: array[0..N - 1] of Integer;
  I, Total: Integer;
  RangeTasks: IThreadPoolTaskBatch;

procedure Compute(Index: Integer);
begin
  Results[Index] := Index * 2;
end;

begin
  RangeTasks := GlobalThreadPool.SubmitRange(@Compute, 0, N - 1);
  RangeTasks.WaitFor;

  Total := 0;
  for I := 0 to N - 1 do
    Inc(Total, Results[I]);

  WriteLn('Range total: ', Total);
end.