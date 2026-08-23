program ParallelIndexProcessing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  ThreadPool.Simple;

const
  N = 100;

var
  Results: array[0..N - 1] of Integer;
  I, Total: Integer;

procedure Compute(Index: Integer);
begin
  Results[Index] := Index * Index;
end;

begin
  for I := 0 to N - 1 do
    GlobalThreadPool.Queue(@Compute, I);

  GlobalThreadPool.WaitForAll;

  Total := 0;
  for I := 0 to N - 1 do
    Inc(Total, Results[I]);

  WriteLn('Squares total: ', Total);
end.