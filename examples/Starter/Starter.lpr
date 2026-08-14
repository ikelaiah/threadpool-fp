program Starter;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  ThreadPool.Simple;

procedure ProcessItem(Index: Integer);
begin
  WriteLn('Processed item ', Index);
end;

var
  I: Integer;
begin
  for I := 1 to 5 do
    GlobalThreadPool.Queue(@ProcessItem, I);

  { Output order may vary because callbacks run concurrently. }
  GlobalThreadPool.WaitForAll;
  WriteLn('All items finished.');
end.
