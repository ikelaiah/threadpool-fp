program CollectErrors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple;

procedure Fail;
begin
  raise Exception.Create('boom');
end;

var
  I: Integer;
begin
  GlobalThreadPool.ClearErrors;

  for I := 1 to 3 do
    GlobalThreadPool.Queue(@Fail);

  GlobalThreadPool.WaitForAll;

  WriteLn(GlobalThreadPool.ErrorCount, ' error(s) recorded');
  if GlobalThreadPool.ErrorCount > 0 then
    WriteLn(GlobalThreadPool.LastError);
end.