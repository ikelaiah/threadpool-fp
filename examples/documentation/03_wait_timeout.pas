program WaitTimeout;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple, ThreadPool.Tasks;

procedure Pause;
begin
  Sleep(20);
end;

var
  Task: IThreadPoolTask;
begin
  Task := GlobalThreadPool.Submit(@Pause);

  if Task.WaitFor(2000) then
    WriteLn('Finished within 2 s')
  else
    WriteLn('Still busy');
end.