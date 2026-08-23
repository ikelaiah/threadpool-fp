program ObserveOneTask;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils,
  ThreadPool.Simple, ThreadPool.Tasks;

procedure DoSomething;
begin
  Sleep(5);
end;

var
  Task: IThreadPoolTask;
begin
  Task := GlobalThreadPool.Submit(@DoSomething);

  if Task.WaitFor(1000) and (Task.State = ttsCompleted) then
    WriteLn('Task completed')
  else
    WriteLn('Task did not complete: ', Task.ErrorMessage);
end.