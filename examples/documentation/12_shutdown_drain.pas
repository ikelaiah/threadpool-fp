program ShutdownDrain;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SyncObjs,
  ThreadPool.Types, ThreadPool.Simple;

type
  TCounter = class
  private
    FLock: TCriticalSection;
    FValue: Integer;
  public
    constructor Create;
    procedure Add(Amount: Integer);
    function GetValue: Integer;
  end;

constructor TCounter.Create;
begin
  FLock := TCriticalSection.Create;
  FValue := 0;
end;

procedure TCounter.Add(Amount: Integer);
begin
  FLock.Enter;
  try
    Inc(FValue, Amount);
  finally
    FLock.Leave;
  end;
end;

function TCounter.GetValue: Integer;
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

var
  Counter: TCounter;
  I: Integer;
begin
  Counter := TCounter.Create;
  try
    for I := 1 to 100 do
      GlobalThreadPool.Queue(@Counter.Add, 1);

    GlobalThreadPool.Shutdown;

    WriteLn('Counted: ', Counter.GetValue);
    WriteLn('Stopped: ', GlobalThreadPool.State = tpsStopped);
  finally
    Counter.Free;
  end;
end.