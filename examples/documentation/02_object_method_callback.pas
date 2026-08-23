program ObjectMethodCallback;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils, SyncObjs,
  ThreadPool.Simple;

type
  TCounter = class
  private
    FLock: TCriticalSection;
    FValue: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Amount: Integer);
    function GetValue: Integer;
  end;

constructor TCounter.Create;
begin
  FLock := TCriticalSection.Create;
  FValue := 0;
end;

destructor TCounter.Destroy;
begin
  FLock.Free;
  inherited;
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
    for I := 1 to 10 do
      GlobalThreadPool.Queue(@Counter.Add, 1);

    GlobalThreadPool.WaitForAll;

    WriteLn('Counted ', Counter.GetValue, ' items');
  finally
    Counter.Free;
  end;
end.