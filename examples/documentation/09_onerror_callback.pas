program OnErrorCallback;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, // must be first on Linux and macOS
  {$ENDIF}
  SysUtils, SyncObjs,
  ThreadPool.Simple;

type
  TErrorRecorder = class
  private
    FLock: TCriticalSection;
    FCount: Integer;
  public
    constructor Create;
    procedure Handle(const AMessage: string);
    function GetCount: Integer;
  end;

constructor TErrorRecorder.Create;
begin
  FLock := TCriticalSection.Create;
  FCount := 0;
end;

procedure TErrorRecorder.Handle(const AMessage: string);
begin
  FLock.Enter;
  try
    Inc(FCount);
  finally
    FLock.Leave;
  end;
end;

function TErrorRecorder.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

procedure Fail;
begin
  raise Exception.Create('worker failure');
end;

var
  Recorder: TErrorRecorder;
  I: Integer;
begin
  Recorder := TErrorRecorder.Create;
  try
    GlobalThreadPool.OnError := @Recorder.Handle;
    GlobalThreadPool.ClearErrors;

    for I := 1 to 3 do
      GlobalThreadPool.Queue(@Fail);

    GlobalThreadPool.WaitForAll;

    WriteLn('OnError fired ', Recorder.GetCount, ' time(s)');
  finally
    GlobalThreadPool.OnError := nil;
    Recorder.Free;
  end;
end.