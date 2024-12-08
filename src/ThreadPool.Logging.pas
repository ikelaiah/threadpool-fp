unit ThreadPool.Logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TThreadSafeLogger = class
  private
    FLock: TCriticalSection;
    FEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const AMsg: string);
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  ThreadLogger: TThreadSafeLogger;

implementation

{ TThreadSafeLogger }

constructor TThreadSafeLogger.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FEnabled := False;  // Disabled by default
end;

destructor TThreadSafeLogger.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TThreadSafeLogger.Log(const AMsg: string);
begin
  if not FEnabled then Exit;
  
  FLock.Enter;
  try
    WriteLn(Format('[%d] %s', [GetCurrentThreadId, AMsg]));
  finally
    FLock.Leave;
  end;
end;

initialization
  ThreadLogger := TThreadSafeLogger.Create;
  
finalization
  ThreadLogger.Free;
  
end. 