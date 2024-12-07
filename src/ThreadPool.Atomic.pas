unit ThreadPool.Atomic;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TAtomicInteger = class
  private
    FValue: Integer;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function Increment: Integer;
    function IncrementBy(AValue: Integer): Integer;
    function GetValue: Integer;
  end;

implementation

constructor TAtomicInteger.Create;
begin
  inherited Create;
  FValue := 0;
  FLock := TCriticalSection.Create;
end;

destructor TAtomicInteger.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TAtomicInteger.Increment: Integer;
begin
  FLock.Enter;
  try
    Inc(FValue);
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

function TAtomicInteger.IncrementBy(AValue: Integer): Integer;
begin
  FLock.Enter;
  try
    Inc(FValue, AValue);
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

function TAtomicInteger.GetValue: Integer;
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

end. 