unit V08.Compatibility.Tests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, fpcunit, testregistry, ThreadPool.Types;

type
  { Compile sentinel for the exact v0.8 IThreadPool surface. If methods are
    added to that interface, this unchanged third-party implementation stops
    compiling and exposes the source-compatibility break. }
  TV08ThirdPartyPool = class(TInterfacedObject, IThreadPool)
  private
    FOnError: TThreadPoolErrorEvent;
  public
    procedure Queue(AProcedure: TThreadProcedure); overload;
    procedure Queue(AMethod: TThreadMethod); overload;
    procedure Queue(AProcedure: TThreadProcedureIndex;
      AIndex: Integer); overload;
    procedure Queue(AMethod: TThreadMethodIndex;
      AIndex: Integer); overload;
    function TryQueue(AProcedure: TThreadProcedure;
      ATimeoutMS: Cardinal): Boolean; overload;
    function TryQueue(AMethod: TThreadMethod;
      ATimeoutMS: Cardinal): Boolean; overload;
    function TryQueue(AProcedure: TThreadProcedureIndex; AIndex: Integer;
      ATimeoutMS: Cardinal): Boolean; overload;
    function TryQueue(AMethod: TThreadMethodIndex; AIndex: Integer;
      ATimeoutMS: Cardinal): Boolean; overload;
    procedure WaitForAll; overload;
    function WaitForAll(ATimeoutMS: Cardinal): Boolean; overload;
    procedure Shutdown;
    procedure ClearLastError;
    procedure ClearErrors;
    function GetLastError: string;
    function GetThreadCount: Integer;
    function GetErrors: TStringArray;
    function GetErrorCount: Integer;
    function GetOnError: TThreadPoolErrorEvent;
    procedure SetOnError(AValue: TThreadPoolErrorEvent);
    function GetState: TThreadPoolState;
  end;

  TV08CompatibilityTests = class(TTestCase)
  published
    procedure Test01_ThirdPartyInterfaceImplementationStillCompiles;
  end;

implementation

procedure TV08ThirdPartyPool.Queue(AProcedure: TThreadProcedure);
begin
  if Assigned(AProcedure) then
    AProcedure;
end;

procedure TV08ThirdPartyPool.Queue(AMethod: TThreadMethod);
begin
  if Assigned(AMethod) then
    AMethod;
end;

procedure TV08ThirdPartyPool.Queue(AProcedure: TThreadProcedureIndex;
  AIndex: Integer);
begin
  if Assigned(AProcedure) then
    AProcedure(AIndex);
end;

procedure TV08ThirdPartyPool.Queue(AMethod: TThreadMethodIndex;
  AIndex: Integer);
begin
  if Assigned(AMethod) then
    AMethod(AIndex);
end;

function TV08ThirdPartyPool.TryQueue(AProcedure: TThreadProcedure;
  ATimeoutMS: Cardinal): Boolean;
begin
  Queue(AProcedure);
  Result := True;
end;

function TV08ThirdPartyPool.TryQueue(AMethod: TThreadMethod;
  ATimeoutMS: Cardinal): Boolean;
begin
  Queue(AMethod);
  Result := True;
end;

function TV08ThirdPartyPool.TryQueue(AProcedure: TThreadProcedureIndex;
  AIndex: Integer; ATimeoutMS: Cardinal): Boolean;
begin
  Queue(AProcedure, AIndex);
  Result := True;
end;

function TV08ThirdPartyPool.TryQueue(AMethod: TThreadMethodIndex;
  AIndex: Integer; ATimeoutMS: Cardinal): Boolean;
begin
  Queue(AMethod, AIndex);
  Result := True;
end;

procedure TV08ThirdPartyPool.WaitForAll;
begin
end;

function TV08ThirdPartyPool.WaitForAll(ATimeoutMS: Cardinal): Boolean;
begin
  Result := True;
end;

procedure TV08ThirdPartyPool.Shutdown;
begin
end;

procedure TV08ThirdPartyPool.ClearLastError;
begin
end;

procedure TV08ThirdPartyPool.ClearErrors;
begin
end;

function TV08ThirdPartyPool.GetLastError: string;
begin
  Result := '';
end;

function TV08ThirdPartyPool.GetThreadCount: Integer;
begin
  Result := 0;
end;

function TV08ThirdPartyPool.GetErrors: TStringArray;
begin
  Result := nil;
end;

function TV08ThirdPartyPool.GetErrorCount: Integer;
begin
  Result := 0;
end;

function TV08ThirdPartyPool.GetOnError: TThreadPoolErrorEvent;
begin
  Result := FOnError;
end;

procedure TV08ThirdPartyPool.SetOnError(AValue: TThreadPoolErrorEvent);
begin
  FOnError := AValue;
end;

function TV08ThirdPartyPool.GetState: TThreadPoolState;
begin
  Result := tpsAccepting;
end;

procedure TV08CompatibilityTests.
  Test01_ThirdPartyInterfaceImplementationStillCompiles;
var
  Pool: IThreadPool;
begin
  Pool := TV08ThirdPartyPool.Create;
  AssertEquals(Ord(tpsAccepting), Ord(Pool.State));
  AssertTrue(Pool.WaitForAll(0));
end;

initialization
  RegisterTest(TV08CompatibilityTests);

end.
