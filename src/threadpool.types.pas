unit ThreadPool.Types;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, Math;

const
  { Upper bound on how many task error messages a pool retains. When exceeded,
    the oldest messages are dropped so a high volume of failing tasks cannot
    exhaust memory. LastError always reflects the most recent error regardless
    of this cap. }
  MAX_STORED_ERRORS = 1000;

type
  { Task Types - Different kinds of work that can be queued }
  TThreadProcedure = procedure;
  TThreadMethod = procedure of object;
  TThreadProcedureIndex = procedure(index: Integer);
  TThreadMethodIndex = procedure(index: Integer) of object;

  { Fired (from a worker thread) each time a queued task raises an exception.
    AMessage is the exception's Message. Handlers must be thread-safe and
    should not block, since they run on the worker thread that caught the error. }
  TThreadPoolErrorEvent = procedure(const AMessage: string) of object;

  { Snapshot of captured error messages, oldest first. }
  TStringArray = array of string;
  
  { Work item types }
  TWorkItemType = (
    witProcedure,
    witMethod,
    witProcedureIndex,
    witMethodIndex
  );

  { Base interface for work items }
  IWorkItem = interface
    ['{F8A7B3E4-D2C1-4E5A-9F8B-1A2B3C4D5E6F}']
    procedure Execute;
    function GetItemType: Integer;
    property ItemType: Integer read GetItemType;
  end;

  { Base interface for work queues }
  IWorkQueue = interface
    ['{A1B2C3D4-E5F6-4A5B-9C8D-7E6F5D4C3B2A}']
    function TryPush(AWorkItem: IWorkItem): Boolean;
    function TryPop(out AWorkItem: IWorkItem): Boolean;
    function IsEmpty: Boolean;
    procedure Clear;
  end;

  { Base interface for worker threads }
  IWorkerThread = interface
    ['{C1D2E3F4-A5B6-4C5D-8E9F-7A6B5C4D3E2F}']
    procedure Start;
    procedure Terminate;
    procedure WaitFor;
    function GetThreadID: TThreadID;
    property ThreadID: TThreadID read GetThreadID;
  end;

  { Base interface for thread pools }
  IThreadPool = interface
    ['{B1A2C3D4-E5F6-4A5B-8C9D-7E6F5D4C3B2A}']
    procedure Queue(AProcedure: TThreadProcedure); overload;
    procedure Queue(AMethod: TThreadMethod); overload;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); overload;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); overload;
    procedure WaitForAll;
    procedure ClearLastError;
    procedure ClearErrors;
    function GetLastError: string;
    function GetThreadCount: Integer;
    function GetErrors: TStringArray;
    function GetErrorCount: Integer;
    function GetOnError: TThreadPoolErrorEvent;
    procedure SetOnError(AValue: TThreadPoolErrorEvent);
    property LastError: string read GetLastError;
    property ThreadCount: Integer read GetThreadCount;
    { All task error messages captured since the last ClearErrors/ClearLastError,
      oldest first, capped at MAX_STORED_ERRORS. }
    property Errors: TStringArray read GetErrors;
    property ErrorCount: Integer read GetErrorCount;
    { Optional callback fired from a worker thread whenever a task raises. }
    property OnError: TThreadPoolErrorEvent read GetOnError write SetOnError;
  end;

  { Base class for thread pool implementations }
  TThreadPoolBase = class(TInterfacedObject, IThreadPool)
  protected
    FLastError: string;
    FThreadCount: Integer;
    FShutdown: Boolean;
    { Collected error messages and the lock guarding them plus FLastError and
      FOnError. This lock is internal to the base class and independent of any
      lock a subclass holds around its own SetLastError call. }
    FErrors: TStringList;
    FErrorsLock: TCriticalSection;
    FOnError: TThreadPoolErrorEvent;

    procedure SetLastError(const AError: string); virtual;
  public
    constructor Create(AThreadCount: Integer); virtual;
    destructor Destroy; override;

    { IThreadPool implementation }
    procedure Queue(AProcedure: TThreadProcedure); virtual; abstract;
    procedure Queue(AMethod: TThreadMethod); virtual; abstract;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); virtual; abstract;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); virtual; abstract;
    procedure WaitForAll; virtual; abstract;
    procedure ClearLastError; virtual;
    procedure ClearErrors; virtual;
    function GetLastError: string; virtual;
    function GetThreadCount: Integer; virtual;
    function GetErrors: TStringArray; virtual;
    function GetErrorCount: Integer; virtual;
    function GetOnError: TThreadPoolErrorEvent; virtual;
    procedure SetOnError(AValue: TThreadPoolErrorEvent); virtual;

    { All task error messages captured since the last ClearErrors/ClearLastError,
      oldest first, capped at MAX_STORED_ERRORS. }
    property Errors: TStringArray read GetErrors;
    property ErrorCount: Integer read GetErrorCount;
    { Optional callback fired from a worker thread whenever a task raises. }
    property OnError: TThreadPoolErrorEvent read GetOnError write SetOnError;
  end;

implementation

{ TThreadPoolBase }

constructor TThreadPoolBase.Create(AThreadCount: Integer);
begin
  inherited Create;
  FShutdown := False;
  FLastError := '';
  FErrors := TStringList.Create;
  FErrorsLock := TCriticalSection.Create;
  FOnError := nil;

  // Apply thread count safety limits
  if AThreadCount <= 0 then
    AThreadCount := TThread.ProcessorCount
  else
    AThreadCount := Min(AThreadCount, TThread.ProcessorCount * 2);
  FThreadCount := Max(AThreadCount, 4);
end;

destructor TThreadPoolBase.Destroy;
begin
  FShutdown := True;
  FErrors.Free;
  FErrorsLock.Free;
  inherited;
end;

procedure TThreadPoolBase.SetLastError(const AError: string);
var
  Handler: TThreadPoolErrorEvent;
begin
  FErrorsLock.Enter;
  try
    FLastError := AError;
    FErrors.Add(AError);
    // Bound memory: drop oldest entries beyond the cap.
    while FErrors.Count > MAX_STORED_ERRORS do
      FErrors.Delete(0);
    Handler := FOnError;
  finally
    FErrorsLock.Leave;
  end;

  // Fire the callback outside the lock so a handler cannot deadlock by calling
  // back into the pool, and so a slow handler does not block other workers.
  if Assigned(Handler) then
    Handler(AError);
end;

procedure TThreadPoolBase.ClearLastError;
begin
  FErrorsLock.Enter;
  try
    FLastError := '';
    FErrors.Clear;
  finally
    FErrorsLock.Leave;
  end;
end;

procedure TThreadPoolBase.ClearErrors;
begin
  ClearLastError;
end;

function TThreadPoolBase.GetLastError: string;
begin
  FErrorsLock.Enter;
  try
    Result := FLastError;
  finally
    FErrorsLock.Leave;
  end;
end;

function TThreadPoolBase.GetThreadCount: Integer;
begin
  Result := FThreadCount;
end;

function TThreadPoolBase.GetErrors: TStringArray;
var
  I: Integer;
begin
  FErrorsLock.Enter;
  try
    SetLength(Result, FErrors.Count);
    for I := 0 to FErrors.Count - 1 do
      Result[I] := FErrors[I];
  finally
    FErrorsLock.Leave;
  end;
end;

function TThreadPoolBase.GetErrorCount: Integer;
begin
  FErrorsLock.Enter;
  try
    Result := FErrors.Count;
  finally
    FErrorsLock.Leave;
  end;
end;

function TThreadPoolBase.GetOnError: TThreadPoolErrorEvent;
begin
  FErrorsLock.Enter;
  try
    Result := FOnError;
  finally
    FErrorsLock.Leave;
  end;
end;

procedure TThreadPoolBase.SetOnError(AValue: TThreadPoolErrorEvent);
begin
  FErrorsLock.Enter;
  try
    FOnError := AValue;
  finally
    FErrorsLock.Leave;
  end;
end;

end.
