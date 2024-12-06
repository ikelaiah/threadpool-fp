unit ThreadPool.Types;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math;

type
  { Task Types - Different kinds of work that can be queued }
  TThreadProcedure = procedure;
  TThreadMethod = procedure of object;
  TThreadProcedureIndex = procedure(index: Integer);
  TThreadMethodIndex = procedure(index: Integer) of object;
  
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
    function GetLastError: string;
    function GetThreadCount: Integer;
    property LastError: string read GetLastError;
    property ThreadCount: Integer read GetThreadCount;
  end;

  { Base class for thread pool implementations }
  TThreadPoolBase = class(TInterfacedObject, IThreadPool)
  protected
    FLastError: string;
    FThreadCount: Integer;
    FShutdown: Boolean;
    
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
    function GetLastError: string; virtual;
    function GetThreadCount: Integer; virtual;
  end;

implementation

{ TThreadPoolBase }

constructor TThreadPoolBase.Create(AThreadCount: Integer);
begin
  inherited Create;
  FShutdown := False;
  FLastError := '';
  
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
  inherited;
end;

procedure TThreadPoolBase.SetLastError(const AError: string);
begin
  FLastError := AError;
end;

procedure TThreadPoolBase.ClearLastError;
begin
  FLastError := '';
end;

function TThreadPoolBase.GetLastError: string;
begin
  Result := FLastError;
end;

function TThreadPoolBase.GetThreadCount: Integer;
begin
  Result := FThreadCount;
end;

end. 
