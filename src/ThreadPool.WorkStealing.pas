unit ThreadPool.WorkStealing;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, Math, 
  ThreadPool.Types;

type
  { Work item implementations }
  TWorkItemBase = class(TInterfacedObject, IWorkItem)
  protected
    function GetItemType: Integer; virtual; abstract;
  public
    procedure Execute; virtual; abstract;
    property ItemType: Integer read GetItemType;
  end;

  TWorkItemProcedure = class(TWorkItemBase)
  private
    FProc: TThreadProcedure;
  protected
    function GetItemType: Integer; override;
  public
    constructor Create(AProc: TThreadProcedure);
    procedure Execute; override;
  end;

  TWorkItemMethod = class(TWorkItemBase)
  private
    FMethod: TThreadMethod;
  protected
    function GetItemType: Integer; override;
  public
    constructor Create(AMethod: TThreadMethod);
    procedure Execute; override;
  end;

  TWorkItemProcedureIndex = class(TWorkItemBase)
  private
    FProc: TThreadProcedureIndex;
    FIndex: Integer;
  protected
    function GetItemType: Integer; override;
  public
    constructor Create(AProc: TThreadProcedureIndex; AIndex: Integer);
    procedure Execute; override;
  end;

  TWorkItemMethodIndex = class(TWorkItemBase)
  private
    FMethod: TThreadMethodIndex;
    FIndex: Integer;
  protected
    function GetItemType: Integer; override;
  public
    constructor Create(AMethod: TThreadMethodIndex; AIndex: Integer);
    procedure Execute; override;
  end;

  { Lock-free work-stealing deque implementation }
  TWorkStealingDeque = class(TInterfacedObject, IWorkQueue)
  private const
    INITIAL_SIZE = 32;
  private
    FItems: array of IWorkItem;
    FMask: NativeUInt;
    FBottom: NativeUInt;  // Push/Pop end (thread-local)
    FTop: NativeUInt;     // Steal end (shared)
    FCasLock: TCriticalSection;
    
    procedure Grow;
    function Size: Integer; inline;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Local thread operations (lock-free)
    function TryPush(AWorkItem: IWorkItem): Boolean;
    function TryPop(out AWorkItem: IWorkItem): Boolean;
    
    // Stealing operations (synchronized)
    function TrySteal(out AWorkItem: IWorkItem): Boolean;
    
    // IWorkQueue implementation
    function IsEmpty: Boolean;
    procedure Clear;
  end;

  { Worker thread with work-stealing capability }
  TWorkStealingThread = class(TThread, IWorkerThread)
  private
    FLocalDeque: TWorkStealingDeque;
    FPool: TObject;  // Actually TWorkStealingPool, avoid circular reference
    FWorkEvent: TEvent;
    FTerminating: Boolean;
    FRefCount: Integer;
    
    function TryGetWork(out AWorkItem: IWorkItem): Boolean;
    function TryStealWork: Boolean;
  protected
    procedure Execute; override;
    
    // IInterface implementation
    function QueryInterface(constref IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(APool: TObject);
    destructor Destroy; override;
    
    // IWorkerThread implementation
    procedure Start;
    procedure SignalWork;
    procedure Terminate;
    procedure WaitFor;
    function GetThreadID: TThreadID;
    
    property LocalDeque: TWorkStealingDeque read FLocalDeque;
  end;

  { Work-stealing thread pool implementation }
  TWorkStealingPool = class(TThreadPoolBase)
  private
    FWorkers: array of TWorkStealingThread;
    FWorkCount: Integer;
    FWorkLock: TCriticalSection;
    FWorkEvent: TEvent;
    FErrorLock: TCriticalSection;
    
    procedure DistributeWork(AWorkItem: IWorkItem);
    function FindLeastLoadedWorker: TWorkStealingThread;
  protected
    procedure HandleError(const AError: string);
  public
    constructor Create(AThreadCount: Integer = 0); override;
    destructor Destroy; override;
    
    // IThreadPool implementation
    procedure Queue(AProcedure: TThreadProcedure); override;
    procedure Queue(AMethod: TThreadMethod); override;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); override;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); override;
    procedure WaitForAll; override;
    function GetThreadCount: Integer; override;
    function GetLastError: string; override;
    procedure ClearLastError; override;
  end;

function CompareAndSwap(var Target: NativeUInt; OldValue, NewValue: NativeUInt): Boolean;

implementation

{ TWorkStealingDeque }

constructor TWorkStealingDeque.Create;
begin
  inherited Create;
  SetLength(FItems, INITIAL_SIZE);
  FMask := INITIAL_SIZE - 1;
  FBottom := 0;
  FTop := 0;
  FCasLock := TCriticalSection.Create;
end;

destructor TWorkStealingDeque.Destroy;
begin
  Clear;
  FCasLock.Free;
  inherited;
end;

procedure TWorkStealingDeque.Grow;
var
  NewItems: array of IWorkItem;
  OldMask, Index: NativeUInt;
begin
  FCasLock.Enter;
  try
    OldMask := FMask;
    SetLength(NewItems, Length(FItems) * 2);
    for Index := 0 to Size - 1 do
      NewItems[Index] := FItems[(FBottom + Index) and OldMask];
    FItems := NewItems;
    FMask := Length(FItems) - 1;
  finally
    FCasLock.Leave;
  end;
end;

function TWorkStealingDeque.Size: Integer;
begin
  Result := FBottom - FTop;
end;

function TWorkStealingDeque.TryPush(AWorkItem: IWorkItem): Boolean;
var
  B, T: NativeUInt;
begin
  B := FBottom;
  T := FTop;
  if (B - T) > FMask then
    Grow;
    
  FItems[B and FMask] := AWorkItem;
  FBottom := B + 1;
  Result := True;
end;

function TWorkStealingDeque.TryPop(out AWorkItem: IWorkItem): Boolean;
var
  B, T: NativeUInt;
begin
  B := FBottom - 1;
  FBottom := B;
  T := FTop;
  
  if T <= B then
  begin
    AWorkItem := FItems[B and FMask];
    if T = B then
    begin
      FCasLock.Enter;
      try
        if not CompareAndSwap(FTop, T, T + 1) then
          AWorkItem := nil;
        FBottom := T + 1;
      finally
        FCasLock.Leave;
      end;
    end;
    Result := AWorkItem <> nil;
  end
  else
  begin
    FBottom := T;
    Result := False;
  end;
end;

function TWorkStealingDeque.TrySteal(out AWorkItem: IWorkItem): Boolean;
var
  T, B: NativeUInt;
begin
  FCasLock.Enter;
  try
    T := FTop;
    B := FBottom;
    if T < B then
    begin
      AWorkItem := FItems[T and FMask];
      Result := CompareAndSwap(FTop, T, T + 1);
      if not Result then
        AWorkItem := nil;
    end
    else
      Result := False;
  finally
    FCasLock.Leave;
  end;
end;

function TWorkStealingDeque.IsEmpty: Boolean;
begin
  Result := Size <= 0;
end;

procedure TWorkStealingDeque.Clear;
begin
  FCasLock.Enter;
  try
    while TryPop(IWorkItem(nil)) do ;
    FBottom := 0;
    FTop := 0;
  finally
    FCasLock.Leave;
  end;
end;

{ TWorkStealingThread }

constructor TWorkStealingThread.Create(APool: TObject);
begin
  inherited Create(True);  // Create suspended
  FPool := APool;
  FLocalDeque := TWorkStealingDeque.Create;
  FWorkEvent := TEvent.Create(nil, False, False, '');
  FTerminating := False;
  FreeOnTerminate := False;
end;

destructor TWorkStealingThread.Destroy;
begin
  FWorkEvent.Free;
  FLocalDeque.Free;
  inherited;
end;

procedure TWorkStealingThread.Execute;
var
  WorkItem: IWorkItem;
begin
  while not FTerminating do
  begin
    if TryGetWork(WorkItem) then
    begin
      try
        WorkItem.Execute;
      except
        on E: Exception do
          TWorkStealingPool(FPool).HandleError(Format('Thread %d: %s', [ThreadID, E.Message]));
      end;
    end
    else
    begin
      if not TryStealWork then
        FWorkEvent.WaitFor(1);  // Short wait if no work found
    end;
  end;
end;

function TWorkStealingThread.TryGetWork(out AWorkItem: IWorkItem): Boolean;
begin
  Result := FLocalDeque.TryPop(AWorkItem);
end;

function TWorkStealingThread.TryStealWork: Boolean;
var
  WorkItem: IWorkItem;
  OtherThread: TWorkStealingThread;
  Workers: TList;
  I: Integer;
begin
  Result := False;
  with TWorkStealingPool(FPool) do
  begin
    for I := 0 to Length(FWorkers) - 1 do
    begin
      OtherThread := FWorkers[I];
      if (OtherThread <> Self) and 
         OtherThread.LocalDeque.TrySteal(WorkItem) then
      begin
        FLocalDeque.TryPush(WorkItem);
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TWorkStealingThread.Start;
begin
  inherited Start;  // Changed from Resume to Start
end;

procedure TWorkStealingThread.SignalWork;
begin
  FWorkEvent.SetEvent;
end;

procedure TWorkStealingThread.Terminate;
begin
  FTerminating := True;
  SignalWork;  // Wake up thread to check terminating flag
end;

function TWorkStealingThread._AddRef: Integer; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TWorkStealingThread._Release: Integer; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TWorkStealingThread.QueryInterface(constref IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TWorkStealingThread.WaitFor;
begin
  inherited WaitFor;
end;

function TWorkStealingThread.GetThreadID: TThreadID;
begin
  Result := ThreadID;
end;

{ TWorkStealingPool }

constructor TWorkStealingPool.Create(AThreadCount: Integer);
var
  I: Integer;
begin
  inherited Create(AThreadCount);
  
  // Initialize synchronization objects
  FWorkLock := TCriticalSection.Create;
  FWorkEvent := TEvent.Create(nil, True, False, '');
  FErrorLock := TCriticalSection.Create;
  FLastError := '';
  
  // Create worker threads
  SetLength(FWorkers, GetThreadCount);
  for I := 0 to High(FWorkers) do
    FWorkers[I] := TWorkStealingThread.Create(Self);
    
  // Start all workers
  for I := 0 to High(FWorkers) do
    FWorkers[I].Start;
end;

destructor TWorkStealingPool.Destroy;
var
  I: Integer;
begin
  // Terminate all workers
  for I := 0 to High(FWorkers) do
    FWorkers[I].Terminate;
    
  // Wait for all workers to finish
  for I := 0 to High(FWorkers) do
  begin
    FWorkers[I].WaitFor;
    FWorkers[I].Free;
  end;
  
  // Cleanup
  FWorkLock.Free;
  FWorkEvent.Free;
  FErrorLock.Free;
  inherited;
end;

procedure TWorkStealingPool.DistributeWork(AWorkItem: IWorkItem);
var
  Worker: TWorkStealingThread;
begin
  Worker := FindLeastLoadedWorker;
  Worker.LocalDeque.TryPush(AWorkItem);
  Worker.SignalWork;
  
  FWorkLock.Enter;
  try
    Inc(FWorkCount);
  finally
    FWorkLock.Leave;
  end;
end;

function TWorkStealingPool.FindLeastLoadedWorker: TWorkStealingThread;
var
  I, MinSize, CurrentSize: Integer;
  MinIndex: Integer;
begin
  MinIndex := 0;
  MinSize := High(Integer);
  
  for I := 0 to High(FWorkers) do
  begin
    CurrentSize := FWorkers[I].LocalDeque.Size;
    if CurrentSize < MinSize then
    begin
      MinSize := CurrentSize;
      MinIndex := I;
    end;
  end;
  
  Result := FWorkers[MinIndex];
end;

procedure TWorkStealingPool.HandleError(const AError: string);
begin
  FErrorLock.Enter;
  try
    FLastError := AError;
  finally
    FErrorLock.Leave;
  end;
end;

procedure TWorkStealingPool.Queue(AProcedure: TThreadProcedure);
begin
  DistributeWork(TWorkItemProcedure.Create(AProcedure));
end;

procedure TWorkStealingPool.Queue(AMethod: TThreadMethod);
begin
  DistributeWork(TWorkItemMethod.Create(AMethod));
end;

procedure TWorkStealingPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
begin
  DistributeWork(TWorkItemProcedureIndex.Create(AProcedure, AIndex));
end;

procedure TWorkStealingPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
begin
  DistributeWork(TWorkItemMethodIndex.Create(AMethod, AIndex));
end;

procedure TWorkStealingPool.WaitForAll;
var
  AllDone: Boolean;
  I: Integer;
begin
  repeat
    FWorkLock.Enter;
    try
      AllDone := FWorkCount = 0;
      for I := 0 to High(FWorkers) do
        AllDone := AllDone and FWorkers[I].LocalDeque.IsEmpty;
    finally
      FWorkLock.Leave;
    end;
    
    if not AllDone then
      Sleep(1);
  until AllDone;
end;

function TWorkStealingPool.GetThreadCount: Integer;
begin
  Result := Length(FWorkers);
end;

function TWorkStealingPool.GetLastError: string;
begin
  FErrorLock.Enter;
  try
    Result := FLastError;
  finally
    FErrorLock.Leave;
  end;
end;

procedure TWorkStealingPool.ClearLastError;
begin
  FErrorLock.Enter;
  try
    FLastError := '';
  finally
    FErrorLock.Leave;
  end;
end;

function CompareAndSwap(var Target: NativeUInt; OldValue, NewValue: NativeUInt): Boolean;
begin
  Result := InterlockedCompareExchange(Target, NewValue, OldValue) = OldValue;
end;

end.
