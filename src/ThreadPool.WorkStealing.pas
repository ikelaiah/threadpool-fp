unit ThreadPool.WorkStealing;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, Math,
  ThreadPool.Types, ThreadPool.Logging;

type
  { Custom exceptions }
  EThreadPool = class(Exception);

  { Atomic type definition based on architecture }
  {$IF Defined(CPU64)}
  TAtomicType = Int64;
  {$ELSE}
  TAtomicType = LongWord;
  {$ENDIF}

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
    FBottom: TAtomicType;  // Push/Pop end (thread-local)
    FTop: TAtomicType;     // Steal end (shared)
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
  TWorkStealingPool = class(TThreadPoolBase, IThreadPool)
  private
    FWorkers: array of TWorkStealingThread;
    FWorkCount: Integer;
    FWorkLock: TCriticalSection;
    FWorkEvent: TEvent;
    FErrorLock: TCriticalSection;
    
    function FindLeastLoadedWorker: TWorkStealingThread;
    procedure DistributeWork(AWorkItem: IWorkItem);
    procedure HandleError(const AError: string);
    procedure DecrementWorkCount;
  public
    constructor Create(AThreadCount: Integer = 0); override;
    destructor Destroy; override;
    
    { IThreadPool implementation }
    procedure Queue(AProcedure: TThreadProcedure); override;
    procedure Queue(AMethod: TThreadMethod); override;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); override;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); override;
    procedure WaitForAll; override;
    procedure ClearLastError; override;
    function GetLastError: string; override;
    function GetThreadCount: Integer; override;
    
    property LastError: string read GetLastError;
    property ThreadCount: Integer read GetThreadCount;
  end;

function CompareAndSwap(var Target: TAtomicType; OldValue, NewValue: TAtomicType): Boolean;

implementation

{ CompareAndSwap Implementation }

function CompareAndSwap(var Target: TAtomicType; OldValue, NewValue: TAtomicType): Boolean;
begin
  {$IF Defined(CPU64)}
    // Use InterlockedCompareExchange64 for 64-bit systems
    Result := InterlockedCompareExchange64(Target, Int64(NewValue), Int64(OldValue)) = Int64(OldValue);
  {$ELSE}
    // Use InterlockedCompareExchange for 32-bit systems
    Result := InterlockedCompareExchange(Target, NewValue, OldValue) = OldValue;
  {$ENDIF}
end;
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
  if Assigned(FCasLock) then
  begin
    FCasLock.Enter;
    try
      Clear;  // Make sure all items are cleared
      SetLength(FItems, 0);  // Explicitly clear array
    finally
      FCasLock.Leave;
      FreeAndNil(FCasLock);  // Use FreeAndNil instead of Free
    end;
  end;
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
  FCasLock.Enter;
  try
    Result := FBottom - FTop;
  finally
    FCasLock.Leave;
  end;
end;

function TWorkStealingDeque.TryPush(AWorkItem: IWorkItem): Boolean;
begin
  Result := False;
  if not Assigned(AWorkItem) then
    Exit;
    
  FCasLock.Enter;  // We need full synchronization to prevent memory issues
  try
    if (FBottom - FTop) >= Length(FItems) then
      Grow;
      
    FItems[FBottom and FMask] := AWorkItem;
    FBottom := FBottom + 1;
    Result := True;
  finally
    FCasLock.Leave;
  end;
end;

function TWorkStealingDeque.TryPop(out AWorkItem: IWorkItem): Boolean;
begin
  Result := False;
  AWorkItem := nil;
  
  FCasLock.Enter;
  try
    if FBottom <= FTop then
      Exit;
      
    FBottom := FBottom - 1;
    AWorkItem := FItems[FBottom and FMask];
    FItems[FBottom and FMask] := nil;  // Clear reference
    
    if FTop = FBottom then
    begin
      if not CompareAndSwap(FTop, FTop, FTop + 1) then
        AWorkItem := nil;
      FBottom := FTop;
    end;
    
    Result := AWorkItem <> nil;
  finally
    FCasLock.Leave;
  end;
end;

function TWorkStealingDeque.TrySteal(out AWorkItem: IWorkItem): Boolean;
begin
  Result := False;
  AWorkItem := nil;
  
  FCasLock.Enter;
  try
    if FTop >= FBottom then
      Exit;
      
    AWorkItem := FItems[FTop and FMask];
    if not Assigned(AWorkItem) then
      Exit;
      
    FItems[FTop and FMask] := nil;  // Clear reference
    FTop := FTop + 1;
    Result := True;
  finally
    FCasLock.Leave;
  end;
end;

function TWorkStealingDeque.IsEmpty: Boolean;
begin
  Result := FBottom <= FTop;
end;

procedure TWorkStealingDeque.Clear;
var
  I: Integer;
begin
  FCasLock.Enter;
  try
    for I := 0 to Length(FItems) - 1 do
      FItems[I] := nil;
    FBottom := 0;
    FTop := 0;
  finally
    FCasLock.Leave;
  end;
end;

{ TWorkStealingThread }

constructor TWorkStealingThread.Create(APool: TObject);
begin
  inherited Create(True);
  FPool := APool;
  FLocalDeque := TWorkStealingDeque.Create;
  FWorkEvent := TEvent.Create(nil, False, False, '');
  FTerminating := False;
  FreeOnTerminate := False;
end;

destructor TWorkStealingThread.Destroy;
begin
  if Assigned(FLocalDeque) then
  begin
    FreeAndNil(FLocalDeque);
  end;
  
  if Assigned(FWorkEvent) then
  begin
    FreeAndNil(FWorkEvent);
  end;
  
  inherited;
end;

procedure TWorkStealingThread.Execute;
var
  WorkItem: IWorkItem;
begin
  ThreadLogger.Log(Format('Thread %d: Starting', [ThreadID]));
  while not Terminated do
  begin
    WorkItem := nil;
    if TryGetWork(WorkItem) then
    begin
      if Assigned(WorkItem) then
      begin
        try
          ThreadLogger.Log(Format('Thread %d: Executing work item', [ThreadID]));
          WorkItem.Execute;
          ThreadLogger.Log(Format('Thread %d: Work item executed', [ThreadID]));
          TWorkStealingPool(FPool).DecrementWorkCount;
          ThreadLogger.Log(Format('Thread %d: Work count decremented', [ThreadID]));
        except
          on E: Exception do
            TWorkStealingPool(FPool).HandleError(
              Format('Thread %d: %s', [ThreadID, E.Message]));
        end;
        WorkItem := nil;
      end;
    end
    else if not Terminated then
    begin
      if not TryStealWork then
      begin
        ThreadLogger.Log(Format('Thread %d: No work found, waiting', [ThreadID]));
        FWorkEvent.WaitFor(100);
      end;
    end;
  end;
  ThreadLogger.Log(Format('Thread %d: Terminating', [ThreadID]));
  WorkItem := nil;
end;

function TWorkStealingThread.TryGetWork(out AWorkItem: IWorkItem): Boolean;
begin
  ThreadLogger.Log(Format('Thread %d: Attempting to get work', [ThreadID]));
  Result := FLocalDeque.TryPop(AWorkItem);
  if Result then
    ThreadLogger.Log(Format('Thread %d: Got work from local queue', [ThreadID]))
  else
    ThreadLogger.Log(Format('Thread %d: Local queue empty', [ThreadID]));
end;

function TWorkStealingThread.TryStealWork: Boolean;
const
  MAX_STEAL_ATTEMPTS = 10;
var
  WorkItem: IWorkItem;
  OtherThread: TWorkStealingThread;
  Pool: TWorkStealingPool;
  I, Attempt, StartIndex: Integer;
begin
  Result := False;
  Pool := TWorkStealingPool(FPool);
  
  if Terminated then
    Exit;
    
  for Attempt := 0 to MAX_STEAL_ATTEMPTS - 1 do
  begin
    StartIndex := Random(Length(Pool.FWorkers));
    
    for I := 0 to Length(Pool.FWorkers) - 1 do
    begin
      if Terminated then
        Exit;
        
      OtherThread := Pool.FWorkers[(StartIndex + I) mod Length(Pool.FWorkers)];
      if (OtherThread = nil) or (OtherThread = Self) or 
         (OtherThread.LocalDeque = nil) then
        Continue;
        
      WorkItem := nil;  // Reset for each attempt
      try
        if OtherThread.LocalDeque.TrySteal(WorkItem) then
        begin
          if Assigned(WorkItem) then
          begin
            if FLocalDeque.TryPush(WorkItem) then
              Exit(True);
          end;
        end;
      except
        WorkItem := nil;
      end;
    end;
    
    if Attempt < MAX_STEAL_ATTEMPTS - 1 then
      Sleep(0);  // Yield to other threads but don't actually sleep
  end;
end;

procedure TWorkStealingThread.Start;
begin
  inherited Start;
end;

procedure TWorkStealingThread.SignalWork;
begin
  FWorkEvent.SetEvent;
end;

procedure TWorkStealingThread.Terminate;
begin
  if FTerminating then
    Exit;
    
  inherited Terminate;
  FTerminating := True;
  if Assigned(FWorkEvent) then
  begin
    SignalWork;
    FWorkEvent.SetEvent;
  end;
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

constructor TWorkStealingPool.Create(AThreadCount: Integer = 0);
var
  I: Integer;
  DesiredThreadCount: Integer;
begin
  if AThreadCount <= 0 then
    DesiredThreadCount := TThread.ProcessorCount
  else
    DesiredThreadCount := Min(AThreadCount, TThread.ProcessorCount * 2);
  DesiredThreadCount := Max(DesiredThreadCount, 4);
  
  inherited Create(DesiredThreadCount);
  
  FWorkLock := TCriticalSection.Create;
  FErrorLock := TCriticalSection.Create;
  FWorkEvent := TEvent.Create(nil, True, False, '');
  FLastError := '';
  
  try
    SetLength(FWorkers, DesiredThreadCount);
    FWorkCount := 0;
    
    for I := 0 to High(FWorkers) do
    begin
      FWorkers[I] := TWorkStealingThread.Create(Self);
      try
        FWorkers[I].Start;
      except
        FWorkers[I].Free;
        raise;
      end;
    end;
  except
    for I := 0 to High(FWorkers) do
      if Assigned(FWorkers[I]) then
        FWorkers[I].Free;
    FWorkEvent.Free;
    FWorkLock.Free;
    FErrorLock.Free;
    raise;
  end;
end;

destructor TWorkStealingPool.Destroy;
var
  I: Integer;
begin
  ThreadLogger.Log('ThreadPool.Destroy: Starting cleanup');
  try
    if Assigned(FWorkLock) then
    begin
      ThreadLogger.Log('ThreadPool.Destroy: Signaling thread termination');
      FWorkLock.Enter;
      try
        for I := Low(FWorkers) to High(FWorkers) do
          if Assigned(FWorkers[I]) then
          begin
            ThreadLogger.Log(Format('ThreadPool.Destroy: Terminating worker %d', [I]));
            FWorkers[I].Terminate;
            FWorkers[I].SignalWork;
          end;
      finally
        FWorkLock.Leave;
      end;
    end;

    if Length(FWorkers) > 0 then
    begin
      ThreadLogger.Log(Format('ThreadPool.Destroy: Cleaning up %d workers', [Length(FWorkers)]));
      for I := 0 to High(FWorkers) do
        if Assigned(FWorkers[I]) then
        begin
          try
            ThreadLogger.Log(Format('ThreadPool.Destroy: Waiting for worker %d', [I]));
            FWorkers[I].WaitFor;
            ThreadLogger.Log(Format('ThreadPool.Destroy: Freeing worker %d', [I]));
            FWorkers[I].Free;
            FWorkers[I] := nil;
          except
            ThreadLogger.Log(Format('ThreadPool.Destroy: Error cleaning up worker %d', [I]));
          end;
        end;
    end;
    SetLength(FWorkers, 0);

    ThreadLogger.Log('ThreadPool.Destroy: Cleaning up synchronization objects');
    FWorkEvent.Free;
    FWorkLock.Free;
    FErrorLock.Free;
    ThreadLogger.Log('ThreadPool.Destroy: Cleanup completed');
  except
    ThreadLogger.Log('ThreadPool.Destroy: Error during cleanup');
  end;

  inherited;
end;

procedure TWorkStealingPool.DistributeWork(AWorkItem: IWorkItem);
var
  Worker: TWorkStealingThread;
begin
  if not Assigned(AWorkItem) then
    Exit;
    
  FWorkLock.Enter;
  try
    Worker := FindLeastLoadedWorker;
    if Assigned(Worker) and Assigned(Worker.LocalDeque) then
    begin
      Worker.LocalDeque.TryPush(AWorkItem);
      Worker.SignalWork;
    end;
  finally
    FWorkLock.Leave;
  end;
end;

function TWorkStealingPool.FindLeastLoadedWorker: TWorkStealingThread;
var
  I, MinSize, CurrentSize: Integer;
  MinIndex: Integer;
begin
  if Length(FWorkers) = 0 then
    raise EThreadPool.Create('No worker threads available');
    
  MinIndex := 0;
  MinSize := High(Integer);
  
  for I := 0 to High(FWorkers) do
  begin
    if Assigned(FWorkers[I]) then
    begin
      CurrentSize := FWorkers[I].LocalDeque.Size;
      if CurrentSize < MinSize then
      begin
        MinSize := CurrentSize;
        MinIndex := I;
      end;
    end;
  end;
  
  Result := FWorkers[MinIndex];
  if not Assigned(Result) then
    raise EThreadPool.Create('No valid worker thread found');
end;

procedure TWorkStealingPool.HandleError(const AError: string);
begin
  FErrorLock.Enter;
  try
    inherited SetLastError(AError);
  finally
    FErrorLock.Leave;
  end;
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

procedure TWorkStealingPool.Queue(AProcedure: TThreadProcedure);
var
  WorkItem: IWorkItem;
begin
  if not Assigned(AProcedure) then
    Exit;
    
  try
    WorkItem := TWorkItemProcedure.Create(AProcedure);
    InterlockedIncrement(FWorkCount);
    ThreadLogger.Log(Format('Queue: Incremented work count to %d', [FWorkCount]));
    
    FWorkLock.Enter;
    try
      DistributeWork(WorkItem);
    finally
      FWorkLock.Leave;
      WorkItem := nil;
    end;
  except
    on E: Exception do
    begin
      InterlockedDecrement(FWorkCount);
      ThreadLogger.Log(Format('Queue Exception: Decremented work count to %d', [FWorkCount]));
      HandleError(Format('Queue(Procedure) error: %s', [E.Message]));
    end;
  end;
end;

procedure TWorkStealingPool.Queue(AMethod: TThreadMethod);
var
  WorkItem: IWorkItem;
begin
  if not Assigned(AMethod) then
    Exit;
    
  try
    WorkItem := TWorkItemMethod.Create(AMethod);
    FWorkLock.Enter;
    try
      ThreadLogger.Log(Format('Adding work, count before: %d', [FWorkCount]));
      InterlockedIncrement(FWorkCount);
      ThreadLogger.Log(Format('Work added, count after: %d', [FWorkCount]));
      DistributeWork(WorkItem);
    finally
      FWorkLock.Leave;
      WorkItem := nil;
    end;
  except
    on E: Exception do
      HandleError(E.Message);
  end;
end;

procedure TWorkStealingPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
var
  WorkItem: IWorkItem;
begin
  if not Assigned(AProcedure) then
    Exit;
    
  try
    WorkItem := TWorkItemProcedureIndex.Create(AProcedure, AIndex);
    InterlockedIncrement(FWorkCount);
    ThreadLogger.Log(Format('[%d] Work added, count: %d', [GetCurrentThreadId, FWorkCount]));
    
    FWorkLock.Enter;
    try
      DistributeWork(WorkItem);
    finally
      FWorkLock.Leave;
      WorkItem := nil;
    end;
  except
    on E: Exception do
    begin
      InterlockedDecrement(FWorkCount);
      HandleError(Format('Queue(ProcedureIndex) error: %s', [E.Message]));
    end;
  end;
end;

procedure TWorkStealingPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
var
  WorkItem: IWorkItem;
begin
  if not Assigned(AMethod) then
    Exit;
    
  WorkItem := TWorkItemMethodIndex.Create(AMethod, AIndex);
  FWorkLock.Enter;
  try
    ThreadLogger.Log(Format('[%d] Adding work, count before: %d', [GetCurrentThreadId, FWorkCount]));
    InterlockedIncrement(FWorkCount);
    ThreadLogger.Log(Format('[%d] Work added, count after: %d', [GetCurrentThreadId, FWorkCount]));
    DistributeWork(WorkItem);
  finally
    FWorkLock.Leave;
  end;
end;

procedure TWorkStealingPool.WaitForAll;
var
  TimeoutCounter: Integer;
  AllThreadsIdle: Boolean;
  I: Integer;
begin
  ThreadLogger.Log('WaitForAll: Starting wait');
  TimeoutCounter := 0;
  
  while True do
  begin
    FWorkLock.Enter;
    try
      ThreadLogger.Log(Format('WaitForAll: Work count = %d', [FWorkCount]));
      
      if FWorkCount = 0 then
      begin
        ThreadLogger.Log('WaitForAll: All work completed');
        Break;
      end;
      
      AllThreadsIdle := True;
      for I := 0 to High(FWorkers) do
      begin
        if Assigned(FWorkers[I]) and (FWorkers[I].LocalDeque.Size > 0) then
        begin
          AllThreadsIdle := False;
          Break;
        end;
      end;
      
      if AllThreadsIdle then
      begin
        Inc(TimeoutCounter);
        ThreadLogger.Log(Format('WaitForAll: All threads idle but work count = %d (timeout: %d)', 
          [FWorkCount, TimeoutCounter]));
        
        if TimeoutCounter > 10 then
        begin
          ThreadLogger.Log('WaitForAll: Possible deadlock detected, resetting work count');
          FWorkCount := 0;
          Break;
        end;
      end else
        TimeoutCounter := 0;
        
    finally
      FWorkLock.Leave;
    end;
    
    ThreadLogger.Log('WaitForAll: Waiting for event');
    FWorkEvent.WaitFor(100);
    FWorkEvent.ResetEvent;
  end;
end;

function TWorkStealingPool.GetThreadCount: Integer;
begin
  Result := Length(FWorkers);
end;

{ TWorkItemProcedure }

constructor TWorkItemProcedure.Create(AProc: TThreadProcedure);
begin
  inherited Create;
  FProc := AProc;
end;

function TWorkItemProcedure.GetItemType: Integer;
begin
  Result := Ord(witProcedure);
end;

procedure TWorkItemProcedure.Execute;
begin
  FProc;
end;

{ TWorkItemMethod }

constructor TWorkItemMethod.Create(AMethod: TThreadMethod);
begin
  inherited Create;
  FMethod := AMethod;
end;

function TWorkItemMethod.GetItemType: Integer;
begin
  Result := Ord(witMethod);
end;

procedure TWorkItemMethod.Execute;
begin
  FMethod;
end;

{ TWorkItemProcedureIndex }

constructor TWorkItemProcedureIndex.Create(AProc: TThreadProcedureIndex; AIndex: Integer);
begin
  inherited Create;
  FProc := AProc;
  FIndex := AIndex;
end;

function TWorkItemProcedureIndex.GetItemType: Integer;
begin
  Result := Ord(witProcedureIndex);
end;

procedure TWorkItemProcedureIndex.Execute;
begin
  FProc(FIndex);
end;

{ TWorkItemMethodIndex }

constructor TWorkItemMethodIndex.Create(AMethod: TThreadMethodIndex; AIndex: Integer);
begin
  inherited Create;
  FMethod := AMethod;
  FIndex := AIndex;
end;

function TWorkItemMethodIndex.GetItemType: Integer;
begin
  Result := Ord(witMethodIndex);
end;

procedure TWorkItemMethodIndex.Execute;
begin
  FMethod(FIndex);
end;

procedure TWorkStealingPool.DecrementWorkCount;
var
  NewCount: Integer;
begin
  NewCount := InterlockedDecrement(FWorkCount);
  ThreadLogger.Log(Format('[%d] Work completed, count: %d', [GetCurrentThreadId, NewCount]));
  if NewCount = 0 then
    FWorkEvent.SetEvent;
end;

end.
