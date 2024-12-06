unit ThreadPool.WorkStealing;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, Math, 
  ThreadPool.Types;

type
  { Custom exceptions }
  EThreadPool = class(Exception);

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
var
  DummyItem: IWorkItem;
begin
  FCasLock.Enter;
  try
    DummyItem := nil;  // Initialize the interface variable
    while TryPop(DummyItem) do ;
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
  while not Terminated do
  begin
    if TryGetWork(WorkItem) then
    begin
      try
        WorkItem.Execute;
      except
        on E: Exception do
          TWorkStealingPool(FPool).HandleError(Format('Thread %d: %s', [ThreadID, E.Message]));
      end;
      
      TWorkStealingPool(FPool).DecrementWorkCount;
    end
    else
    begin
      if not TryStealWork then
        Sleep(1);  // Short sleep if no work found
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

constructor TWorkStealingPool.Create(AThreadCount: Integer = 0);
var
  I: Integer;
  DesiredThreadCount: Integer;
begin
  // Calculate desired thread count first
  if AThreadCount <= 0 then
    DesiredThreadCount := TThread.ProcessorCount
  else
    DesiredThreadCount := Min(AThreadCount, TThread.ProcessorCount * 2);
  DesiredThreadCount := Max(DesiredThreadCount, 4);  // Minimum of 4 threads
  
  // Call inherited constructor first with the calculated thread count
  inherited Create(DesiredThreadCount);
  
  // Initialize synchronization objects
  FWorkLock := TCriticalSection.Create;
  FErrorLock := TCriticalSection.Create;
  FWorkEvent := TEvent.Create(nil, True, False, '');
  FLastError := '';
  
  try
    SetLength(FWorkers, DesiredThreadCount);
    FWorkCount := 0;
    
    // Create workers
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
  WorkerCount: Integer;
begin
  try
    // Signal termination first
    if Assigned(FWorkLock) then
    begin
      FWorkLock.Enter;
      try
        for I := Low(FWorkers) to High(FWorkers) do
          if Assigned(FWorkers[I]) then
            FWorkers[I].Terminate;
      finally
        FWorkLock.Leave;
      end;
    end;

    // Wait for and free threads
    WorkerCount := Length(FWorkers);
    if WorkerCount > 0 then
      for I := 0 to WorkerCount - 1 do
        if Assigned(FWorkers[I]) then
        begin
          try
            FWorkers[I].WaitFor;
            FWorkers[I].Free;
          except
            // Ignore cleanup errors
          end;
        end;

    // Clean up synchronization objects
    FWorkEvent.Free;
    FWorkLock.Free;
    FErrorLock.Free;
  except
    // Log or handle cleanup errors
    WriteLn('Error during thread pool cleanup');
  end;

  inherited;
end;

procedure TWorkStealingPool.DistributeWork(AWorkItem: IWorkItem);
var
  Worker: TWorkStealingThread;
  Distributed: Boolean;
begin
  if not Assigned(AWorkItem) then
    Exit;
    
  FWorkLock.Enter;
  try
    Worker := FindLeastLoadedWorker;
    if Assigned(Worker) and Assigned(Worker.LocalDeque) then
    begin
      Distributed := Worker.LocalDeque.TryPush(AWorkItem);
      if Distributed then
      begin
        WriteLn('DistributeWork: Work item pushed to worker queue');
        if Assigned(FWorkEvent) then
        begin
          WriteLn('DistributeWork: Signaling work event');
          FWorkEvent.SetEvent;
        end;
      end
      else
        WriteLn('DistributeWork: Failed to push work item');
    end
    else
      WriteLn('DistributeWork: No valid worker found');
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
    
  WorkItem := TWorkItemProcedure.Create(AProcedure);
  FWorkLock.Enter;
  try
    Inc(FWorkCount);
    WriteLn(Format('Queue: Work count increased to %d', [FWorkCount]));
    DistributeWork(WorkItem);
  finally
    FWorkLock.Leave;
  end;
end;

procedure TWorkStealingPool.Queue(AMethod: TThreadMethod);
var
  WorkItem: IWorkItem;
begin
  if not Assigned(AMethod) then
    Exit;
    
  WorkItem := TWorkItemMethod.Create(AMethod);
  FWorkLock.Enter;
  try
    Inc(FWorkCount);
    DistributeWork(WorkItem);
  finally
    FWorkLock.Leave;
  end;
end;

procedure TWorkStealingPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
var
  WorkItem: IWorkItem;
begin
  if not Assigned(AProcedure) then
    Exit;
    
  WorkItem := TWorkItemProcedureIndex.Create(AProcedure, AIndex);
  FWorkLock.Enter;
  try
    Inc(FWorkCount);
    DistributeWork(WorkItem);
  finally
    FWorkLock.Leave;
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
    Inc(FWorkCount);
    DistributeWork(WorkItem);
  finally
    FWorkLock.Leave;
  end;
end;

procedure TWorkStealingPool.WaitForAll;
begin
  WriteLn('WaitForAll: Starting wait');
  FWorkLock.Enter;
  try
    while FWorkCount > 0 do
    begin
      WriteLn(Format('WaitForAll: Still waiting, work count = %d', [FWorkCount]));
      FWorkLock.Leave;
      try
        WriteLn('WaitForAll: About to wait for event');
        FWorkEvent.WaitFor(100);
        WriteLn('WaitForAll: Event wait completed');
        FWorkEvent.ResetEvent;
      finally
        FWorkLock.Enter;
      end;
    end;
    WriteLn('WaitForAll: All work completed');
  finally
    FWorkLock.Leave;
  end;
end;

function TWorkStealingPool.GetThreadCount: Integer;
begin
  Result := Length(FWorkers);
end;

function CompareAndSwap(var Target: NativeUInt; OldValue, NewValue: NativeUInt): Boolean;
var
  TempTarget: LongInt;
  TempOld: LongInt;
  TempNew: LongInt;
begin
  {$IFDEF CPU64}
  TempTarget := LongInt(Target);
  TempOld := LongInt(OldValue);
  TempNew := LongInt(NewValue);
  Result := InterlockedCompareExchange(TempTarget, TempNew, TempOld) = TempOld;
  Target := NativeUInt(TempTarget);
  {$ELSE}
  Result := InterlockedCompareExchange(LongWord(Target), LongWord(NewValue), LongWord(OldValue)) = LongWord(OldValue);
  {$ENDIF}
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
begin
  FWorkLock.Enter;
  try
    Dec(FWorkCount);
    WriteLn(Format('DecrementWorkCount: Work count decreased to %d', [FWorkCount]));
    if FWorkCount = 0 then
    begin
      WriteLn('DecrementWorkCount: Signaling work completion');
      FWorkEvent.SetEvent;  // Signal completion
    end;
  finally
    FWorkLock.Leave;
  end;
end;

end.
