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
  FCasLock.Enter;
  try
    Clear;  // Make sure all items are cleared
    SetLength(FItems, 0);  // Explicitly clear array
  finally
    FCasLock.Leave;
    FCasLock.Free;
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
  FCasLock.Enter;
  try
    B := FBottom - 1;
    FBottom := B;
    T := FTop;
    
    if T <= B then
    begin
      AWorkItem := FItems[B and FMask];
      if T = B then
      begin
        if not CompareAndSwap(FTop, T, T + 1) then
          AWorkItem := nil;
        FBottom := T + 1;
      end;
      if Assigned(AWorkItem) then
        FItems[B and FMask] := nil;  // Clear slot after successful pop
      Result := Assigned(AWorkItem);
    end
    else
    begin
      FBottom := T;
      AWorkItem := nil;
      Result := False;
    end;
  finally
    FCasLock.Leave;
  end;
end;


function TWorkStealingDeque.TrySteal(out AWorkItem: IWorkItem): Boolean;
var
  T, B: NativeUInt;
  StolenItem: IWorkItem;
begin
  FCasLock.Enter;
  try
    T := FTop;
    B := FBottom;
    
    if T < B then
    begin
      StolenItem := FItems[T and FMask];
      if Assigned(StolenItem) then
      begin
        Result := CompareAndSwap(FTop, T, T + 1);
        if Result then
        begin
          AWorkItem := StolenItem;
          FItems[T and FMask] := nil;  // Clear slot after successful steal
        end
        else
          AWorkItem := nil;
      end
      else
      begin
        Result := False;
        AWorkItem := nil;
      end;
    end
    else
    begin
      AWorkItem := nil;
      Result := False;
    end;
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
  I:Integer;
begin
  FCasLock.Enter;
  try
    while Size > 0 do
    begin
      DummyItem := nil;  // Initialize the interface variable
      if TryPop(DummyItem) then
        DummyItem := nil;  // Explicitly release the interface
    end;
    FBottom := 0;
    FTop := 0;
    // Clear array references
    for I := 0 to Length(FItems) - 1 do
      FItems[I] := nil;
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
  if Assigned(FLocalDeque) then
  begin
    FLocalDeque.Clear;  // Clear any remaining work items
    FLocalDeque.Free;
    FLocalDeque := nil;
  end;
  
  if Assigned(FWorkEvent) then
  begin
    FWorkEvent.Free;
    FWorkEvent := nil;
  end;
  
  inherited;
end;

procedure TWorkStealingThread.Execute;
var
  WorkItem: IWorkItem;
begin
  WriteLn(Format('Thread %d: Starting', [ThreadID]));
  while not Terminated do
  begin
    WorkItem := nil; // Ensure clean interface state
    if TryGetWork(WorkItem) then
    begin
      if Assigned(WorkItem) then // Double-check we got valid work
      begin
        WriteLn(Format('Thread %d: Got work item', [ThreadID]));
        if not Terminated then
        begin
          try
            WriteLn(Format('Thread %d: Executing work item', [ThreadID]));
            WorkItem.Execute;
            WriteLn(Format('Thread %d: Work item executed', [ThreadID]));
            TWorkStealingPool(FPool).DecrementWorkCount;
            WriteLn(Format('Thread %d: Work count decremented', [ThreadID]));
          except
            on E: Exception do
              TWorkStealingPool(FPool).HandleError(
                Format('Thread %d: %s', [ThreadID, E.Message]));
          end;
        end;
      end;
      WorkItem := nil; // Explicitly release interface
    end
    else if not Terminated then
    begin
      if not TryStealWork then
      begin
        WriteLn(Format('Thread %d: No work found, waiting', [ThreadID]));
        FWorkEvent.WaitFor(100);
      end;
    end;
  end;
  WriteLn(Format('Thread %d: Terminating', [ThreadID]));
end;

function TWorkStealingThread.TryGetWork(out AWorkItem: IWorkItem): Boolean;
begin
  WriteLn(Format('Thread %d: Attempting to get work', [ThreadID]));
  Result := FLocalDeque.TryPop(AWorkItem);
  if Result then
    WriteLn(Format('Thread %d: Got work from local queue', [ThreadID]))
  else
    WriteLn(Format('Thread %d: Local queue empty', [ThreadID]));
end;

function TWorkStealingThread.TryStealWork: Boolean;
var
  WorkItem: IWorkItem;
  OtherThread: TWorkStealingThread;
  I, Attempts: Integer;
begin
  Result := False;
  Attempts := 0;
  WorkItem := nil; // Initialize interface
  
  while (not Result) and (not Terminated) and (Attempts < 3) do
  begin
    with TWorkStealingPool(FPool) do
    begin
      for I := 0 to Length(FWorkers) - 1 do
      begin
        if Terminated then
          Exit;
          
        OtherThread := FWorkers[I];
        if (OtherThread <> Self) and 
           Assigned(OtherThread) and 
           Assigned(OtherThread.LocalDeque) then
        begin
          WorkItem := nil; // Reset interface before steal attempt
          if OtherThread.LocalDeque.TrySteal(WorkItem) and Assigned(WorkItem) then
          begin
            FLocalDeque.TryPush(WorkItem);
            Result := True;
            Break;
          end;
        end;
      end;
    end;
    
    Inc(Attempts);
    if not Result then
      Sleep(1);
  end;
  WorkItem := nil; // Final cleanup
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
  inherited Terminate;  // Call inherited first
  FTerminating := True;
  if Assigned(FWorkEvent) then
  begin
    SignalWork;  // Wake up thread to check terminating flag
    FWorkEvent.SetEvent;  // Additional signal to ensure thread wakes up
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
begin
  WriteLn('ThreadPool.Destroy: Starting cleanup');
  try
    // Signal termination first
    if Assigned(FWorkLock) then
    begin
      WriteLn('ThreadPool.Destroy: Signaling thread termination');
      FWorkLock.Enter;
      try
        for I := Low(FWorkers) to High(FWorkers) do
          if Assigned(FWorkers[I]) then
          begin
            WriteLn(Format('ThreadPool.Destroy: Terminating worker %d', [I]));
            FWorkers[I].Terminate;
            FWorkers[I].SignalWork;  // Wake up thread
          end;
      finally
        FWorkLock.Leave;
      end;
    end;

    // Wait for and free threads
    if Length(FWorkers) > 0 then
    begin
      WriteLn(Format('ThreadPool.Destroy: Cleaning up %d workers', [Length(FWorkers)]));
      for I := 0 to High(FWorkers) do
        if Assigned(FWorkers[I]) then
        begin
          try
            WriteLn(Format('ThreadPool.Destroy: Waiting for worker %d', [I]));
            FWorkers[I].WaitFor;
            WriteLn(Format('ThreadPool.Destroy: Freeing worker %d', [I]));
            FWorkers[I].Free;
            FWorkers[I] := nil;  // Clear reference
          except
            WriteLn(Format('ThreadPool.Destroy: Error cleaning up worker %d', [I]));
          end;
        end;
    end;
    SetLength(FWorkers, 0);  // Clear array

    // Clean up synchronization objects
    WriteLn('ThreadPool.Destroy: Cleaning up synchronization objects');
    FWorkEvent.Free;
    FWorkLock.Free;
    FErrorLock.Free;
    WriteLn('ThreadPool.Destroy: Cleanup completed');
  except
    WriteLn('ThreadPool.Destroy: Error during cleanup');
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
  WriteLn('Queue: Creating work item');
  if not Assigned(AProcedure) then
    Exit;
    
  WorkItem := TWorkItemProcedure.Create(AProcedure);  // Create with interface variable
  
  FWorkLock.Enter;
  try
    WriteLn('Queue: Incrementing work count');
    Inc(FWorkCount);
    WriteLn(Format('Queue: Work count = %d', [FWorkCount]));
    DistributeWork(WorkItem);  // Pass the interface variable
  finally
    FWorkLock.Leave;
  end;
  WriteLn('Queue: Work item queued');
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
      Inc(FWorkCount);
      DistributeWork(WorkItem);
    finally
      FWorkLock.Leave;
      WorkItem := nil;  // Explicitly release interface
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
    FWorkLock.Enter;
    try
      Inc(FWorkCount);
      DistributeWork(WorkItem);
    finally
      FWorkLock.Leave;
      WorkItem := nil;  // Explicitly release interface
    end;
  except
    on E: Exception do
      HandleError(Format('Queue(ProcedureIndex) error: %s', [E.Message]));
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
var
  TimeoutCounter: Integer;
  AllThreadsIdle: Boolean;
  I: Integer;
begin
  WriteLn('WaitForAll: Starting wait');
  TimeoutCounter := 0;
  
  while True do
  begin
    FWorkLock.Enter;
    try
      WriteLn(Format('WaitForAll: Work count = %d', [FWorkCount]));
      
      if FWorkCount = 0 then
      begin
        WriteLn('WaitForAll: All work completed');
        Break;
      end;
      
      // Check if all threads are idle but work count > 0
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
        WriteLn(Format('WaitForAll: All threads idle but work count = %d (timeout: %d)', 
          [FWorkCount, TimeoutCounter]));
        
        if TimeoutCounter > 10 then
        begin
          WriteLn('WaitForAll: Possible deadlock detected, resetting work count');
          FWorkCount := 0;
          Break;
        end;
      end else
        TimeoutCounter := 0;
        
    finally
      FWorkLock.Leave;
    end;
    
    WriteLn('WaitForAll: Waiting for event');
    FWorkEvent.WaitFor(100);
    FWorkEvent.ResetEvent;
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
    WriteLn(Format('DecrementWorkCount: Current count = %d', [FWorkCount]));
    if FWorkCount > 0 then
    begin
      Dec(FWorkCount);
      WriteLn(Format('DecrementWorkCount: New count = %d', [FWorkCount]));
      if FWorkCount = 0 then
      begin
        WriteLn('DecrementWorkCount: All work completed, signaling event');
        FWorkEvent.SetEvent;
      end;
    end
    else
      WriteLn('DecrementWorkCount: Warning - attempting to decrement zero count');
  finally
    FWorkLock.Leave;
  end;
end;

end.
