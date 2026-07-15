unit ThreadPool.Simple;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadPool.Types, ThreadPool.Tasks;

type
  {$REGION 'Internal: Work Item'}
  { Simple work item implementation }
  TSimpleWorkItem = class(TInterfacedObject, IWorkItem)
  private
    FProcedure: TThreadProcedure;
    FMethod: TThreadMethod;
    FProcedureIndex: TThreadProcedureIndex;
    FMethodIndex: TThreadMethodIndex;
    FIndex: Integer;
    FItemType: TWorkItemType;
    FThreadPool: TObject;
  public
    constructor Create(AThreadPool: TObject);
    destructor Destroy; override;
    { IWorkItem implementation }
    procedure Execute;
    function GetItemType: Integer;
  end;

  {$ENDREGION}

  {$REGION 'Internal: Worker Thread'}
  { Simple worker thread implementation }
  TSimpleWorkerThread = class(TThread, IWorkerThread)
  private
    FThreadPool: TObject;
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadPool: TObject);
    destructor Destroy; override;
    { IWorkerThread implementation }
    procedure Start;
    procedure Terminate;
    procedure WaitFor;
    function GetThreadID: TThreadID;
    function QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  end;

  {$ENDREGION}

  {$REGION 'Public API: TSimpleThreadPool'}
  { Simple thread pool implementation }
  TSimpleThreadPool = class(TThreadPoolBase, IThreadPoolTaskSource)
  private
    FThreads: TThreadList;
    FWorkItems: array of IWorkItem;
    FQueueHead: Integer;
    FQueueTail: Integer;
    FQueueCount: Integer;
    FQueueLock: TCriticalSection;
    FWorkItemLock: TCriticalSection;
    FWorkItemCount: Integer;
    FWorkItemEvent: TEvent;
    FWorkAvailableEvent: TEvent;
    procedure ClearThreads;
    procedure ClearWorkItems;
    procedure EnqueueWorkItem(const AWorkItem: IWorkItem);
    function TryDequeueWorkItem(out AWorkItem: IWorkItem): Boolean;
    function TrySubmitWorkItem(const AWorkItem: IWorkItem): Boolean;
    function SubmitRangeWorkItem(const AWorkItem: IWorkItem): Boolean;
    procedure CompleteWorkItem;
    function IsCurrentWorkerThread: Boolean;
  public
    constructor Create(AThreadCount: Integer = 0); override;
    destructor Destroy; override;
    
    { IThreadPool implementation }
    procedure Queue(AProcedure: TThreadProcedure); override;
    procedure Queue(AMethod: TThreadMethod); override;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); override;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); override;
    function TryQueue(AProcedure: TThreadProcedure;
      ATimeoutMS: Cardinal): Boolean; overload; override;
    function TryQueue(AMethod: TThreadMethod;
      ATimeoutMS: Cardinal): Boolean; overload; override;
    function TryQueue(AProcedure: TThreadProcedureIndex; AIndex: Integer;
      ATimeoutMS: Cardinal): Boolean; overload; override;
    function TryQueue(AMethod: TThreadMethodIndex; AIndex: Integer;
      ATimeoutMS: Cardinal): Boolean; overload; override;
    function Submit(AProcedure: TThreadProcedure): IThreadPoolTask; overload;
    function Submit(AMethod: TThreadMethod): IThreadPoolTask; overload;
    function Submit(AProcedure: TThreadProcedureIndex;
      AIndex: Integer): IThreadPoolTask; overload;
    function Submit(AMethod: TThreadMethodIndex;
      AIndex: Integer): IThreadPoolTask; overload;
    function TrySubmit(AProcedure: TThreadProcedure; ATimeoutMS: Cardinal;
      out ATask: IThreadPoolTask): Boolean; overload;
    function TrySubmit(AMethod: TThreadMethod; ATimeoutMS: Cardinal;
      out ATask: IThreadPoolTask): Boolean; overload;
    function TrySubmit(AProcedure: TThreadProcedureIndex; AIndex: Integer;
      ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean; overload;
    function TrySubmit(AMethod: TThreadMethodIndex; AIndex: Integer;
      ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean; overload;
    function SubmitRange(AProcedure: TThreadProcedureIndex;
      AFirstIndex, ALastIndex: Integer;
      AChunkSize: Integer = 0): IThreadPoolTaskBatch; overload;
    function SubmitRange(AMethod: TThreadMethodIndex;
      AFirstIndex, ALastIndex: Integer;
      AChunkSize: Integer = 0): IThreadPoolTaskBatch; overload;
    procedure WaitForAll; overload; override;
    function WaitForAll(ATimeoutMS: Cardinal): Boolean; overload; override;
    procedure Shutdown; override;
    function GetThreadCount: Integer; override;
    function GetLastError: string; override;
    property ThreadCount: Integer read GetThreadCount;
    property LastError: string read GetLastError;
  end;

  {$ENDREGION}

var
  {$REGION 'Public API: Global instance'}
  { Global thread pool instance.
    Created automatically at unit initialization; freed at finalization.
    Do NOT call GlobalThreadPool.Free — the unit manages its lifetime. }
  GlobalThreadPool: TSimpleThreadPool;
  {$ENDREGION}

implementation

{$REGION 'TSimpleWorkerThread'}

{ TSimpleWorkerThread }

constructor TSimpleWorkerThread.Create(AThreadPool: TObject);
begin
  inherited Create(True);  // Create suspended
  FThreadPool := AThreadPool;
  FreeOnTerminate := False;
end;

destructor TSimpleWorkerThread.Destroy;
begin
  inherited;
end;

procedure TSimpleWorkerThread.Start;
begin
  inherited Start;
end;

procedure TSimpleWorkerThread.Terminate;
begin
  inherited Terminate;
end;

procedure TSimpleWorkerThread.WaitFor;
begin
  inherited WaitFor;
end;

function TSimpleWorkerThread.GetThreadID: TThreadID;
begin
  Result := ThreadID;
end;

function TSimpleWorkerThread.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TSimpleWorkerThread._AddRef: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  // The pool owns this thread's lifetime via FThreads/ClearThreads, so the
  // IWorkerThread interface must NOT reference-count. Returning -1 marks this
  // as a non-ref-counted interface (the same contract TComponent uses), which
  // prevents an interface assignment from freeing the still-live worker.
  Result := -1;
end;

function TSimpleWorkerThread._Release: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  // See _AddRef: lifetime is owned by the pool, never by interface refcount.
  Result := -1;
end;

procedure TSimpleWorkerThread.Execute;
var
  Pool: TSimpleThreadPool;
  WorkItem: IWorkItem;
begin
  Pool := TSimpleThreadPool(FThreadPool);

  while not Terminated do
  begin
    { Workers sleep without polling until submission or shutdown signals them. }
    Pool.FWorkAvailableEvent.WaitFor(INFINITE);
    if Terminated then
      Break;

    while (not Terminated) and Pool.TryDequeueWorkItem(WorkItem) do
    begin
      try
        ExecuteThreadPoolWorkItem(WorkItem, @Pool.SetLastError);
      finally
        WorkItem := nil;
        { Completion accounting is independent of task and callback failures. }
        Pool.CompleteWorkItem;
      end;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TSimpleWorkItem'}

{ TSimpleWorkItem }

constructor TSimpleWorkItem.Create(AThreadPool: TObject);
begin
  inherited Create;
  FThreadPool := AThreadPool;
  FItemType := witProcedure;
  FIndex := 0;
end;

destructor TSimpleWorkItem.Destroy;
begin
  inherited;
end;

procedure TSimpleWorkItem.Execute;
begin
  case FItemType of
    witProcedure: if Assigned(FProcedure) then FProcedure;
    witMethod: if Assigned(FMethod) then FMethod;
    witProcedureIndex: if Assigned(FProcedureIndex) then FProcedureIndex(FIndex);
    witMethodIndex: if Assigned(FMethodIndex) then FMethodIndex(FIndex);
  end;
end;

function TSimpleWorkItem.GetItemType: Integer;
begin
  Result := Ord(FItemType);
end;

{$ENDREGION}

{$REGION 'TSimpleThreadPool — Public API'}

{ TSimpleThreadPool }

constructor TSimpleThreadPool.Create(AThreadCount: Integer = 0);
var
  I: Integer;
  Thread: TSimpleWorkerThread;
begin
  inherited Create(AThreadCount);

  FThreads := TThreadList.Create;
  FQueueLock := TCriticalSection.Create;
  FWorkItemLock := TCriticalSection.Create;
  FWorkAvailableEvent := TEvent.Create(nil, True, False, '');
  FWorkItemEvent := TEvent.Create(nil, True, True, '');
  SetLength(FWorkItems, 64);
  FQueueHead := 0;
  FQueueTail := 0;
  FQueueCount := 0;
  FWorkItemCount := 0;

  for I := 1 to FThreadCount do
  begin
    Thread := TSimpleWorkerThread.Create(Self);
    FThreads.Add(Thread);
    Thread.Start;
  end;
end;

destructor TSimpleThreadPool.Destroy;
begin
  Shutdown;
  ClearWorkItems;
  FQueueLock.Free;
  FWorkItemLock.Free;
  FThreads.Free;
  FWorkAvailableEvent.Free;
  FWorkItemEvent.Free;
  inherited Destroy;
end;

procedure TSimpleThreadPool.EnqueueWorkItem(const AWorkItem: IWorkItem);
var
  NewItems: array of IWorkItem;
  I, NewCapacity: Integer;
begin
  NewItems := nil;
  FQueueLock.Enter;
  try
    if FQueueCount = Length(FWorkItems) then
    begin
      NewCapacity := Length(FWorkItems) * 2;
      if NewCapacity = 0 then
        NewCapacity := 64;
      SetLength(NewItems, NewCapacity);
      for I := 0 to FQueueCount - 1 do
        NewItems[I] := FWorkItems[(FQueueHead + I) mod Length(FWorkItems)];
      FWorkItems := NewItems;
      FQueueHead := 0;
      FQueueTail := FQueueCount;
    end;

    FWorkItems[FQueueTail] := AWorkItem;
    FQueueTail := (FQueueTail + 1) mod Length(FWorkItems);
    Inc(FQueueCount);

    FWorkItemLock.Enter;
    try
      Inc(FWorkItemCount);
      FWorkItemEvent.ResetEvent;
    finally
      FWorkItemLock.Leave;
    end;
    FWorkAvailableEvent.SetEvent;
  finally
    FQueueLock.Leave;
  end;
end;

function TSimpleThreadPool.TryDequeueWorkItem(
  out AWorkItem: IWorkItem): Boolean;
begin
  FQueueLock.Enter;
  try
    Result := FQueueCount > 0;
    if Result then
    begin
      AWorkItem := FWorkItems[FQueueHead];
      FWorkItems[FQueueHead] := nil;
      FQueueHead := (FQueueHead + 1) mod Length(FWorkItems);
      Dec(FQueueCount);
      if FQueueCount = 0 then
        FWorkAvailableEvent.ResetEvent;
    end
    else
      AWorkItem := nil;
  finally
    FQueueLock.Leave;
  end;
end;

procedure TSimpleThreadPool.CompleteWorkItem;
begin
  FWorkItemLock.Enter;
  try
    Dec(FWorkItemCount);
    if FWorkItemCount = 0 then
      FWorkItemEvent.SetEvent;
  finally
    FWorkItemLock.Leave;
  end;
end;

function TSimpleThreadPool.IsCurrentWorkerThread: Boolean;
var
  List: TList;
  I: Integer;
begin
  Result := False;
  if not Assigned(FThreads) then
    Exit;
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
      if TThread(List[I]).ThreadID = GetCurrentThreadID then
        Exit(True);
  finally
    FThreads.UnlockList;
  end;
end;

procedure TSimpleThreadPool.Shutdown;
begin
  if IsCurrentWorkerThread then
    raise EThreadPoolShutdown.Create('Shutdown cannot be called from a pool worker');
  if not BeginShutdown then
    Exit;
  try
    if Assigned(FWorkItemEvent) then
      WaitForAll;
    if Assigned(FThreads) then
      ClearThreads;
  finally
    FinishShutdown;
  end;
end;

procedure TSimpleThreadPool.ClearThreads;
var
  Thread: TSimpleWorkerThread;
  List: TList;
  I: Integer;
begin
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
      TSimpleWorkerThread(List[I]).Terminate;
    if Assigned(FWorkAvailableEvent) then
      FWorkAvailableEvent.SetEvent;
  finally
    FThreads.UnlockList;
  end;

  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TSimpleWorkerThread(List[I]);
      Thread.WaitFor;
      Thread.Free;
    end;
    List.Clear;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TSimpleThreadPool.ClearWorkItems;
var
  WorkItem: IWorkItem;
begin
  if not Assigned(FQueueLock) then
    Exit;
  while TryDequeueWorkItem(WorkItem) do
    WorkItem := nil;
  SetLength(FWorkItems, 0);
end;

function TSimpleThreadPool.TrySubmitWorkItem(
  const AWorkItem: IWorkItem): Boolean;
begin
  BeginQueue;
  try
    EnqueueWorkItem(AWorkItem);
    Result := True;
  finally
    EndQueue;
  end;
end;

function TSimpleThreadPool.SubmitRangeWorkItem(
  const AWorkItem: IWorkItem): Boolean;
begin
  EnqueueWorkItem(AWorkItem);
  Result := True;
end;

function TSimpleThreadPool.Submit(
  AProcedure: TThreadProcedure): IThreadPoolTask;
begin
  if not TrySubmit(AProcedure, 0, Result) then
    Result := nil;
end;

function TSimpleThreadPool.Submit(
  AMethod: TThreadMethod): IThreadPoolTask;
begin
  if not TrySubmit(AMethod, 0, Result) then
    Result := nil;
end;

function TSimpleThreadPool.Submit(AProcedure: TThreadProcedureIndex;
  AIndex: Integer): IThreadPoolTask;
begin
  if not TrySubmit(AProcedure, AIndex, 0, Result) then
    Result := nil;
end;

function TSimpleThreadPool.Submit(AMethod: TThreadMethodIndex;
  AIndex: Integer): IThreadPoolTask;
begin
  if not TrySubmit(AMethod, AIndex, 0, Result) then
    Result := nil;
end;

function TSimpleThreadPool.TrySubmit(AProcedure: TThreadProcedure;
  ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AProcedure, ATask);
  try
    Result := TrySubmitWorkItem(WorkItem);
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TSimpleThreadPool.TrySubmit(AMethod: TThreadMethod;
  ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AMethod, ATask);
  try
    Result := TrySubmitWorkItem(WorkItem);
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TSimpleThreadPool.TrySubmit(AProcedure: TThreadProcedureIndex;
  AIndex: Integer; ATimeoutMS: Cardinal;
  out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AProcedure, AIndex, ATask);
  try
    Result := TrySubmitWorkItem(WorkItem);
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TSimpleThreadPool.TrySubmit(AMethod: TThreadMethodIndex;
  AIndex: Integer; ATimeoutMS: Cardinal;
  out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AMethod, AIndex, ATask);
  try
    Result := TrySubmitWorkItem(WorkItem);
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TSimpleThreadPool.SubmitRange(
  AProcedure: TThreadProcedureIndex; AFirstIndex, ALastIndex: Integer;
  AChunkSize: Integer): IThreadPoolTaskBatch;
begin
  if GetThreadPoolRangeChunkSize(AFirstIndex, ALastIndex,
    FThreadCount, AChunkSize) = 0 then
    Exit(NewThreadPoolTaskBatch);
  BeginQueue;
  try
    Result := NewThreadPoolRangeBatch(AProcedure, AFirstIndex,
      ALastIndex, FThreadCount, AChunkSize, @SubmitRangeWorkItem);
  finally
    EndQueue;
  end;
end;

function TSimpleThreadPool.SubmitRange(AMethod: TThreadMethodIndex;
  AFirstIndex, ALastIndex: Integer;
  AChunkSize: Integer): IThreadPoolTaskBatch;
begin
  if GetThreadPoolRangeChunkSize(AFirstIndex, ALastIndex,
    FThreadCount, AChunkSize) = 0 then
    Exit(NewThreadPoolTaskBatch);
  BeginQueue;
  try
    Result := NewThreadPoolRangeBatch(AMethod, AFirstIndex,
      ALastIndex, FThreadCount, AChunkSize, @SubmitRangeWorkItem);
  finally
    EndQueue;
  end;
end;

function TSimpleThreadPool.TryQueue(AProcedure: TThreadProcedure;
  ATimeoutMS: Cardinal): Boolean;
var
  WorkItemObject: TSimpleWorkItem;
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItemObject := TSimpleWorkItem.Create(Self);
    WorkItemObject.FProcedure := AProcedure;
    WorkItemObject.FItemType := witProcedure;
    WorkItem := WorkItemObject;
    EnqueueWorkItem(WorkItem);
    Result := True;
  finally
    EndQueue;
  end;
end;

function TSimpleThreadPool.TryQueue(AMethod: TThreadMethod;
  ATimeoutMS: Cardinal): Boolean;
var
  WorkItemObject: TSimpleWorkItem;
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItemObject := TSimpleWorkItem.Create(Self);
    WorkItemObject.FMethod := AMethod;
    WorkItemObject.FItemType := witMethod;
    WorkItem := WorkItemObject;
    EnqueueWorkItem(WorkItem);
    Result := True;
  finally
    EndQueue;
  end;
end;

function TSimpleThreadPool.TryQueue(AProcedure: TThreadProcedureIndex;
  AIndex: Integer; ATimeoutMS: Cardinal): Boolean;
var
  WorkItemObject: TSimpleWorkItem;
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItemObject := TSimpleWorkItem.Create(Self);
    WorkItemObject.FProcedureIndex := AProcedure;
    WorkItemObject.FIndex := AIndex;
    WorkItemObject.FItemType := witProcedureIndex;
    WorkItem := WorkItemObject;
    EnqueueWorkItem(WorkItem);
    Result := True;
  finally
    EndQueue;
  end;
end;

function TSimpleThreadPool.TryQueue(AMethod: TThreadMethodIndex;
  AIndex: Integer; ATimeoutMS: Cardinal): Boolean;
var
  WorkItemObject: TSimpleWorkItem;
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItemObject := TSimpleWorkItem.Create(Self);
    WorkItemObject.FMethodIndex := AMethod;
    WorkItemObject.FIndex := AIndex;
    WorkItemObject.FItemType := witMethodIndex;
    WorkItem := WorkItemObject;
    EnqueueWorkItem(WorkItem);
    Result := True;
  finally
    EndQueue;
  end;
end;

procedure TSimpleThreadPool.Queue(AProcedure: TThreadProcedure);
begin
  TryQueue(AProcedure, 0);
end;

procedure TSimpleThreadPool.Queue(AMethod: TThreadMethod);
begin
  TryQueue(AMethod, 0);
end;

procedure TSimpleThreadPool.Queue(AProcedure: TThreadProcedureIndex;
  AIndex: Integer);
begin
  TryQueue(AProcedure, AIndex, 0);
end;

procedure TSimpleThreadPool.Queue(AMethod: TThreadMethodIndex;
  AIndex: Integer);
begin
  TryQueue(AMethod, AIndex, 0);
end;

procedure TSimpleThreadPool.WaitForAll;
begin
  WaitForAll(THREADPOOL_INFINITE);
end;

function TSimpleThreadPool.WaitForAll(ATimeoutMS: Cardinal): Boolean;
begin
  Result := FWorkItemEvent.WaitFor(ATimeoutMS) = wrSignaled;
end;

function TSimpleThreadPool.GetThreadCount: Integer;
begin
  Result := inherited GetThreadCount;
end;

function TSimpleThreadPool.GetLastError: string;
begin
  Result := inherited GetLastError;
end;

{$ENDREGION}

initialization
  GlobalThreadPool := TSimpleThreadPool.Create;

finalization
  GlobalThreadPool.Free;

end.
