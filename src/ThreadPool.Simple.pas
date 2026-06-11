unit ThreadPool.Simple;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadPool.Types;

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
  TSimpleThreadPool = class(TThreadPoolBase)
  private
    FThreads: TThreadList;
    FWorkItems: TThreadList;
    FWorkItemLock: TCriticalSection;
    FWorkItemCount: Integer;
    FWorkItemEvent: TEvent;
    FErrorLock: TCriticalSection;
    FErrorEvent: TEvent;
    procedure ClearThreads;
    procedure ClearWorkItems;
  public
    constructor Create(AThreadCount: Integer = 0); override;
    destructor Destroy; override;
    
    { IThreadPool implementation }
    procedure Queue(AProcedure: TThreadProcedure); override;
    procedure Queue(AMethod: TThreadMethod); override;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); override;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); override;
    procedure WaitForAll; override;
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
  List: TList;
  WorkItem: TSimpleWorkItem;
begin
  Pool := TSimpleThreadPool(FThreadPool);
  
  while not Terminated do
  begin
    // Try to get a work item
    List := Pool.FWorkItems.LockList;
    try
      if List.Count > 0 then
      begin
        WorkItem := TSimpleWorkItem(List[0]);
        List.Delete(0);
      end
      else
        WorkItem := nil;
    finally
      Pool.FWorkItems.UnlockList;
    end;

    // Process work item if we got one
    if Assigned(WorkItem) then
    begin
      try
        WorkItem.Execute;
      except
        on E: Exception do
        begin
          // Capture error
          Pool.FErrorLock.Enter;
          try
            Pool.SetLastError(E.Message);
            Pool.FErrorEvent.SetEvent;
          finally
            Pool.FErrorLock.Leave;
          end;
        end;
      end;
      WorkItem.Free;
    end
    else
      Sleep(1); // Prevent busy waiting
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
var
  Pool: TSimpleThreadPool;
begin
  try
    // Execute the appropriate work item based on its type
    case FItemType of
      witProcedure: if Assigned(FProcedure) then FProcedure;
      witMethod: if Assigned(FMethod) then FMethod;
      witProcedureIndex: if Assigned(FProcedureIndex) then FProcedureIndex(FIndex);
      witMethodIndex: if Assigned(FMethodIndex) then FMethodIndex(FIndex);
    end;
  finally
    // Update work item count and signal completion if necessary
    Pool := TSimpleThreadPool(FThreadPool);
    Pool.FWorkItemLock.Enter;
    try
      Dec(Pool.FWorkItemCount);
      if Pool.FWorkItemCount = 0 then
        Pool.FWorkItemEvent.SetEvent;
    finally
      Pool.FWorkItemLock.Leave;
    end;
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
  
  // Initialize thread-safe collections and synchronization
  FThreads := TThreadList.Create;
  FWorkItems := TThreadList.Create;
  FWorkItemLock := TCriticalSection.Create;
  FErrorLock := TCriticalSection.Create;
  FErrorEvent := TEvent.Create(nil, True, False, '');
  FWorkItemEvent := TEvent.Create(nil, True, True, '');
  FWorkItemCount := 0;

  // Create and start worker threads
  for I := 1 to FThreadCount do
  begin
    Thread := TSimpleWorkerThread.Create(Self);
    FThreads.Add(Thread);
    Thread.Start;
  end;
end;

destructor TSimpleThreadPool.Destroy;
begin
  // Only drain outstanding work if the pool was fully constructed. If the
  // constructor failed partway, FPC still calls this destructor on the
  // half-built object, so guard against the sync objects being nil.
  if Assigned(FWorkItemEvent) and Assigned(FErrorEvent) then
    WaitForAll;  // Ensure all tasks complete before destroying

  FShutdown := True;

  if Assigned(FWorkItems) then
    ClearWorkItems;
  if Assigned(FThreads) then
    ClearThreads;

  // Clean up synchronization objects (each may be nil after a partial
  // construction, so Free — which is nil-safe — is used throughout).
  FWorkItemLock.Free;
  FThreads.Free;
  FWorkItems.Free;
  FWorkItemEvent.Free;
  FErrorEvent.Free;
  FErrorLock.Free;

  inherited Destroy;
end;

procedure TSimpleThreadPool.ClearThreads;
var
  Thread: TSimpleWorkerThread;
  List: TList;
  I: Integer;
begin
  // Signal all threads to terminate
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TSimpleWorkerThread(List[I]);
      Thread.Terminate;
    end;
  finally
    FThreads.UnlockList;
  end;

  // Wait for all threads to finish and clean up
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
  List: TList;
  I: Integer;
begin
  // Clean up any remaining work items
  List := FWorkItems.LockList;
  try
    for I := 0 to List.Count - 1 do
      TSimpleWorkItem(List[I]).Free;
    List.Clear;
  finally
    FWorkItems.UnlockList;
  end;
end;

{ Queue overloads for different types of work items }

procedure TSimpleThreadPool.Queue(AProcedure: TThreadProcedure);
var
  WorkItem: TSimpleWorkItem;
begin
  if FShutdown then Exit;  // Don't queue if shutting down
  
  // Update work item count
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    if FWorkItemCount > 0 then
      FWorkItemEvent.ResetEvent;  // Reset completion event
  finally
    FWorkItemLock.Leave;
  end;
  
  // Create and queue work item
  WorkItem := TSimpleWorkItem.Create(Self);
  WorkItem.FProcedure := AProcedure;
  WorkItem.FItemType := witProcedure;
  
  FWorkItems.Add(WorkItem);
end;

procedure TSimpleThreadPool.Queue(AMethod: TThreadMethod);
var
  WorkItem: TSimpleWorkItem;
begin
  if FShutdown then Exit;
  
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    if FWorkItemCount > 0 then
      FWorkItemEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;
  
  WorkItem := TSimpleWorkItem.Create(Self);
  WorkItem.FMethod := AMethod;
  WorkItem.FItemType := witMethod;
  
  FWorkItems.Add(WorkItem);
end;

procedure TSimpleThreadPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
var
  WorkItem: TSimpleWorkItem;
begin
  if FShutdown then Exit;
  
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    if FWorkItemCount > 0 then
      FWorkItemEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;
  
  WorkItem := TSimpleWorkItem.Create(Self);
  WorkItem.FProcedureIndex := AProcedure;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witProcedureIndex;
  
  FWorkItems.Add(WorkItem);
end;

procedure TSimpleThreadPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
var
  WorkItem: TSimpleWorkItem;
begin
  if FShutdown then Exit;
  
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    if FWorkItemCount > 0 then
      FWorkItemEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;
  
  WorkItem := TSimpleWorkItem.Create(Self);
  WorkItem.FMethodIndex := AMethod;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witMethodIndex;
  
  FWorkItems.Add(WorkItem);
end;

procedure TSimpleThreadPool.WaitForAll;
begin
  FWorkItemEvent.WaitFor(INFINITE);  // Wait for all work items to complete
  // If there was an error, ensure it's fully captured
  if FErrorEvent.WaitFor(100) = wrSignaled then
    FErrorEvent.ResetEvent;
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
  GlobalThreadPool := TSimpleThreadPool.Create;  // Create global instance

finalization
  GlobalThreadPool.Free;  // Clean up global instance

end.
