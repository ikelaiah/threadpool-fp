unit ThreadPool.Simple;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadPool.Types;

type
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

  { Simple worker thread implementation }
  TSimpleWorkerThread = class(TThread, IWorkerThread) 
  private
    FRefCount: Integer;
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
    function QueryInterface(constref IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

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

var
  { Global thread pool instance
    Automatically created at unit initialization
    Freed at unit finalization }
  GlobalThreadPool: TSimpleThreadPool;

implementation

{ TSimpleWorkerThread }

constructor TSimpleWorkerThread.Create(AThreadPool: TObject);
begin
  inherited Create(True);  // Create suspended
  FRefCount := 0;
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

function TSimpleWorkerThread.QueryInterface(constref IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TSimpleWorkerThread._AddRef: Integer; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TSimpleWorkerThread._Release: Integer; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
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
  WaitForAll;  // Ensure all tasks complete before destroying
  FShutdown := True;
  ClearWorkItems;
  ClearThreads;
  
  // Clean up synchronization objects
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

initialization
  GlobalThreadPool := TSimpleThreadPool.Create;  // Create global instance

finalization
  GlobalThreadPool.Free;  // Clean up global instance

end.
