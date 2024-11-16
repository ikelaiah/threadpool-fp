unit ThreadPool;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TThreadProcedure = procedure;
  TThreadMethod = procedure of object;
  TThreadProcedureIndex = procedure(index: Integer);
  TThreadMethodIndex = procedure(index: Integer) of object;

  { TWorkItem }
  TWorkItem = class
  private
    FProcedure: TThreadProcedure;
    FMethod: TThreadMethod;
    FProcedureIndex: TThreadProcedureIndex;
    FMethodIndex: TThreadMethodIndex;
    FIndex: Integer;
    FObject: TObject;
    FItemType: (witProcedure, witMethod, witProcedureIndex, witMethodIndex);
    FThreadPool: TObject;
  public
    constructor Create(AThreadPool: TObject);
    procedure Execute;
  end;

  { TWorkerThread }
  TWorkerThread = class(TThread)
  private
    FThreadPool: TObject; // Will be TThreadPool
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadPool: TObject);
  end;

  { TThreadPool }
  TThreadPool = class
  private
    FThreads: TThreadList;
    FWorkItems: TThreadList;
    FThreadCount: Integer;
    FShutdown: Boolean;
    FWorkItemLock: TCriticalSection;
    FWorkItemCount: Integer;
    FWorkItemEvent: TEvent;
    procedure ClearThreads;
    procedure ClearWorkItems;
  public
    constructor Create(AThreadCount: Integer = 0);
    destructor Destroy; override;
    
    procedure Queue(AProcedure: TThreadProcedure); overload;
    procedure Queue(AMethod: TThreadMethod); overload;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); overload;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); overload;
    procedure WaitForAll;
    
    property ThreadCount: Integer read FThreadCount;
  end;

var
  GlobalThreadPool: TThreadPool;

implementation

{ TWorkItem }

constructor TWorkItem.Create(AThreadPool: TObject);
begin
  inherited Create;
  FThreadPool := AThreadPool;
  FObject := nil;
  FIndex := 0;
end;

procedure TWorkItem.Execute;
var
  Pool: TThreadPool;
begin
  try
    case FItemType of
      witProcedure: if Assigned(FProcedure) then FProcedure;
      witMethod: if Assigned(FMethod) then FMethod;
      witProcedureIndex: if Assigned(FProcedureIndex) then FProcedureIndex(FIndex);
      witMethodIndex: if Assigned(FMethodIndex) then FMethodIndex(FIndex);
    end;
  finally
    Pool := TThreadPool(FThreadPool);
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

{ TWorkerThread }

constructor TWorkerThread.Create(AThreadPool: TObject);
begin
  inherited Create(True);
  FThreadPool := AThreadPool;
  FreeOnTerminate := False;
end;

procedure TWorkerThread.Execute;
var
  Pool: TThreadPool;
  WorkItem: TWorkItem;
  List: TList;
begin
  Pool := TThreadPool(FThreadPool);
  
  while not Terminated do
  begin
    WorkItem := nil;
    List := Pool.FWorkItems.LockList;
    try
      if List.Count > 0 then
      begin
        WorkItem := TWorkItem(List[0]);
        List.Delete(0);
      end;
    finally
      Pool.FWorkItems.UnlockList;
    end;

    if WorkItem <> nil then
    begin
      try
        WorkItem.Execute;
      finally
        WorkItem.Free;
      end;
    end
    else
      Sleep(1); // Prevent busy waiting
  end;
end;

{ TThreadPool }

constructor TThreadPool.Create(AThreadCount: Integer = 0);
var
  I: Integer;
begin
  inherited Create;
  
  FThreads := TThreadList.Create;
  FWorkItems := TThreadList.Create;
  FWorkItemLock := TCriticalSection.Create;
  FShutdown := False;

  if AThreadCount <= 0 then
    FThreadCount := TThread.ProcessorCount
  else
    FThreadCount := AThreadCount;

  // Create worker threads
  for I := 1 to FThreadCount do
  begin
    FThreads.Add(TWorkerThread.Create(Self));
  end;

  // Start all threads
  with FThreads.LockList do
  try
    for I := 0 to Count - 1 do
      TWorkerThread(Items[I]).Start;
  finally
    FThreads.UnlockList;
  end;

  FWorkItemCount := 0;
  FWorkItemEvent := TEvent.Create(nil, True, True, ''); // Manual reset event
end;

destructor TThreadPool.Destroy;
begin
  WaitForAll; // Ensure all tasks complete before destroying
  FShutdown := True;
  ClearWorkItems;
  ClearThreads;
  
  FWorkItemLock.Free;
  FThreads.Free;
  FWorkItems.Free;
  FWorkItemEvent.Free;
  
  inherited Destroy;
end;

procedure TThreadPool.ClearThreads;
var
  Thread: TWorkerThread;
  List: TList;
  I: Integer;
begin
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TWorkerThread(List[I]);
      Thread.Terminate;
    end;
  finally
    FThreads.UnlockList;
  end;

  // Wait for all threads to finish
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TWorkerThread(List[I]);
      Thread.WaitFor;
      Thread.Free;
    end;
    List.Clear;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TThreadPool.ClearWorkItems;
var
  List: TList;
  I: Integer;
begin
  List := FWorkItems.LockList;
  try
    for I := 0 to List.Count - 1 do
      TWorkItem(List[I]).Free;
    List.Clear;
  finally
    FWorkItems.UnlockList;
  end;
end;

procedure TThreadPool.Queue(AProcedure: TThreadProcedure);
var
  WorkItem: TWorkItem;
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
  
  WorkItem := TWorkItem.Create(Self);
  WorkItem.FProcedure := AProcedure;
  WorkItem.FItemType := witProcedure;
  
  FWorkItems.Add(WorkItem);
end;

procedure TThreadPool.Queue(AMethod: TThreadMethod);
var
  WorkItem: TWorkItem;
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
  
  WorkItem := TWorkItem.Create(Self);
  WorkItem.FMethod := AMethod;
  WorkItem.FItemType := witMethod;
  
  FWorkItems.Add(WorkItem);
end;

procedure TThreadPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
var
  WorkItem: TWorkItem;
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
  
  WorkItem := TWorkItem.Create(Self);
  WorkItem.FProcedureIndex := AProcedure;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witProcedureIndex;
  
  FWorkItems.Add(WorkItem);
end;

procedure TThreadPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
var
  WorkItem: TWorkItem;
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
  
  WorkItem := TWorkItem.Create(Self);
  WorkItem.FMethodIndex := AMethod;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witMethodIndex;
  
  FWorkItems.Add(WorkItem);
end;

procedure TThreadPool.WaitForAll;
begin
  FWorkItemEvent.WaitFor(INFINITE);
end;

initialization
  GlobalThreadPool := TThreadPool.Create;

finalization
  GlobalThreadPool.Free;

end.