{*******************************************************}
{                                                       }
{       ThreadPool for Free Pascal                      }
{                                                       }
{       A lightweight thread pool implementation        }
{       designed for simple parallel processing tasks   }
{                                                       }
{*******************************************************}

unit ThreadPool;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, Math;

type
  { Function pointer types for different kinds of work items }
  TThreadProcedure = procedure;                              // Simple procedure
  TThreadMethod = procedure of object;                       // Object method
  TThreadProcedureIndex = procedure(index: Integer);         // Indexed procedure
  TThreadMethodIndex = procedure(index: Integer) of object;  // Indexed method

  { TWorkItem - Encapsulates work to be executed by worker threads }
  TWorkItem = class
  private
    FProcedure: TThreadProcedure;           // Simple procedure to execute
    FMethod: TThreadMethod;                 // Object method to execute
    FProcedureIndex: TThreadProcedureIndex; // Indexed procedure
    FMethodIndex: TThreadMethodIndex;       // Indexed method
    FIndex: Integer;                        // Index for indexed operations
    FObject: TObject;                       // Associated object (if any)
    FItemType: (witProcedure,               // Type of work item
                witMethod,
                witProcedureIndex,
                witMethodIndex);
    FThreadPool: TObject;                   // Owner thread pool
  public
    constructor Create(AThreadPool: TObject);
    procedure Execute;                    // Executes the work item
  end;

  { TWorkerThread - Thread that processes work items }
  TWorkerThread = class(TThread)
  private
    FThreadPool: TObject;                 // Owner thread pool
    FLastError: string;                   // Add this to store last error
  protected
    procedure Execute; override;          // Main thread execution loop
  public
    constructor Create(AThreadPool: TObject);
    property LastError: string read FLastError;  // Allow reading the error
  end;

  { TThreadPool - Main thread pool manager }
  TThreadPool = class
  private
    FThreads: TThreadList;               // List of worker threads
    FWorkItems: TThreadList;             // Queue of pending work items
    FThreadCount: Integer;               // Number of worker threads
    FShutdown: Boolean;                  // Shutdown flag
    FWorkItemLock: TCriticalSection;     // Synchronizes work item count
    FWorkItemCount: Integer;             // Number of pending work items
    FWorkItemEvent: TEvent;              // Signals work item completion
    FLastError: string;                  // Add this to store pool-level error
    FErrorLock: TCriticalSection;        // Add this for thread-safe error handling
    procedure ClearThreads;              // Cleanup worker threads
    procedure ClearWorkItems;            // Cleanup pending work items
  public
    { Creates a thread pool with specified number of threads
      If AThreadCount <= 0, uses TThread.ProcessorCount }
    constructor Create(AThreadCount: Integer = 0);
    destructor Destroy; override;
    
    { Queue different types of work items }
    procedure Queue(AProcedure: TThreadProcedure); overload;
    procedure Queue(AMethod: TThreadMethod); overload;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); overload;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); overload;
    
    { Waits for all queued work items to complete }
    procedure WaitForAll;
    
    property ThreadCount: Integer read FThreadCount;
    property LastError: string read FLastError;  // Allow reading the error
    procedure ClearLastError;
  end;

var
  { Global thread pool instance - automatically initialized }
  GlobalThreadPool: TThreadPool;

implementation

{ TWorkItem }

constructor TWorkItem.Create(AThreadPool: TObject);
begin
  inherited Create;
  FThreadPool := AThreadPool;  // Store reference to owner pool
  FObject := nil;             // Initialize object reference
  FIndex := 0;               // Initialize index
end;

procedure TWorkItem.Execute;
var
  Pool: TThreadPool;
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
    Pool := TThreadPool(FThreadPool);
    Pool.FWorkItemLock.Enter;
    try
      Dec(Pool.FWorkItemCount);
      // If this was the last work item, signal completion
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
  inherited Create(True);  // Create suspended
  FThreadPool := AThreadPool;
  FreeOnTerminate := False;  // Pool manages thread lifetime
end;

procedure TWorkerThread.Execute;
var
  Pool: TThreadPool;
  WorkItem: TWorkItem;
  List: TList;
begin
  Pool := TThreadPool(FThreadPool);
  
  // Main worker thread loop
  while not Terminated do
  begin
    WorkItem := nil;
    // Try to get work from the queue
    List := Pool.FWorkItems.LockList;
    try
      if List.Count > 0 then
      begin
        WorkItem := TWorkItem(List[0]);  // Get first item
        List.Delete(0);                  // Remove from queue
      end;
    finally
      Pool.FWorkItems.UnlockList;
    end;

    if WorkItem <> nil then
    begin
      try
        WorkItem.Execute;
      except
        on E: Exception do
        begin
          // Store the error message with thread ID
          Pool.FErrorLock.Enter;
          try
            Pool.FLastError := Format('[Thread %d] %s', 
              [ThreadID, E.Message]);
          finally
            Pool.FErrorLock.Leave;
          end;
        end;
      end;
      WorkItem.Free;
    end
    else
      Sleep(1);
  end;
end;

{ TThreadPool }

constructor TThreadPool.Create(AThreadCount: Integer = 0);
var
  I: Integer;
begin
  inherited Create;
  
  // Ensure thread count is reasonable:
  // - If <= 0: use ProcessorCount
  // - If too high: limit to 2x ProcessorCount
  // - Ensure minimum of 4 threads
  if AThreadCount <= 0 then
    AThreadCount := TThread.ProcessorCount
  else
    AThreadCount := Min(AThreadCount, TThread.ProcessorCount * 2);
    
  AThreadCount := Max(AThreadCount, 4);  // Ensure minimum of 4 threads
  
  // Initialize the thread pool
  FThreadCount := AThreadCount;
  
  // Initialize thread-safe collections
  FThreads := TThreadList.Create;
  FWorkItems := TThreadList.Create;
  FWorkItemLock := TCriticalSection.Create;
  FShutdown := False;
  FErrorLock := TCriticalSection.Create;
  FLastError := '';

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
  // Create manual reset event for synchronization
  FWorkItemEvent := TEvent.Create(nil, True, True, '');
end;

destructor TThreadPool.Destroy;
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
  FErrorLock.Free;
  
  inherited Destroy;
end;

procedure TThreadPool.ClearThreads;
var
  Thread: TWorkerThread;
  List: TList;
  I: Integer;
begin
  // Signal all threads to terminate
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

  // Wait for all threads to finish and clean up
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
  // Clean up any remaining work items
  List := FWorkItems.LockList;
  try
    for I := 0 to List.Count - 1 do
      TWorkItem(List[I]).Free;
    List.Clear;
  finally
    FWorkItems.UnlockList;
  end;
end;

{ Queue overloads for different types of work items }

procedure TThreadPool.Queue(AProcedure: TThreadProcedure);
var
  WorkItem: TWorkItem;
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
  FWorkItemEvent.WaitFor(INFINITE);  // Wait for all work items to complete
end;

procedure TThreadPool.ClearLastError;
begin
  FErrorLock.Enter;
  try
    FLastError := '';
  finally
    FErrorLock.Leave;
  end;
end;

initialization
  GlobalThreadPool := TThreadPool.Create;  // Create global instance

finalization
  GlobalThreadPool.Free;  // Clean up global instance

end.
