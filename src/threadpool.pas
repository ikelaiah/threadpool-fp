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
  { Task Types - Different kinds of work that can be queued }
  
  // Simple procedure with no parameters
  TThreadProcedure = procedure;
  
  // Method of an object with no parameters
  TThreadMethod = procedure of object;
  
  // Procedure that receives an index parameter
  // Useful for parallel processing of arrays or collections
  TThreadProcedureIndex = procedure(index: Integer);
  
  // Method that receives an index parameter
  // Useful for object-oriented parallel processing
  TThreadMethodIndex = procedure(index: Integer) of object;

  { TWorkItem - Internal wrapper for queued tasks }
  // This class encapsulates different types of work items
  // and manages their execution in worker threads
  TWorkItem = class
  private
    FProcedure: TThreadProcedure;            // Simple procedure reference
    FMethod: TThreadMethod;                  // Object method reference
    FProcedureIndex: TThreadProcedureIndex;  // Indexed procedure reference
    FMethodIndex: TThreadMethodIndex;        // Indexed method reference
    FIndex: Integer;                         // Index for indexed operations
    FObject: TObject;                        // Associated object reference
    FItemType: (witProcedure,                // Type discriminator for work items
                witMethod,
                witProcedureIndex,
                witMethodIndex);
    FThreadPool: TObject;                    // Owner thread pool reference
  public
    constructor Create(AThreadPool: TObject);
    procedure Execute;                       // Executes the work item
  end;

  { TWorkerThread - Thread that processes work items }
  // Worker threads continuously poll for and execute work items
  // They handle exceptions and report errors back to the pool
  TWorkerThread = class(TThread)
  private
    FThreadPool: TObject;                    // Owner thread pool reference
    FLastError: string;                      // Last error message from this thread
    FLocalQueue: TThreadSafeQueue;          // Add this line
  protected
    procedure Execute; override;             // Main thread execution loop
  public
    constructor Create(AThreadPool: TObject);
    destructor Destroy; override;
    property LastError: string read FLastError;  // Access to last error message
    property LocalQueue: TThreadSafeQueue read FLocalQueue;  // Add this line
  end;

  { TThreadPool - Main thread pool manager }
  // Manages a collection of worker threads and work items
  // Provides thread-safe queueing and synchronization
  TThreadPool = class
  private
    FThreads: TThreadList;                   // List of worker threads
    FWorkItems: TThreadList;                 // Queue of pending work items
    FThreadCount: Integer;                   // Number of worker threads
    FShutdown: Boolean;                      // Shutdown flag
    FWorkItemLock: TCriticalSection;         // Synchronizes work item count
    FWorkItemCount: Integer;                 // Number of pending work items
    FWorkItemEvent: TEvent;                  // Signals work item completion
    FLastError: string;                      // Last error from any thread
    FErrorLock: TCriticalSection;            // Synchronizes error handling
    FErrorEvent: TEvent;                     // Signals error capture
    procedure ClearThreads;                  // Cleanup worker threads
    procedure ClearWorkItems;                // Cleanup pending work items
    procedure QueueToLeastBusyThread(WorkItem: TWorkItem);
    function CreateWorkItem: TWorkItem;
    procedure PrepareForNewWorkItem;
  public
    { Creates a thread pool with specified number of threads
      @param AThreadCount Number of threads to create (0 = CPU count)
      Thread count is automatically adjusted to safe limits:
      - Minimum: 4 threads
      - Maximum: 2 × ProcessorCount
      - Default: ProcessorCount when AThreadCount ≤ 0 }
    constructor Create(AThreadCount: Integer = 0);
    destructor Destroy; override;
    
    { Queue different types of work items }
    // Queue a simple procedure
    procedure Queue(AProcedure: TThreadProcedure); overload;
    // Queue an object method
    procedure Queue(AMethod: TThreadMethod); overload;
    // Queue an indexed procedure
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); overload;
    // Queue an indexed method
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); overload;
    
    { Waits for all queued work items to complete
      Also ensures any pending errors are captured }
    procedure WaitForAll;
    
    { Thread count and error handling }
    property ThreadCount: Integer read FThreadCount;  // Current thread count
    property LastError: string read FLastError;       // Last error message
    procedure ClearLastError;                         // Reset error state
  end;

  { TThreadSafeQueue - Thread-safe work queue }
  // This class provides thread-safe operations for managing work items
  // It supports both LIFO operations for local queue (Pop from end)
  // and FIFO operations for work stealing (Steal from front)
  TThreadSafeQueue = class
  private
    FItems: TList;                // Internal list storing work items
    FLock: TCriticalSection;      // Synchronization object for thread safety
  public
    constructor Create;
    destructor Destroy; override;
    // Adds a work item to the end of the queue (thread-safe)
    procedure Push(AItem: TWorkItem);
    // Removes and returns an item from the end of the queue (LIFO, thread-safe)
    function Pop: TWorkItem;
    // Removes and returns an item from the front of the queue (FIFO, thread-safe)
    // Used by other threads when stealing work
    function Steal: TWorkItem;
    // Returns the current number of items in the queue (thread-safe)
    function Count: Integer;
  end;

var
  { Global thread pool instance
    Automatically created at unit initialization
    Freed at unit finalization }
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
  FLocalQueue := TThreadSafeQueue.Create;  // Initialize local queue
end;

procedure TWorkerThread.Execute;
var
  Pool: TThreadPool;
  WorkItem: TWorkItem;
  OtherThread: TWorkerThread;
  ThreadList: TList;
  I: Integer;
  FoundWork: Boolean;
begin
  Pool := TThreadPool(FThreadPool);
  
  // Main worker thread loop - continues until thread is terminated
  while not Terminated do
  begin
    WorkItem := nil;
    FoundWork := False;
    
    // Work acquisition priority:
    // 1. Try local queue first (fastest, no contention)
    WorkItem := FLocalQueue.Pop;
    if WorkItem = nil then
    begin
      // 2. Try stealing from other threads' queues
      ThreadList := Pool.FThreads.LockList;
      try
        for I := 0 to ThreadList.Count - 1 do
        begin
          OtherThread := TWorkerThread(ThreadList[I]);
          // Only steal from other threads that have work
          if (OtherThread <> Self) and (OtherThread.LocalQueue.Count > 0) then
          begin
            WorkItem := OtherThread.LocalQueue.Steal;
            if WorkItem <> nil then
            begin
              FoundWork := True;
              Break;
            end;
          end;
        end;
      finally
        Pool.FThreads.UnlockList;
      end;
      
      // 3. Finally, try the global queue if no work was stolen
      if not FoundWork then
      begin
        ThreadList := Pool.FWorkItems.LockList;
        try
          if ThreadList.Count > 0 then
          begin
            WorkItem := TWorkItem(ThreadList[0]);
            ThreadList.Delete(0);
          end;
        finally
          Pool.FWorkItems.UnlockList;
        end;
      end;
    end;

    if WorkItem <> nil then
    begin
      try
        WorkItem.Execute;
      except
        on E: Exception do
        begin
          Pool.FErrorLock.Enter;
          try
            Pool.FLastError := Format('[Thread %d] %s', 
              [ThreadID, E.Message]);
            Pool.FErrorEvent.SetEvent;
          finally
            Pool.FErrorLock.Leave;
          end;
        end;
      end;
      WorkItem.Free;
    end
    else
      Sleep(1);  // No work found, sleep briefly
  end;
end;

destructor TWorkerThread.Destroy;
begin
  FLocalQueue.Free;  // Clean up local queue
  inherited;
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
  FErrorEvent := TEvent.Create(nil, True, False, '');  // Manual reset event

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
  FErrorEvent.Free;
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
  Thread: TWorkerThread;
begin
  // Clean up any remaining work items in global queue
  List := FWorkItems.LockList;
  try
    for I := 0 to List.Count - 1 do
      TWorkItem(List[I]).Free;
    List.Clear;
  finally
    FWorkItems.UnlockList;
  end;

  // Clean up items in local queues
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TWorkerThread(List[I]);
      while Thread.LocalQueue.Count > 0 do
        Thread.LocalQueue.Pop.Free;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

{ Queue overloads for different types of work items }

procedure TThreadPool.Queue(AProcedure: TThreadProcedure);
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit;
  
  WorkItem := CreateWorkItem;
  WorkItem.FProcedure := AProcedure;
  WorkItem.FItemType := witProcedure;
  QueueToLeastBusyThread(WorkItem);
end;

procedure TThreadPool.Queue(AMethod: TThreadMethod);
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit;
  
  WorkItem := CreateWorkItem;
  WorkItem.FMethod := AMethod;
  WorkItem.FItemType := witMethod;
  QueueToLeastBusyThread(WorkItem);
end;

procedure TThreadPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit;
  
  WorkItem := CreateWorkItem;
  WorkItem.FProcedureIndex := AProcedure;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witProcedureIndex;
  QueueToLeastBusyThread(WorkItem);
end;

procedure TThreadPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit;
  
  WorkItem := CreateWorkItem;
  WorkItem.FMethodIndex := AMethod;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witMethodIndex;
  QueueToLeastBusyThread(WorkItem);
end;

procedure TThreadPool.WaitForAll;
begin
  FWorkItemEvent.WaitFor(INFINITE);  // Wait for all work items to complete
  // If there was an error, ensure it's fully captured
  if FErrorEvent.WaitFor(100) = wrSignaled then
    FErrorEvent.ResetEvent;
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

{ TThreadSafeQueue }

constructor TThreadSafeQueue.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeQueue.Destroy;
begin
  FLock.Free;
  FItems.Free;
  inherited;
end;

procedure TThreadSafeQueue.Push(AItem: TWorkItem);
begin
  FLock.Enter;
  try
    FItems.Add(AItem);
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.Pop: TWorkItem;
begin
  Result := nil;
  FLock.Enter;
  try
    if FItems.Count > 0 then
    begin
      Result := TWorkItem(FItems[FItems.Count - 1]);
      FItems.Delete(FItems.Count - 1);
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.Steal: TWorkItem;
begin
  Result := nil;
  FLock.Enter;
  try
    if FItems.Count > 0 then
    begin
      Result := TWorkItem(FItems[0]);  // Steal from the front
      FItems.Delete(0);
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

{ Load balancing implementation }
procedure TThreadPool.QueueToLeastBusyThread(WorkItem: TWorkItem);
var
  ThreadList: TList;
  LeastBusyThread: TWorkerThread;
  I: Integer;
  MinCount: Integer;
begin
  // Find the worker thread with the shortest queue
  ThreadList := FThreads.LockList;
  try
    // Start with the first thread as our baseline
    LeastBusyThread := TWorkerThread(ThreadList[0]);
    MinCount := LeastBusyThread.LocalQueue.Count;
    
    // Compare with all other threads
    for I := 1 to ThreadList.Count - 1 do
    begin
      // If we find a thread with fewer pending items, select it
      if TWorkerThread(ThreadList[I]).LocalQueue.Count < MinCount then
      begin
        LeastBusyThread := TWorkerThread(ThreadList[I]);
        MinCount := LeastBusyThread.LocalQueue.Count;
      end;
    end;
    
    // Assign the work item to the thread with the shortest queue
    // This helps maintain balanced load across all threads
    LeastBusyThread.LocalQueue.Push(WorkItem);
  finally
    FThreads.UnlockList;
  end;
end;

{ Work item management helpers }
procedure TThreadPool.PrepareForNewWorkItem;
begin
  // Don't accept new work if shutting down
  if FShutdown then Exit;
  
  // Thread-safe increment of work item count
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    // Reset completion event since we have pending work
    if FWorkItemCount > 0 then
      FWorkItemEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;
end;

function TThreadPool.CreateWorkItem: TWorkItem;
begin
  // Update internal counters and create new work item
  PrepareForNewWorkItem;
  Result := TWorkItem.Create(Self);
end;

initialization
  GlobalThreadPool := TThreadPool.Create;  // Create global instance

finalization
  GlobalThreadPool.Free;  // Clean up global instance

end.
