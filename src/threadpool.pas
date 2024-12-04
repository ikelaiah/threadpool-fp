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
  Classes, SysUtils, SyncObjs, Math, DateUtils;

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

  { Task Priorities - Different priorities for tasks }
  TTaskPriority = (
    tpLow,      // Background tasks
    tpNormal,   // Default priority
    tpHigh,     // Important tasks
    tpCritical  // Urgent tasks
  );

  { Task Statuses - Different statuses for tasks }
  TTaskStatus = (
    tsQueued,     // Task is queued but not started
    tsExecuting,  // Task is currently executing
    tsCompleted,  // Task completed successfully
    tsFailed      // Task failed with error
  );

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
    FPriority: TTaskPriority;
    FDependencies: TList;
    FStatus: TTaskStatus;
    FCompletedEvent: TEvent;
    FErrorMessage: string;
    FCancelled: Boolean;
    FCancellationEvent: TEvent;
  public
    constructor Create(AThreadPool: TObject);
    destructor Destroy; override;
    procedure Execute;
    function CanExecute: Boolean;
    
    property Priority: TTaskPriority read FPriority write FPriority;
    property Status: TTaskStatus read FStatus;
    property ErrorMessage: string read FErrorMessage;
    property CompletedEvent: TEvent read FCompletedEvent;
    property IsCancelled: Boolean read FCancelled;
    procedure Cancel;
    function ContinueWith(ANextTask: TWorkItem): TWorkItem;
  end;

  { TWorkerThread - Thread that processes work items }
  // Worker threads continuously poll for and execute work items
  // They handle exceptions and report errors back to the pool
  TWorkerThread = class(TThread)
  private
    FThreadPool: TObject;                    // Owner thread pool reference
    FLastError: string;                      // Last error message from this thread
  protected
    procedure Execute; override;             // Main thread execution loop
  public
    constructor Create(AThreadPool: TObject);
    property LastError: string read FLastError;  // Access to last error message
  end;

  { TLoadMonitorThread - Monitors and adjusts thread count }
  TLoadMonitorThread = class(TThread)
  private
    FThreadPool: TObject;  // Use TObject to avoid circular reference
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadPool: TObject);
  end;

  { TPriorityWorkQueue - Manages prioritized work items }
  TPriorityWorkQueue = class
  private
    FQueues: array[TTaskPriority] of TThreadList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(AWorkItem: TWorkItem);
    function Dequeue: TWorkItem;
    function IsEmpty: Boolean;
  end;

  { TThreadPool - Main thread pool manager }
  TThreadPool = class
  private
    FThreads: TThreadList;                   // List of worker threads
    FWorkQueue: TPriorityWorkQueue;           // Priority queue of pending work items
    FThreadCount: Integer;                   // Number of worker threads
    FShutdown: Boolean;                      // Shutdown flag
    FWorkItemLock: TCriticalSection;         // Synchronizes work item count
    FWorkItemCount: Integer;                 // Number of pending work items
    FWorkItemEvent: TEvent;                  // Signals work item completion
    FLastError: string;                      // Last error from any thread
    FErrorLock: TCriticalSection;            // Synchronizes error handling
    FErrorEvent: TEvent;                     // Signals error capture
    FMinThreads: Integer;
    FMaxThreads: Integer;
    FLoadCheckInterval: Integer;  // Milliseconds between load checks
    FLastLoadCheck: TDateTime;
    FTargetQueueLength: Integer;  // Ideal items per thread
    FLoadMonitorThread: TLoadMonitorThread;  // Dedicated thread for monitoring
    FTerminated: Boolean;
    
    procedure AdjustThreadCount;
    function CalculateOptimalThreadCount: Integer;
    procedure AddWorkerThread;
    procedure RemoveWorkerThread;
    procedure ClearThreads;
    procedure ClearWorkItems;
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
    function Queue(AProcedure: TThreadProcedure): TWorkItem; overload;
    // Queue an object method
    function Queue(AMethod: TThreadMethod): TWorkItem; overload;
    // Queue an indexed procedure
    function Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer): TWorkItem; overload;
    // Queue an indexed method
    function Queue(AMethod: TThreadMethodIndex; AIndex: Integer): TWorkItem; overload;
    // Queue a simple procedure with a priority
    function QueueWithPriority(AProcedure: TThreadProcedure; 
      APriority: TTaskPriority = tpNormal): TWorkItem;
    // Add a dependency to a work item
    procedure AddDependency(AWorkItem, ADependsOn: TWorkItem);
    
    { Waits for all queued work items to complete
      Also ensures any pending errors are captured }
    procedure WaitForAll;
    
    { Thread count and error handling }
    property ThreadCount: Integer read FThreadCount;  // Current thread count
    property LastError: string read FLastError;       // Last error message
    procedure ClearLastError;                         // Reset error state
    property MinThreads: Integer read FMinThreads write FMinThreads;
    property MaxThreads: Integer read FMaxThreads write FMaxThreads;
    property LoadCheckInterval: Integer read FLoadCheckInterval write FLoadCheckInterval;
    property Terminated: Boolean read FTerminated;
    procedure Shutdown;
  end;

  generic TTaskResult<T> = class
  private
    FValue: T;
    FIsCompleted: Boolean;
    FHasError: Boolean;
    FErrorMessage: string;
  public
    property Value: T read FValue;
    property IsCompleted: Boolean read FIsCompleted;
    property HasError: Boolean read FHasError;
    property ErrorMessage: string read FErrorMessage;
  end;

  TTaskGroup = class
  private
    FTasks: TList;
    FName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(ATask: TWorkItem);
    procedure WaitForAll;
    procedure CancelAll;
    property Name: string read FName write FName;
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
  FThreadPool := AThreadPool;
  FPriority := tpNormal;
  FDependencies := TList.Create;
  FStatus := tsQueued;
  FCompletedEvent := TEvent.Create(nil, True, False, '');
  FErrorMessage := '';
  FCancelled := False;
  FCancellationEvent := TEvent.Create(nil, True, False, '');
end;

destructor TWorkItem.Destroy;
begin
  FDependencies.Free;
  FCompletedEvent.Free;
  FCancellationEvent.Free;
  inherited;
end;

function TWorkItem.CanExecute: Boolean;
var
  I: Integer;
  Dependency: TWorkItem;
begin
  Result := True;
  // Check if all dependencies are completed
  for I := 0 to FDependencies.Count - 1 do
  begin
    Dependency := TWorkItem(FDependencies[I]);
    if Dependency.Status <> tsCompleted then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TWorkItem.Execute;
begin
  WriteLn('WorkItem.Execute: Starting execution');
  try
    FStatus := tsExecuting;
    
    // Execute the task based on type
    case FItemType of
      witProcedure: 
        begin
          WriteLn('Executing procedure');
          if Assigned(FProcedure) then FProcedure;
        end;
      witMethod:
        begin
          WriteLn('Executing method');
          if Assigned(FMethod) then FMethod;
        end;
      witProcedureIndex: if Assigned(FProcedureIndex) then FProcedureIndex(FIndex);
      witMethodIndex: if Assigned(FMethodIndex) then FMethodIndex(FIndex);
    end;
    
    FStatus := tsCompleted;
    WriteLn('WorkItem.Execute: Completed successfully');
  except
    on E: Exception do
    begin
      WriteLn('WorkItem.Execute: Failed with error: ', E.Message);
      FStatus := tsFailed;
      FErrorMessage := E.Message;
    end;
  end;
  
  FCompletedEvent.SetEvent;
  
  // Decrement work item count
  TThreadPool(FThreadPool).FWorkItemLock.Enter;
  try
    Dec(TThreadPool(FThreadPool).FWorkItemCount);
    if TThreadPool(FThreadPool).FWorkItemCount = 0 then
    begin
      WriteLn('Setting WorkItemEvent - all tasks complete');
      TThreadPool(FThreadPool).FWorkItemEvent.SetEvent;
    end;
  finally
    TThreadPool(FThreadPool).FWorkItemLock.Leave;
  end;
end;

procedure TWorkItem.Cancel;
begin
  FCancelled := True;
  FCancellationEvent.SetEvent;
end;

function TWorkItem.ContinueWith(ANextTask: TWorkItem): TWorkItem;
begin
  if Assigned(ANextTask) then
    TThreadPool(FThreadPool).AddDependency(ANextTask, Self);
  Result := ANextTask;
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
begin
  WriteLn('Worker thread ', ThreadID, ' starting');
  
  Pool := TThreadPool(FThreadPool);
  
  while not Terminated do
  begin
    WorkItem := Pool.FWorkQueue.Dequeue;
    
    if WorkItem <> nil then
    begin
      WriteLn('Thread ', ThreadID, ' executing work item');
      try
        WorkItem.Execute;
        WriteLn('Thread ', ThreadID, ' completed work item');
      except
        on E: Exception do
        begin
          WriteLn('Thread ', ThreadID, ' error: ', E.Message);
          Pool.FErrorLock.Enter;
          try
            Pool.FLastError := Format('[Thread %d] %s', [ThreadID, E.Message]);
            Pool.FErrorEvent.SetEvent;
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
  
  WriteLn('Worker thread ', ThreadID, ' terminating');
end;

{ TLoadMonitorThread }

constructor TLoadMonitorThread.Create(AThreadPool: TObject);
begin
  inherited Create(True);  // Create suspended
  FThreadPool := AThreadPool;
  FreeOnTerminate := False;
end;

procedure TLoadMonitorThread.Execute;
var
  Pool: TThreadPool;
begin
  Pool := TThreadPool(FThreadPool);
  while not Terminated do
  begin
    Pool.AdjustThreadCount;
    Sleep(Pool.LoadCheckInterval);
  end;
end;

{ TPriorityWorkQueue }

constructor TPriorityWorkQueue.Create;
var
  Priority: TTaskPriority;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  for Priority := Low(TTaskPriority) to High(TTaskPriority) do
    FQueues[Priority] := TThreadList.Create;
end;

destructor TPriorityWorkQueue.Destroy;
var
  Priority: TTaskPriority;
begin
  for Priority := Low(TTaskPriority) to High(TTaskPriority) do
    FQueues[Priority].Free;
  FLock.Free;
  inherited;
end;

procedure TPriorityWorkQueue.Enqueue(AWorkItem: TWorkItem);
begin
  FQueues[AWorkItem.Priority].Add(AWorkItem);
end;

function TPriorityWorkQueue.Dequeue: TWorkItem;
var
  Priority: TTaskPriority;
  List: TList;
begin
  Result := nil;
  FLock.Enter;
  try
    // Check queues from highest to lowest priority
    for Priority := High(TTaskPriority) downto Low(TTaskPriority) do
    begin
      List := FQueues[Priority].LockList;
      try
        if List.Count > 0 then
        begin
          Result := TWorkItem(List[0]);
          // Only remove if all dependencies are satisfied
          if Result.CanExecute then
            List.Delete(0)
          else
            Result := nil;
        end;
      finally
        FQueues[Priority].UnlockList;
      end;
      if Result <> nil then
        Break;
    end;
  finally
    FLock.Leave;
  end;
end;

function TPriorityWorkQueue.IsEmpty: Boolean;
var
  Priority: TTaskPriority;
  List: TList;
begin
  Result := True;
  for Priority := Low(TTaskPriority) to High(TTaskPriority) do
  begin
    List := FQueues[Priority].LockList;
    try
      if List.Count > 0 then
      begin
        Result := False;
        Exit;
      end;
    finally
      FQueues[Priority].UnlockList;
    end;
  end;
end;

{ TThreadPool }

constructor TThreadPool.Create(AThreadCount: Integer = 0);
var
  I: Integer;
begin
  inherited Create;
  
  // Initialize load management fields
  FMinThreads := Max(4, TThread.ProcessorCount);
  FMaxThreads := TThread.ProcessorCount * 2;
  FLoadCheckInterval := 1000; // Check every second
  FLastLoadCheck := Now;
  FTargetQueueLength := 4;    // Aim for 4 tasks per thread
  
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
  FWorkQueue := TPriorityWorkQueue.Create;
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

  // Create and start load monitor thread
  FLoadMonitorThread := TLoadMonitorThread.Create(Self);
  FLoadMonitorThread.Start;
end;

destructor TThreadPool.Destroy;
begin
  WaitForAll;  // Ensure all tasks complete before destroying
  FShutdown := True;
  
  // Terminate and free load monitor thread
  if Assigned(FLoadMonitorThread) then
  begin
    FLoadMonitorThread.Terminate;
    FLoadMonitorThread.WaitFor;
    FLoadMonitorThread.Free;
  end;
  
  // Clear work items and threads
  if Assigned(FWorkQueue) then
    ClearWorkItems;
  if Assigned(FThreads) then
    ClearThreads;
  
  // Free synchronization objects
  FWorkItemLock.Free;
  FThreads.Free;
  FWorkQueue.Free;
  FWorkItemEvent.Free;
  FErrorEvent.Free;
  FErrorLock.Free;
  
  inherited;
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
  Priority: TTaskPriority;
  List: TList;
  I: Integer;
begin
  // Clean up items in all priority queues
  for Priority := Low(TTaskPriority) to High(TTaskPriority) do
  begin
    List := FWorkQueue.FQueues[Priority].LockList;
    try
      for I := 0 to List.Count - 1 do
        TWorkItem(List[I]).Free;
      List.Clear;
    finally
      FWorkQueue.FQueues[Priority].UnlockList;
    end;
  end;
end;

{ Queue overloads for different types of work items }

function TThreadPool.Queue(AProcedure: TThreadProcedure): TWorkItem;
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
  
  FWorkQueue.Enqueue(WorkItem);
  Result := WorkItem;
end;

function TThreadPool.Queue(AMethod: TThreadMethod): TWorkItem;
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit(nil);
  
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
  
  FWorkQueue.Enqueue(WorkItem);
  Result := WorkItem;
end;

function TThreadPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer): TWorkItem;
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit(nil);
  
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
  
  FWorkQueue.Enqueue(WorkItem);
  Result := WorkItem;
end;

function TThreadPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer): TWorkItem;
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit(nil);
  
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
  
  FWorkQueue.Enqueue(WorkItem);
  Result := WorkItem;
end;

function TThreadPool.QueueWithPriority(AProcedure: TThreadProcedure; 
  APriority: TTaskPriority): TWorkItem;
var
  WorkItem: TWorkItem;
begin
  if FShutdown then Exit(nil);
  
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
  WorkItem.FPriority := APriority;
  
  FWorkQueue.Enqueue(WorkItem);
  Result := WorkItem;
end;

procedure TThreadPool.WaitForAll;
const
  TIMEOUT_MS = 30000; // 30 seconds timeout
begin
  WriteLn('ThreadPool.WaitForAll: Waiting for tasks to complete...');
  WriteLn('  Work item count: ', FWorkItemCount);
  WriteLn('  Thread count: ', FThreadCount);
  WriteLn('  Queue empty: ', FWorkQueue.IsEmpty);
  WriteLn('  WorkItemEvent state: ', FWorkItemEvent.WaitFor(0) = wrSignaled);
  
  if FWorkItemCount = 0 then
  begin
    WriteLn('No work items to wait for');
    Exit;
  end;
  
  if FWorkItemEvent.WaitFor(TIMEOUT_MS) <> wrSignaled then
  begin
    WriteLn('ThreadPool.WaitForAll: Timeout after ', TIMEOUT_MS, 'ms');
    WriteLn('  Remaining work items: ', FWorkItemCount);
    WriteLn('  Queue empty: ', FWorkQueue.IsEmpty);
    raise Exception.Create('Thread pool wait timeout after ' + IntToStr(TIMEOUT_MS) + 'ms');
  end;
  
  WriteLn('ThreadPool.WaitForAll: Tasks completed');
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

procedure TThreadPool.AddDependency(AWorkItem, ADependsOn: TWorkItem);
begin
  if not Assigned(AWorkItem) or not Assigned(ADependsOn) then
    Exit;
  AWorkItem.FDependencies.Add(ADependsOn);
end;

procedure TThreadPool.AdjustThreadCount;
var
  OptimalCount, CurrentCount: Integer;
  ThreadList: TList;
begin
  if MilliSecondsBetween(Now, FLastLoadCheck) < FLoadCheckInterval then
    Exit;
    
  FLastLoadCheck := Now;
  OptimalCount := CalculateOptimalThreadCount;
  
  ThreadList := FThreads.LockList;
  try
    CurrentCount := ThreadList.Count;
    
    // Add or remove threads to reach optimal count
    while CurrentCount < OptimalCount do
    begin
      AddWorkerThread;
      Inc(CurrentCount);
    end;
    
    while (CurrentCount > OptimalCount) and (CurrentCount > FMinThreads) do
    begin
      RemoveWorkerThread;
      Dec(CurrentCount);
    end;
    
  finally
    FThreads.UnlockList;
  end;
end;

function TThreadPool.CalculateOptimalThreadCount: Integer;
var
  QueueLength: Integer;
  ProcessorCount: Integer;
  UtilizationRate: Double;
begin
  // Get current queue length
  QueueLength := FWorkItemCount;
  
  // Calculate utilization rate (tasks per thread)
  UtilizationRate := QueueLength / FThreadCount;
  
  // Get processor count
  ProcessorCount := TThread.ProcessorCount;
  
  // Calculate optimal thread count based on:
  // - Current queue length
  // - Target tasks per thread
  // - Available processors
  // - Current utilization
  if UtilizationRate > FTargetQueueLength then
    // Need more threads
    Result := Min(
      FMaxThreads, 
      Max(
        FMinThreads,
        Min(
          ProcessorCount * 2,
          QueueLength div FTargetQueueLength
        )
      )
    )
  else if UtilizationRate < (FTargetQueueLength div 2) then
    // Can reduce threads
    Result := Max(
      FMinThreads,
      QueueLength div FTargetQueueLength
    )
  else
    // Current count is good
    Result := FThreadCount;
end;

procedure TThreadPool.AddWorkerThread;
var
  Thread: TWorkerThread;
begin
  Thread := TWorkerThread.Create(Self);
  FThreads.Add(Thread);
  Thread.Start;
  Inc(FThreadCount);
end;

procedure TThreadPool.RemoveWorkerThread;
var
  Thread: TWorkerThread;
  List: TList;
begin
  List := FThreads.LockList;
  try
    if List.Count > FMinThreads then
    begin
      Thread := TWorkerThread(List[List.Count - 1]);
      Thread.Terminate;
      List.Delete(List.Count - 1);
      Dec(FThreadCount);
      // Thread will free itself since FreeOnTerminate is True
    end;
  finally
    FThreads.UnlockList;
  end;
end;

{ TTaskGroup }

constructor TTaskGroup.Create;
begin
  inherited Create;
  FTasks := TList.Create;
end;

destructor TTaskGroup.Destroy;
begin
  if Assigned(FTasks) then
  begin
    CancelAll;  // Cancel any remaining tasks
    FTasks.Free;
  end;
  inherited;
end;

procedure TTaskGroup.AddTask(ATask: TWorkItem);
begin
  if not Assigned(FTasks) then
    FTasks := TList.Create;
  FTasks.Add(ATask);
end;

procedure TTaskGroup.WaitForAll;
var
  I: Integer;
  Task: TWorkItem;
begin
  if Assigned(FTasks) then
    for I := 0 to FTasks.Count - 1 do
    begin
      Task := TWorkItem(FTasks[I]);
      Task.CompletedEvent.WaitFor(INFINITE);
    end;
end;

procedure TTaskGroup.CancelAll;
var
  I: Integer;
  Task: TWorkItem;
begin
  if Assigned(FTasks) then
    for I := 0 to FTasks.Count - 1 do
    begin
      Task := TWorkItem(FTasks[I]);
      Task.Cancel;
    end;
end;

procedure TThreadPool.Shutdown;
begin
  FTerminated := True;
  WaitForAll;
end;

initialization
  GlobalThreadPool := TThreadPool.Create;  // Create global instance

finalization
  GlobalThreadPool.Free;  // Clean up global instance

end.
