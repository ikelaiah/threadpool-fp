unit ThreadPool.ProducerConsumer;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, ThreadPool.Types, SyncObjs;

const
  DEBUG_LOG = True;  // Set to False to disable logging

procedure DebugLog(const Msg: string);

type

  EQueueFullException = class(Exception);

  TBackpressureConfig = record
    LowLoadThreshold: Double;    // e.g., 0.5 for 50%
    MediumLoadThreshold: Double; // e.g., 0.7 for 70%
    HighLoadThreshold: Double;   // e.g., 0.9 for 90%
    LowLoadDelay: Integer;       // milliseconds
    MediumLoadDelay: Integer;    // milliseconds
    HighLoadDelay: Integer;      // milliseconds
    MaxAttempts: Integer;        // maximum queue attempts
  end;


  { Worker thread implementation for producer-consumer pattern }
  TProducerConsumerWorkerThread = class(TThread)
  private
    FThreadPool: TObject;
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadPool: TObject);
  end;

  { Thread-safe circular queue for work items }
  TThreadSafeQueue = class(TObject)
  private
    FItems: array of IWorkItem;
    FHead: integer;
    FTail: integer;
    FCount: integer;
    FCapacity: integer;
    FLock: TCriticalSection;
    FLastEnqueueTime: TDateTime;
    FBackpressureConfig: TBackpressureConfig;
  protected
    function GetLoadFactor: Double;
    procedure ApplyBackpressure;
  public
    constructor Create(ACapacity: integer);
    destructor Destroy; override;
    function TryEnqueue(AItem: IWorkItem): boolean;
    function TryDequeue(out AItem: IWorkItem): boolean;
    function GetCount: integer;
    procedure Clear;
    property LoadFactor: Double read GetLoadFactor;
    property BackpressureConfig: TBackpressureConfig read FBackpressureConfig write FBackpressureConfig;
  end;

  { Work item implementation for producer-consumer pattern }
  TProducerConsumerWorkItem = class(TInterfacedObject, IWorkItem)
  private
    FProcedure: TThreadProcedure;
    FMethod: TThreadMethod;
    FProcedureIndex: TThreadProcedureIndex;
    FMethodIndex: TThreadMethodIndex;
    FIndex: integer;
    FItemType: TWorkItemType;
    FThreadPool: TObject;
  public
    constructor Create(AThreadPool: TObject);
    { IWorkItem implementation }
    procedure Execute;
    function GetItemType: integer;
  end;

  { Producer-consumer thread pool implementation }
  TProducerConsumerThreadPool = class(TThreadPoolBase)
  private
    FThreads: TThreadList;
    FWorkQueue: TThreadSafeQueue;  // Use our custom thread-safe queue
    FCompletionEvent: TEvent;
    FErrorLock: TCriticalSection;
    FWorkItemCount: integer;
    FWorkItemLock: TCriticalSection;
    FLocalThreadCount: integer;

    procedure ClearThreads;
    function GetThreadCount: integer; override;
    function GetLastError: string; override;
    function TryQueueWorkItem(WorkItem: IWorkItem): Boolean;
  public
    constructor Create(AThreadCount: Integer = 0; AQueueSize: Integer = 1024);
    destructor Destroy; override;

    { IThreadPool implementation }
    procedure Queue(AProcedure: TThreadProcedure); override;
    procedure Queue(AMethod: TThreadMethod); override;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: integer); override;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: integer); override;
    procedure WaitForAll; override;
    property WorkQueue: TThreadSafeQueue read FWorkQueue;  // Added this line
    property ThreadCount: integer read GetThreadCount;
    property LastError: string read GetLastError;
  end;

implementation

procedure DebugLog(const Msg: string);
begin
  if DEBUG_LOG then
    WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', GetThreadID, ': ', Msg);
end;

constructor TProducerConsumerThreadPool.Create(AThreadCount: Integer = 0; AQueueSize: Integer = 1024);
var
  I: integer;
  Thread: TProducerConsumerWorkerThread;
begin
  DebugLog('Creating thread pool with ' + IntToStr(AThreadCount) + ' threads');
  inherited Create(AThreadCount);

  if AThreadCount <= 0 then
    FLocalThreadCount := CPUCount
  // Default to CPUCount if AThreadCount is 0 or negative
  else
    FLocalThreadCount := AThreadCount;

  DebugLog('Actual thread count: ' + IntToStr(FLocalThreadCount));

  FThreads := TThreadList.Create;
  FWorkQueue := TThreadSafeQueue.Create(AQueueSize);
  FCompletionEvent := TEvent.Create(nil, True, True, '');
  FErrorLock := TCriticalSection.Create;
  FWorkItemLock := TCriticalSection.Create;
  FWorkItemCount := 0;
  FLastError := '';

  // Create worker threads
  for I := 1 to FLocalThreadCount do
  begin
    DebugLog('Creating worker thread ' + IntToStr(I));
    Thread := TProducerConsumerWorkerThread.Create(Self);
    FThreads.Add(Thread);
    Thread.Start;
  end;
  DebugLog('Thread pool created');
end;

destructor TProducerConsumerThreadPool.Destroy;
begin
  ClearThreads;
  FWorkQueue.Free;
  FCompletionEvent.Free;
  FErrorLock.Free;
  FWorkItemLock.Free;
  FThreads.Free;
  inherited;
end;

{
  Note:
  The retry logic is in one place only: TThreadSafeQueue.TryEnqueue

  Benefits of using IWorkItem:
  - Dependency Inversion: We depend on abstractions (interfaces) rather than concrete implementations
  - Loose Coupling: The method doesn't need to know about the specific work item implementation
  - Flexibility: We can add new work item types without modifying this method
  - Testability: Easier to mock work items in unit tests
  - Interface Segregation: We only need the methods defined in IWorkItem
}
function TProducerConsumerThreadPool.TryQueueWorkItem(WorkItem: IWorkItem): Boolean;
begin
  Result := False;
  
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    FCompletionEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;

  try
    Result := FWorkQueue.TryEnqueue(WorkItem);
    if Result then
    begin
      DebugLog(Format('Work item queued (Load: %.1f%%)', 
        [FWorkQueue.LoadFactor * 100]));
      Exit;
    end;
    
    // If we couldn't enqueue, raise the exception here
    raise EQueueFullException.Create('Queue is full');
  except
    FWorkItemLock.Enter;
    try
      Dec(FWorkItemCount);
      if FWorkItemCount = 0 then
        FCompletionEvent.SetEvent;
    finally
      FWorkItemLock.Leave;
    end;
    raise;  // Re-raise the exception
  end;
end;


procedure TProducerConsumerThreadPool.Queue(AProcedure: TThreadProcedure);
var
  WorkItem: TProducerConsumerWorkItem;
begin
  WorkItem := TProducerConsumerWorkItem.Create(Self);
  try
    WorkItem.FProcedure := AProcedure;
    WorkItem.FItemType := witProcedure;
    TryQueueWorkItem(WorkItem);  // Exception will be raised by TryQueueWorkItem if needed
  except
    WorkItem.Free;
    raise;
  end;
end;

procedure TProducerConsumerThreadPool.Queue(AMethod: TThreadMethod);
var
  WorkItem: TProducerConsumerWorkItem;
begin
  WorkItem := TProducerConsumerWorkItem.Create(Self);
  try
    WorkItem.FMethod := AMethod;
    WorkItem.FItemType := witMethod;
    TryQueueWorkItem(WorkItem);
  except
    WorkItem.Free;
    raise;
  end;
end;

procedure TProducerConsumerThreadPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
var
  WorkItem: TProducerConsumerWorkItem;
begin
  WorkItem := TProducerConsumerWorkItem.Create(Self);
  try
    WorkItem.FProcedureIndex := AProcedure;
    WorkItem.FIndex := AIndex;
    WorkItem.FItemType := witProcedureIndex;
    TryQueueWorkItem(WorkItem);
  except
    WorkItem.Free;
    raise;
  end;
end;

procedure TProducerConsumerThreadPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
var
  WorkItem: TProducerConsumerWorkItem;
begin
  WorkItem := TProducerConsumerWorkItem.Create(Self);
  try
    WorkItem.FMethodIndex := AMethod;
    WorkItem.FIndex := AIndex;
    WorkItem.FItemType := witMethodIndex;
    TryQueueWorkItem(WorkItem);
  except
    WorkItem.Free;
    raise;
  end;
end;

procedure TProducerConsumerThreadPool.WaitForAll;
begin
  DebugLog('Waiting for all work items to complete');
  FCompletionEvent.WaitFor(INFINITE);
  DebugLog('All work items completed');
end;

procedure TProducerConsumerThreadPool.ClearThreads;
var
  Thread: TThread;
  List: TList;
  I: integer;
begin
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TThread(List[I]);
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
      Thread := TThread(List[I]);
      Thread.WaitFor;
      Thread.Free;
    end;
    List.Clear;
  finally
    FThreads.UnlockList;
  end;
end;

function TProducerConsumerThreadPool.GetThreadCount: integer;
begin
  Result := FLocalThreadCount;
end;

function TProducerConsumerThreadPool.GetLastError: string;
begin
  Result := inherited GetLastError;
end;

{ TProducerConsumerWorkItem }

constructor TProducerConsumerWorkItem.Create(AThreadPool: TObject);
begin
  inherited Create;
  FThreadPool := AThreadPool;
  FItemType := witProcedure;
  FIndex := 0;
end;

procedure TProducerConsumerWorkItem.Execute;
begin
  case FItemType of
    witProcedure: if Assigned(FProcedure) then FProcedure;
    witMethod: if Assigned(FMethod) then FMethod;
    witProcedureIndex: if Assigned(FProcedureIndex) then FProcedureIndex(FIndex);
    witMethodIndex: if Assigned(FMethodIndex) then FMethodIndex(FIndex);
  end;
end;

function TProducerConsumerWorkItem.GetItemType: integer;
begin
  Result := Ord(FItemType);
end;

{ TThreadSafeQueue }

constructor TThreadSafeQueue.Create(ACapacity: integer);
begin
  inherited Create;
  FCapacity := ACapacity;
  SetLength(FItems, FCapacity);
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FLock := TCriticalSection.Create;

  // Initialize default backpressure configuration
  FBackpressureConfig.LowLoadThreshold := 0.5;     // 50%
  FBackpressureConfig.MediumLoadThreshold := 0.7;  // 70%
  FBackpressureConfig.HighLoadThreshold := 0.9;    // 90%
  FBackpressureConfig.LowLoadDelay := 10;          // 10ms
  FBackpressureConfig.MediumLoadDelay := 50;       // 50ms
  FBackpressureConfig.HighLoadDelay := 100;        // 100ms
  FBackpressureConfig.MaxAttempts := 5;            // 5 attempts

end;

procedure TThreadSafeQueue.ApplyBackpressure;
var
  CurrentLoad: Double;
  WaitTime: Integer;
begin
  CurrentLoad := GetLoadFactor;
  
  // Determine wait time based on load thresholds
  if CurrentLoad >= FBackpressureConfig.HighLoadThreshold then
    WaitTime := FBackpressureConfig.HighLoadDelay
  else if CurrentLoad >= FBackpressureConfig.MediumLoadThreshold then
    WaitTime := FBackpressureConfig.MediumLoadDelay
  else if CurrentLoad >= FBackpressureConfig.LowLoadThreshold then
    WaitTime := FBackpressureConfig.LowLoadDelay
  else
    WaitTime := 0;
    
  if WaitTime > 0 then
  begin
    DebugLog(Format('Queue load at %.1f%%, applying backpressure: %dms', 
      [CurrentLoad * 100, WaitTime]));
    Sleep(WaitTime);
  end;
end;

destructor TThreadSafeQueue.Destroy;
begin
  Clear;
  FLock.Free;
  inherited;
end;



function TThreadSafeQueue.TryDequeue(out AItem: IWorkItem): boolean;
begin
  Result := False;
  FLock.Enter;
  try
    if FCount > 0 then
    begin
      AItem := FItems[FHead];
      FItems[FHead] := nil;
      FHead := (FHead + 1) mod FCapacity;
      Dec(FCount);
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.GetCount: integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeQueue.Clear;
var
  I: integer;
begin
  FLock.Enter;
  try
    for I := 0 to Length(FItems) - 1 do
      FItems[I] := nil;
    FHead := 0;
    FTail := 0;
    FCount := 0;
  finally
    FLock.Leave;
  end;
end;

{ TProducerConsumerWorkerThread }

constructor TProducerConsumerWorkerThread.Create(AThreadPool: TObject);
begin
  inherited Create(True);  // Create suspended
  FThreadPool := AThreadPool;
  FreeOnTerminate := False;
end;

procedure TProducerConsumerWorkerThread.Execute;
var
  Pool: TProducerConsumerThreadPool;
  WorkItem: IWorkItem;
begin
  DebugLog('Worker thread started');
  Pool := TProducerConsumerThreadPool(FThreadPool);

  while not Terminated do
  begin
    try
      if Pool.FWorkQueue.TryDequeue(WorkItem) then
      begin
        DebugLog('Got work item');
        try
          WorkItem.Execute;
          DebugLog('Work item executed');
        except
          on E: Exception do
          begin
            DebugLog('Error executing work item: ' + E.Message);
            Pool.FErrorLock.Enter;
            try
              Pool.SetLastError(E.Message);
            finally
              Pool.FErrorLock.Leave;
            end;
          end;
        end;

        Pool.FWorkItemLock.Enter;
        try
          Dec(Pool.FWorkItemCount);
          DebugLog('Work items remaining: ' + IntToStr(Pool.FWorkItemCount));
          if Pool.FWorkItemCount = 0 then
          begin
            Pool.FCompletionEvent.SetEvent;
            DebugLog('All work items completed');
          end;
        finally
          Pool.FWorkItemLock.Leave;
        end;
      end
      else
        Sleep(100);  // Wait a bit before trying again
    except
      on E: Exception do
        DebugLog('Queue error: ' + E.Message);
    end;
  end;
  DebugLog('Worker thread terminating');
end;

function TThreadSafeQueue.GetLoadFactor: Double;
begin
  FLock.Enter;
  try
    Result := FCount / FCapacity;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.TryEnqueue(AItem: IWorkItem): boolean;
var
  Attempts: Integer;
begin
  DebugLog('TThreadSafeQueue.TryEnqueue: Starting');
  if AItem = nil then
  begin
    DebugLog('TThreadSafeQueue.TryEnqueue: Nil item received');
    Exit(False);
  end;
    
  Attempts := 0;
  Result := False;
  
  repeat
    ApplyBackpressure;
    
    FLock.Enter;
    try
      if FCount < FCapacity then
      begin
        DebugLog('TThreadSafeQueue.TryEnqueue: Adding item to queue');
        FItems[FTail] := AItem;
        FTail := (FTail + 1) mod FCapacity;
        Inc(FCount);
        FLastEnqueueTime := Now;
        Result := True;
        Exit;
      end;
      DebugLog(Format('TThreadSafeQueue.TryEnqueue: Queue full (Count: %d, Capacity: %d)', [FCount, FCapacity]));
    finally
      FLock.Leave;
    end;
    
    Inc(Attempts);
    if Attempts >= FBackpressureConfig.MaxAttempts then
    begin
      DebugLog('TThreadSafeQueue.TryEnqueue: Max attempts reached');
      raise EQueueFullException.Create('Queue is full after maximum attempts');
    end;
      
    Sleep(10);
  until Result;
end;

end.
