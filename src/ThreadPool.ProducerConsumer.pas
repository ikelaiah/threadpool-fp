unit ThreadPool.ProducerConsumer;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Math, ThreadPool.Types, ThreadPool.Tasks, SyncObjs,
  ThreadPool.Internal.WorkItems;

var
  DEBUG_LOG: Boolean = False;  // Opt-in only; disabled by default

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


  {$REGION 'Internal: Worker Thread'}
  { Worker thread implementation for producer-consumer pattern }
  TProducerConsumerWorkerThread = class(TThread)
  private
    FThreadPool: TObject;
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadPool: TObject);
  end;

  {$ENDREGION}

  {$REGION 'Internal: Thread-Safe Queue'}
  { Thread-safe circular queue for work items }
  TThreadSafeQueue = class(TObject)
  private
    FItems: array of IWorkItem;
    FHead: integer;
    FTail: integer;
    FCount: integer;
    FCapacity: integer;
    FLock: TCriticalSection;
    FNotEmptyEvent: TEvent;
    FNotFullEvent: TEvent;
    FBackpressureConfig: TBackpressureConfig;
  protected
    function GetLoadFactor: Double;
    procedure ApplyBackpressure;
    function GetDefaultTimeout: Cardinal;
  public
    constructor Create(ACapacity: integer);
    destructor Destroy; override;
    function TryEnqueue(AItem: IWorkItem): boolean;
    function TryEnqueue(AItem: IWorkItem;
      ATimeoutMS: Cardinal): boolean; overload;
    function TryDequeue(out AItem: IWorkItem): boolean;
    function WaitForItem(ATimeoutMS: Cardinal): Boolean;
    procedure WakeAll;
    function GetCount: integer;
    function GetCapacity: integer;
    function GetBackpressureConfig: TBackpressureConfig;
    procedure SetBackpressureConfig(const AValue: TBackpressureConfig);
    procedure Clear;
    property Capacity: integer read GetCapacity;
    property LoadFactor: Double read GetLoadFactor;
    property BackpressureConfig: TBackpressureConfig read GetBackpressureConfig
      write SetBackpressureConfig;
  end;

  {$ENDREGION}

  {$REGION 'Internal: Work Item'}
  { Work item implementation for producer-consumer pattern }
  TProducerConsumerWorkItem = class(TThreadPoolCallbackWorkItem)
  public
    constructor Create(AThreadPool: TObject);
  end;

  {$ENDREGION}

  {$REGION 'Public API: TProducerConsumerThreadPool'}
  { Producer-consumer thread pool implementation }
  TProducerConsumerThreadPool = class(TThreadPoolBase, IThreadPoolTaskSource)
  private
    FThreads: TThreadList;
    FWorkQueue: TThreadSafeQueue;
    FCompletionEvent: TEvent;
    FWorkItemCount: integer;
    FWorkItemLock: TCriticalSection;

    procedure ClearThreads;
    function TryQueueWorkItem(WorkItem: IWorkItem;
      ATimeoutMS: Cardinal): Boolean;
    function SubmitRangeWorkItem(const AWorkItem: IWorkItem): Boolean;
    procedure CompleteWorkItem;
    function IsCurrentWorkerThread: Boolean;
    function GetQueueCount: integer;
    function GetQueueCapacity: integer;
    function GetQueueLoadFactor: Double;
    function GetBackpressureConfig: TBackpressureConfig;
    procedure SetBackpressureConfig(const AValue: TBackpressureConfig);
  public
    constructor Create(AThreadCount: Integer = 0;
      AQueueSize: Integer = 1024); reintroduce;
    destructor Destroy; override;

    { IThreadPool implementation }
    procedure Queue(AProcedure: TThreadProcedure); override;
    procedure Queue(AMethod: TThreadMethod); override;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: integer); override;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: integer); override;
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
    { Legacy compatibility access. New code should use the read-only queue
      metrics below; mutating WorkQueue bypasses pool completion accounting. }
    property WorkQueue: TThreadSafeQueue read FWorkQueue;
    property QueueCount: integer read GetQueueCount;
    property QueueCapacity: integer read GetQueueCapacity;
    property QueueLoadFactor: Double read GetQueueLoadFactor;
    property BackpressureConfig: TBackpressureConfig
      read GetBackpressureConfig write SetBackpressureConfig;
  end;

  {$ENDREGION}

implementation

{$REGION 'DebugLog'}

procedure DebugLog(const Msg: string);
begin
  if DEBUG_LOG then
    WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', GetThreadID, ': ', Msg);
end;

{$ENDREGION}

{$REGION 'TProducerConsumerThreadPool — Public API'}

constructor TProducerConsumerThreadPool.Create(AThreadCount: Integer = 0; AQueueSize: Integer = 1024);
var
  I: integer;
  Thread: TProducerConsumerWorkerThread;
begin
  DebugLog('Creating thread pool with ' + IntToStr(AThreadCount) + ' threads');
  inherited Create(AThreadCount);

  if AQueueSize <= 0 then
    raise EArgumentOutOfRangeException.Create('Queue size must be greater than zero');

  DebugLog('Actual thread count: ' + IntToStr(FThreadCount));

  FThreads := TThreadList.Create;
  FWorkQueue := TThreadSafeQueue.Create(AQueueSize);
  FCompletionEvent := TEvent.Create(nil, True, True, '');
  FWorkItemLock := TCriticalSection.Create;
  FWorkItemCount := 0;
  FLastError := '';

  // Create worker threads
  for I := 1 to FThreadCount do
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
  Shutdown;
  FWorkQueue.Free;
  FCompletionEvent.Free;
  FWorkItemLock.Free;
  FThreads.Free;
  inherited;
end;

function TProducerConsumerThreadPool.TryQueueWorkItem(WorkItem: IWorkItem;
  ATimeoutMS: Cardinal): Boolean;
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
    Result := FWorkQueue.TryEnqueue(WorkItem, ATimeoutMS);
  finally
    if not Result then
    begin
      FWorkItemLock.Enter;
      try
        Dec(FWorkItemCount);
        if FWorkItemCount = 0 then
          FCompletionEvent.SetEvent;
      finally
        FWorkItemLock.Leave;
      end;
    end;
  end;
end;

procedure TProducerConsumerThreadPool.CompleteWorkItem;
begin
  FWorkItemLock.Enter;
  try
    Dec(FWorkItemCount);
    if FWorkItemCount = 0 then
      FCompletionEvent.SetEvent;
  finally
    FWorkItemLock.Leave;
  end;
end;

function TProducerConsumerThreadPool.SubmitRangeWorkItem(
  const AWorkItem: IWorkItem): Boolean;
begin
  Result := TryQueueWorkItem(AWorkItem, THREADPOOL_INFINITE);
end;

function TProducerConsumerThreadPool.Submit(
  AProcedure: TThreadProcedure): IThreadPoolTask;
begin
  if not TrySubmit(AProcedure, FWorkQueue.GetDefaultTimeout, Result) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

function TProducerConsumerThreadPool.Submit(
  AMethod: TThreadMethod): IThreadPoolTask;
begin
  if not TrySubmit(AMethod, FWorkQueue.GetDefaultTimeout, Result) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

function TProducerConsumerThreadPool.Submit(
  AProcedure: TThreadProcedureIndex; AIndex: Integer): IThreadPoolTask;
begin
  if not TrySubmit(AProcedure, AIndex, FWorkQueue.GetDefaultTimeout,
    Result) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

function TProducerConsumerThreadPool.Submit(
  AMethod: TThreadMethodIndex; AIndex: Integer): IThreadPoolTask;
begin
  if not TrySubmit(AMethod, AIndex, FWorkQueue.GetDefaultTimeout,
    Result) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

function TProducerConsumerThreadPool.TrySubmit(
  AProcedure: TThreadProcedure; ATimeoutMS: Cardinal;
  out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AProcedure, ATask);
  try
    BeginQueue;
    try
      Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
    finally
      EndQueue;
    end;
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TProducerConsumerThreadPool.TrySubmit(AMethod: TThreadMethod;
  ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AMethod, ATask);
  try
    BeginQueue;
    try
      Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
    finally
      EndQueue;
    end;
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TProducerConsumerThreadPool.TrySubmit(
  AProcedure: TThreadProcedureIndex; AIndex: Integer;
  ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AProcedure, AIndex, ATask);
  try
    BeginQueue;
    try
      Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
    finally
      EndQueue;
    end;
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TProducerConsumerThreadPool.TrySubmit(
  AMethod: TThreadMethodIndex; AIndex: Integer;
  ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean;
var
  WorkItem: IWorkItem;
begin
  ATask := nil;
  WorkItem := NewTrackedWorkItem(AMethod, AIndex, ATask);
  try
    BeginQueue;
    try
      Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
    finally
      EndQueue;
    end;
  except
    ATask := nil;
    raise;
  end;
  if not Result then
    ATask := nil;
end;

function TProducerConsumerThreadPool.SubmitRange(
  AProcedure: TThreadProcedureIndex; AFirstIndex, ALastIndex: Integer;
  AChunkSize: Integer): IThreadPoolTaskBatch;
begin
  if GetThreadPoolRangeChunkSize(AFirstIndex, ALastIndex,
    FThreadCount, AChunkSize) = 0 then
    Exit(NewThreadPoolTaskBatch);
  if IsCurrentWorkerThread then
    raise EThreadPoolDeadlock.Create(
      'SubmitRange cannot be called from a worker of the same bounded pool');
  BeginQueue;
  try
    Result := NewThreadPoolRangeBatch(AProcedure, AFirstIndex,
      ALastIndex, FThreadCount, AChunkSize, @SubmitRangeWorkItem);
  finally
    EndQueue;
  end;
end;

function TProducerConsumerThreadPool.SubmitRange(
  AMethod: TThreadMethodIndex; AFirstIndex, ALastIndex: Integer;
  AChunkSize: Integer): IThreadPoolTaskBatch;
begin
  if GetThreadPoolRangeChunkSize(AFirstIndex, ALastIndex,
    FThreadCount, AChunkSize) = 0 then
    Exit(NewThreadPoolTaskBatch);
  if IsCurrentWorkerThread then
    raise EThreadPoolDeadlock.Create(
      'SubmitRange cannot be called from a worker of the same bounded pool');
  BeginQueue;
  try
    Result := NewThreadPoolRangeBatch(AMethod, AFirstIndex,
      ALastIndex, FThreadCount, AChunkSize, @SubmitRangeWorkItem);
  finally
    EndQueue;
  end;
end;

function TProducerConsumerThreadPool.TryQueue(AProcedure: TThreadProcedure;
  ATimeoutMS: Cardinal): Boolean;
var
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItem := TThreadPoolCallbackWorkItem.Create(AProcedure);
    Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
  finally
    EndQueue;
  end;
end;

function TProducerConsumerThreadPool.TryQueue(AMethod: TThreadMethod;
  ATimeoutMS: Cardinal): Boolean;
var
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItem := TThreadPoolCallbackWorkItem.Create(AMethod);
    Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
  finally
    EndQueue;
  end;
end;

function TProducerConsumerThreadPool.TryQueue(
  AProcedure: TThreadProcedureIndex; AIndex: Integer;
  ATimeoutMS: Cardinal): Boolean;
var
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItem := TThreadPoolCallbackWorkItem.Create(AProcedure, AIndex);
    Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
  finally
    EndQueue;
  end;
end;

function TProducerConsumerThreadPool.TryQueue(AMethod: TThreadMethodIndex;
  AIndex: Integer; ATimeoutMS: Cardinal): Boolean;
var
  WorkItem: IWorkItem;
begin
  BeginQueue;
  try
    WorkItem := TThreadPoolCallbackWorkItem.Create(AMethod, AIndex);
    Result := TryQueueWorkItem(WorkItem, ATimeoutMS);
  finally
    EndQueue;
  end;
end;

procedure TProducerConsumerThreadPool.Queue(AProcedure: TThreadProcedure);
begin
  if not TryQueue(AProcedure, FWorkQueue.GetDefaultTimeout) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

procedure TProducerConsumerThreadPool.Queue(AMethod: TThreadMethod);
begin
  if not TryQueue(AMethod, FWorkQueue.GetDefaultTimeout) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

procedure TProducerConsumerThreadPool.Queue(AProcedure: TThreadProcedureIndex;
  AIndex: Integer);
begin
  if not TryQueue(AProcedure, AIndex, FWorkQueue.GetDefaultTimeout) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

procedure TProducerConsumerThreadPool.Queue(AMethod: TThreadMethodIndex;
  AIndex: Integer);
begin
  if not TryQueue(AMethod, AIndex, FWorkQueue.GetDefaultTimeout) then
    raise EQueueFullException.Create('Queue is full (submission timed out)');
end;

procedure TProducerConsumerThreadPool.WaitForAll;
begin
  WaitForAll(THREADPOOL_INFINITE);
end;

function TProducerConsumerThreadPool.WaitForAll(
  ATimeoutMS: Cardinal): Boolean;
begin
  Result := FCompletionEvent.WaitFor(ATimeoutMS) = wrSignaled;
end;

function TProducerConsumerThreadPool.IsCurrentWorkerThread: Boolean;
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

procedure TProducerConsumerThreadPool.Shutdown;
begin
  if IsCurrentWorkerThread then
    raise EThreadPoolShutdown.Create('Shutdown cannot be called from a pool worker');
  if not BeginShutdown then
    Exit;
  try
    if Assigned(FCompletionEvent) then
      WaitForAll;
    if Assigned(FThreads) then
      ClearThreads;
  finally
    FinishShutdown;
  end;
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
    if Assigned(FWorkQueue) then
      FWorkQueue.WakeAll;
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

function TProducerConsumerThreadPool.GetQueueCount: integer;
begin
  Result := FWorkQueue.GetCount;
end;

function TProducerConsumerThreadPool.GetQueueCapacity: integer;
begin
  Result := FWorkQueue.GetCapacity;
end;

function TProducerConsumerThreadPool.GetQueueLoadFactor: Double;
begin
  Result := FWorkQueue.GetLoadFactor;
end;

function TProducerConsumerThreadPool.GetBackpressureConfig:
  TBackpressureConfig;
begin
  Result := FWorkQueue.GetBackpressureConfig;
end;

procedure TProducerConsumerThreadPool.SetBackpressureConfig(
  const AValue: TBackpressureConfig);
begin
  FWorkQueue.SetBackpressureConfig(AValue);
end;

{$ENDREGION}

{$REGION 'TProducerConsumerWorkItem'}

{ TProducerConsumerWorkItem }

constructor TProducerConsumerWorkItem.Create(AThreadPool: TObject);
begin
  inherited Create(TThreadProcedure(nil));
end;

{$ENDREGION}

{$REGION 'TThreadSafeQueue'}

{ TThreadSafeQueue }

constructor TThreadSafeQueue.Create(ACapacity: integer);
begin
  inherited Create;
  if ACapacity <= 0 then
    raise EArgumentOutOfRangeException.Create('Queue capacity must be greater than zero');
  FCapacity := ACapacity;
  SetLength(FItems, FCapacity);
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FNotEmptyEvent := TEvent.Create(nil, True, False, '');
  FNotFullEvent := TEvent.Create(nil, True, True, '');
  { Create the lock after both events so a partial constructor failure can be
    destroyed without Clear touching a missing event. }
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
begin
  { Kept for source compatibility. Backpressure is now event-driven: a
    producer waits only while the bounded queue is actually full. }
end;

function TThreadSafeQueue.GetDefaultTimeout: Cardinal;
var
  Config: TBackpressureConfig;
  Attempts: Integer;
  Total: QWord;
begin
  Config := GetBackpressureConfig;
  Attempts := Config.MaxAttempts;
  if Attempts <= 1 then
    Exit(0);
  Total := QWord(Attempts) * QWord(Max(0, Config.HighLoadDelay)) +
    QWord(Attempts - 1) * 10;
  if Total > High(Cardinal) - 1 then
    Result := High(Cardinal) - 1
  else
    Result := Cardinal(Total);
end;

destructor TThreadSafeQueue.Destroy;
begin
  if Assigned(FLock) then
    Clear;
  FNotFullEvent.Free;
  FNotEmptyEvent.Free;
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
      FNotFullEvent.SetEvent;
      if FCount = 0 then
        FNotEmptyEvent.ResetEvent;
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

function TThreadSafeQueue.GetCapacity: integer;
begin
  Result := FCapacity;
end;

function TThreadSafeQueue.GetBackpressureConfig: TBackpressureConfig;
begin
  FLock.Enter;
  try
    Result := FBackpressureConfig;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeQueue.SetBackpressureConfig(
  const AValue: TBackpressureConfig);
begin
  FLock.Enter;
  try
    FBackpressureConfig := AValue;
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
    FNotEmptyEvent.ResetEvent;
    FNotFullEvent.SetEvent;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.WaitForItem(ATimeoutMS: Cardinal): Boolean;
begin
  Result := FNotEmptyEvent.WaitFor(ATimeoutMS) = wrSignaled;
end;

procedure TThreadSafeQueue.WakeAll;
begin
  FNotEmptyEvent.SetEvent;
  FNotFullEvent.SetEvent;
end;

{$ENDREGION}

{$REGION 'TProducerConsumerWorkerThread'}

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
    Pool.FWorkQueue.WaitForItem(INFINITE);
    if Terminated then
      Break;

    while (not Terminated) and Pool.FWorkQueue.TryDequeue(WorkItem) do
    begin
      try
        ExecuteThreadPoolWorkItem(WorkItem, @Pool.SetLastError);
      finally
        WorkItem := nil;
        { This must execute even when the task or OnError callback fails. }
        Pool.CompleteWorkItem;
      end;
    end;
  end;
  DebugLog('Worker thread terminating');
end;

{$ENDREGION}

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
begin
  Result := TryEnqueue(AItem, GetDefaultTimeout);
end;

function TThreadSafeQueue.TryEnqueue(AItem: IWorkItem;
  ATimeoutMS: Cardinal): boolean;
var
  StartedAt, Elapsed: QWord;
  Remaining: Cardinal;
begin
  if AItem = nil then
    Exit(False);

  StartedAt := GetTickCount64;
  repeat
  begin
    FLock.Enter;
    try
      if FCount < FCapacity then
      begin
        FItems[FTail] := AItem;
        FTail := (FTail + 1) mod FCapacity;
        Inc(FCount);
        FNotEmptyEvent.SetEvent;
        if FCount = FCapacity then
          FNotFullEvent.ResetEvent;
        Exit(True);
      end;
    finally
      FLock.Leave;
    end;

    if ATimeoutMS = 0 then
      Exit(False);
    if ATimeoutMS = THREADPOOL_INFINITE then
      Remaining := INFINITE
    else
    begin
      Elapsed := GetTickCount64 - StartedAt;
      if Elapsed >= ATimeoutMS then
        Exit(False);
      Remaining := ATimeoutMS - Cardinal(Elapsed);
    end;

    if FNotFullEvent.WaitFor(Remaining) <> wrSignaled then
      Exit(False);
  end;
  until False;
end;

end.
