unit ThreadPool.ProducerConsumer;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, ThreadPool.Types, SyncObjs;

const
  DEBUG_LOG = True;  // Set to False to disable logging

procedure DebugLog(const Msg: string);

type
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
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FLock: TCriticalSection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    function TryEnqueue(AItem: IWorkItem): Boolean;
    function TryDequeue(out AItem: IWorkItem): Boolean;
    function GetCount: Integer;
    procedure Clear;
  end;

  { Work item implementation for producer-consumer pattern }
  TProducerConsumerWorkItem = class(TInterfacedObject, IWorkItem)
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
    { IWorkItem implementation }
    procedure Execute;
    function GetItemType: Integer;
  end;

  { Producer-consumer thread pool implementation }
  TProducerConsumerThreadPool = class(TThreadPoolBase)
  private
    FThreads: TThreadList;
    FWorkQueue: TThreadSafeQueue;  // Use our custom thread-safe queue
    FCompletionEvent: TEvent;
    FErrorLock: TCriticalSection;
    FWorkItemCount: Integer;
    FWorkItemLock: TCriticalSection;
    FLocalThreadCount: Integer;
    
    procedure ClearThreads;
    function GetThreadCount: Integer; override;
    function GetLastError: string; override;
  public
    constructor Create(AThreadCount: Integer = 0); override;
    destructor Destroy; override;
    
    { IThreadPool implementation }
    procedure Queue(AProcedure: TThreadProcedure); override;
    procedure Queue(AMethod: TThreadMethod); override;
    procedure Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer); override;
    procedure Queue(AMethod: TThreadMethodIndex; AIndex: Integer); override;
    procedure WaitForAll; override;
    property ThreadCount: Integer read GetThreadCount;
    property LastError: string read GetLastError;
  end;

implementation

procedure DebugLog(const Msg: string);
begin
  if DEBUG_LOG then
    WriteLn('[', FormatDateTime('hh:nn:ss.zzz', Now), '] ', GetThreadID, ': ', Msg);
end;

constructor TProducerConsumerThreadPool.Create(AThreadCount: Integer);
var
  I: Integer;
  Thread: TProducerConsumerWorkerThread;
begin
  DebugLog('Creating thread pool with ' + IntToStr(AThreadCount) + ' threads');
  inherited Create(AThreadCount);
  
  if AThreadCount <= 0 then
    FLocalThreadCount := CPUCount
  else
    FLocalThreadCount := AThreadCount;
    
  DebugLog('Actual thread count: ' + IntToStr(FLocalThreadCount));
  
  FThreads := TThreadList.Create;
  FWorkQueue := TThreadSafeQueue.Create(1024);
  FCompletionEvent := TEvent.Create(nil, True, True, '');
  FErrorLock := TCriticalSection.Create;
  FWorkItemLock := TCriticalSection.Create;
  FWorkItemCount := 0;

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

procedure TProducerConsumerThreadPool.Queue(AProcedure: TThreadProcedure);
var
  WorkItem: TProducerConsumerWorkItem;
begin
  DebugLog('Queueing procedure');
  WorkItem := TProducerConsumerWorkItem.Create(Self);
  try
    WorkItem.FProcedure := AProcedure;
    WorkItem.FItemType := witProcedure;
    
    FWorkItemLock.Enter;
    try
      Inc(FWorkItemCount);
      DebugLog('Work item count: ' + IntToStr(FWorkItemCount));
      FCompletionEvent.ResetEvent;
    finally
      FWorkItemLock.Leave;
    end;

    try
      if not FWorkQueue.TryEnqueue(WorkItem) then
      begin
        DebugLog('Queue is full');
        FWorkItemLock.Enter;
        try
          Dec(FWorkItemCount);
          if FWorkItemCount = 0 then
            FCompletionEvent.SetEvent;
        finally
          FWorkItemLock.Leave;
        end;
        raise Exception.Create('Queue is full');
      end;
      DebugLog('Work item queued');
    except
      on E: Exception do
      begin
        DebugLog('Failed to queue work item: ' + E.Message);
        FWorkItemLock.Enter;
        try
          Dec(FWorkItemCount);
          if FWorkItemCount = 0 then
            FCompletionEvent.SetEvent;
        finally
          FWorkItemLock.Leave;
        end;
        raise;
      end;
    end;
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
  WorkItem.FMethod := AMethod;
  WorkItem.FItemType := witMethod;
  
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    FCompletionEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;

  try
    FWorkQueue.TryEnqueue(WorkItem);
  except
    FWorkItemLock.Enter;
    try
      Dec(FWorkItemCount);
      if FWorkItemCount = 0 then
        FCompletionEvent.SetEvent;
    finally
      FWorkItemLock.Leave;
    end;
    raise;
  end;
end;

procedure TProducerConsumerThreadPool.Queue(AProcedure: TThreadProcedureIndex; AIndex: Integer);
var
  WorkItem: TProducerConsumerWorkItem;
begin
  WorkItem := TProducerConsumerWorkItem.Create(Self);
  WorkItem.FProcedureIndex := AProcedure;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witProcedureIndex;
  
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    FCompletionEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;

  try
    FWorkQueue.TryEnqueue(WorkItem);
  except
    FWorkItemLock.Enter;
    try
      Dec(FWorkItemCount);
      if FWorkItemCount = 0 then
        FCompletionEvent.SetEvent;
    finally
      FWorkItemLock.Leave;
    end;
    raise;
  end;
end;

procedure TProducerConsumerThreadPool.Queue(AMethod: TThreadMethodIndex; AIndex: Integer);
var
  WorkItem: TProducerConsumerWorkItem;
begin
  WorkItem := TProducerConsumerWorkItem.Create(Self);
  WorkItem.FMethodIndex := AMethod;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witMethodIndex;
  
  FWorkItemLock.Enter;
  try
    Inc(FWorkItemCount);
    FCompletionEvent.ResetEvent;
  finally
    FWorkItemLock.Leave;
  end;

  try
    FWorkQueue.TryEnqueue(WorkItem);
  except
    FWorkItemLock.Enter;
    try
      Dec(FWorkItemCount);
      if FWorkItemCount = 0 then
        FCompletionEvent.SetEvent;
    finally
      FWorkItemLock.Leave;
    end;
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
  I: Integer;
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

function TProducerConsumerThreadPool.GetThreadCount: Integer;
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

function TProducerConsumerWorkItem.GetItemType: Integer;
begin
  Result := Ord(FItemType);
end;

{ TThreadSafeQueue }

constructor TThreadSafeQueue.Create(ACapacity: Integer);
begin
  inherited Create;
  FCapacity := ACapacity;
  SetLength(FItems, FCapacity);
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeQueue.Destroy;
begin
  Clear;
  FLock.Free;
  inherited;
end;

function TThreadSafeQueue.TryEnqueue(AItem: IWorkItem): Boolean;
begin
  Result := False;
  FLock.Enter;
  try
    if FCount < FCapacity then
    begin
      FItems[FTail] := AItem;
      FTail := (FTail + 1) mod FCapacity;
      Inc(FCount);
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue.TryDequeue(out AItem: IWorkItem): Boolean;
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

function TThreadSafeQueue.GetCount: Integer;
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
  I: Integer;
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

end. 
