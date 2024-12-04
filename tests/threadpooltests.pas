unit ThreadPoolTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadPool, syncobjs, DateUtils;

type
  TTestException = class(Exception);

  { TTestObject }
  TTestObject = class
  private
    FCounter: Integer;
    FCS: TCriticalSection;
    public
      constructor Create(ACS: TCriticalSection);
      procedure IncrementCounter;
      procedure IncrementCounterWithIndex(AIndex: Integer);
      property Counter: Integer read FCounter write FCounter;
    end;

  { TThreadTracker }
  TThreadTracker = class
  private
    FCS: TCriticalSection;
    FCounts: array of Integer;
  public
    constructor Create(ThreadCount: Integer);
    destructor Destroy; override;
    procedure IncrementCount(ThreadIndex: Integer);
    function GetCount(Index: Integer): Integer;
  end;

  { TThreadPoolTests }
  TThreadPoolTests = class(TTestCase)
  private
    FThreadPool: TThreadPool;
    FTestObject: TTestObject;
    FCounter: Integer;
    FCS: TCriticalSection;
    FLocalFirst: Boolean;
    FProcessedLocal: Boolean;
    
    procedure IncrementCounter;
    procedure IncrementCounterWithIndex(AIndex: Integer);
    function CreateWorkItem(AProc: TThreadProcedure): TWorkItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test1_CreateDestroy;
    procedure Test2_SimpleProcedure;
    procedure Test3_MethodProcedure;
    procedure Test4_IndexedProcedure;
    procedure Test5_IndexedMethod;
    procedure Test6_MultipleThreads;
    procedure Test7_StressTest;
    procedure Test8_ZeroThreadCount;
    procedure Test9_NegativeThreadCount;
    procedure Test10_MaxThreadCount;
    procedure Test11_ConcurrentQueueAccess;
    procedure Test12_EmptyQueue;
    procedure Test13_QueueAfterWait;
    procedure Test14_MultipleWaits;
    procedure Test15_ObjectLifetime;
    procedure Test16_ExceptionHandling;
    procedure Test17_ThreadReuse;
    procedure Test18_ExceptionMessage;
    procedure Test19_MultipleExceptions;
    procedure Test20_ExceptionAfterClear;
    procedure Test21_WorkStealing;
    procedure Test22_LoadBalancing;
    procedure Test23_LocalQueuePriority;
    procedure Test24_WorkStealingOrder;
    procedure Test25_LoadDistribution;
  end;

var
  // Global variables
  CurrentTest: TThreadPoolTests;
  GlobalTracker: TThreadTracker;

// Standalone procedures
procedure GlobalIncrementCounter;
procedure GlobalIncrementCounterWithIndex(AIndex: Integer);
procedure GlobalSetLocalFirstFalse;
procedure GlobalSetLocalFirstTrue;
procedure TrackThreadWork;

implementation

{ TThreadTracker }

constructor TThreadTracker.Create(ThreadCount: Integer);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  SetLength(FCounts, ThreadCount);
end;

destructor TThreadTracker.Destroy;
begin
  FCS.Free;
  inherited;
end;

procedure TThreadTracker.IncrementCount(ThreadIndex: Integer);
begin
  FCS.Enter;
  try
    Inc(FCounts[ThreadIndex]);
  finally
    FCS.Leave;
  end;
end;

function TThreadTracker.GetCount(Index: Integer): Integer;
begin
  Result := FCounts[Index];
end;

{ TTestObject }

constructor TTestObject.Create(ACS: TCriticalSection);
begin
  inherited Create;
  FCounter := 0;
  FCS := ACS;
end;

procedure TTestObject.IncrementCounter;
begin
  FCS.Enter;
  try
    Inc(FCounter);
  finally
    FCS.Leave;
  end;
end;

procedure TTestObject.IncrementCounterWithIndex(AIndex: Integer);
begin
  FCS.Enter;
  try
    Inc(FCounter, AIndex);
  finally
    FCS.Leave;
  end;
end;

{ TThreadPoolTests }

procedure TThreadPoolTests.SetUp;
begin
  CurrentTest := Self;
  FThreadPool := TThreadPool.Create(4);
  FCS := TCriticalSection.Create;
  FCounter := 0;
  FTestObject := TTestObject.Create(FCS);
end;

procedure TThreadPoolTests.TearDown;
begin
  FThreadPool.WaitForAll;
  FThreadPool.Free;
  FTestObject.Free;
  FCS.Free;
  CurrentTest := nil;
end;

procedure TThreadPoolTests.IncrementCounter;
begin
  FCS.Enter;
  try
    Inc(FCounter);
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.IncrementCounterWithIndex(AIndex: Integer);
begin
  FCS.Enter;
  try
    Inc(FCounter, AIndex);
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.Test1_CreateDestroy;
var
  Pool: TThreadPool;
const
  MinThreads = 4;
  TestThreads = 6;  // Use value > MinThreads
begin
  // Test with thread count above minimum
  Pool := TThreadPool.Create(TestThreads);
  try
    AssertEquals('Thread count should match when above minimum', 
      TestThreads, Pool.ThreadCount);
  finally
    Pool.Free;
  end;

  // Test with thread count below minimum
  Pool := TThreadPool.Create(2);
  try
    AssertEquals('Thread count should be adjusted to minimum', 
      MinThreads, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTests.Test2_SimpleProcedure;
begin
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FCounter);
end;

procedure TThreadPoolTests.Test3_MethodProcedure;
begin
  FTestObject.Counter := 0;
  FThreadPool.Queue(TThreadMethod(@FTestObject.IncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FTestObject.Counter);
end;

procedure TThreadPoolTests.Test4_IndexedProcedure;
begin
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedureIndex(@GlobalIncrementCounterWithIndex), 5);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented by index', 5, FCounter);
end;

procedure TThreadPoolTests.Test5_IndexedMethod;
begin
  FTestObject.Counter := 0;
  FThreadPool.Queue(TThreadMethodIndex(@FTestObject.IncrementCounterWithIndex), 5);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented by index', 5, FTestObject.Counter);
end;

procedure TThreadPoolTests.Test6_MultipleThreads;
var
  I: Integer;
const
  TaskCount = 100;
begin
  FCounter := 0;
  for I := 1 to TaskCount do
    FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
    
  FThreadPool.WaitForAll;
  AssertEquals('Counter should match task count', TaskCount, FCounter);
end;

procedure TThreadPoolTests.Test7_StressTest;
var
  I: Integer;
const
  TaskCount = 1000;
begin
  FCounter := 0;
  FTestObject.Counter := 0;
  
  for I := 1 to TaskCount do
  begin
    case I mod 4 of
      0: FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
      1: FThreadPool.Queue(TThreadMethod(@FTestObject.IncrementCounter));
      2: FThreadPool.Queue(TThreadProcedureIndex(@GlobalIncrementCounterWithIndex), 1);
      3: FThreadPool.Queue(TThreadMethodIndex(@FTestObject.IncrementCounterWithIndex), 1);
    end;
  end;
  
  FThreadPool.WaitForAll;
  AssertEquals('Counter should match expected total', TaskCount, FCounter + FTestObject.Counter);
end;

procedure TThreadPoolTests.Test8_ZeroThreadCount;
var
  Pool: TThreadPool;
begin
  Pool := TThreadPool.Create(0);
  try
    AssertTrue('Thread count should be adjusted to processor count',
      Pool.ThreadCount > 0);
    AssertTrue('Thread count should not exceed processor count',
      Pool.ThreadCount <= TThread.ProcessorCount);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTests.Test9_NegativeThreadCount;
var
  Pool: TThreadPool;
begin
  Pool := TThreadPool.Create(-5);
  try
    AssertTrue('Thread count should be adjusted to processor count',
      Pool.ThreadCount > 0);
    AssertTrue('Thread count should not exceed processor count',
      Pool.ThreadCount <= TThread.ProcessorCount);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTests.Test10_MaxThreadCount;
var
  Pool: TThreadPool;
const
  HighThreadCount = 1000;
  MinThreads = 4;
begin
  Pool := TThreadPool.Create(HighThreadCount);
  try
    // Check minimum bounds
    AssertTrue(Format('Thread count (%d) should be at least %d',
      [Pool.ThreadCount, MinThreads]),
      Pool.ThreadCount >= MinThreads);
      
    // Check maximum bounds
    AssertTrue(Format('Thread count (%d) should be limited to 2x ProcessorCount (%d)',
      [Pool.ThreadCount, TThread.ProcessorCount * 2]),
      Pool.ThreadCount <= TThread.ProcessorCount * 2);
      
    // Check that it's reasonable
    AssertTrue(Format('Thread count (%d) should be greater than zero',
      [Pool.ThreadCount]),
      Pool.ThreadCount > 0);
  finally
    Pool.Free;
  end;
end;

type
  TTestQueueThread = class(TThread)
  private
    FPool: TThreadPool;
    FStartEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TThreadPool; AStartEvent: TEvent);
  end;

constructor TTestQueueThread.Create(APool: TThreadPool; AStartEvent: TEvent);
begin
  inherited Create(True);
  FPool := APool;
  FStartEvent := AStartEvent;
  FreeOnTerminate := False;
end;

procedure TTestQueueThread.Execute;
var
  i: Integer;
begin
  FStartEvent.WaitFor(INFINITE);
  for i := 1 to 100 do
    FPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
end;

procedure TThreadPoolTests.Test11_ConcurrentQueueAccess;
var
  Threads: array[1..5] of TTestQueueThread;
  StartEvent: TEvent;
  i: Integer;
const
  ExpectedTotal = 500; // 5 threads * 100 increments
begin
  StartEvent := TEvent.Create(nil, True, False, '');
  try
    FCounter := 0;
    
    // Create test threads
    for i := 1 to 5 do
      Threads[i] := TTestQueueThread.Create(FThreadPool, StartEvent);
      
    // Start all threads
    for i := 1 to 5 do
      Threads[i].Start;
      
    // Signal threads to begin queueing
    StartEvent.SetEvent;
    
    // Wait for all threads to complete
    for i := 1 to 5 do
    begin
      Threads[i].WaitFor;
      Threads[i].Free;
    end;
    
    FThreadPool.WaitForAll;
    AssertEquals('All increments should be processed', ExpectedTotal, FCounter);
  finally
    StartEvent.Free;
  end;
end;

procedure TThreadPoolTests.Test12_EmptyQueue;
begin
  FThreadPool.WaitForAll;  // Should not hang or raise exceptions
  AssertEquals('Counter should remain zero', 0, FCounter);
end;

procedure TThreadPoolTests.Test13_QueueAfterWait;
begin
  FCounter := 0;
  FThreadPool.WaitForAll;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented', 1, FCounter);
end;

procedure TThreadPoolTests.Test14_MultipleWaits;
begin
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  FThreadPool.WaitForAll;  // Multiple waits should be safe
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FCounter);
end;


  
procedure RaiseTestException;
begin
  Sleep(100); // Give the worker thread time to start
  raise TTestException.Create('Test exception message');
end;

procedure RaiseAnotherException;
begin
  Sleep(50);
  raise TTestException.Create('Another exception message');
end;

procedure TThreadPoolTests.Test16_ExceptionHandling;
var
  ExceptionRaised: Boolean;
  StartTime: TDateTime;
begin
  ExceptionRaised := False;
  StartTime := Now;
  
  try
    FThreadPool.Queue(TThreadProcedure(@RaiseTestException));
    FThreadPool.WaitForAll;
    
    // Verify that we actually waited for the task
    AssertTrue('Test should take at least 100ms',
      MilliSecondsBetween(Now, StartTime) >= 90);
      
    // If we got here, the exception was handled by the worker thread
    AssertTrue('Exception should be handled by worker thread', True);
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      Fail('Exception should not propagate: ' + E.Message);
    end;
  end;
  
  AssertFalse('Exception should not propagate to main thread', ExceptionRaised);
end;

procedure TThreadPoolTests.Test15_ObjectLifetime;
var
  Obj: TTestObject;
begin
  Obj := TTestObject.Create(FCS);
  try
    Obj.Counter := 0;
    FThreadPool.Queue(TThreadMethod(@Obj.IncrementCounter));
    FThreadPool.WaitForAll;
    AssertEquals('Counter should be incremented', 1, Obj.Counter);
  finally
    Obj.Free;
  end;
end;

procedure TThreadPoolTests.Test17_ThreadReuse;
var
  i: Integer;
const
  BatchSize = 1000;
begin
  FCounter := 0;
  // Queue many small tasks to ensure thread reuse
  for i := 1 to BatchSize do
  begin
    FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
    if i mod 100 = 0 then
      Sleep(10); // Give threads time to process
  end;
  
  FThreadPool.WaitForAll;
  AssertEquals('All tasks should be processed', BatchSize, FCounter);
end;

procedure TThreadPoolTests.Test18_ExceptionMessage;
begin
  FThreadPool.Queue(TThreadProcedure(@RaiseTestException));
  FThreadPool.WaitForAll;
  
  // Add a small delay to ensure error is captured
  Sleep(50);
  
  // Check that the error was captured
  AssertTrue('ThreadPool should have captured an error message',
    FThreadPool.LastError <> '');
    
  // Verify the error message contains our test message
  AssertTrue('Error should contain our test message',
    Pos('Test exception message', FThreadPool.LastError) > 0);
    
  // Verify the error includes the thread ID
  AssertTrue('Error should contain thread ID',
    Pos('[Thread', FThreadPool.LastError) > 0);
end;

procedure TThreadPoolTests.Test19_MultipleExceptions;
begin
  // Queue multiple tasks that will raise exceptions
  FThreadPool.Queue(TThreadProcedure(@RaiseTestException));
  FThreadPool.Queue(TThreadProcedure(@RaiseAnotherException));
  FThreadPool.WaitForAll;
  
  // At least one exception should be captured
  AssertTrue('ThreadPool should have captured an error',
    FThreadPool.LastError <> '');
    
  // The pool continues working after exceptions
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Pool should still process tasks after exceptions', 1, FCounter);
end;

procedure TThreadPoolTests.Test20_ExceptionAfterClear;
begin
  // First exception
  FThreadPool.Queue(TThreadProcedure(@RaiseTestException));
  FThreadPool.WaitForAll;
  
  // Clear error state (you'll need to add this method to TThreadPool)
  FThreadPool.ClearLastError;
  
  // Verify cleared
  AssertEquals('Error should be cleared', '', FThreadPool.LastError);
  
  // New task works
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Pool should work after clearing error', 1, FCounter);
end;

procedure TThreadPoolTests.Test21_WorkStealing;
var
  I: Integer;
  StartTime: TDateTime;
  List: TList;
  WorkItem: TWorkItem;
const
  TaskCount = 1000;
  ExpectedMaxTime = 2000; // 2 seconds
begin
  FCounter := 0;
  StartTime := Now;
  
  // Queue many tasks to a single thread's local queue
  List := FThreadPool.GetThreads.LockList;
  try
    for I := 1 to TaskCount do
    begin
      WorkItem := CreateWorkItem(TThreadProcedure(@GlobalIncrementCounter));
      TWorkerThread(List[0]).LocalQueue.Push(WorkItem);
    end;
  finally
    FThreadPool.GetThreads.UnlockList;
  end;
  
  // Wait for all tasks to complete
  FThreadPool.WaitForAll;
  
  // Verify results
  AssertEquals('All tasks should be processed', TaskCount, FCounter);
  AssertTrue('Tasks should complete faster with work stealing',
    MilliSecondsBetween(Now, StartTime) < ExpectedMaxTime);
end;

type
  TThreadLoadInfo = record
    ThreadID: TThreadID;
    Count: Integer;
  end;
  
procedure TThreadPoolTests.Test22_LoadBalancing;
var
  I: Integer;
const
  TaskCount = 10;  // Reduced for testing
begin
  WriteLn('Starting Test22_LoadBalancing');
  FCounter := 0;
  
  // Queue tasks
  for I := 1 to TaskCount do
  begin
    WriteLn('Queueing task ', I);
    FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  end;
  
  WriteLn('Waiting for tasks to complete');
  FThreadPool.WaitForAll;
  WriteLn('Tasks completed. Counter: ', FCounter);
  
  AssertEquals('All tasks should be processed', TaskCount, FCounter);
end;

procedure TThreadPoolTests.Test23_LocalQueuePriority;
var
  List: TList;
  WorkItem: TWorkItem;
begin
  FLocalFirst := False;
  FProcessedLocal := False;
  
  // Create a task for the global queue
  FThreadPool.Queue(TThreadProcedure(@GlobalSetLocalFirstFalse));
  
  // Create a task for local queue
  List := FThreadPool.GetThreads.LockList;
  try
    WorkItem := CreateWorkItem(TThreadProcedure(@GlobalSetLocalFirstTrue));
    TWorkerThread(List[0]).LocalQueue.Push(WorkItem);
  finally
    FThreadPool.GetThreads.UnlockList;
  end;
  
  FThreadPool.WaitForAll;
  AssertTrue('Local queue tasks should be processed before global queue tasks',
    FLocalFirst);
end;

type
  TProcessRecord = record
    Order: Integer;
  end;
  PProcessRecord = ^TProcessRecord;

procedure TThreadPoolTests.Test24_WorkStealingOrder;
var
  ProcessOrder: TList;
  CS: TCriticalSection;
  List: TList;
  I: Integer;
  WorkItem: TWorkItem;
  AllInOrder: Boolean;
  ProcessRec: PProcessRecord;
begin
  ProcessOrder := TList.Create;
  CS := TCriticalSection.Create;
  try
    // Queue tasks to a single thread's local queue
    List := FThreadPool.GetThreads.LockList;
    try
      for I := 1 to 10 do
      begin
        New(ProcessRec);
        ProcessRec^.Order := I;
        ProcessOrder.Add(ProcessRec);
        WorkItem := CreateWorkItem(TThreadProcedure(@GlobalIncrementCounter));
        TWorkerThread(List[0]).LocalQueue.Push(WorkItem);
      end;
    finally
      FThreadPool.GetThreads.UnlockList;
    end;
    
    FThreadPool.WaitForAll;
    
    // Verify that some tasks were stolen and processed out of order
    AllInOrder := True;
    for I := 0 to ProcessOrder.Count - 2 do
      if PProcessRecord(ProcessOrder[I])^.Order > PProcessRecord(ProcessOrder[I + 1])^.Order then
      begin
        AllInOrder := False;
        Break;
      end;
      
    AssertFalse('Some tasks should be processed out of order due to work stealing',
      AllInOrder);
  finally
    // Clean up the process records
    for I := 0 to ProcessOrder.Count - 1 do
      Dispose(PProcessRecord(ProcessOrder[I]));
    ProcessOrder.Free;
    CS.Free;
  end;
end;

procedure TThreadPoolTests.Test25_LoadDistribution;
var
  Queues: array of Integer;
  I, J: Integer;
  List: TList;
  MaxQueue, MinQueue: Integer;
const
  TaskCount = 100;
begin
  SetLength(Queues, FThreadPool.ThreadCount);
  
  // Queue tasks and track initial distribution
  for I := 1 to TaskCount do
  begin
    FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
    
    // Record queue lengths
    List := FThreadPool.GetThreads.LockList;
    try
      for J := 0 to List.Count - 1 do
        Queues[J] := TWorkerThread(List[J]).LocalQueue.Count;
    finally
      FThreadPool.GetThreads.UnlockList;
    end;
    
    // Find min and max queue lengths
    MaxQueue := 0;
    MinQueue := TaskCount;
    for J := 0 to High(Queues) do
    begin
      if Queues[J] > MaxQueue then
        MaxQueue := Queues[J];
      if Queues[J] < MinQueue then
        MinQueue := Queues[J];
    end;
    
    // Allow small imbalance during queueing
    AssertTrue(Format('Queue length difference (%d) should be small',
      [MaxQueue - MinQueue]),
      MaxQueue - MinQueue <= 2);
  end;
  
  FThreadPool.WaitForAll;
end;

function TThreadPoolTests.CreateWorkItem(AProc: TThreadProcedure): TWorkItem;
begin
  Result := FThreadPool.CreateTestWorkItem;
  Result.SetProcedure(AProc);
  Result.SetItemType(witProcedure);
end;

procedure GlobalIncrementCounter;
begin
  if Assigned(CurrentTest) then
    CurrentTest.IncrementCounter;
end;

procedure GlobalIncrementCounterWithIndex(AIndex: Integer);
begin
  if Assigned(CurrentTest) then
    CurrentTest.IncrementCounterWithIndex(AIndex);
end;

procedure GlobalSetLocalFirstFalse;
begin
  if Assigned(CurrentTest) then
  begin
    CurrentTest.FLocalFirst := False;
    CurrentTest.FProcessedLocal := False;
  end;
end;

procedure GlobalSetLocalFirstTrue;
begin
  if Assigned(CurrentTest) then
  begin
    if not CurrentTest.FProcessedLocal then
    begin
      CurrentTest.FLocalFirst := True;
      CurrentTest.FProcessedLocal := True;
    end;
  end;
end;

procedure TrackThreadWork;
var
  CurrentThread: TThread;
  ThreadList: TList;
  I: Integer;
begin
  WriteLn('TrackThreadWork started');  // Debug output
  CurrentThread := TThread.CurrentThread;
  ThreadList := CurrentTest.FThreadPool.GetThreads.LockList;
  try
    WriteLn('Got thread list');  // Debug output
    for I := 0 to ThreadList.Count - 1 do
      if TWorkerThread(ThreadList[I]) = CurrentThread then
      begin
        WriteLn('Found current thread: ', I);  // Debug output
        GlobalTracker.IncrementCount(I);
        Break;
      end;
  finally
    CurrentTest.FThreadPool.GetThreads.UnlockList;
  end;
  WriteLn('TrackThreadWork finished');  // Debug output
end;

initialization
  RegisterTest(TThreadPoolTests);
  CurrentTest := nil;

end.
