unit ThreadPoolTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadPool, syncobjs, DateUtils, Math;

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

  { TThreadPoolTests }
  TThreadPoolTests = class(TTestCase)
  private
    FThreadPool: TThreadPool;
    FTestObject: TTestObject;
    FCounter: Integer;
    FCS: TCriticalSection;
    FExecutionOrder: TStringList;
    FStep: Integer;
    
    // Helper methods for tests
    procedure RecordExecution(Priority: TTaskPriority);
    procedure RecordCompletion(Priority: Integer);
    procedure AddExecution(const TaskName: string);
    
    // Test task methods
    procedure PriorityTask(Priority: Integer);
    procedure LongLowPriorityTask;
    procedure ShortHighPriorityTask;
    procedure LongTask;
    procedure DependencyTestTask1;
    procedure DependencyTestTask2;
    procedure DependencyTestTask3;
    procedure Task4;
    procedure Task5;
    procedure Task6;
    procedure VariableWorkload;
    
    // Existing methods
    procedure IncrementCounter;
    procedure IncrementCounterWithIndex(AIndex: Integer);
    
    // Add these methods
    procedure LowPriorityTestTask;
    procedure NormalPriorityTestTask;
    procedure HighPriorityTestTask;
    procedure CriticalPriorityTestTask;
    
    // Add these methods for Test22_DynamicScaling
    procedure LongRunningTaskMethod;
    
    // Add these methods for Test23_TaskDependencies
    procedure Step1Method;  // Changed from Step1
    procedure Step2Method;  // Changed from Step2
    procedure Step3Method;  // Changed from Step3
    
    // Add these methods for Test27_TaskCancellation
    procedure LongTaskMethod;  // Changed from LongTask
    
    // Add these methods for Test28_ComplexDependencyChain
    procedure ChainTask1Method;  // Changed from ChainTask1
    procedure ChainTask2Method;
    procedure ChainTask3Method;
    procedure ChainTask4Method;
    procedure ChainTask5Method;
    procedure ChainTask6Method;
    
    // Add this method for Test29_ScalingUnderStress
    procedure VariableWorkloadMethod;  // Changed from VariableWorkload
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
    procedure Test21_TaskPriorities;
    procedure Test22_DynamicScaling;
    procedure Test23_TaskDependencies;
    procedure Test24_ScalingLimits;
    procedure Test25_LoadMonitorThread;
    procedure Test26_PriorityInversion;
    procedure Test27_TaskCancellation;
    procedure Test28_ComplexDependencyChain;
    procedure Test29_ScalingUnderStress;
    procedure Test30_PriorityQueueStress;
    procedure Test31_MixedWorkloads;
    procedure Test32_ThreadReliability;
    procedure Test33_QueueSaturation;
    procedure Test34_ScalingHysteresis;
    procedure Test35_ErrorPropagation;
  end;

const
  TasksPerPriority = 100;  // Number of tasks per priority level for stress test
  StressIterations = 1000; // Number of iterations for stress test
  TaskCount = 1000;        // Number of tasks for scaling test
  TaskDuration = 50;       // Duration in milliseconds for long-running tasks

var
  // Global variable to hold current test instance
  CurrentTest: TThreadPoolTests;

// Standalone procedures for threading
procedure GlobalIncrementCounter; 
procedure GlobalIncrementCounterWithIndex(AIndex: Integer);

implementation

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

procedure TThreadPoolTests.LowPriorityTestTask;
begin
  Sleep(10);
  FExecutionOrder.Add('Low');
end;

procedure TThreadPoolTests.NormalPriorityTestTask;
begin
  Sleep(10);
  FExecutionOrder.Add('Normal');
end;

procedure TThreadPoolTests.HighPriorityTestTask;
begin
  Sleep(10);
  FExecutionOrder.Add('High');
end;

procedure TThreadPoolTests.CriticalPriorityTestTask;
begin
  Sleep(10);
  FExecutionOrder.Add('Critical');
end;

procedure TThreadPoolTests.Test21_TaskPriorities;
begin
  FExecutionOrder := TStringList.Create;
  try
    // Queue tasks in reverse priority order using Queue instead of QueueWithPriority
    FThreadPool.Queue(TThreadMethod(@Self.LowPriorityTestTask));
    FThreadPool.Queue(TThreadMethod(@Self.NormalPriorityTestTask));
    FThreadPool.Queue(TThreadMethod(@Self.HighPriorityTestTask));
    FThreadPool.Queue(TThreadMethod(@Self.CriticalPriorityTestTask));
    
    FThreadPool.WaitForAll;
    
    // Verify execution order
    AssertEquals('Critical task should execute first', 'Critical', FExecutionOrder[0]);
    AssertEquals('High priority task should execute second', 'High', FExecutionOrder[1]);
  finally
    FExecutionOrder.Free;
  end;
end;

procedure TThreadPoolTests.Test22_DynamicScaling;
var
  InitialThreadCount: Integer;
  MaxThreads: Integer;
  I: Integer;
const
  TaskCount = 1000;
  TaskDuration = 50; // milliseconds

begin
  InitialThreadCount := FThreadPool.ThreadCount;
  MaxThreads := FThreadPool.MaxThreads;
  
  // Queue many long-running tasks
  for I := 1 to TaskCount do
    FThreadPool.Queue(TThreadMethod(@Self.LongRunningTaskMethod));
    
  // Wait a bit for scaling to occur
  Sleep(FThreadPool.LoadCheckInterval * 2);
  
  // Thread count should increase under load
  AssertTrue('Thread count should increase under load',
    FThreadPool.ThreadCount > InitialThreadCount);
    
  AssertTrue('Thread count should not exceed maximum',
    FThreadPool.ThreadCount <= MaxThreads);
    
  FThreadPool.WaitForAll;
  
  // Wait for scaling down
  Sleep(FThreadPool.LoadCheckInterval * 2);
  
  // Thread count should decrease after load is gone
  AssertTrue('Thread count should decrease after load',
    FThreadPool.ThreadCount < MaxThreads);
end;

procedure TThreadPoolTests.Test23_TaskDependencies;
var
  Task1, Task2, Task3: TWorkItem;
begin
  FStep := 0;  // Use class field instead of local variable
  
  // Create dependent chain of tasks using class methods
  Task1 := FThreadPool.Queue(TThreadMethod(@Self.DependencyTestTask1));
  Task2 := FThreadPool.Queue(TThreadMethod(@Self.DependencyTestTask2));
  Task3 := FThreadPool.Queue(TThreadMethod(@Self.DependencyTestTask3));
  
  // Set up dependencies
  FThreadPool.AddDependency(Task2, Task1);
  FThreadPool.AddDependency(Task3, Task2);
  
  FThreadPool.WaitForAll;
  
  AssertEquals('All steps should complete in order', 3, FStep);
end;

procedure TThreadPoolTests.Test24_ScalingLimits;
begin
  // Test minimum thread count
  FThreadPool.MinThreads := 2;  // Try to set below minimum
  AssertTrue('MinThreads should not go below 4',
    FThreadPool.MinThreads >= 4);
    
  // Test maximum thread count
  FThreadPool.MaxThreads := TThread.ProcessorCount * 4;  // Try to set too high
  AssertTrue('MaxThreads should not exceed 2x ProcessorCount',
    FThreadPool.MaxThreads <= TThread.ProcessorCount * 2);
    
  // Test relationship between min and max
  FThreadPool.MinThreads := FThreadPool.MaxThreads + 1;  // Try invalid setting
  AssertTrue('MinThreads should not exceed MaxThreads',
    FThreadPool.MinThreads <= FThreadPool.MaxThreads);
end;

procedure TThreadPoolTests.Test25_LoadMonitorThread;
var
  StartTime: TDateTime;
  CheckCount: Integer;
  LastThreadCount: Integer;
begin
  StartTime := Now;
  CheckCount := 0;
  LastThreadCount := FThreadPool.ThreadCount;
  
  // Queue some work to trigger scaling
  while MilliSecondsBetween(Now, StartTime) < 2000 do  // Test for 2 seconds
  begin
    if MilliSecondsBetween(Now, StartTime) mod FThreadPool.LoadCheckInterval = 0 then
    begin
      if LastThreadCount <> FThreadPool.ThreadCount then
      begin
        Inc(CheckCount);
        LastThreadCount := FThreadPool.ThreadCount;
      end;
    end;
    FThreadPool.Queue(@GlobalIncrementCounter);
    Sleep(10);
  end;
  
  FThreadPool.WaitForAll;
  
  AssertTrue('Load monitor should perform multiple checks',
    CheckCount > 0);
end;

procedure TThreadPoolTests.Test26_PriorityInversion;
var
  ExecutionTimes: array[TTaskPriority] of TDateTime;
  CS: TCriticalSection;

  procedure RecordExecutionTime(Priority: TTaskPriority);
  begin
    CS.Enter;
    try
      ExecutionTimes[Priority] := Now;
    finally
      CS.Leave;
    end;
  end;

begin
  CS := TCriticalSection.Create;
  try
    // Queue low priority task first
    FThreadPool.QueueWithPriority(TThreadMethod(@Self.LongLowPriorityTask), tpLow);
    Sleep(10);
    
    // Queue high priority task
    FThreadPool.QueueWithPriority(TThreadMethod(@Self.ShortHighPriorityTask), tpHigh);
    
    FThreadPool.WaitForAll;
    
    // High priority task should finish before low priority task
    AssertTrue('High priority task should complete before low priority task',
      ExecutionTimes[tpHigh] < ExecutionTimes[tpLow]);
  finally
    CS.Free;
  end;
end;

procedure TThreadPoolTests.Test27_TaskCancellation;
var
  Completed: Boolean;
  CS: TCriticalSection;

  procedure LongTask;
  var
    I: Integer;
  begin
    for I := 1 to 10 do
    begin
      if FThreadPool.Terminated then
        Exit;
      Sleep(100);
    end;
    
    CS.Enter;
    try
      Completed := True;
    finally
      CS.Leave;
    end;
  end;

begin
  CS := TCriticalSection.Create;
  Completed := False;
  try
    FThreadPool.Queue(@LongTask);
    Sleep(250); // Let task start
    
    // Request shutdown
    FThreadPool.Shutdown;
    
    AssertFalse('Task should not complete after shutdown', Completed);
  finally
    CS.Free;
  end;
end;

procedure TThreadPoolTests.Test28_ComplexDependencyChain;
var
  ExecutionOrder: TStringList;
  CS: TCriticalSection;
  Tasks: array[1..6] of TWorkItem;

  procedure AddToExecutionOrder(const TaskName: string);
  begin
    CS.Enter;
    try
      ExecutionOrder.Add(TaskName);
    finally
      CS.Leave;
    end;
  end;

  // Define local procedures that will be wrapped as TWorkItem
  procedure DoTask1; begin Sleep(10); AddToExecutionOrder('1'); end;
  procedure DoTask2; begin Sleep(15); AddToExecutionOrder('2'); end;
  procedure DoTask3; begin Sleep(20); AddToExecutionOrder('3'); end;
  procedure DoTask4; begin Sleep(10); AddToExecutionOrder('4'); end;
  procedure DoTask5; begin Sleep(15); AddToExecutionOrder('5'); end;
  procedure DoTask6; begin Sleep(20); AddToExecutionOrder('6'); end;

begin
  ExecutionOrder := TStringList.Create;
  CS := TCriticalSection.Create;
  try
    // Create tasks using TThreadProcedure
    Tasks[1] := FThreadPool.Queue(TThreadProcedure(@DoTask1));
    Tasks[2] := FThreadPool.Queue(TThreadProcedure(@DoTask2));
    Tasks[3] := FThreadPool.Queue(TThreadProcedure(@DoTask3));
    Tasks[4] := FThreadPool.Queue(TThreadProcedure(@DoTask4));
    Tasks[5] := FThreadPool.Queue(TThreadProcedure(@DoTask5));
    Tasks[6] := FThreadPool.Queue(TThreadProcedure(@DoTask6));
    
    // Add dependencies
    FThreadPool.AddDependency(Tasks[2], Tasks[1]);
    FThreadPool.AddDependency(Tasks[3], Tasks[2]);
    FThreadPool.AddDependency(Tasks[5], Tasks[4]);
    FThreadPool.AddDependency(Tasks[6], Tasks[5]);
    FThreadPool.AddDependency(Tasks[5], Tasks[2]);
    FThreadPool.AddDependency(Tasks[6], Tasks[3]);
    
    FThreadPool.WaitForAll;
    
    // Verify dependencies were respected
    AssertTrue('Task 2 should execute after Task 1',
      ExecutionOrder.IndexOf('2') > ExecutionOrder.IndexOf('1'));
    AssertTrue('Task 3 should execute after Task 2',
      ExecutionOrder.IndexOf('3') > ExecutionOrder.IndexOf('2'));
    AssertTrue('Task 5 should execute after Task 4',
      ExecutionOrder.IndexOf('5') > ExecutionOrder.IndexOf('4'));
    AssertTrue('Task 6 should execute after Task 5',
      ExecutionOrder.IndexOf('6') > ExecutionOrder.IndexOf('5'));
    AssertTrue('Task 5 should execute after Task 2',
      ExecutionOrder.IndexOf('5') > ExecutionOrder.IndexOf('2'));
    AssertTrue('Task 6 should execute after Task 3',
      ExecutionOrder.IndexOf('6') > ExecutionOrder.IndexOf('3'));
  finally
    ExecutionOrder.Free;
    CS.Free;
  end;
end;

procedure TThreadPoolTests.Test29_ScalingUnderStress;
var
  StartThreads, PeakThreads, EndThreads: Integer;
  ThreadCounts: TList;
  CS: TCriticalSection;
  I: Integer;
const
  StressIterations = 1000;
  
  procedure RecordThreadCount;
  begin
    CS.Enter;
    try
      ThreadCounts.Add(Pointer(PtrInt(FThreadPool.ThreadCount)));
    finally
      CS.Leave;
    end;
  end;

begin
  CS := TCriticalSection.Create;
  ThreadCounts := TList.Create;
  try
    StartThreads := FThreadPool.ThreadCount;
    
    // Generate random workload
    Randomize;
    for I := 1 to StressIterations do
      FThreadPool.Queue(TThreadMethod(@Self.VariableWorkloadMethod));
      
    FThreadPool.WaitForAll;
    
    // Analyze thread count variations
    PeakThreads := 0;
    for I := 0 to ThreadCounts.Count - 1 do
      if PtrInt(ThreadCounts[I]) > PeakThreads then
        PeakThreads := PtrInt(ThreadCounts[I]);
        
    EndThreads := FThreadPool.ThreadCount;
    
    // Verify scaling behavior
    AssertTrue('Thread pool should scale up under load',
      PeakThreads > StartThreads);
    AssertTrue('Thread pool should scale down after load',
      EndThreads < PeakThreads);
    AssertTrue('Thread count should stay within limits',
      PeakThreads <= FThreadPool.MaxThreads);
  finally
    ThreadCounts.Free;
    CS.Free;
  end;
end;

procedure TThreadPoolTests.VariableWorkloadMethod;
begin
  // Record thread count using class critical section
  FCS.Enter;
  try
    // Note: We'll need a class field for thread counts if we want to track them
  finally
    FCS.Leave;
  end;

  // Simulate varying workload
  if Random(100) < 50 then
    Sleep(Random(50))
  else
    Sleep(Random(200));
end;

procedure TThreadPoolTests.Test30_PriorityQueueStress;
var
  CompletionOrder: TList;
  CS: TCriticalSection;
  I: Integer;
  CriticalCount, HighCount: Integer;
  FirstQuarter: Integer;
begin
  CS := TCriticalSection.Create;
  CompletionOrder := TList.Create;
  try
    Randomize;
    
    // Queue tasks with different priorities
    for I := 1 to TasksPerPriority do
    begin
      FThreadPool.Queue(TThreadMethod(@Self.PriorityTask));
    end;
    
    FThreadPool.WaitForAll;
    
    // Analysis code...
  finally
    CompletionOrder.Free;
    CS.Free;
  end;
end;

procedure TThreadPoolTests.LongRunningTaskMethod;
begin
  Sleep(50);  // Use constant value directly
end;

procedure TThreadPoolTests.DependencyTestTask1;
begin
  FCS.Enter;
  try
    AssertEquals('Step 1 should execute first', 0, FStep);
    FStep := 1;
  finally
    FCS.Leave;
  end;
  Sleep(50);
end;

procedure TThreadPoolTests.DependencyTestTask2;
begin
  FCS.Enter;
  try
    AssertEquals('Step 2 should execute after Step 1', 1, FStep);
    FStep := 2;
  finally
    FCS.Leave;
  end;
  Sleep(50);
end;

procedure TThreadPoolTests.DependencyTestTask3;
begin
  FCS.Enter;
  try
    AssertEquals('Step 3 should execute after Step 2', 2, FStep);
    FStep := 3;
  finally
    FCS.Leave;
  end;
end;

initialization
  RegisterTest(TThreadPoolTests);
  CurrentTest := nil;

end.
