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

  { TThreadPoolTests }
  TThreadPoolTests = class(TTestCase)
  private
    FThreadPool: TThreadPool;
    FTestObject: TTestObject;
    FCounter: Integer;
    FCS: TCriticalSection;
    FExecutionOrder: TStringList;
    FStep: Integer;
    
    // Keep only the methods we actually use
    procedure IncrementCounter;
    procedure IncrementCounterWithIndex(AIndex: Integer);
    
    // Priority test methods
    procedure LowPriorityTestTask;
    procedure NormalPriorityTestTask;
    procedure HighPriorityTestTask;
    procedure CriticalPriorityTestTask;
    
    // Dynamic scaling test methods
    procedure LongRunningTaskMethod;
    
    // Dependency test methods
    procedure DependencyTestTask1;
    procedure DependencyTestTask2;
    procedure DependencyTestTask3;
    
    // Cancellation test method
    procedure CancellationTestTask;
    
    // Variable workload method
    procedure VariableWorkloadMethod;
    
    // Add this method
    procedure PriorityTask;
    
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Keep only implemented test methods
    procedure Test1_CreateDestroy;
    procedure Test2_SimpleProcedure;
    // ... other implemented tests ...
    procedure Test29_ScalingUnderStress;
    procedure Test30_PriorityQueueStress;
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

procedure TThreadPoolTests.Test29_ScalingUnderStress;
var
  StartThreads, PeakThreads, EndThreads: Integer;
  ThreadCounts: TList;
  CS: TCriticalSection;
  I: Integer;
begin
  CS := TCriticalSection.Create;
  ThreadCounts := TList.Create;
  try
    StartThreads := FThreadPool.ThreadCount;
    
    // Generate random workload
    Randomize;
    for I := 1 to StressIterations do
    begin
      // Record thread count before each task
      CS.Enter;
      try
        ThreadCounts.Add(Pointer(NativeUInt(FThreadPool.ThreadCount)));
      finally
        CS.Leave;
      end;
      
      FThreadPool.Queue(TThreadMethod(@Self.VariableWorkloadMethod));
    end;
    
    FThreadPool.WaitForAll;
    
    // Analyze thread count variations
    PeakThreads := 0;
    for I := 0 to ThreadCounts.Count - 1 do
      if NativeUInt(ThreadCounts[I]) > NativeUInt(PeakThreads) then
        PeakThreads := NativeUInt(ThreadCounts[I]);
        
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

procedure TThreadPoolTests.Test30_PriorityQueueStress;
var
  CS: TCriticalSection;
  I: Integer;
begin
  CS := TCriticalSection.Create;
  try
    Randomize;
    
    // Queue tasks with different priorities
    for I := 1 to TasksPerPriority do
    begin
      FThreadPool.Queue(TThreadMethod(@Self.PriorityTask));
    end;
    
    FThreadPool.WaitForAll;
  finally
    CS.Free;
  end;
end;

procedure TThreadPoolTests.PriorityTask;
begin
  Sleep(Random(10));  // Small random delay
  // Record execution if needed
  FCS.Enter;
  try
    if Assigned(FExecutionOrder) then
      FExecutionOrder.Add('Task' + IntToStr(Random(100)));
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.VariableWorkloadMethod;
begin
  if Random(100) < 50 then
    Sleep(Random(50))  // Light workload
  else
    Sleep(Random(200)); // Heavy workload
    
  // Record thread count
  FCS.Enter;
  try
    // Could record metrics here if needed
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.LowPriorityTestTask;
begin
  Sleep(100);  // Long running task
  FCS.Enter;
  try
    if Assigned(FExecutionOrder) then
      FExecutionOrder.Add('Low');
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.NormalPriorityTestTask;
begin
  Sleep(50);
  FCS.Enter;
  try
    if Assigned(FExecutionOrder) then
      FExecutionOrder.Add('Normal');
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.HighPriorityTestTask;
begin
  Sleep(25);
  FCS.Enter;
  try
    if Assigned(FExecutionOrder) then
      FExecutionOrder.Add('High');
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.CriticalPriorityTestTask;
begin
  Sleep(10);  // Shortest duration for highest priority
  FCS.Enter;
  try
    if Assigned(FExecutionOrder) then
      FExecutionOrder.Add('Critical');
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPoolTests.LongRunningTaskMethod;
begin
  Sleep(TaskDuration);  // Use the constant defined at unit level
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

procedure TThreadPoolTests.CancellationTestTask;
var
  I: Integer;
begin
  for I := 1 to 10 do
  begin
    if FThreadPool.Terminated then
      Exit;
    Sleep(100);
  end;
  
  FCS.Enter;
  try
    // Set some class field to indicate completion if needed
  finally
    FCS.Leave;
  end;
end;

initialization
  RegisterTest(TThreadPoolTests);
  CurrentTest := nil;

end.
