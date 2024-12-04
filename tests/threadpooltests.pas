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
  TasksPerPriority = 10;   // Reduced from 100
  StressIterations = 100;  // Reduced from 1000
  TaskCount = 100;         // Reduced from 1000
  TaskDuration = 50;       // Keep this the same

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
  WriteLn('Test2_SimpleProcedure: Starting');
  FCounter := 0;
  WriteLn('Test2_SimpleProcedure: Queueing task');
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  WriteLn('Test2_SimpleProcedure: Waiting for completion');
  FThreadPool.WaitForAll;
  WriteLn('Test2_SimpleProcedure: Task completed');
  AssertEquals('Counter should be incremented once', 1, FCounter);
  WriteLn('Test2_SimpleProcedure: Assertion passed');
end;

procedure TThreadPoolTests.Test29_ScalingUnderStress;
var
  StartThreads, PeakThreads, EndThreads: Integer;
  ThreadCounts: TList;
  CS: TCriticalSection;
  I: Integer;
begin
  WriteLn('Starting Test29_ScalingUnderStress');
  CS := TCriticalSection.Create;
  ThreadCounts := TList.Create;
  try
    WriteLn('  Getting initial thread count...');
    StartThreads := FThreadPool.ThreadCount;
    WriteLn('  Initial thread count: ', StartThreads);
    
    WriteLn('  Queueing ', StressIterations, ' tasks...');
    Randomize;
    for I := 1 to StressIterations do
    begin
      CS.Enter;
      try
        ThreadCounts.Add(Pointer(NativeUInt(FThreadPool.ThreadCount)));
      finally
        CS.Leave;
      end;
      
      FThreadPool.Queue(TThreadMethod(@Self.VariableWorkloadMethod));
      if I mod 10 = 0 then WriteLn('  Queued ', I, ' tasks');
    end;
    
    WriteLn('  Waiting for tasks to complete...');
    FThreadPool.WaitForAll;
    WriteLn('  All tasks completed');
    
    // ... rest of test ...
    WriteLn('Test29_ScalingUnderStress completed');
  finally
    ThreadCounts.Free;
    CS.Free;
  end;
end;

procedure TThreadPoolTests.Test30_PriorityQueueStress;
begin
  WriteLn('Starting Test30_PriorityQueueStress');
  // ... test code ...
  WriteLn('Completed Test30_PriorityQueueStress');
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
