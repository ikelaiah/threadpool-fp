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
    
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Keep only implemented test methods
    procedure Test1_CreateDestroy;
    procedure Test2_SimpleProcedure;
    // ... other implemented tests ...
    procedure Test30_PriorityQueueStress;
    // Remove unimplemented tests 31-35
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

procedure TThreadPoolTests.Test30_PriorityQueueStress;
var
  CompletionOrder: TList;
  CS: TCriticalSection;
  I: Integer;
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
  finally
    CompletionOrder.Free;
    CS.Free;
  end;
end;

procedure TThreadPoolTests.Test29_ScalingUnderStress;
var
  StartThreads, PeakThreads, EndThreads: Integer;
  ThreadCounts: TList;
  CS: TCriticalSection;
  I: Integer;
  
  procedure RecordThreadCount;
  begin
    CS.Enter;
    try
      ThreadCounts.Add(Pointer(NativeInt(FThreadPool.ThreadCount)));
    finally
      CS.Leave;
    end;
  end;

begin
  // ... rest of implementation ...
  
  // When reading:
  for I := 0 to ThreadCounts.Count - 1 do
    if NativeInt(ThreadCounts[I]) > PeakThreads then
      PeakThreads := NativeInt(ThreadCounts[I]);
end;

initialization
  RegisterTest(TThreadPoolTests);
  CurrentTest := nil;

end.
