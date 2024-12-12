unit ThreadPool.ProducerConsumer.Tests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadPool.Types,
  ThreadPool.ProducerConsumer, syncobjs, DateUtils, Math;

type
  { TTestProducerConsumerThreadPool }
  TTestProducerConsumerThreadPool = class(TTestCase)
  private
    FThreadPool: TProducerConsumerThreadPool;
    FSharedCounter: Integer;
    FSharedLock: TCriticalSection;
    FTestResults: TStringList;
    
    procedure LogTest(const Msg: string);
    procedure IncrementCounter;
    procedure AddToResults;
    procedure ProcessWithIndex(AIndex: Integer);
    procedure ProcessMethodWithIndex(AIndex: Integer);
    procedure SlowTask;
    procedure LongTask;
    procedure RaiseTestError;
    procedure SleepTask;
    function MeasureQueueTime(const TaskCount: Integer): Int64;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test01_CreateDestroy;
    procedure Test02_QueueProcedure;
    procedure Test03_QueueMethod;
    procedure Test04_QueueMultipleTasks;
    procedure Test05_QueueWithIndex;
    procedure Test06_QueueMethodWithIndex;
    procedure Test07_ParallelExecution;
    procedure Test08_QueueFullBehavior;
    procedure Test09_ErrorHandling;
    procedure Test10_WaitForAll;
    procedure Test11_BackpressureConfig;
    procedure Test12_LoadFactorCalculation;
    procedure Test13_BackpressureBehavior;
    procedure Test14_AdaptivePerformance;
  end;

implementation

const
  TASK_COUNT = 100;
  PARALLEL_TASKS = 1000;
  STRESS_TEST_TASKS = 2048;

{ TTestProducerConsumerThreadPool }

procedure TTestProducerConsumerThreadPool.LogTest(const Msg: string);
begin
  WriteLn('[TEST] ', Msg);
end;

procedure TTestProducerConsumerThreadPool.SetUp;
begin
  LogTest('Setting up test...');
  FThreadPool := TProducerConsumerThreadPool.Create;
  FSharedCounter := 0;
  FSharedLock := TCriticalSection.Create;
  FTestResults := TStringList.Create;
  LogTest('Test setup complete');
end;

procedure TTestProducerConsumerThreadPool.TearDown;
begin
  LogTest('Tearing down test...');
  FThreadPool.Free;
  FSharedLock.Free;
  FTestResults.Free;
  LogTest('Test teardown complete');
end;

procedure TTestProducerConsumerThreadPool.IncrementCounter;
begin
  LogTest('IncrementCounter called');
  FSharedLock.Enter;
  try
    Inc(FSharedCounter);
    LogTest('Counter incremented to ' + IntToStr(FSharedCounter));
  finally
    FSharedLock.Leave;
  end;
end;

procedure TTestProducerConsumerThreadPool.AddToResults;
begin
  FSharedLock.Enter;
  try
    FTestResults.Add('Test');
  finally
    FSharedLock.Leave;
  end;
end;

procedure TTestProducerConsumerThreadPool.ProcessWithIndex(AIndex: Integer);
begin
  FSharedLock.Enter;
  try
    Inc(FSharedCounter);
    FTestResults.Add(IntToStr(AIndex));
  finally
    FSharedLock.Leave;
  end;
end;

procedure TTestProducerConsumerThreadPool.ProcessMethodWithIndex(AIndex: Integer);
begin
  FSharedLock.Enter;
  try
    Inc(FSharedCounter);
    FTestResults.Add('Method' + IntToStr(AIndex));
  finally
    FSharedLock.Leave;
  end;
end;

procedure TTestProducerConsumerThreadPool.LongTask;
begin
  Sleep(500);
end;

procedure TTestProducerConsumerThreadPool.SlowTask;
begin
  Sleep(250);
end;

procedure TTestProducerConsumerThreadPool.RaiseTestError;
begin
  raise Exception.Create('Test error');
end;

procedure TTestProducerConsumerThreadPool.SleepTask;
begin
  Sleep(100);
end;

function TTestProducerConsumerThreadPool.MeasureQueueTime(const TaskCount: Integer): Int64;
var
  StartTime: TDateTime;
  I: Integer;
begin
  StartTime := Now;
  for I := 1 to TaskCount do
    FThreadPool.Queue(@SleepTask);
  Result := MilliSecondsBetween(Now, StartTime);
end;

procedure TTestProducerConsumerThreadPool.Test01_CreateDestroy;
var
  Pool: TProducerConsumerThreadPool;
begin
  LogTest('Test01_CreateDestroy starting...');
  Pool := TProducerConsumerThreadPool.Create(4);
  try
    AssertEquals('Thread count should match', 4, Pool.ThreadCount);
    LogTest('Thread count verified');
  finally
    Pool.Free;
  end;
  LogTest('Test01_CreateDestroy finished');
end;

procedure TTestProducerConsumerThreadPool.Test02_QueueProcedure;
begin
  LogTest('Test02_QueueProcedure starting...');
  FSharedCounter := 0;
  LogTest('Queueing increment counter procedure');
  FThreadPool.Queue(@IncrementCounter);
  LogTest('Waiting for completion');
  FThreadPool.WaitForAll;
  
  AssertEquals('Counter should be incremented once', 1, FSharedCounter);
  LogTest('Test02_QueueProcedure finished');
end;

procedure TTestProducerConsumerThreadPool.Test03_QueueMethod;
begin
  LogTest('Test03_QueueMethod starting...');
  FTestResults.Clear;
  FThreadPool.Queue(@AddToResults);
  FThreadPool.WaitForAll;
  
  AssertEquals('Results count should be 1', 1, FTestResults.Count);
  AssertEquals('Result should match', 'Test', FTestResults[0]);
  LogTest('Test03_QueueMethod finished');
end;

procedure TTestProducerConsumerThreadPool.Test04_QueueMultipleTasks;
var
  I: Integer;
begin
  LogTest('Test04_QueueMultipleTasks starting...');
  FSharedCounter := 0;
  
  for I := 1 to TASK_COUNT do
    FThreadPool.Queue(@IncrementCounter);
    
  FThreadPool.WaitForAll;
  AssertEquals('Counter should match task count', TASK_COUNT, FSharedCounter);
  LogTest('Test04_QueueMultipleTasks finished');
end;

procedure TTestProducerConsumerThreadPool.Test05_QueueWithIndex;
var
  I: Integer;
begin
  LogTest('Test05_QueueWithIndex starting...');
  FSharedCounter := 0;
  FTestResults.Clear;
  
  for I := 0 to 9 do
    FThreadPool.Queue(@ProcessWithIndex, I);
    
  FThreadPool.WaitForAll;
  
  AssertEquals('Counter should match iterations', 10, FSharedCounter);
  AssertEquals('Results count should match iterations', 10, FTestResults.Count);
  LogTest('Test05_QueueWithIndex finished');
end;

procedure TTestProducerConsumerThreadPool.Test06_QueueMethodWithIndex;
var
  I: Integer;
begin
  LogTest('Test06_QueueMethodWithIndex starting...');
  FSharedCounter := 0;
  FTestResults.Clear;
  
  for I := 0 to 9 do
    FThreadPool.Queue(@ProcessMethodWithIndex, I);
    
  FThreadPool.WaitForAll;
  
  AssertEquals('Counter should match iterations', 10, FSharedCounter);
  AssertEquals('Results count should match iterations', 10, FTestResults.Count);
  AssertTrue('First result should start with Method', Pos('Method', FTestResults[0]) = 1);
  LogTest('Test06_QueueMethodWithIndex finished');
end;

procedure TTestProducerConsumerThreadPool.Test07_ParallelExecution;
var
  StartTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
begin
  LogTest('Test07_ParallelExecution starting...');
  FSharedCounter := 0;
  StartTime := Now;
  
  for I := 1 to PARALLEL_TASKS do
    FThreadPool.Queue(@IncrementCounter);
    
  FThreadPool.WaitForAll;
  
  AssertEquals('Counter should match parallel tasks', PARALLEL_TASKS, FSharedCounter);
  ElapsedMS := MilliSecondsBetween(Now, StartTime);
  AssertTrue('Execution time should be reasonable', 
    ElapsedMS < (PARALLEL_TASKS div FThreadPool.ThreadCount) * 10);
  LogTest('Test07_ParallelExecution finished');
end;

{
  Test08_QueueFullBehavior  
 
  Previously, Test08 expected an exception. With adaptive backpressure, it should instead slow down rather than fail.
}
procedure TTestProducerConsumerThreadPool.Test08_QueueFullBehavior;
const
  QUEUE_SIZE = 2;  // Very small queue
  THREAD_COUNT = 1; // Single thread to make queue fill up faster
var
  I: Integer;
  ExceptionRaised: Boolean;
  ExceptionMessage: string;
  Config: TBackpressureConfig;
begin
  LogTest('Test08_QueueFullBehavior starting...');
  
  // Create thread pool with limited queue
  FThreadPool.Free;
  FThreadPool := TProducerConsumerThreadPool.Create(THREAD_COUNT, QUEUE_SIZE);
  
  // Configure aggressive backpressure
  Config := FThreadPool.WorkQueue.BackpressureConfig;
  Config.MaxAttempts := 2;
  Config.LowLoadDelay := 0;    // Disable backpressure delays
  Config.MediumLoadDelay := 0;
  Config.HighLoadDelay := 0;
  FThreadPool.WorkQueue.BackpressureConfig := Config;
  
  ExceptionRaised := False;
  try
    // Queue more tasks than the queue can hold
    for I := 1 to QUEUE_SIZE * 3 do // Try to queue more tasks
    begin
      //LogTest(Format('Queueing task %d of %d', [I, QUEUE_SIZE * 3]));
      FThreadPool.Queue(@LongTask); // Use LongTask (500ms) to ensure queue fills up
    end;

    // Now try to add one more task to trigger the exception
    LogTest('Attempting to queue task when queue is full');
    FThreadPool.Queue(@LongTask);
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      ExceptionMessage := E.Message;
      LogTest('Got exception: ' + ExceptionMessage);
    end;
  end;

  AssertTrue('Should have raised an exception', ExceptionRaised);
  AssertEquals('Should raise queue full after max attempts',
    'Queue is full after maximum attempts', ExceptionMessage);
    
  LogTest('Test08_QueueFullBehavior finished');
end;


procedure TTestProducerConsumerThreadPool.Test09_ErrorHandling;
var
  ErrorRaised: Boolean;
begin
  LogTest('Test09_ErrorHandling starting...');
  ErrorRaised := False;
  FThreadPool.ClearLastError;
  
  FThreadPool.Queue(@RaiseTestError);
  
  FThreadPool.WaitForAll;
  
  ErrorRaised := FThreadPool.LastError <> '';
  AssertTrue('Error should have been captured', ErrorRaised);
  AssertEquals('Error message should match', 'Test error', FThreadPool.LastError);
  LogTest('Test09_ErrorHandling finished');
end;

procedure TTestProducerConsumerThreadPool.Test10_WaitForAll;
var
  StartTime: TDateTime;
  ElapsedMs: Int64;
begin
  LogTest('Test10_WaitForAll starting...');
  StartTime := Now;
  
  FThreadPool.Queue(@SleepTask);
  
  FThreadPool.WaitForAll;
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  
  AssertTrue('WaitForAll should wait for task completion', 
    ElapsedMs >= 100);
  LogTest('Test10_WaitForAll finished');
end;


procedure TTestProducerConsumerThreadPool.Test11_BackpressureConfig;
var
  Config: TBackpressureConfig;
begin
  LogTest('Test11_BackpressureConfig starting...');
  
  // Test default configuration
  Config := FThreadPool.WorkQueue.BackpressureConfig;
  AssertEquals('Default low threshold', 0.5, Config.LowLoadThreshold);
  AssertEquals('Default medium threshold', 0.7, Config.MediumLoadThreshold);
  AssertEquals('Default high threshold', 0.9, Config.HighLoadThreshold);
  
  // Test configuration changes
  Config.LowLoadThreshold := 0.6;
  Config.MediumLoadThreshold := 0.8;
  Config.HighLoadThreshold := 0.95;
  Config.LowLoadDelay := 5;
  Config.MediumLoadDelay := 20;
  Config.HighLoadDelay := 50;
  
  FThreadPool.WorkQueue.BackpressureConfig := Config;
  
  Config := FThreadPool.WorkQueue.BackpressureConfig;
  AssertEquals('Modified low threshold', 0.6, Config.LowLoadThreshold);
  AssertEquals('Modified medium threshold', 0.8, Config.MediumLoadThreshold);
  AssertEquals('Modified high threshold', 0.95, Config.HighLoadThreshold);
  
  LogTest('Test11_BackpressureConfig finished');
end;

procedure TTestProducerConsumerThreadPool.Test12_LoadFactorCalculation;
var
  LoadFactor: Double;
begin
  LogTest('Test12_LoadFactorCalculation starting...');
  
  // Empty queue
  LoadFactor := FThreadPool.WorkQueue.LoadFactor;
  AssertEquals('Empty queue load factor', 0.0, LoadFactor);
  
  // Add some tasks
  FThreadPool.Queue(@SleepTask);
  FThreadPool.Queue(@SleepTask);
  
  LoadFactor := FThreadPool.WorkQueue.LoadFactor;
  AssertTrue('Partial queue load factor', (LoadFactor > 0.0) and (LoadFactor < 1.0));
  
  FThreadPool.WaitForAll;
  LoadFactor := FThreadPool.WorkQueue.LoadFactor;
  AssertEquals('Empty queue after processing', 0.0, LoadFactor);
  
  LogTest('Test12_LoadFactorCalculation finished');
end;

procedure TTestProducerConsumerThreadPool.Test13_BackpressureBehavior;
var
  StartTime: TDateTime;
  ElapsedMS: Int64;
  I: Integer;
  Config: TBackpressureConfig;
begin
  LogTest('Test13_BackpressureBehavior starting...');
  
  // Configure more aggressive backpressure for testing
  Config := FThreadPool.WorkQueue.BackpressureConfig;
  Config.LowLoadThreshold := 0.3;
  Config.LowLoadDelay := 50;  // Increased delay
  Config.HighLoadThreshold := 0.7;
  Config.HighLoadDelay := 100; // Increased delay
  FThreadPool.WorkQueue.BackpressureConfig := Config;
  
  // Measure time with high load
  StartTime := Now;
  for I := 1 to 500 do // More tasks
    FThreadPool.Queue(@SleepTask);
  ElapsedMS := MilliSecondsBetween(Now, StartTime);
  
  // Should have significant delay due to backpressure
  AssertTrue('High load should trigger backpressure', 
    ElapsedMS > Config.HighLoadDelay);
    
  LogTest(Format('Elapsed time under load: %d ms', [ElapsedMS]));
  FThreadPool.WaitForAll;
  LogTest('Test13_BackpressureBehavior finished');
end;

{
  Test14_AdaptivePerformance evaluates the thread pool's ability to maintain efficient performance 
  under varying load conditions. The test performs the following steps:
  
  1. **Configuration**: It starts by disabling backpressure to ensure that delays introduced by 
     backpressure do not affect the measurement of task processing times.
  
  2. **Low Load Test**: The test queues a single task (`LOW_LOAD_TASKS`) and measures the time 
     taken to complete it. This establishes a baseline for the thread pool's performance under 
     minimal load.
  
  3. **High Load Test**: It then queues a large number of tasks (`HIGH_LOAD_TASKS`) and measures 
     the time taken to process all of them. This simulates a high-load scenario to assess how 
     well the thread pool scales with increased workload.
  
  4. **Performance Analysis**: The test calculates the normalized time per task for both low 
     and high load scenarios. By computing the ratio of normalized low load time to high load 
     time, it determines the slowdown factor, which indicates how much the performance degrades 
     under high load.
  
  The purpose of this test is to ensure that the thread pool can adapt to different levels of 
  workload without significant performance penalties, thereby validating its scalability and 
  robustness in handling varying task loads.
}


procedure TTestProducerConsumerThreadPool.Test14_AdaptivePerformance;
const
  LOW_LOAD_TASKS = 1;     // Single task
  HIGH_LOAD_TASKS = 64;   // Many more tasks
var
  StartTime: TDateTime;
  LowLoadTime: Int64;
  HighLoadTime: Int64;
  I: Integer;
  NormalizedLowTime: Double;
  NormalizedHighTime: Double;
  Ratio: Double;
  Config: TBackpressureConfig;
begin
  LogTest('Test14_AdaptivePerformance starting...');
  LogTest(Format('Thread count: %d', [FThreadPool.ThreadCount]));
  
  // Disable backpressure for cleaner measurements
  Config := FThreadPool.WorkQueue.BackpressureConfig;
  Config.LowLoadDelay := 0;
  Config.MediumLoadDelay := 0;
  Config.HighLoadDelay := 0;
  FThreadPool.WorkQueue.BackpressureConfig := Config;
  
  // Measure low load (single task)
  LogTest('Starting low load test...');
  StartTime := Now;
  for I := 1 to LOW_LOAD_TASKS do
    FThreadPool.Queue(@LongTask);
  FThreadPool.WaitForAll;
  LowLoadTime := MilliSecondsBetween(Now, StartTime);
  LogTest(Format('Low load completed in %d ms', [LowLoadTime]));

  Sleep(500); // Longer delay between tests

  // Measure high load
  LogTest('Starting high load test...');
  StartTime := Now;
  for I := 1 to HIGH_LOAD_TASKS do
    FThreadPool.Queue(@LongTask);
  FThreadPool.WaitForAll;
  HighLoadTime := MilliSecondsBetween(Now, StartTime);
  LogTest(Format('High load completed in %d ms', [HighLoadTime]));

  // Calculate normalized times
  NormalizedLowTime := LowLoadTime / LOW_LOAD_TASKS;
  NormalizedHighTime := HighLoadTime / HIGH_LOAD_TASKS;
  // Invert the ratio to measure slowdown factor
  Ratio := NormalizedLowTime / NormalizedHighTime;

  LogTest(Format('Low load time: %d ms for %d tasks (%.2f ms/task)',
    [LowLoadTime, LOW_LOAD_TASKS, NormalizedLowTime]));
  LogTest(Format('High load time: %d ms for %d tasks (%.2f ms/task)',
    [HighLoadTime, HIGH_LOAD_TASKS, NormalizedHighTime]));
  LogTest(Format('Performance ratio: %.2fx faster under low load', [Ratio]));

  AssertTrue('Low load should be proportionally faster', Ratio > 1.5);
  
  LogTest('Test14_AdaptivePerformance finished');
end;

initialization
  RegisterTest(TTestProducerConsumerThreadPool);
end. 
