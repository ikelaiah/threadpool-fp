unit ThreadPool.ProducerConsumer.Tests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadPool.Types, ThreadPool.ProducerConsumer, syncobjs, DateUtils;

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
    procedure RaiseTestError;
    procedure SleepTask;
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

procedure TTestProducerConsumerThreadPool.SlowTask;
begin
  Sleep(100);
end;

procedure TTestProducerConsumerThreadPool.RaiseTestError;
begin
  raise Exception.Create('Test error');
end;

procedure TTestProducerConsumerThreadPool.SleepTask;
begin
  Sleep(100);
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

procedure TTestProducerConsumerThreadPool.Test08_QueueFullBehavior;
var
  I: Integer;
  ExceptionRaised: Boolean;
begin
  LogTest('Test08_QueueFullBehavior starting...');
  ExceptionRaised := False;
  
  FThreadPool.Free;
  FThreadPool := TProducerConsumerThreadPool.Create(2);
  
  try
    for I := 1 to STRESS_TEST_TASKS do
    begin
      FThreadPool.Queue(@SlowTask);
      LogTest('Queued task ' + IntToStr(I));
    end;
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      LogTest('Got expected exception: ' + E.Message);
      AssertTrue('Should raise queue full exception', 
        E.Message = 'Queue is full');
    end;
  end;
  
  AssertTrue('Should have raised queue full exception', ExceptionRaised);
  FThreadPool.WaitForAll;
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

initialization
  RegisterTest(TTestProducerConsumerThreadPool);
end. 
