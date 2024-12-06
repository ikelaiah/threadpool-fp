unit ThreadPool.WorkStealing.Tests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, SyncObjs,
  ThreadPool.Types, ThreadPool.WorkStealing, DateUtils;

type
  { TWorkStealingPoolTests }
  TWorkStealingPoolTests = class(TTestCase)
  private
    FPool: TWorkStealingPool;
    FCounter: Integer;
    FCounterLock: TCriticalSection;
    
    procedure IncrementCounter;
    procedure ProcessWithIndex(AIndex: Integer);
    procedure RaiseException;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test01_CreateDestroy;
    procedure Test02_QueueSimpleProcedure;
    procedure Test03_QueueMultipleProcedures;
    procedure Test04_QueueWithIndex;
    procedure Test05_WorkStealing;
    procedure Test06_ExceptionHandling;
    procedure Test07_StressTest;
    procedure Test08_ConcurrentQueuing;
    procedure Test09_ThreadCount;
    procedure Test10_ClearErrors;
  end;

implementation

{ TWorkStealingPoolTests }

procedure TWorkStealingPoolTests.SetUp;
begin
  FPool := TWorkStealingPool.Create(4);  // Use minimum thread count
  FCounter := 0;
  FCounterLock := TCriticalSection.Create;
end;

procedure TWorkStealingPoolTests.TearDown;
begin
  FPool.Free;
  FCounterLock.Free;
end;

procedure TWorkStealingPoolTests.IncrementCounter;
begin
  FCounterLock.Enter;
  try
    Inc(FCounter);
  finally
    FCounterLock.Leave;
  end;
end;

procedure TWorkStealingPoolTests.ProcessWithIndex(AIndex: Integer);
begin
  FCounterLock.Enter;
  try
    Inc(FCounter, AIndex);
  finally
    FCounterLock.Leave;
  end;
end;

procedure TWorkStealingPoolTests.RaiseException;
begin
  raise Exception.Create('Test exception');
end;

procedure TWorkStealingPoolTests.Test01_CreateDestroy;
var
  Pool: TWorkStealingPool;
begin
  Pool := TWorkStealingPool.Create;
  try
    AssertEquals('Default thread count should match processor count', 
      TThread.ProcessorCount, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
  
  Pool := TWorkStealingPool.Create(6);
  try
    AssertEquals('Custom thread count should be respected', 6, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
end;

procedure TWorkStealingPoolTests.Test02_QueueSimpleProcedure;
begin
  FPool.Queue(@IncrementCounter);
  FPool.WaitForAll;
  
  AssertEquals('Counter should be incremented once', 1, FCounter);
end;

procedure TWorkStealingPoolTests.Test03_QueueMultipleProcedures;
var
  I: Integer;
  ExpectedCount: Integer;
begin
  ExpectedCount := 100;
  
  for I := 1 to ExpectedCount do
    FPool.Queue(@IncrementCounter);
    
  FPool.WaitForAll;
  
  AssertEquals('Counter should match number of queued procedures', 
    ExpectedCount, FCounter);
end;

procedure TWorkStealingPoolTests.Test04_QueueWithIndex;
var
  I: Integer;
  ExpectedSum: Integer;
begin
  ExpectedSum := 0;
  
  for I := 1 to 10 do
  begin
    FPool.Queue(@ProcessWithIndex, I);
    Inc(ExpectedSum, I);
  end;
  
  FPool.WaitForAll;
  
  AssertEquals('Sum should match expected total', ExpectedSum, FCounter);
end;

procedure TWorkStealingPoolTests.Test05_WorkStealing;
var
  I: Integer;
  StartTime: TDateTime;
  UnbalancedPool: TWorkStealingPool;
begin
  UnbalancedPool := TWorkStealingPool.Create(4);
  try
    // Queue many tasks to a single worker
    StartTime := Now;
    
    for I := 1 to 1000 do
      UnbalancedPool.Queue(@IncrementCounter);
      
    UnbalancedPool.WaitForAll;
    
    // Work stealing should help complete faster than sequential
    AssertTrue('Work stealing should distribute load effectively',
      MilliSecondsBetween(Now, StartTime) < 1000);
      
    AssertEquals('All tasks should complete', 1000, FCounter);
  finally
    UnbalancedPool.Free;
  end;
end;

procedure TWorkStealingPoolTests.Test06_ExceptionHandling;
begin
  FPool.Queue(@RaiseException);
  FPool.WaitForAll;
  
  AssertTrue('Exception should be captured', FPool.LastError <> '');
  AssertTrue('Error should include thread ID', 
    Pos('Thread', FPool.LastError) > 0);
end;

procedure TWorkStealingPoolTests.Test07_StressTest;
var
  I: Integer;
begin
  for I := 1 to 10000 do
  begin
    if I mod 2 = 0 then
      FPool.Queue(@IncrementCounter)
    else
      FPool.Queue(@ProcessWithIndex, I);
  end;
  
  FPool.WaitForAll;
  
  AssertTrue('Counter should be greater than zero', FCounter > 0);
  AssertEquals('No errors should occur', '', FPool.LastError);
end;

procedure TWorkStealingPoolTests.Test08_ConcurrentQueuing;
var
  I: Integer;
  Pools: array[0..3] of TWorkStealingPool;
begin
  // Create multiple pools queuing work simultaneously
  for I := 0 to High(Pools) do
    Pools[I] := TWorkStealingPool.Create(4);
  
  try
    for I := 1 to 100 do
    begin
      Pools[0].Queue(@IncrementCounter);
      Pools[1].Queue(@IncrementCounter);
      Pools[2].Queue(@IncrementCounter);
      Pools[3].Queue(@IncrementCounter);
    end;
    
    for I := 0 to High(Pools) do
      Pools[I].WaitForAll;
      
    AssertEquals('All increments should complete', 400, FCounter);
  finally
    for I := 0 to High(Pools) do
      Pools[I].Free;
  end;
end;

procedure TWorkStealingPoolTests.Test09_ThreadCount;
var
  Pool: TWorkStealingPool;
begin
  // Test minimum thread count
  Pool := TWorkStealingPool.Create(2);
  try
    AssertEquals('Should enforce minimum thread count', 4, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
  
  // Test maximum thread count
  Pool := TWorkStealingPool.Create(100);
  try
    AssertEquals('Should enforce maximum thread count', 
      2 * TThread.ProcessorCount, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
end;

procedure TWorkStealingPoolTests.Test10_ClearErrors;
begin
  FPool.Queue(@RaiseException);
  FPool.WaitForAll;
  
  AssertTrue('Error should be captured', FPool.LastError <> '');
  
  FPool.ClearLastError;
  AssertEquals('Error should be cleared', '', FPool.LastError);
  
  FPool.Queue(@IncrementCounter);
  FPool.WaitForAll;
  AssertEquals('Should continue working after error', 1, FCounter);
end;

initialization
  RegisterTest(TWorkStealingPoolTests);
end.
