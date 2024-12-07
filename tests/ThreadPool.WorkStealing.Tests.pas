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
  try
    FPool := TWorkStealingPool.Create(4);  // Use minimum thread count
    FCounter := 0;
    FCounterLock := TCriticalSection.Create;
  except
    on E: Exception do
    begin
      WriteLn('Exception in SetUp: ', E.Message);
      raise;
    end;
  end;
end;

procedure TWorkStealingPoolTests.TearDown;
begin
  WriteLn('TearDown: Starting cleanup');
  try
    if Assigned(FPool) then
    begin
      WriteLn('TearDown: Freeing thread pool');
      FPool.Free;
      FPool := nil;
      WriteLn('TearDown: Thread pool freed');
    end;
    
    if Assigned(FCounterLock) then
    begin
      WriteLn('TearDown: Freeing counter lock');
      FCounterLock.Free;
      FCounterLock := nil;
      WriteLn('TearDown: Counter lock freed');
    end;
    WriteLn('TearDown: Cleanup completed');
  except
    on E: Exception do
    begin
      WriteLn('Exception in TearDown: ', E.Message);
      raise;
    end;
  end;
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
  WriteLn('Test01_CreateDestroy');
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
  WriteLn('Test01_CreateDestroy completed');
end;

procedure TWorkStealingPoolTests.Test02_QueueSimpleProcedure;
begin
  WriteLn('Test02_QueueSimpleProcedure');
  FPool.Queue(@IncrementCounter);
  FPool.WaitForAll;
  
  AssertEquals('Counter should be incremented once', 1, FCounter);
  WriteLn('Test02_QueueSimpleProcedure completed');
end;

procedure TWorkStealingPoolTests.Test03_QueueMultipleProcedures;
var
  I: Integer;
  ExpectedCount: Integer;
begin
  WriteLn('Test03_QueueMultipleProcedures');
  ExpectedCount := 100;
  
  for I := 1 to ExpectedCount do
    FPool.Queue(@IncrementCounter);
    
  FPool.WaitForAll;
  
  AssertEquals('Counter should match number of queued procedures', 
    ExpectedCount, FCounter);
  WriteLn('Test03_QueueMultipleProcedures completed');
end;

procedure TWorkStealingPoolTests.Test04_QueueWithIndex;
var
  I: Integer;
  ExpectedSum: Integer;
begin
  WriteLn('Test04_QueueWithIndex');
  ExpectedSum := 0;
  
  for I := 1 to 10 do
  begin
    FPool.Queue(@ProcessWithIndex, I);
    Inc(ExpectedSum, I);
  end;
  
  FPool.WaitForAll;
  
  AssertEquals('Sum should match expected total', ExpectedSum, FCounter);
  WriteLn('Test04_QueueWithIndex completed');
end;

procedure TWorkStealingPoolTests.Test05_WorkStealing;
var
  I: Integer;
  StartTime: TDateTime;
  UnbalancedPool: TWorkStealingPool;
begin
  WriteLn('Test05_WorkStealing');
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
  WriteLn('Test05_WorkStealing completed');
end;

procedure TWorkStealingPoolTests.Test06_ExceptionHandling;
begin
  WriteLn('Test06_ExceptionHandling');
  FPool.Queue(@RaiseException);
  FPool.WaitForAll;
  
  AssertTrue('Exception should be captured', FPool.LastError <> '');
  AssertTrue('Error should include thread ID', 
    Pos('Thread', FPool.LastError) > 0);
  WriteLn('Test06_ExceptionHandling completed');
end;

procedure TWorkStealingPoolTests.Test07_StressTest;
var
  I: Integer;
begin
  WriteLn('Test07_StressTest');
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
  WriteLn('Test07_StressTest completed');
end;

procedure TWorkStealingPoolTests.Test08_ConcurrentQueuing;
var
  I: Integer;
  Pools: array[0..3] of TWorkStealingPool;
begin
  WriteLn('Test08_ConcurrentQueuing');
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
  WriteLn('Test08_ConcurrentQueuing completed');
end;

procedure TWorkStealingPoolTests.Test09_ThreadCount;
var
  Pool: TWorkStealingPool;
begin
  WriteLn('Test09_ThreadCount');
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
  WriteLn('Test09_ThreadCount completed');
end;

procedure TWorkStealingPoolTests.Test10_ClearErrors;
begin
  WriteLn('Test10_ClearErrors');
  FPool.Queue(@RaiseException);
  FPool.WaitForAll;
  
  AssertTrue('Error should be captured', FPool.LastError <> '');
  
  FPool.ClearLastError;
  AssertEquals('Error should be cleared', '', FPool.LastError);
  
  FPool.Queue(@IncrementCounter);
  FPool.WaitForAll;
  AssertEquals('Should continue working after error', 1, FCounter);
  WriteLn('Test10_ClearErrors completed');
end;

initialization
  RegisterTest(TWorkStealingPoolTests);
end.
