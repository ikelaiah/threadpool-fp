unit ThreadPool.Simple.Tests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, 
  ThreadPool.Types, ThreadPool.Simple, syncobjs, DateUtils;

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

  { TSimpleThreadPoolTests }
  TSimpleThreadPoolTests = class(TTestCase)
  private
    FThreadPool: TSimpleThreadPool;
    FTestObject: TTestObject;
    FCounter: Integer;
    FCS: TCriticalSection;
    
    procedure IncrementCounter;
    procedure IncrementCounterWithIndex(AIndex: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Interface compliance tests
    procedure Test01_InterfaceCompliance;
    procedure Test02_WorkItemInterface;
    procedure Test03_WorkerThreadInterface;
    
    // Core functionality tests
    procedure Test04_CreateDestroy;
    procedure Test05_SimpleProcedure;
    procedure Test06_MethodProcedure;
    procedure Test07_IndexedProcedure;
    procedure Test08_IndexedMethod;
    procedure Test09_MultipleThreads;
    procedure Test10_StressTest;
    
    // Thread management tests
    procedure Test11_ThreadCount;
    procedure Test12_ThreadReuse;
    
    // Queue operation tests
    procedure Test13_ConcurrentQueueAccess;
    procedure Test14_EmptyQueue;
    procedure Test15_QueueAfterWait;
    procedure Test16_MultipleWaits;
    
    // Object and error handling tests
    procedure Test17_ObjectLifetime;
    procedure Test18_ExceptionHandling;
    procedure Test19_ExceptionMessage;
    procedure Test20_MultipleExceptions;
    procedure Test21_ExceptionAfterClear;
  end;

var
  CurrentTest: TSimpleThreadPoolTests;

// Standalone procedures for threading
procedure GlobalIncrementCounter;
procedure GlobalIncrementCounterWithIndex(AIndex: Integer);
procedure RaiseTestException;
procedure RaiseAnotherException;

implementation

{ Test helper procedures }

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

procedure RaiseTestException;
begin
  Sleep(100);
  raise TTestException.Create('Test exception message');
end;

procedure RaiseAnotherException;
begin
  Sleep(50);
  raise TTestException.Create('Another exception message');
end;

{ TTestObject }

constructor TTestObject.Create(ACS: TCriticalSection);
begin
  inherited Create;
  FCS := ACS;
  FCounter := 0;
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

{ TSimpleThreadPoolTests }

procedure TSimpleThreadPoolTests.SetUp;
begin
  CurrentTest := Self;
  FThreadPool := TSimpleThreadPool.Create(4);
  FCS := TCriticalSection.Create;
  FCounter := 0;
  FTestObject := TTestObject.Create(FCS);
end;

procedure TSimpleThreadPoolTests.TearDown;
begin
  FThreadPool.WaitForAll;
  FThreadPool.Free;
  FTestObject.Free;
  FCS.Free;
  CurrentTest := nil;
end;

procedure TSimpleThreadPoolTests.Test01_InterfaceCompliance;
var
  Pool: IThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  AssertTrue('TSimpleThreadPool should implement IThreadPool', Pool <> nil);
end;

procedure TSimpleThreadPoolTests.Test02_WorkItemInterface;
var
  WorkItem: IWorkItem;
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    WorkItem := TSimpleWorkItem.Create(Pool);
    AssertTrue('TSimpleWorkItem should implement IWorkItem', WorkItem <> nil);
  finally
    Pool.Free;
  end;
end;

procedure TSimpleThreadPoolTests.Test03_WorkerThreadInterface;
var
  Thread: IWorkerThread;
  WorkerThread: TSimpleWorkerThread;
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    WorkerThread := TSimpleWorkerThread.Create(Pool);
    Thread := WorkerThread;  // Explicit interface conversion
    AssertTrue('TSimpleWorkerThread should implement IWorkerThread', Thread <> nil);
  finally
    Thread := nil;
    Pool.Free;
  end;
end;

procedure TSimpleThreadPoolTests.Test04_CreateDestroy;
var
  Pool: TSimpleThreadPool;
const
  MinThreads = 4;
  TestThreads = 6;
begin
  Pool := TSimpleThreadPool.Create(TestThreads);
  try
    AssertEquals('Thread count should match when above minimum', 
      TestThreads, Pool.ThreadCount);
  finally
    Pool.Free;
  end;

  Pool := TSimpleThreadPool.Create(2);
  try
    AssertEquals('Thread count should be adjusted to minimum', 
      MinThreads, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
end;

procedure TSimpleThreadPoolTests.Test05_SimpleProcedure;
begin
  FCounter := 0;
  FThreadPool.Queue(@GlobalIncrementCounter);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FCounter);
end;

procedure TSimpleThreadPoolTests.Test06_MethodProcedure;
begin
  FTestObject.Counter := 0;
  FThreadPool.Queue(@FTestObject.IncrementCounter);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FTestObject.Counter);
end;

procedure TSimpleThreadPoolTests.Test07_IndexedProcedure;
begin
  FCounter := 0;
  FThreadPool.Queue(@GlobalIncrementCounterWithIndex, 5);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented by index', 5, FCounter);
end;

procedure TSimpleThreadPoolTests.Test08_IndexedMethod;
begin
  FTestObject.Counter := 0;
  FThreadPool.Queue(@FTestObject.IncrementCounterWithIndex, 5);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented by index', 5, FTestObject.Counter);
end;

procedure TSimpleThreadPoolTests.Test09_MultipleThreads;
var
  i: Integer;
const
  TaskCount = 100;
begin
  FCounter := 0;
  for i := 1 to TaskCount do
    FThreadPool.Queue(@GlobalIncrementCounter);
    
  FThreadPool.WaitForAll;
  AssertEquals('All tasks should be processed', TaskCount, FCounter);
end;

procedure TSimpleThreadPoolTests.Test10_StressTest;
var
  i: Integer;
const
  TaskCount = 10000;
begin
  FCounter := 0;
  for i := 1 to TaskCount do
    FThreadPool.Queue(@GlobalIncrementCounter);
    
  FThreadPool.WaitForAll;
  AssertEquals('All stress test tasks should complete', TaskCount, FCounter);
end;

procedure TSimpleThreadPoolTests.Test11_ThreadCount;
var
  Pool: TSimpleThreadPool;
begin
  // Test maximum thread count
  Pool := TSimpleThreadPool.Create(TThread.ProcessorCount * 3);
  try
    AssertEquals('Thread count should be limited to 2x processor count',
      TThread.ProcessorCount * 2, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
  
  // Test zero thread count
  Pool := TSimpleThreadPool.Create(0);
  try
    AssertEquals('Thread count should default to processor count',
      TThread.ProcessorCount, Pool.ThreadCount);
  finally
    Pool.Free;
  end;
end;

procedure TSimpleThreadPoolTests.Test12_ThreadReuse;
var
  i: Integer;
const
  BatchSize = 1000;
  SleepInterval = 10;
begin
  FCounter := 0;
  // Queue many small tasks to ensure thread reuse
  for i := 1 to BatchSize do
  begin
    FThreadPool.Queue(@GlobalIncrementCounter);
    if i mod 100 = 0 then
      Sleep(SleepInterval);
  end;
  
  FThreadPool.WaitForAll;
  AssertEquals('All tasks should be processed with thread reuse', 
    BatchSize, FCounter);
end;

type
  TTestQueueThread = class(TThread)
  private
    FPool: TSimpleThreadPool;
    FStartEvent: TEvent;
  public
    constructor Create(APool: TSimpleThreadPool; AStartEvent: TEvent);
    procedure Execute; override;
  end;

constructor TTestQueueThread.Create(APool: TSimpleThreadPool; AStartEvent: TEvent);
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
    FPool.Queue(@GlobalIncrementCounter);
end;

procedure TSimpleThreadPoolTests.Test13_ConcurrentQueueAccess;
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
    
    // Create and start test threads
    for i := 1 to 5 do
    begin
      Threads[i] := TTestQueueThread.Create(FThreadPool, StartEvent);
      Threads[i].Start;
    end;
    
    StartEvent.SetEvent;
    
    // Wait for completion
    for i := 1 to 5 do
    begin
      Threads[i].WaitFor;
      Threads[i].Free;
    end;
    
    FThreadPool.WaitForAll;
    AssertEquals('Concurrent queue access should work correctly', 
      ExpectedTotal, FCounter);
  finally
    StartEvent.Free;
  end;
end;

procedure TSimpleThreadPoolTests.Test14_EmptyQueue;
begin
  FThreadPool.WaitForAll;
  AssertEquals('Empty queue should not affect counter', 0, FCounter);
end;

procedure TSimpleThreadPoolTests.Test15_QueueAfterWait;
begin
  FCounter := 0;
  FThreadPool.WaitForAll;
  FThreadPool.Queue(@GlobalIncrementCounter);
  FThreadPool.WaitForAll;
  AssertEquals('Should process task queued after wait', 1, FCounter);
end;

procedure TSimpleThreadPoolTests.Test16_MultipleWaits;
begin
  FCounter := 0;
  FThreadPool.Queue(@GlobalIncrementCounter);
  FThreadPool.WaitForAll;
  FThreadPool.WaitForAll;
  FThreadPool.WaitForAll;
  AssertEquals('Multiple waits should not affect result', 1, FCounter);
end;

procedure TSimpleThreadPoolTests.Test17_ObjectLifetime;
var
  Obj: TTestObject;
begin
  Obj := TTestObject.Create(FCS);
  try
    Obj.Counter := 0;
    FThreadPool.Queue(@Obj.IncrementCounter);
    FThreadPool.WaitForAll;
    AssertEquals('Object should survive until task completion', 1, Obj.Counter);
  finally
    Obj.Free;
  end;
end;

procedure TSimpleThreadPoolTests.Test18_ExceptionHandling;
var
  ExceptionRaised: Boolean;
  StartTime: TDateTime;
begin
  ExceptionRaised := False;
  StartTime := Now;
  
  try
    FThreadPool.Queue(@RaiseTestException);
    FThreadPool.WaitForAll;
    
    AssertTrue('Task should take minimum time',
      MilliSecondsBetween(Now, StartTime) >= 90);
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      Fail('Exception should not propagate: ' + E.Message);
    end;
  end;
  
  AssertFalse('Exception should be handled internally', ExceptionRaised);
end;

procedure TSimpleThreadPoolTests.Test19_ExceptionMessage;
begin
  FThreadPool.Queue(@RaiseTestException);
  FThreadPool.WaitForAll;
  Sleep(50);
  
  AssertTrue('Error should be captured',
    FThreadPool.LastError <> '');
  AssertTrue('Error should contain exception message',
    Pos('Test exception message', FThreadPool.LastError) > 0);
end;

procedure TSimpleThreadPoolTests.Test20_MultipleExceptions;
begin
  FThreadPool.Queue(@RaiseTestException);
  FThreadPool.Queue(@RaiseAnotherException);
  FThreadPool.WaitForAll;
  
  AssertTrue('Should capture at least one error',
    FThreadPool.LastError <> '');
    
  FCounter := 0;
  FThreadPool.Queue(@GlobalIncrementCounter);
  FThreadPool.WaitForAll;
  AssertEquals('Should continue working after exceptions', 1, FCounter);
end;

procedure TSimpleThreadPoolTests.Test21_ExceptionAfterClear;
begin
  FThreadPool.Queue(@RaiseTestException);
  FThreadPool.WaitForAll;
  FThreadPool.ClearLastError;
  
  AssertEquals('Error should be cleared', '', FThreadPool.LastError);
  
  FCounter := 0;
  FThreadPool.Queue(@GlobalIncrementCounter);
  FThreadPool.WaitForAll;
  AssertEquals('Should work normally after clearing error', 1, FCounter);
end;

procedure TSimpleThreadPoolTests.IncrementCounter;
begin
  FCS.Enter;
  try
    Inc(FCounter);
  finally
    FCS.Leave;
  end;
end;

procedure TSimpleThreadPoolTests.IncrementCounterWithIndex(AIndex: Integer);
begin
  FCS.Enter;
  try
    Inc(FCounter, AIndex);
  finally
    FCS.Leave;
  end;
end;

initialization
  RegisterTest(TSimpleThreadPoolTests);
  CurrentTest := nil;

end.
