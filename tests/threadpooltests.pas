unit ThreadPoolTests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ThreadPool, syncobjs, Math;

type
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
    
    procedure IncrementCounter;
    procedure IncrementCounterWithIndex(AIndex: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateDestroy;
    procedure TestSimpleProcedure;
    procedure TestMethodProcedure;
    procedure TestIndexedProcedure;
    procedure TestIndexedMethod;
    procedure TestMultipleThreads;
    procedure TestStressTest;
    procedure TestZeroThreadCount;
    procedure TestNegativeThreadCount;
    procedure TestMaxThreadCount;
    procedure TestConcurrentQueueAccess;
    procedure TestEmptyQueue;
    procedure TestQueueAfterWait;
    procedure TestMultipleWaits;
    procedure TestObjectLifetime;
    procedure TestExceptionHandling;
    procedure TestThreadReuse;
  end;

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

procedure TThreadPoolTests.TestCreateDestroy;
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

procedure TThreadPoolTests.TestSimpleProcedure;
begin
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FCounter);
end;

procedure TThreadPoolTests.TestMethodProcedure;
begin
  FTestObject.Counter := 0;
  FThreadPool.Queue(TThreadMethod(@FTestObject.IncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FTestObject.Counter);
end;

procedure TThreadPoolTests.TestIndexedProcedure;
begin
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedureIndex(@GlobalIncrementCounterWithIndex), 5);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented by index', 5, FCounter);
end;

procedure TThreadPoolTests.TestIndexedMethod;
begin
  FTestObject.Counter := 0;
  FThreadPool.Queue(TThreadMethodIndex(@FTestObject.IncrementCounterWithIndex), 5);
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented by index', 5, FTestObject.Counter);
end;

procedure TThreadPoolTests.TestMultipleThreads;
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

procedure TThreadPoolTests.TestStressTest;
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

procedure TThreadPoolTests.TestZeroThreadCount;
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

procedure TThreadPoolTests.TestNegativeThreadCount;
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

procedure TThreadPoolTests.TestMaxThreadCount;
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

procedure TThreadPoolTests.TestConcurrentQueueAccess;
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

procedure TThreadPoolTests.TestEmptyQueue;
begin
  FThreadPool.WaitForAll;  // Should not hang or raise exceptions
  AssertEquals('Counter should remain zero', 0, FCounter);
end;

procedure TThreadPoolTests.TestQueueAfterWait;
begin
  FCounter := 0;
  FThreadPool.WaitForAll;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented', 1, FCounter);
end;

procedure TThreadPoolTests.TestMultipleWaits;
begin
  FCounter := 0;
  FThreadPool.Queue(TThreadProcedure(@GlobalIncrementCounter));
  FThreadPool.WaitForAll;
  FThreadPool.WaitForAll;  // Multiple waits should be safe
  FThreadPool.WaitForAll;
  AssertEquals('Counter should be incremented once', 1, FCounter);
end;

type
  TTestException = class(Exception);
  
procedure RaiseTestException;
begin
  raise TTestException.Create('Test exception');
end;

procedure TThreadPoolTests.TestExceptionHandling;
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    FThreadPool.Queue(TThreadProcedure(@RaiseTestException));
    FThreadPool.WaitForAll;
    // If we got here, the exception was handled by the worker thread
    AssertTrue('Exception should be handled by worker thread', True);
  except
    on E: Exception do
      ExceptionRaised := True;
  end;
  
  AssertFalse('Exception should not propagate to main thread', ExceptionRaised);
end;

procedure TThreadPoolTests.TestObjectLifetime;
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

procedure TThreadPoolTests.TestThreadReuse;
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

initialization
  RegisterTest(TThreadPoolTests);
  CurrentTest := nil;

end.
