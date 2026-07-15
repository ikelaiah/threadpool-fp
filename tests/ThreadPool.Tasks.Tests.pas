unit ThreadPool.Tasks.Tests;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, fpcunit, testregistry,
  ThreadPool.Types, ThreadPool.Tasks, ThreadPool.Simple,
  ThreadPool.ProducerConsumer;

type
  TTaskWaitThread = class(TThread)
  private
    FTask: IThreadPoolTask;
    FWaitResult: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const ATask: IThreadPoolTask);
    property WaitResult: Boolean read FWaitResult;
  end;

  TThreadPoolTasksTests = class(TTestCase)
  private
    FLock: TCriticalSection;
    FGateEvent: TEvent;
    FAllStartedEvent: TEvent;
    FExpectedStarts: Integer;
    FStartedCount: Integer;
    FCounter: Integer;
    FCoverage: array of Integer;
    FRangeFirst: Integer;
    FRangeFailIndex: Integer;
    FNestedPool: TProducerConsumerThreadPool;
    FNestedRejected: Integer;
    FSelfTask: IThreadPoolTask;
    FSelfWaitRejected: Integer;
    procedure CountTask;
    procedure GateTask;
    procedure RaiseTask;
    procedure ProcessIndex(AIndex: Integer);
    procedure SubmitNestedRange;
    procedure WaitOnOwnTask;
    procedure PrepareGate(AExpectedStarts: Integer);
    procedure PrepareCoverage(AFirstIndex, ALastIndex: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test01_TaskCapabilityInterfaces;
    procedure Test02_SimpleTaskCompletes;
    procedure Test03_ProducerTaskCompletes;
    procedure Test04_TaskWaitTimeoutAndMultipleWaiters;
    procedure Test05_TaskFailureIsObservable;
    procedure Test06_SimplePendingCancellation;
    procedure Test07_ProducerPendingCancellation;
    procedure Test08_RunningTaskCannotBeCancelled;
    procedure Test09_TaskHandleOutlivesPool;
    procedure Test10_ProducerTrySubmitTimeoutReturnsNil;
    procedure Test11_BatchWaitAndCounts;
    procedure Test12_BatchCancelsPendingTasks;
    procedure Test13_SimpleAutomaticRangeRunsExactlyOnce;
    procedure Test14_ProducerExplicitRangeChunks;
    procedure Test15_EmptyAndNegativeRangeBounds;
    procedure Test16_RangeFailureIsIsolated;
    procedure Test17_RangePendingCancellation;
    procedure Test18_BoundedNestedRangeIsRejected;
    procedure Test19_InvalidRangeChunkSize;
    procedure Test20_SubmitCancelRaceHasOneTerminalOutcome;
    procedure Test21_SubmitAfterShutdownIsRejected;
    procedure Test22_TaskCannotWaitOnItself;
  end;

implementation

{ TTaskWaitThread }

constructor TTaskWaitThread.Create(const ATask: IThreadPoolTask);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTask := ATask;
  FWaitResult := False;
end;

procedure TTaskWaitThread.Execute;
begin
  FWaitResult := FTask.WaitFor(2000);
end;

{ TThreadPoolTasksTests }

procedure TThreadPoolTasksTests.SetUp;
begin
  FLock := TCriticalSection.Create;
  FGateEvent := TEvent.Create(nil, True, False, '');
  FAllStartedEvent := TEvent.Create(nil, True, False, '');
  FExpectedStarts := 0;
  FStartedCount := 0;
  FCounter := 0;
  FRangeFirst := 0;
  FRangeFailIndex := High(Integer);
  FNestedPool := nil;
  FNestedRejected := 0;
  FSelfTask := nil;
  FSelfWaitRejected := 0;
end;

procedure TThreadPoolTasksTests.TearDown;
begin
  FGateEvent.SetEvent;
  FAllStartedEvent.Free;
  FGateEvent.Free;
  FLock.Free;
  SetLength(FCoverage, 0);
end;

procedure TThreadPoolTasksTests.PrepareGate(AExpectedStarts: Integer);
begin
  FLock.Enter;
  try
    FExpectedStarts := AExpectedStarts;
    FStartedCount := 0;
    FAllStartedEvent.ResetEvent;
    FGateEvent.ResetEvent;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolTasksTests.PrepareCoverage(AFirstIndex,
  ALastIndex: Integer);
begin
  FRangeFirst := AFirstIndex;
  if AFirstIndex > ALastIndex then
    SetLength(FCoverage, 0)
  else
    SetLength(FCoverage, Int64(ALastIndex) - Int64(AFirstIndex) + 1);
  if Length(FCoverage) > 0 then
    FillChar(FCoverage[0], Length(FCoverage) * SizeOf(Integer), 0);
  FCounter := 0;
end;

procedure TThreadPoolTasksTests.CountTask;
begin
  FLock.Enter;
  try
    Inc(FCounter);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolTasksTests.GateTask;
begin
  FLock.Enter;
  try
    Inc(FStartedCount);
    if FStartedCount >= FExpectedStarts then
      FAllStartedEvent.SetEvent;
  finally
    FLock.Leave;
  end;
  FGateEvent.WaitFor(INFINITE);
end;

procedure TThreadPoolTasksTests.RaiseTask;
begin
  raise Exception.Create('tracked task failure');
end;

procedure TThreadPoolTasksTests.ProcessIndex(AIndex: Integer);
var
  CoverageIndex: Integer;
begin
  if AIndex = FRangeFailIndex then
    raise Exception.CreateFmt('range failure at %d', [AIndex]);
  CoverageIndex := AIndex - FRangeFirst;
  FLock.Enter;
  try
    if (CoverageIndex >= 0) and (CoverageIndex < Length(FCoverage)) then
      Inc(FCoverage[CoverageIndex]);
    Inc(FCounter);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolTasksTests.SubmitNestedRange;
var
  Batch: IThreadPoolTaskBatch;
begin
  try
    Batch := FNestedPool.SubmitRange(@ProcessIndex, 0, 3);
    Batch.WaitFor;
  except
    on E: EThreadPoolDeadlock do
      InterlockedIncrement(FNestedRejected);
  end;
end;

procedure TThreadPoolTasksTests.WaitOnOwnTask;
begin
  FGateEvent.WaitFor(INFINITE);
  try
    FSelfTask.WaitFor;
  except
    on E: EThreadPoolDeadlock do
      InterlockedIncrement(FSelfWaitRejected);
  end;
end;

procedure TThreadPoolTasksTests.Test01_TaskCapabilityInterfaces;
var
  SimpleSource, ProducerSource: IThreadPoolTaskSource;
begin
  SimpleSource := TSimpleThreadPool.Create(4);
  ProducerSource := TProducerConsumerThreadPool.Create(4, 8);
  AssertTrue('Simple pool should expose task capabilities',
    SimpleSource <> nil);
  AssertTrue('Producer pool should expose task capabilities',
    ProducerSource <> nil);
end;

procedure TThreadPoolTasksTests.Test02_SimpleTaskCompletes;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    Task := Pool.Submit(@CountTask);
    AssertTrue('Task should finish', Task.WaitFor(2000));
    AssertEquals(Ord(ttsCompleted), Ord(Task.State));
    AssertEquals(1, FCounter);
    AssertEquals('', Task.ErrorMessage);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test03_ProducerTaskCompletes;
var
  Pool: TProducerConsumerThreadPool;
  Task: IThreadPoolTask;
begin
  Pool := TProducerConsumerThreadPool.Create(4, 8);
  try
    Task := Pool.Submit(@CountTask);
    Task.WaitFor;
    AssertEquals(Ord(ttsCompleted), Ord(Task.State));
    AssertEquals(1, FCounter);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test04_TaskWaitTimeoutAndMultipleWaiters;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
  Waiter1, Waiter2: TTaskWaitThread;
begin
  Pool := TSimpleThreadPool.Create(4);
  Waiter1 := nil;
  Waiter2 := nil;
  PrepareGate(1);
  try
    Task := Pool.Submit(@GateTask);
    AssertEquals(Ord(wrSignaled), Ord(FAllStartedEvent.WaitFor(2000)));
    AssertFalse('Immediate wait should time out', Task.WaitFor(0));
    Waiter1 := TTaskWaitThread.Create(Task);
    Waiter2 := TTaskWaitThread.Create(Task);
    Waiter1.Start;
    Waiter2.Start;
    FGateEvent.SetEvent;
    Waiter1.WaitFor;
    Waiter2.WaitFor;
    AssertTrue(Waiter1.WaitResult);
    AssertTrue(Waiter2.WaitResult);
    AssertEquals(Ord(ttsCompleted), Ord(Task.State));
  finally
    FGateEvent.SetEvent;
    Waiter1.Free;
    Waiter2.Free;
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test05_TaskFailureIsObservable;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    Task := Pool.Submit(@RaiseTask);
    Task.WaitFor;
    AssertEquals(Ord(ttsFailed), Ord(Task.State));
    AssertEquals('tracked task failure', Task.ErrorMessage);
    AssertEquals('tracked task failure', Pool.LastError);
    AssertEquals(1, Pool.ErrorCount);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test06_SimplePendingCancellation;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
  I: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  PrepareGate(Pool.ThreadCount);
  try
    for I := 1 to Pool.ThreadCount do
      Pool.Queue(@GateTask);
    AssertEquals(Ord(wrSignaled), Ord(FAllStartedEvent.WaitFor(2000)));
    Task := Pool.Submit(@CountTask);
    AssertTrue('Pending task should be cancelled', Task.Cancel);
    AssertFalse('Cancellation should be idempotent', Task.Cancel);
    AssertTrue(Task.WaitFor(0));
    AssertEquals(Ord(ttsCancelled), Ord(Task.State));
    FGateEvent.SetEvent;
    Pool.WaitForAll;
    AssertEquals('Cancelled callback must not run', 0, FCounter);
  finally
    FGateEvent.SetEvent;
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test07_ProducerPendingCancellation;
var
  Pool: TProducerConsumerThreadPool;
  Task: IThreadPoolTask;
  I: Integer;
begin
  Pool := TProducerConsumerThreadPool.Create(4, 4);
  PrepareGate(Pool.ThreadCount);
  try
    for I := 1 to Pool.ThreadCount do
      Pool.Queue(@GateTask);
    AssertEquals(Ord(wrSignaled), Ord(FAllStartedEvent.WaitFor(2000)));
    Task := Pool.Submit(@CountTask);
    AssertTrue(Task.Cancel);
    AssertEquals(Ord(ttsCancelled), Ord(Task.State));
    FGateEvent.SetEvent;
    Pool.WaitForAll;
    AssertEquals(0, FCounter);
  finally
    FGateEvent.SetEvent;
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test08_RunningTaskCannotBeCancelled;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
begin
  Pool := TSimpleThreadPool.Create(4);
  PrepareGate(1);
  try
    Task := Pool.Submit(@GateTask);
    AssertEquals(Ord(wrSignaled), Ord(FAllStartedEvent.WaitFor(2000)));
    AssertEquals(Ord(ttsRunning), Ord(Task.State));
    AssertFalse(Task.Cancel);
    FGateEvent.SetEvent;
    Task.WaitFor;
    AssertEquals(Ord(ttsCompleted), Ord(Task.State));
  finally
    FGateEvent.SetEvent;
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test09_TaskHandleOutlivesPool;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
begin
  Pool := TSimpleThreadPool.Create(4);
  Task := Pool.Submit(@CountTask);
  Pool.Free;
  AssertTrue(Task.WaitFor(0));
  AssertEquals(Ord(ttsCompleted), Ord(Task.State));
  AssertEquals(1, FCounter);
end;

procedure TThreadPoolTasksTests.Test10_ProducerTrySubmitTimeoutReturnsNil;
var
  Pool: TProducerConsumerThreadPool;
  PendingTask, RejectedTask: IThreadPoolTask;
  I: Integer;
begin
  Pool := TProducerConsumerThreadPool.Create(4, 1);
  PrepareGate(Pool.ThreadCount);
  try
    for I := 1 to Pool.ThreadCount do
      Pool.Queue(@GateTask);
    AssertEquals(Ord(wrSignaled), Ord(FAllStartedEvent.WaitFor(2000)));
    PendingTask := Pool.Submit(@CountTask);
    AssertFalse(Pool.TrySubmit(@CountTask, 0, RejectedTask));
    AssertTrue('Rejected task handle must be nil', RejectedTask = nil);
    FGateEvent.SetEvent;
    PendingTask.WaitFor;
    AssertEquals(1, FCounter);
  finally
    FGateEvent.SetEvent;
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test11_BatchWaitAndCounts;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
begin
  Pool := TSimpleThreadPool.Create(4);
  Batch := NewThreadPoolTaskBatch;
  try
    AssertTrue('Empty batch is complete', Batch.WaitFor(0));
    Batch.Add(Pool.Submit(@CountTask));
    Batch.Add(Pool.Submit(@RaiseTask));
    AssertTrue(Batch.WaitFor(2000));
    AssertEquals(2, Batch.Count);
    AssertEquals(2, Batch.FinishedCount);
    AssertEquals(1, Batch.FailedCount);
    AssertEquals(0, Batch.CancelledCount);
    AssertEquals(1, FCounter);
    AssertEquals(Ord(ttsCompleted), Ord(Batch[0].State));
    AssertEquals(Ord(ttsFailed), Ord(Batch[1].State));
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test12_BatchCancelsPendingTasks;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
  I: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  Batch := NewThreadPoolTaskBatch;
  PrepareGate(Pool.ThreadCount);
  try
    for I := 1 to Pool.ThreadCount do
      Pool.Queue(@GateTask);
    AssertEquals(Ord(wrSignaled), Ord(FAllStartedEvent.WaitFor(2000)));
    Batch.Add(Pool.Submit(@CountTask));
    Batch.Add(Pool.Submit(@CountTask));
    AssertEquals(2, Batch.CancelPending);
    AssertEquals(0, Batch.CancelPending);
    AssertTrue(Batch.WaitFor(0));
    AssertEquals(2, Batch.CancelledCount);
    FGateEvent.SetEvent;
    Pool.WaitForAll;
    AssertEquals(0, FCounter);
  finally
    FGateEvent.SetEvent;
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test13_SimpleAutomaticRangeRunsExactlyOnce;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
  I: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  PrepareCoverage(0, 100);
  try
    Batch := Pool.SubmitRange(@ProcessIndex, 0, 100);
    AssertTrue(Batch.Count <= Pool.ThreadCount * 4);
    Batch.WaitFor;
    AssertEquals(101, FCounter);
    for I := 0 to 100 do
      AssertEquals('Index should run exactly once', 1, FCoverage[I]);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test14_ProducerExplicitRangeChunks;
var
  Pool: TProducerConsumerThreadPool;
  Batch: IThreadPoolTaskBatch;
  I: Integer;
begin
  Pool := TProducerConsumerThreadPool.Create(4, 2);
  PrepareCoverage(-20, 19);
  try
    Batch := Pool.SubmitRange(@ProcessIndex, -20, 19, 7);
    AssertEquals('40 indexes in chunks of 7', 6, Batch.Count);
    Batch.WaitFor;
    AssertEquals(40, FCounter);
    for I := 0 to 39 do
      AssertEquals(1, FCoverage[I]);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test15_EmptyAndNegativeRangeBounds;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
  I: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  try
    Batch := Pool.SubmitRange(@ProcessIndex, 5, 4);
    AssertEquals(0, Batch.Count);
    AssertTrue(Batch.WaitFor(0));

    PrepareCoverage(-3, 3);
    Batch := Pool.SubmitRange(@ProcessIndex, -3, 3, 2);
    Batch.WaitFor;
    AssertEquals(7, FCounter);
    for I := 0 to 6 do
      AssertEquals(1, FCoverage[I]);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test16_RangeFailureIsIsolated;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
begin
  Pool := TSimpleThreadPool.Create(4);
  PrepareCoverage(0, 31);
  FRangeFailIndex := 5;
  try
    Batch := Pool.SubmitRange(@ProcessIndex, 0, 31, 4);
    Batch.WaitFor;
    AssertEquals(1, Batch.FailedCount);
    AssertEquals(1, Pool.ErrorCount);
    AssertEquals(Ord(ttsFailed), Ord(Batch[1].State));
    AssertTrue('Other chunks should continue', FCounter > 20);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test17_RangePendingCancellation;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
  I: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  PrepareCoverage(0, 31);
  PrepareGate(Pool.ThreadCount);
  try
    for I := 1 to Pool.ThreadCount do
      Pool.Queue(@GateTask);
    AssertEquals(Ord(wrSignaled), Ord(FAllStartedEvent.WaitFor(2000)));
    Batch := Pool.SubmitRange(@ProcessIndex, 0, 31, 1);
    AssertEquals(32, Batch.CancelPending);
    AssertTrue(Batch.WaitFor(0));
    FGateEvent.SetEvent;
    Pool.WaitForAll;
    AssertEquals(0, FCounter);
  finally
    FGateEvent.SetEvent;
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test18_BoundedNestedRangeIsRejected;
var
  Task: IThreadPoolTask;
begin
  FNestedPool := TProducerConsumerThreadPool.Create(4, 4);
  PrepareCoverage(0, 3);
  try
    Task := FNestedPool.Submit(@SubmitNestedRange);
    Task.WaitFor;
    AssertEquals(1, FNestedRejected);
    AssertEquals(Ord(ttsCompleted), Ord(Task.State));
  finally
    FNestedPool.Free;
    FNestedPool := nil;
  end;
end;

procedure TThreadPoolTasksTests.Test19_InvalidRangeChunkSize;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
  RaisedExpected: Boolean;
begin
  Pool := TSimpleThreadPool.Create(4);
  RaisedExpected := False;
  try
    try
      Batch := Pool.SubmitRange(@ProcessIndex, 0, 10, -1);
      AssertTrue(Batch <> nil);
    except
      on E: EArgumentOutOfRangeException do
        RaisedExpected := True;
    end;
    AssertTrue('Negative chunk size should be rejected', RaisedExpected);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test20_SubmitCancelRaceHasOneTerminalOutcome;
const
  TASK_COUNT = 5000;
var
  Pool: TSimpleThreadPool;
  Tasks: TThreadPoolTaskArray;
  CompletedCount, CancelledCount: Integer;
  I: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  Tasks := nil;
  SetLength(Tasks, TASK_COUNT);
  CompletedCount := 0;
  CancelledCount := 0;
  try
    for I := 0 to TASK_COUNT - 1 do
    begin
      Tasks[I] := Pool.Submit(@CountTask);
      Tasks[I].Cancel;
    end;
    Pool.WaitForAll;

    for I := 0 to TASK_COUNT - 1 do
    begin
      case Tasks[I].State of
        ttsCompleted: Inc(CompletedCount);
        ttsCancelled: Inc(CancelledCount);
      else
        Fail('Every raced task must have one terminal outcome');
      end;
      AssertFalse('Terminal task cannot be cancelled again', Tasks[I].Cancel);
    end;
    AssertEquals(TASK_COUNT, CompletedCount + CancelledCount);
    AssertEquals(CompletedCount, FCounter);
  finally
    Pool.Free;
    SetLength(Tasks, 0);
  end;
end;

procedure TThreadPoolTasksTests.Test21_SubmitAfterShutdownIsRejected;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
  RaisedExpected: Boolean;
begin
  Pool := TSimpleThreadPool.Create(4);
  RaisedExpected := False;
  try
    Pool.Shutdown;
    try
      Task := Pool.Submit(@CountTask);
      AssertTrue(Task <> nil);
    except
      on E: EThreadPoolShutdown do
        RaisedExpected := True;
    end;
    AssertTrue('Submit after shutdown should be rejected', RaisedExpected);
  finally
    Pool.Free;
  end;
end;

procedure TThreadPoolTasksTests.Test22_TaskCannotWaitOnItself;
var
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool.Create(4);
  FGateEvent.ResetEvent;
  try
    FSelfTask := Pool.Submit(@WaitOnOwnTask);
    FGateEvent.SetEvent;
    AssertTrue(FSelfTask.WaitFor(2000));
    AssertEquals(1, FSelfWaitRejected);
    AssertEquals(Ord(ttsCompleted), Ord(FSelfTask.State));
  finally
    FGateEvent.SetEvent;
    Pool.Free;
    FSelfTask := nil;
  end;
end;

initialization
  RegisterTest(TThreadPoolTasksTests);

end.
