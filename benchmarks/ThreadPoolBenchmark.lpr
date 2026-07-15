program ThreadPoolBenchmark;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  ThreadPool.Tasks, ThreadPool.Simple, ThreadPool.ProducerConsumer;

const
  BURST_TASKS = 20000;
  RANGE_ITEMS = 200000;
  IDLE_SAMPLES = 10;
  IDLE_PAUSE_MS = 120;

var
  Counter: Integer;
  IdleEvent: TEvent;
  SubmittedAt: QWord;
  LastIdleLatency: QWord;

procedure CountTask;
begin
  InterlockedIncrement(Counter);
end;

procedure CountIndexedTask(AIndex: Integer);
begin
  if AIndex >= 0 then
    InterlockedIncrement(Counter);
end;

procedure IdleTask;
begin
  LastIdleLatency := GetTickCount64 - SubmittedAt;
  IdleEvent.SetEvent;
end;

function BenchmarkSimpleBurst: QWord;
var
  Pool: TSimpleThreadPool;
  StartedAt: QWord;
  I: Integer;
begin
  Counter := 0;
  Pool := TSimpleThreadPool.Create(4);
  try
    StartedAt := GetTickCount64;
    for I := 1 to BURST_TASKS do
      Pool.Queue(@CountTask);
    Pool.WaitForAll;
    Result := GetTickCount64 - StartedAt;
    if Counter <> BURST_TASKS then
      raise Exception.CreateFmt('Simple burst lost tasks: %d/%d',
        [Counter, BURST_TASKS]);
  finally
    Pool.Free;
  end;
end;

function BenchmarkProducerBurst: QWord;
var
  Pool: TProducerConsumerThreadPool;
  StartedAt: QWord;
  I: Integer;
begin
  Counter := 0;
  Pool := TProducerConsumerThreadPool.Create(4, 1024);
  try
    StartedAt := GetTickCount64;
    for I := 1 to BURST_TASKS do
      Pool.Queue(@CountTask);
    Pool.WaitForAll;
    Result := GetTickCount64 - StartedAt;
    if Counter <> BURST_TASKS then
      raise Exception.CreateFmt('Producer burst lost tasks: %d/%d',
        [Counter, BURST_TASKS]);
  finally
    Pool.Free;
  end;
end;

function BenchmarkSimpleSubmitBurst: QWord;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
  StartedAt: QWord;
  I: Integer;
begin
  Counter := 0;
  Pool := TSimpleThreadPool.Create(4);
  try
    StartedAt := GetTickCount64;
    for I := 1 to BURST_TASKS do
      Task := Pool.Submit(@CountTask);
    if Task = nil then
      raise Exception.Create('Simple Submit returned nil');
    Pool.WaitForAll;
    Result := GetTickCount64 - StartedAt;
    if Counter <> BURST_TASKS then
      raise Exception.CreateFmt('Simple submit burst lost tasks: %d/%d',
        [Counter, BURST_TASKS]);
  finally
    Task := nil;
    Pool.Free;
  end;
end;

function BenchmarkProducerSubmitBurst: QWord;
var
  Pool: TProducerConsumerThreadPool;
  Task: IThreadPoolTask;
  StartedAt: QWord;
  I: Integer;
begin
  Counter := 0;
  Pool := TProducerConsumerThreadPool.Create(4, 1024);
  try
    StartedAt := GetTickCount64;
    for I := 1 to BURST_TASKS do
      Task := Pool.Submit(@CountTask);
    if Task = nil then
      raise Exception.Create('Producer Submit returned nil');
    Pool.WaitForAll;
    Result := GetTickCount64 - StartedAt;
    if Counter <> BURST_TASKS then
      raise Exception.CreateFmt('Producer submit burst lost tasks: %d/%d',
        [Counter, BURST_TASKS]);
  finally
    Task := nil;
    Pool.Free;
  end;
end;

function BenchmarkIndividualIndexedSubmit: QWord;
var
  Pool: TSimpleThreadPool;
  Task: IThreadPoolTask;
  StartedAt: QWord;
  I: Integer;
begin
  Counter := 0;
  Pool := TSimpleThreadPool.Create(4);
  try
    StartedAt := GetTickCount64;
    for I := 0 to RANGE_ITEMS - 1 do
      Task := Pool.Submit(@CountIndexedTask, I);
    if Task = nil then
      raise Exception.Create('Indexed Submit returned nil');
    Pool.WaitForAll;
    Result := GetTickCount64 - StartedAt;
    if Counter <> RANGE_ITEMS then
      raise Exception.CreateFmt('Individual range lost indexes: %d/%d',
        [Counter, RANGE_ITEMS]);
  finally
    Task := nil;
    Pool.Free;
  end;
end;

function BenchmarkSimpleRange: QWord;
var
  Pool: TSimpleThreadPool;
  Batch: IThreadPoolTaskBatch;
  StartedAt: QWord;
begin
  Counter := 0;
  Pool := TSimpleThreadPool.Create(4);
  try
    StartedAt := GetTickCount64;
    Batch := Pool.SubmitRange(@CountIndexedTask, 0, RANGE_ITEMS - 1);
    Batch.WaitFor;
    Result := GetTickCount64 - StartedAt;
    if Counter <> RANGE_ITEMS then
      raise Exception.CreateFmt('Simple range lost indexes: %d/%d',
        [Counter, RANGE_ITEMS]);
  finally
    Batch := nil;
    Pool.Free;
  end;
end;

function BenchmarkProducerRange: QWord;
var
  Pool: TProducerConsumerThreadPool;
  Batch: IThreadPoolTaskBatch;
  StartedAt: QWord;
begin
  Counter := 0;
  Pool := TProducerConsumerThreadPool.Create(4, 1024);
  try
    StartedAt := GetTickCount64;
    Batch := Pool.SubmitRange(@CountIndexedTask, 0, RANGE_ITEMS - 1);
    Batch.WaitFor;
    Result := GetTickCount64 - StartedAt;
    if Counter <> RANGE_ITEMS then
      raise Exception.CreateFmt('Producer range lost indexes: %d/%d',
        [Counter, RANGE_ITEMS]);
  finally
    Batch := nil;
    Pool.Free;
  end;
end;

function BenchmarkSimpleIdle: Double;
var
  Pool: TSimpleThreadPool;
  Total: QWord;
  I: Integer;
begin
  Total := 0;
  Pool := TSimpleThreadPool.Create(4);
  try
    for I := 1 to IDLE_SAMPLES do
    begin
      Sleep(IDLE_PAUSE_MS);
      IdleEvent.ResetEvent;
      SubmittedAt := GetTickCount64;
      Pool.Queue(@IdleTask);
      if IdleEvent.WaitFor(2000) <> wrSignaled then
        raise Exception.Create('Simple idle task timed out');
      Pool.WaitForAll;
      Inc(Total, LastIdleLatency);
    end;
    Result := Total / IDLE_SAMPLES;
  finally
    Pool.Free;
  end;
end;

function BenchmarkProducerIdle: Double;
var
  Pool: TProducerConsumerThreadPool;
  Total: QWord;
  I: Integer;
begin
  Total := 0;
  Pool := TProducerConsumerThreadPool.Create(4, 1024);
  try
    for I := 1 to IDLE_SAMPLES do
    begin
      Sleep(IDLE_PAUSE_MS);
      IdleEvent.ResetEvent;
      SubmittedAt := GetTickCount64;
      Pool.Queue(@IdleTask);
      if IdleEvent.WaitFor(2000) <> wrSignaled then
        raise Exception.Create('Producer idle task timed out');
      Pool.WaitForAll;
      Inc(Total, LastIdleLatency);
    end;
    Result := Total / IDLE_SAMPLES;
  finally
    Pool.Free;
  end;
end;

begin
  IdleEvent := TEvent.Create(nil, True, False, '');
  try
    WriteLn('burst_tasks=', BURST_TASKS);
    WriteLn('range_items=', RANGE_ITEMS);
    WriteLn('idle_samples=', IDLE_SAMPLES);
    WriteLn('simple_burst_ms=', BenchmarkSimpleBurst);
    WriteLn('producer_burst_ms=', BenchmarkProducerBurst);
    WriteLn('simple_submit_burst_ms=', BenchmarkSimpleSubmitBurst);
    WriteLn('producer_submit_burst_ms=', BenchmarkProducerSubmitBurst);
    WriteLn('individual_indexed_submit_ms=', BenchmarkIndividualIndexedSubmit);
    WriteLn('simple_range_ms=', BenchmarkSimpleRange);
    WriteLn('producer_range_ms=', BenchmarkProducerRange);
    WriteLn('simple_idle_avg_ms=', FormatFloat('0.00', BenchmarkSimpleIdle));
    WriteLn('producer_idle_avg_ms=', FormatFloat('0.00', BenchmarkProducerIdle));
  finally
    IdleEvent.Free;
  end;
end.
