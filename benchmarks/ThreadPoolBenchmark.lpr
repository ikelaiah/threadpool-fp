program ThreadPoolBenchmark;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  ThreadPool.Simple, ThreadPool.ProducerConsumer;

const
  BURST_TASKS = 20000;
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
    WriteLn('idle_samples=', IDLE_SAMPLES);
    WriteLn('simple_burst_ms=', BenchmarkSimpleBurst);
    WriteLn('producer_burst_ms=', BenchmarkProducerBurst);
    WriteLn('simple_idle_avg_ms=', FormatFloat('0.00', BenchmarkSimpleIdle));
    WriteLn('producer_idle_avg_ms=', FormatFloat('0.00', BenchmarkProducerIdle));
  finally
    IdleEvent.Free;
  end;
end.
