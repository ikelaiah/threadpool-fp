program ParallelLogAnalyzer;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, ThreadPool.Tasks, ThreadPool.Simple;

const
  ENTRY_COUNT = 100000;
  ENDPOINT_COUNT = 4;
  INVALID_ENTRY_INDEX = 12345;
  ENDPOINT_NAMES: array[0..ENDPOINT_COUNT - 1] of string = (
    '/catalog', '/checkout', '/account', '/health');

type
  TAccessLogEntry = record
    EndpointIndex: Integer;
    StatusCode: Integer;
    LatencyMS: Integer;
    BytesSent: Integer;
  end;

  TEntryAnalysis = record
    IsValid: Boolean;
    IsError: Boolean;
    IsSlow: Boolean;
  end;

  TEndpointSummary = record
    RequestCount: Integer;
    ErrorCount: Integer;
    SlowCount: Integer;
    BytesSent: Int64;
  end;

  TAccessLogEntryArray = array of TAccessLogEntry;
  TEntryAnalysisArray = array of TEntryAnalysis;
  TEndpointSummaryArray = array of TEndpointSummary;

  TLogAnalysisJob = class
  private
    FEntries: TAccessLogEntryArray;
    FAnalysis: TEntryAnalysisArray;
    FSummaries: TEndpointSummaryArray;
    FInvalidCount: Integer;
  public
    constructor Create(AEntryCount: Integer);
    procedure AnalyzeEntry(AIndex: Integer);
    procedure SummarizeEndpoint(AEndpointIndex: Integer);
    procedure CountInvalidEntries;
    procedure PrintReport;
  end;

constructor TLogAnalysisJob.Create(AEntryCount: Integer);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FEntries, AEntryCount);
  SetLength(FAnalysis, AEntryCount);
  SetLength(FSummaries, ENDPOINT_COUNT);

  { Generate deterministic parsed log records so the example is fast,
    dependency-free, and produces the same report on every platform. }
  for I := 0 to High(FEntries) do
  begin
    FEntries[I].EndpointIndex := I mod ENDPOINT_COUNT;
    FEntries[I].LatencyMS := 20 + ((I * 37) mod 900);
    FEntries[I].BytesSent := 300 + ((I * 53) mod 20000);

    if (I mod 97) = 0 then
      FEntries[I].StatusCode := 500
    else if (I mod 31) = 0 then
      FEntries[I].StatusCode := 404
    else
      FEntries[I].StatusCode := 200;
  end;

  { A malformed line in a real access log should be reported without losing
    the other records that happen to share its range chunk. }
  FEntries[INVALID_ENTRY_INDEX].StatusCode := 0;
end;

procedure TLogAnalysisJob.AnalyzeEntry(AIndex: Integer);
var
  Entry: TAccessLogEntry;
begin
  Entry := FEntries[AIndex];
  FAnalysis[AIndex].IsValid :=
    (Entry.EndpointIndex >= 0) and
    (Entry.EndpointIndex < ENDPOINT_COUNT) and
    (Entry.StatusCode >= 100) and (Entry.StatusCode <= 599) and
    (Entry.LatencyMS >= 0) and (Entry.BytesSent >= 0);

  if FAnalysis[AIndex].IsValid then
  begin
    FAnalysis[AIndex].IsError := Entry.StatusCode >= 400;
    FAnalysis[AIndex].IsSlow := Entry.LatencyMS >= 500;
  end;
end;

procedure TLogAnalysisJob.SummarizeEndpoint(AEndpointIndex: Integer);
var
  I: Integer;
  Summary: TEndpointSummary;
begin
  Summary.RequestCount := 0;
  Summary.ErrorCount := 0;
  Summary.SlowCount := 0;
  Summary.BytesSent := 0;
  for I := 0 to High(FEntries) do
    if FAnalysis[I].IsValid and
      (FEntries[I].EndpointIndex = AEndpointIndex) then
    begin
      Inc(Summary.RequestCount);
      if FAnalysis[I].IsError then
        Inc(Summary.ErrorCount);
      if FAnalysis[I].IsSlow then
        Inc(Summary.SlowCount);
      Inc(Summary.BytesSent, FEntries[I].BytesSent);
    end;

  { Each report task writes one distinct slot. All analysis slots are now
    read-only because the range batch below is used as a phase barrier. }
  FSummaries[AEndpointIndex] := Summary;
end;

procedure TLogAnalysisJob.CountInvalidEntries;
var
  I: Integer;
begin
  FInvalidCount := 0;
  for I := 0 to High(FAnalysis) do
    if not FAnalysis[I].IsValid then
      Inc(FInvalidCount);
end;

procedure TLogAnalysisJob.PrintReport;
var
  I: Integer;
begin
  WriteLn;
  WriteLn('Access-log report');
  WriteLn('-----------------');
  for I := 0 to ENDPOINT_COUNT - 1 do
    WriteLn(Format('%-10s requests=%6d  errors=%4d  slow=%5d  bytes=%d',
      [ENDPOINT_NAMES[I], FSummaries[I].RequestCount,
       FSummaries[I].ErrorCount, FSummaries[I].SlowCount,
       FSummaries[I].BytesSent]));
  WriteLn('Malformed records: ', FInvalidCount);
end;

var
  Pool: TSimpleThreadPool;
  Job: TLogAnalysisJob;
  AnalysisBatch, ReportBatch: IThreadPoolTaskBatch;
  ReportTasks: array[0..ENDPOINT_COUNT - 1] of IThreadPoolTask;
  InvalidCountTask: IThreadPoolTask;
  StartedAt: QWord;
  I: Integer;
begin
  Pool := TSimpleThreadPool.Create(4);
  Job := TLogAnalysisJob.Create(ENTRY_COUNT);
  try
    { Phase 1: automatic chunking keeps queue traffic low while preserving a
      task handle for each chunk. Range bounds are inclusive. }
    StartedAt := GetTickCount64;
    AnalysisBatch := Pool.SubmitRange(@Job.AnalyzeEntry, 0,
      ENTRY_COUNT - 1);
    WriteLn('Analyzing ', ENTRY_COUNT, ' records in ', AnalysisBatch.Count,
      ' chunks...');
    while not AnalysisBatch.WaitFor(10) do
      WriteLn('Analysis progress: ', AnalysisBatch.FinishedCount, '/',
        AnalysisBatch.Count, ' chunks');

    if AnalysisBatch.FailedCount > 0 then
      raise Exception.CreateFmt('Analysis failed in %d chunk(s)',
        [AnalysisBatch.FailedCount]);
    WriteLn('Analysis phase complete in ', GetTickCount64 - StartedAt, ' ms.');

    { Phase 2 starts only after phase 1. Independent report sections and the
      validation count can then scan the immutable analysis in parallel. }
    ReportBatch := NewThreadPoolTaskBatch;
    for I := 0 to ENDPOINT_COUNT - 1 do
    begin
      ReportTasks[I] := Pool.Submit(@Job.SummarizeEndpoint, I);
      ReportBatch.Add(ReportTasks[I]);
    end;
    InvalidCountTask := Pool.Submit(@Job.CountInvalidEntries);
    ReportBatch.Add(InvalidCountTask);

    ReportBatch.WaitFor;
    if ReportBatch.FailedCount > 0 then
      raise Exception.CreateFmt('Report failed in %d section(s)',
        [ReportBatch.FailedCount]);

    WriteLn('Report sections coordinated: ', ReportBatch.FinishedCount, '/',
      ReportBatch.Count);
    Job.PrintReport;
  finally
    Pool.WaitForAll;
    Pool.Free;
    Job.Free;
  end;
end.
