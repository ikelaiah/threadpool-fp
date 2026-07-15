program CoordinatedFileBackup;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, ThreadPool.Tasks, ThreadPool.ProducerConsumer;

const
  FILE_COUNT = 18;
  WORKER_COUNT = 4;
  QUEUE_SIZE = 32;
  MISSING_FILE_INDEX = 0;
  COPY_BUFFER_SIZE = 4096;
  BLOCKS_PER_FILE = 16;

type
  TStringArray = array of string;
  TChecksumArray = array of QWord;

  { One job object owns the data used by every callback. It must outlive the
    pool and every task handle that refers to one of its methods. }
  TBackupJob = class
  private
    FRootDirectory: string;
    FSourceDirectory: string;
    FTargetDirectory: string;
    FFileNames: TStringArray;
    FChecksums: TChecksumArray;
    function SourcePath(AIndex: Integer): string;
    function TargetPath(AIndex: Integer): string;
    function ChecksumFile(const AFileName: string): QWord;
  public
    constructor Create;
    procedure CreateDemoFiles;
    procedure CopyAndVerify(AIndex: Integer);
    procedure Cleanup;
    property FileNames: TStringArray read FFileNames;
    property Checksums: TChecksumArray read FChecksums;
  end;

function TaskStateName(AState: TThreadPoolTaskState): string;
begin
  Result := 'unknown';
  case AState of
    ttsPending: Result := 'pending';
    ttsRunning: Result := 'running';
    ttsCompleted: Result := 'completed';
    ttsFailed: Result := 'failed';
    ttsCancelled: Result := 'cancelled';
  end;
end;

constructor TBackupJob.Create;
var
  I: Integer;
begin
  inherited Create;
  FRootDirectory := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'threadpool-fp-backup-' + IntToStr(GetProcessID) + '-' +
    IntToStr(Int64(GetTickCount64));
  FSourceDirectory := IncludeTrailingPathDelimiter(FRootDirectory) + 'source';
  FTargetDirectory := IncludeTrailingPathDelimiter(FRootDirectory) + 'target';
  SetLength(FFileNames, FILE_COUNT);
  SetLength(FChecksums, FILE_COUNT);
  FFileNames[MISSING_FILE_INDEX] := 'required-settings.ini';
  for I := 1 to High(FFileNames) do
    FFileNames[I] := Format('customer-document-%.2d.dat', [I]);
end;

function TBackupJob.SourcePath(AIndex: Integer): string;
begin
  Result := IncludeTrailingPathDelimiter(FSourceDirectory) +
    FFileNames[AIndex];
end;

function TBackupJob.TargetPath(AIndex: Integer): string;
begin
  Result := IncludeTrailingPathDelimiter(FTargetDirectory) +
    FFileNames[AIndex];
end;

procedure TBackupJob.Cleanup;
var
  I: Integer;
begin
  for I := 0 to High(FFileNames) do
  begin
    DeleteFile(SourcePath(I));
    DeleteFile(TargetPath(I));
  end;
  RemoveDir(FSourceDirectory);
  RemoveDir(FTargetDirectory);
  RemoveDir(FRootDirectory);
end;

procedure TBackupJob.CreateDemoFiles;
var
  Buffer: array[0..COPY_BUFFER_SIZE - 1] of Byte;
  OutputFile: TFileStream;
  I, J, Block: Integer;
begin
  Cleanup;
  ForceDirectories(FSourceDirectory);
  ForceDirectories(FTargetDirectory);

  { Index zero is intentionally absent. A backup policy can treat a missing
    required settings file as fatal and cancel files still waiting in line. }
  for I := 1 to High(FFileNames) do
  begin
    for J := 0 to High(Buffer) do
      Buffer[J] := Byte((I * 31 + J) mod 256);

    OutputFile := TFileStream.Create(SourcePath(I), fmCreate);
    try
      for Block := 1 to BLOCKS_PER_FILE do
        OutputFile.WriteBuffer(Buffer, SizeOf(Buffer));
    finally
      OutputFile.Free;
    end;
  end;
end;

function TBackupJob.ChecksumFile(const AFileName: string): QWord;
var
  Buffer: array[0..COPY_BUFFER_SIZE - 1] of Byte;
  InputFile: TFileStream;
  BytesRead, I: Integer;
begin
  Result := 0;
  for I := 0 to High(Buffer) do
    Buffer[I] := 0;
  InputFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    repeat
      BytesRead := InputFile.Read(Buffer, SizeOf(Buffer));
      for I := 0 to BytesRead - 1 do
        Inc(Result, Buffer[I]);
    until BytesRead = 0;
  finally
    InputFile.Free;
  end;
end;

procedure TBackupJob.CopyAndVerify(AIndex: Integer);
var
  Buffer: array[0..COPY_BUFFER_SIZE - 1] of Byte;
  InputFile, OutputFile: TFileStream;
  BytesRead, I: Integer;
  SourceChecksum, TargetChecksum: QWord;
begin
  if not FileExists(SourcePath(AIndex)) then
    raise Exception.CreateFmt('Required source is missing: %s',
      [FFileNames[AIndex]]);

  SourceChecksum := 0;
  for I := 0 to High(Buffer) do
    Buffer[I] := 0;
  InputFile := TFileStream.Create(SourcePath(AIndex),
    fmOpenRead or fmShareDenyNone);
  try
    OutputFile := TFileStream.Create(TargetPath(AIndex), fmCreate);
    try
      repeat
        BytesRead := InputFile.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
        begin
          OutputFile.WriteBuffer(Buffer, BytesRead);
          for I := 0 to BytesRead - 1 do
            Inc(SourceChecksum, Buffer[I]);

          { This models a rate-limited remote backup target. It also keeps
            enough work pending to make cancellation visible in the demo. }
          Sleep(2);
        end;
      until BytesRead = 0;
    finally
      OutputFile.Free;
    end;
  finally
    InputFile.Free;
  end;

  TargetChecksum := ChecksumFile(TargetPath(AIndex));
  if TargetChecksum <> SourceChecksum then
    raise Exception.CreateFmt('Verification failed: %s', [FFileNames[AIndex]]);

  { Each callback owns a distinct array slot, so no lock is needed. }
  FChecksums[AIndex] := TargetChecksum;
end;

function FindFailedTask(const ABatch: IThreadPoolTaskBatch): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ABatch.Count - 1 do
    if ABatch[I].State = ttsFailed then
      Exit(I);
end;

procedure PrintProgress(const ABatch: IThreadPoolTaskBatch);
begin
  WriteLn('Progress: ', ABatch.FinishedCount, '/', ABatch.Count,
    ' finished (failed=', ABatch.FailedCount,
    ', cancelled=', ABatch.CancelledCount, ')');
end;

var
  Pool: TProducerConsumerThreadPool;
  Job: TBackupJob;
  Batch: IThreadPoolTaskBatch;
  Task: IThreadPoolTask;
  FailedIndex, CancelledCount, I: Integer;
begin
  Pool := TProducerConsumerThreadPool.Create(WORKER_COUNT, QUEUE_SIZE);
  Job := TBackupJob.Create;
  try
    Job.CreateDemoFiles;
    Batch := NewThreadPoolTaskBatch;

    WriteLn('Starting a coordinated backup of ', FILE_COUNT, ' files...');
    for I := 0 to FILE_COUNT - 1 do
    begin
      Task := Pool.Submit(@Job.CopyAndVerify, I);
      Batch.Add(Task);
    end;

    { Observe the batch without blocking forever. A real service could update
      a UI, answer a health endpoint, or enforce a deadline in this loop. }
    repeat
      FailedIndex := FindFailedTask(Batch);
      if FailedIndex >= 0 then
      begin
        WriteLn('Critical backup error: ', Batch[FailedIndex].ErrorMessage);
        CancelledCount := Batch.CancelPending;
        WriteLn('Policy response: cancelled ', CancelledCount,
          ' file(s) that had not started.');
        Break;
      end;

      if Batch.WaitFor(10) then
        Break;
      PrintProgress(Batch);
    until False;

    Batch.WaitFor;

    { Cancelled queue entries are harmless tombstones. WaitForAll lets the
      bounded pool consume them before the callback owner is released. }
    Pool.WaitForAll;
    PrintProgress(Batch);

    WriteLn;
    WriteLn('Per-file outcome:');
    for I := 0 to Batch.Count - 1 do
    begin
      Write('  ', Job.FileNames[I], ': ', TaskStateName(Batch[I].State));
      case Batch[I].State of
        ttsCompleted:
          Write(' (checksum ', Job.Checksums[I], ')');
        ttsFailed:
          Write(' (', Batch[I].ErrorMessage, ')');
      end;
      WriteLn;
    end;
  finally
    Pool.WaitForAll;
    Pool.Free;
    Job.Cleanup;
    Job.Free;
  end;
end.
