program ParallelFileHasher;

{ Parallel file hashing with natural error handling (v0.7.0)
  =========================================================
  A realistic use of a thread pool: compute the MD5 of many files at once.
  Hashing is CPU- and I/O-bound, so spreading it across worker threads is a
  genuine win — unlike toy demos that just Sleep().

  The error handling here is NOT simulated. A worker simply opens each file and
  hashes it. If a file is missing, locked, or unreadable, TFileStream.Create
  raises EFOpenError on the worker thread. The pool catches that real exception
  and records it, so afterwards you can see exactly which files failed and why:

    * OnError    — log each failure the moment it happens (worker thread)
    * ErrorCount — how many files could not be hashed
    * Errors     — the message for every failed file
    * LastError  — the most recent failure

  To make the demo self-contained it creates a few sample files, then queues
  those PLUS one path that does not exist — producing a real, not faked, error.

  HOW TO COMPILE AND RUN
  ----------------------
  Option A — Free Pascal compiler:
    fpc -Fu../../src ParallelFileHasher.lpr && ./ParallelFileHasher

  Option B — Lazarus IDE / lazbuild:
    lazbuild ParallelFileHasher.lpi && ./ParallelFileHasher }

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,  // MUST be first: enables threading support on Unix/Linux
  {$ENDIF}
  Classes, SysUtils, SyncObjs, md5, ThreadPool.Simple;

const
  SAMPLE_COUNT = 4;

var
  { Work is addressed by index: a task receives an index into these parallel
    arrays. Each task writes only its OWN slot, so the result array needs no
    lock (the array is never resized while tasks run). }
  Files: array of string;
  Hashes: array of string;

{ Logs failures as they happen. Runs on a worker thread, so the WriteLn is
  guarded by a critical section to keep lines from interleaving. }
type
  TFailureLog = class
  private
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnTaskError(const AMessage: string);  // assigned to Pool.OnError
  end;

constructor TFailureLog.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TFailureLog.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TFailureLog.OnTaskError(const AMessage: string);
begin
  FLock.Enter;
  try
    WriteLn('  [OnError] ', AMessage);
  finally
    FLock.Leave;
  end;
end;

{ The actual work: hash one file, streaming it in chunks so large files do not
  have to fit in memory. Any I/O problem raises here and the pool captures it. }
procedure HashFile(index: Integer);
var
  Stream: TFileStream;
  Ctx: TMD5Context;
  Digest: TMD5Digest;
  Buffer: array[0..65535] of Byte;
  BytesRead: Integer;
begin
  // A missing or locked file raises EFOpenError right here — a natural failure.
  Stream := TFileStream.Create(Files[index], fmOpenRead or fmShareDenyWrite);
  try
    MD5Init(Ctx);
    repeat
      BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        MD5Update(Ctx, Buffer, BytesRead);
    until BytesRead = 0;
    MD5Final(Ctx, Digest);
    Hashes[index] := MD5Print(Digest);
  finally
    Stream.Free;
  end;
end;

{ Create a handful of real files so the demo needs no external data. }
procedure CreateSampleFiles;
var
  i, j: Integer;
  F: TextFile;
begin
  for i := 0 to SAMPLE_COUNT - 1 do
  begin
    AssignFile(F, Files[i]);
    Rewrite(F);
    try
      for j := 0 to i do
        WriteLn(F, 'Sample line ', j, ' for ', Files[i]);
    finally
      CloseFile(F);
    end;
  end;
end;

procedure DeleteSampleFiles;
var
  i: Integer;
begin
  for i := 0 to SAMPLE_COUNT - 1 do
    DeleteFile(Files[i]);
end;

var
  Pool: TSimpleThreadPool;
  Log: TFailureLog;
  i: Integer;
  Msg: string;
begin
  WriteLn('=== Parallel file hasher (v0.7.0 error handling) ===');
  WriteLn;

  // Build the work list: SAMPLE_COUNT real files, plus one that does not exist.
  SetLength(Files, SAMPLE_COUNT + 1);
  for i := 0 to SAMPLE_COUNT - 1 do
    Files[i] := Format('hash_sample_%d.txt', [i]);
  Files[SAMPLE_COUNT] := 'this_file_does_not_exist.txt';  // will fail naturally
  SetLength(Hashes, Length(Files));

  CreateSampleFiles;

  Pool := TSimpleThreadPool.Create(4);
  Log := TFailureLog.Create;
  try
    Pool.OnError := @Log.OnTaskError;

    WriteLn('Hashing ', Length(Files), ' files across ', Pool.ThreadCount,
            ' worker threads...');
    for i := 0 to High(Files) do
      Pool.Queue(@HashFile, i);

    Pool.WaitForAll;
    WriteLn;

    // Results: a file with an empty hash is one that failed.
    WriteLn('Results:');
    for i := 0 to High(Files) do
      if Hashes[i] <> '' then
        WriteLn(Format('  OK    %s  %s', [Hashes[i], Files[i]]))
      else
        WriteLn(Format('  FAIL  %-32s  %s', ['(not hashed)', Files[i]]));
    WriteLn;

    // Aggregate view from the pool's error API.
    WriteLn(Format('%d file(s) failed:', [Pool.ErrorCount]));
    for Msg in Pool.Errors do
      WriteLn('  - ', Msg);
    if Pool.ErrorCount > 0 then
      WriteLn('LastError: ', Pool.LastError);

    Pool.ClearErrors;  // ready for reuse
  finally
    Log.Free;
    Pool.Free;
    DeleteSampleFiles;
  end;

  WriteLn;
  WriteLn('Press enter to quit ...');
  ReadLn;
end.
