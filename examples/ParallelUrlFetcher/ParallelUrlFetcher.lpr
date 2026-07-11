program ParallelUrlFetcher;

{ Parallel URL fetching with natural error handling (v0.7.0)
  =========================================================
  Fetching many URLs is the classic case for a thread pool: each request spends
  most of its time waiting on the network, so running them concurrently is a
  large, real speed-up.

  The failures here are real network conditions, not simulated ones:
    * a host that does not resolve raises ESocketError (DNS/connection failure)
    * a 4xx/5xx response is detected from the status code and reported as an error
  Both are caught by the pool, so afterwards you can see which URLs failed:

    * OnError    — log each failure as it happens (worker thread)
    * ErrorCount / Errors — the full list of failures after WaitForAll
    * LastError  — the most recent failure

  REQUIREMENTS
  ------------
  * Internet access (the demo contacts real hosts).
  * HTTPS support needs the OpenSSL libraries available at runtime; the
    opensslsockets unit below wires fphttpclient up to them.

  IMPORTANT: each task uses its OWN TFPHTTPClient. A single client instance is
  not safe to share across threads.

  HOW TO COMPILE AND RUN
  ----------------------
  Option A — Free Pascal compiler:
    fpc -Fu../../src ParallelUrlFetcher.lpr && ./ParallelUrlFetcher

  Option B — Lazarus IDE / lazbuild:
    lazbuild ParallelUrlFetcher.lpi && ./ParallelUrlFetcher }

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,  // MUST be first: enables threading support on Unix/Linux
  {$ENDIF}
  Classes, SysUtils, SyncObjs, fphttpclient, opensslsockets,
  ThreadPool.ProducerConsumer;

var
  { Indexed work: a task receives an index into these parallel arrays and writes
    only its own slot, so the result array needs no lock. }
  Urls: array of string;
  Results: array of string;

{ Thread-safe logger for OnError notifications (runs on worker threads). }
type
  TFetchLog = class
  private
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnTaskError(const AMessage: string);  // assigned to Pool.OnError
  end;

constructor TFetchLog.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TFetchLog.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TFetchLog.OnTaskError(const AMessage: string);
begin
  FLock.Enter;
  try
    WriteLn('  [OnError] ', AMessage);
  finally
    FLock.Leave;
  end;
end;

{ The work: GET one URL. Connection/DNS problems raise inside Get; a non-2xx
  status is a real failure we surface ourselves by raising. Either way the pool
  records it against this task. }
procedure FetchUrl(index: Integer);
var
  Client: TFPHTTPClient;
  Body: string;
begin
  Client := TFPHTTPClient.Create(nil);  // one client per task — never shared
  try
    Client.AllowRedirect := True;
    Client.ConnectTimeout := 10000;  // ms
    Client.IOTimeout := 10000;       // ms

    // A bad host or refused connection raises ESocketError here.
    Body := Client.Get(Urls[index]);

    // fphttpclient does not treat 4xx/5xx as exceptions, so we decide here that
    // anything outside 2xx is a failure for this program.
    if (Client.ResponseStatusCode < 200) or (Client.ResponseStatusCode >= 300) then
      raise Exception.CreateFmt('HTTP %d for %s',
        [Client.ResponseStatusCode, Urls[index]]);

    Results[index] := Format('OK (%d) - %d bytes',
      [Client.ResponseStatusCode, Length(Body)]);
  finally
    Client.Free;
  end;
end;

var
  Pool: TProducerConsumerThreadPool;
  Log: TFetchLog;
  i: Integer;
  Msg: string;
begin
  WriteLn('=== Parallel URL fetcher (v0.7.0 error handling) ===');
  WriteLn;

  // A mix of good and bad URLs so both success and the two failure modes show.
  Urls := [
    'https://example.com',
    'https://www.iana.org/help/example-domains',
    'https://httpbin.org/status/404',                 // real 4xx response
    'https://no-such-host.invalid/'                   // real DNS failure
  ];
  SetLength(Results, Length(Urls));

  Pool := TProducerConsumerThreadPool.Create;
  Log := TFetchLog.Create;
  try
    Pool.OnError := @Log.OnTaskError;

    WriteLn('Fetching ', Length(Urls), ' URLs across ', Pool.ThreadCount,
            ' worker threads...');
    for i := 0 to High(Urls) do
      Pool.Queue(@FetchUrl, i);

    Pool.WaitForAll;
    WriteLn;

    WriteLn('Results:');
    for i := 0 to High(Urls) do
      if Results[i] <> '' then
        WriteLn(Format('  %-40s %s', [Urls[i], Results[i]]))
      else
        WriteLn(Format('  %-40s FAILED', [Urls[i]]));
    WriteLn;

    WriteLn(Format('%d request(s) failed:', [Pool.ErrorCount]));
    for Msg in Pool.Errors do
      WriteLn('  - ', Msg);
    if Pool.ErrorCount > 0 then
      WriteLn('LastError: ', Pool.LastError);

    Pool.ClearErrors;
  finally
    Log.Free;
    Pool.Free;
  end;

  WriteLn;
  WriteLn('Press enter to quit ...');
  ReadLn;
end.
