program SimpleThreadpoolDemo;

{$mode objfpc}{$H+}{$J-}  // Standard FPC mode directives

uses
  Classes,           // For TThread and other basic classes
  SysUtils,          // For string handling and formatting
  ThreadPool.Simple, // Our thread pool implementation
  syncobjs;          // For TCriticalSection and synchronization

type
  { TDataProcessor - Example class showing thread-safe processing }
  TDataProcessor = class
  private
    FCS: TCriticalSection;      // Protects access to shared resources
    FProcessedCount: Integer;   // Counter of processed items (shared resource)
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessItem(index: Integer);
    property ProcessedCount: Integer read FProcessedCount;
  end;

{ Simple procedure to demonstrate basic thread execution }
procedure SimpleProcedure;
begin
  WriteLn('Hello from thread #', ThreadID);  // ThreadID is provided by the system
end;

{ Procedure with parameter to demonstrate indexed operations }
procedure ProcessWithIndex(AIndex: Integer);
begin
  WriteLn('Processing item: ', AIndex);
end;

{ TDataProcessor implementation }

constructor TDataProcessor.Create;
begin
  inherited Create;
  FCS := TCriticalSection.Create;    // Create synchronization object
  FProcessedCount := 0;              // Initialize counter
end;

destructor TDataProcessor.Destroy;
begin
  FCS.Free;      // Clean up synchronization object
  inherited;     // Always call inherited destructor last
end;

procedure TDataProcessor.ProcessItem(index: Integer);
begin
  Sleep(100);    // Simulate some work (not needed in real code)
  
  // Thread-safe increment of counter
  FCS.Enter;     // Lock the critical section
  try
    Inc(FProcessedCount);
    WriteLn(Format('Processed item %d (Total: %d)', [index, FProcessedCount]));
  finally
    FCS.Leave;   // Always unlock in finally block
  end;
end;

var
  Processor: TDataProcessor;
  CustomPool: TSimpleThreadPool;
  i: Integer;
begin
  WriteLn('ThreadPool Basic Usage Demo');
  WriteLn('-------------------------');

  { Example 1: Using the global thread pool (simplest approach) }
  WriteLn('1. Using GlobalThreadPool:');
  GlobalThreadPool.Queue(@SimpleProcedure);    // Queue a simple procedure
  GlobalThreadPool.WaitForAll;                 // Wait for completion

  { Example 2: Using global pool with indexed operations }
  WriteLn('2. Using GlobalThreadPool with index:');
  for i := 1 to 3 do
    GlobalThreadPool.Queue(@ProcessWithIndex, i);  // Queue with parameter
  GlobalThreadPool.WaitForAll;

  { Example 3: Using custom thread pool for more control }
  WriteLn('3. Using TSimpleThreadPool:');
  CustomPool := TSimpleThreadPool.Create(4);   // Create pool with 4 threads
  Processor := TDataProcessor.Create;
  try
    // Queue multiple items for parallel processing
    for i := 1 to 5 do
      CustomPool.Queue(@Processor.ProcessItem, i);
    CustomPool.WaitForAll;
    
    WriteLn(Format('Total items processed: %d', [Processor.ProcessedCount]));
  finally
    // Clean up (always in reverse order of creation)
    Processor.Free;
    CustomPool.Free;
  end;

  WriteLn('Press Enter to exit...');
  ReadLn;
end.