program SquareNumbers;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool;

const
  NUMBERS_COUNT = 15;  // Small count for clear output

var
  Numbers: array[0..NUMBERS_COUNT-1] of Integer;
  CalculatedNumbers: array[0..NUMBERS_COUNT-1] of Integer;

// Simple procedure to calculate square of a number
procedure CalculateSquare(index: Integer);
begin
  { Simulate other some work to show the threads process in the non-sequential order,
    showing parallel execution.
    In real applications, you wouldn't need Sleep because:
    - Real calculations take time naturally
    - File I/O operations are naturally slow
    - Network operations have built-in delays
    - Image processing is CPU-intensive }
  Sleep(100);

  // Calculate square and store in CalculatedNumbers
  CalculatedNumbers[index] := Numbers[index] * Numbers[index];

  // Show what's happening
  WriteLn('Thread calculated: ', Numbers[index], ' squared = ', CalculatedNumbers[index]);
end;

var
  i: Integer;
  StartTime: TDateTime;
begin
  WriteLn('Parallel Number Squaring Demo');
  WriteLn('----------------------------');

  // Initialize numbers
  for i := 0 to NUMBERS_COUNT-1 do
    Numbers[i] := i + 1;

  // Initialize results array
  for i := 0 to NUMBERS_COUNT-1 do
    CalculatedNumbers[i] := 0;

  WriteLn('Numbers to process: ');
  for i := 0 to NUMBERS_COUNT-1 do
    Write(Numbers[i], ' ');
  WriteLn;
  WriteLn;

  // Start timing
  StartTime := Now;

  WriteLn('Processing numbers in parallel...');
  WriteLn;

  // Queue each number for processing
  for i := 0 to NUMBERS_COUNT-1 do
    GlobalThreadPool.Queue(@CalculateSquare, i);

  // Wait for all calculations to complete
  GlobalThreadPool.WaitForAll;

  WriteLn;
  WriteLn('All calculations completed in ',
          FormatDateTime('ss.zzz', Now - StartTime), ' seconds');

  WriteLn;
  WriteLn('Final Results:');
  for i := 0 to NUMBERS_COUNT-1 do
    WriteLn(Numbers[i], ' squared = ', CalculatedNumbers[i]);
end.
