program ProdConSquareNumbers;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool.ProducerConsumer, SyncObjs;

const
  ARRAY_SIZE = 2000;

type
  TNumberArray = array of Integer;

var
  Numbers: TNumberArray;  // Input numbers
  Squares: TNumberArray;  // Results
  Lock: TCriticalSection;
  ProcessedCount: Integer;

// Process a number at given index
procedure SquareNumber(Index: Integer);
var
  Square: Integer;
  LocalCount: Integer;
begin
  // Calculate square
  Square := Numbers[Index] * Numbers[Index];

  // Store result thread-safely
  Lock.Enter;
  try
    Squares[Index] := Square;
    Inc(ProcessedCount);
    LocalCount := ProcessedCount;  // Get current count safely
  finally
    Lock.Leave;
  end;

  // Show progress every 1000 items
  if LocalCount mod 1000 = 0 then
    WriteLn(Format('Progress: %d/%d', [LocalCount, ARRAY_SIZE]));
end;

var
  Pool: TProducerConsumerThreadPool;
  I: Integer;

// Main program
begin
  ProcessedCount := 0;  // Initialize counter
  // Initialize arrays
  SetLength(Numbers, ARRAY_SIZE);
  SetLength(Squares, ARRAY_SIZE);
  Lock := TCriticalSection.Create;

  try
    // Fill input array with numbers 1 to 200
    for I := 0 to ARRAY_SIZE - 1 do
      Numbers[I] := I + 1;

    // Create thread pool
    Pool := TProducerConsumerThreadPool.Create;
    try
      WriteLn('Processing squares of numbers 1 to ', ARRAY_SIZE);

      // Queue all numbers for processing
      for I := 0 to ARRAY_SIZE - 1 do
      begin
        try
          Pool.Queue(@SquareNumber, I);
        except
          on E: Exception do
          begin
            if E.Message.Contains('Queue is full') then
            begin
              // In case the queue is full, wait, then retry again
              WriteLn('Queue full, waiting...');
              Pool.WaitForAll;  // Wait for queue to clear
              Pool.Queue(@SquareNumber, I);  // Try again
            end
            else
              raise;
          end;
        end;
      end;

      // Wait for all calculations to complete
      Pool.WaitForAll;

      // Show some results
      WriteLn('Some results:');
      for I := 0 to 9 do  // Show first 10 numbers
        WriteLn(Format('%d² = %d', [Numbers[I], Squares[I]]));
      WriteLn('...');
      for I := ARRAY_SIZE - 10 to ARRAY_SIZE - 1 do  // Show last 10 numbers
        WriteLn(Format('%d² = %d', [Numbers[I], Squares[I]]));

    finally
      Pool.Free;
    end;

  finally
    Lock.Free;
  end;

  // Pause console
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
