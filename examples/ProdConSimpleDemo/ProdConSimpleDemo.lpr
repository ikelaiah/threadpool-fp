program ProdConSimpleDemo;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  ThreadPool.ProducerConsumer;

  procedure DoWork;
  begin
    WriteLn('Working in thread: ', GetCurrentThreadId);
  end;

var
  Pool: TProducerConsumerThreadPool;

begin
  Pool := TProducerConsumerThreadPool.Create;  // Uses CPU count for threads
  try
    // Queue some work
    Pool.Queue(@DoWork);
    Pool.Queue(@DoWork);
    Pool.Queue(@DoWork);

    // Wait for all tasks to complete
    Pool.WaitForAll;
  finally
    Pool.Free;
  end;

  // Pause console
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
