program ProdConSimpleDemo;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,  // MUST be first: enables threading support on Unix/Linux
  {$ENDIF}
  Classes, SysUtils, ThreadPool.ProducerConsumer;

type
  { TMyClass declaration }
  TMyClass = class
    procedure DoSomething;
    procedure DoSomethingWithIndex(index: Integer);
  end;

{ Simple procedure }
procedure SimpleProc;
begin
  WriteLn('Simple procedure executed in thread: ', GetCurrentThreadId);
end;

{ Indexed procedure }
procedure IndexProc(index: Integer);
begin
  WriteLn('Indexed procedure executed with index: ', index,
          ' in thread: ', GetCurrentThreadId);
end;

{ TMyClass implementation }

{ Simple method of a class}
procedure TMyClass.DoSomething;
begin
  WriteLn('Method executed in thread: ', GetCurrentThreadId);
end;

{ Method with index of a class }
procedure TMyClass.DoSomethingWithIndex(index: Integer);
begin
  WriteLn('Method with index executed: ', index,
          ' in thread: ', GetCurrentThreadId);
end;

var
  MyObject: TMyClass;
  Pool: TProducerConsumerThreadPool;
begin
  MyObject := TMyClass.Create;
  Pool := TProducerConsumerThreadPool.Create;  // Uses CPU count for threads
  try
    WriteLn('Demo of Producer-Consumer ThreadPool:');
    WriteLn('--------------------------------');

    // Queue different types of work
    WriteLn('1. Queueing simple procedure');
    Pool.Queue(@SimpleProc);

    // WARNING: MyObject must NOT be freed before Pool.WaitForAll returns.
    // Worker threads call MyObject's methods asynchronously — freeing early
    // causes an access violation. The try/finally below ensures correct order.
    WriteLn('2. Queueing method of a class');
    Pool.Queue(@MyObject.DoSomething);

    WriteLn('3. Queueing indexed procedure');
    Pool.Queue(@IndexProc, 1);

    WriteLn('4. Queueing method with index of a class');
    Pool.Queue(@MyObject.DoSomethingWithIndex, 2);

    WriteLn('--------------------------------');
    WriteLn('Waiting for all tasks to complete...');
    Pool.WaitForAll;  // MUST be called before freeing Pool or MyObject (see finally)
    WriteLn('--------------------------------');
    WriteLn('All tasks completed successfully!');

    // Check for any errors — only the LAST exception is stored in LastError
    if Pool.LastError <> '' then
      WriteLn('Error occurred: ', Pool.LastError);

  finally
    Pool.Free;        // Safe: WaitForAll has already returned above
    MyObject.Free;    // Safe: all method calls have completed
  end;

  // Pause console
  WriteLn('Press enter to quit ...');
  ReadLn;
end.
