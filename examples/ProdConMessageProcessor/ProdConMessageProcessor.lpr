program ProdConMessageProcessor;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool.ProducerConsumer;

type
  TMessageHandler = class
  public
    procedure ProcessMessage(Index: Integer);
  end;

procedure TMessageHandler.ProcessMessage(Index: Integer);
var
  Message: string;
begin
  case Index of
    1: Message := 'Hello';
    2: Message := 'Bonjour';
    3: Message := 'Hola';
    4: Message := 'Ciao';
    5: Message := 'Hallo';
    else Message := 'Hi';
  end;

  WriteLn(Format('Thread %d says: %s',
    [TThread.CurrentThread.ThreadID, Message]));
  Sleep(100); // Simulate processing time
end;

var
  Pool: TProducerConsumerThreadPool;
  Handler: TMessageHandler;
  I: Integer;
begin
  Pool := TProducerConsumerThreadPool.Create;
  Handler := TMessageHandler.Create;
  try
    WriteLn('Processing messages...');

    // Queue several messages
    for I := 1 to 5 do
      Pool.Queue(@Handler.ProcessMessage, I);

    // Wait for completion
    Pool.WaitForAll;
    WriteLn('All messages processed!');

  finally
    Handler.Free;
    Pool.Free;
  end;

  // Pause console
  WriteLn('Press Enter to exit...');
  ReadLn; 
end.
