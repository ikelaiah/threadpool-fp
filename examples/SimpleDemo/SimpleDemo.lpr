program SimpleDemo;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool;

type
  { TMyClass declaration }
  TMyClass = class
    procedure DoSomething;
    procedure DoSomethingWithIndex(index: Integer);
  end;

{ Simple procedure }
procedure SimpleProc;
begin
  WriteLn('Simple procedure executed');
end;

{ Indexed procedure }
procedure IndexProc(index: Integer);
begin
  WriteLn('Indexed procedure executed with index: ', index);
end;

{ TMyClass implementation }

{ Simple method of a class}
procedure TMyClass.DoSomething;
begin
  WriteLn('Method executed');
end;

{ Method with index of a class }
procedure TMyClass.DoSomethingWithIndex(index: Integer);
begin
  WriteLn('Method with index executed: ', index);
end;

var
  MyObject: TMyClass;
begin
  MyObject := TMyClass.Create;
  try
    WriteLn('Demo of ThreadPool functionality:');
    WriteLn('--------------------------------');
    
    // Queue different types of work
    WriteLn('1. Queueing simple procedure');
    GlobalThreadPool.Queue(@SimpleProc);
    
    WriteLn('2. Queueing method of a class');
    GlobalThreadPool.Queue(@MyObject.DoSomething);
    
    WriteLn('3. Queueing indexed procedure');
    GlobalThreadPool.Queue(@IndexProc, 1);
    
    WriteLn('4. Queueing method with index of a class');
    GlobalThreadPool.Queue(@MyObject.DoSomethingWithIndex, 2);

    WriteLn('--------------------------------');
    WriteLn('Waiting for all tasks to complete...');
    GlobalThreadPool.WaitForAll;
    WriteLn('--------------------------------');
    WriteLn('All tasks completed successfully!');
    
  finally
    MyObject.Free;
  end;
end.
