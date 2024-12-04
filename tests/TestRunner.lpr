program TestRunner;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner, ThreadPoolTests;

type
  TMyTestRunner = class(TTestRunner)
  protected
    procedure RunTest(ATest: TTest); override;
  end;

procedure TMyTestRunner.RunTest(ATest: TTest);
begin
  WriteLn('Running test: ', ATest.TestName);
  inherited RunTest(ATest);
  WriteLn('Test completed: ', ATest.TestName);
end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
