unit ThreadPool.Internal.WorkItems;

{$mode objfpc}{$H+}{$J-}

interface

uses
  ThreadPool.Types;

type
  { Shared callback work item used by both pool implementations. This unit is
    implementation support; applications should submit callbacks through a
    thread pool instead of constructing work items directly. }
  TThreadPoolCallbackWorkItem = class(TInterfacedObject, IWorkItem)
  private
    FProcedure: TThreadProcedure;
    FMethod: TThreadMethod;
    FProcedureIndex: TThreadProcedureIndex;
    FMethodIndex: TThreadMethodIndex;
    FIndex: Integer;
    FItemType: TWorkItemType;
  public
    constructor Create(AProcedure: TThreadProcedure); overload;
    constructor Create(AMethod: TThreadMethod); overload;
    constructor Create(AProcedure: TThreadProcedureIndex;
      AIndex: Integer); overload;
    constructor Create(AMethod: TThreadMethodIndex;
      AIndex: Integer); overload;
    procedure Execute;
    function GetItemType: Integer;
  end;

implementation

constructor TThreadPoolCallbackWorkItem.Create(AProcedure: TThreadProcedure);
begin
  inherited Create;
  FProcedure := AProcedure;
  FItemType := witProcedure;
end;

constructor TThreadPoolCallbackWorkItem.Create(AMethod: TThreadMethod);
begin
  inherited Create;
  FMethod := AMethod;
  FItemType := witMethod;
end;

constructor TThreadPoolCallbackWorkItem.Create(
  AProcedure: TThreadProcedureIndex; AIndex: Integer);
begin
  inherited Create;
  FProcedureIndex := AProcedure;
  FIndex := AIndex;
  FItemType := witProcedureIndex;
end;

constructor TThreadPoolCallbackWorkItem.Create(AMethod: TThreadMethodIndex;
  AIndex: Integer);
begin
  inherited Create;
  FMethodIndex := AMethod;
  FIndex := AIndex;
  FItemType := witMethodIndex;
end;

procedure TThreadPoolCallbackWorkItem.Execute;
begin
  case FItemType of
    witProcedure:
      if Assigned(FProcedure) then
        FProcedure;
    witMethod:
      if Assigned(FMethod) then
        FMethod;
    witProcedureIndex:
      if Assigned(FProcedureIndex) then
        FProcedureIndex(FIndex);
    witMethodIndex:
      if Assigned(FMethodIndex) then
        FMethodIndex(FIndex);
  end;
end;

function TThreadPoolCallbackWorkItem.GetItemType: Integer;
begin
  Result := Ord(FItemType);
end;

end.
