unit ThreadPool.Tasks;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, SyncObjs, ThreadPool.Types;

type
  EThreadPoolDeadlock = class(Exception);
  EThreadPoolTaskSubmission = class(Exception);

  { Observable lifecycle of one submitted task. }
  TThreadPoolTaskState = (
    ttsPending,
    ttsRunning,
    ttsCompleted,
    ttsFailed,
    ttsCancelled
  );

  IThreadPoolTask = interface
    ['{D5E86E1A-9585-4CC3-90AD-76C1B34CD9F3}']
    procedure WaitFor; overload;
    function WaitFor(ATimeoutMS: Cardinal): Boolean; overload;
    function Cancel: Boolean;
    function GetState: TThreadPoolTaskState;
    function GetErrorMessage: string;
    function GetIsFinished: Boolean;
    property State: TThreadPoolTaskState read GetState;
    property ErrorMessage: string read GetErrorMessage;
    property IsFinished: Boolean read GetIsFinished;
  end;

  TThreadPoolTaskArray = array of IThreadPoolTask;

  { Internal admission callback used by the shared range builder. }
  TThreadPoolWorkItemSubmit = function(
    const AWorkItem: IWorkItem): Boolean of object;

  { A thread-safe collection of task handles. WaitFor operates on a snapshot
    taken when the call begins, so producers may continue adding tasks without
    changing an in-progress wait. }
  IThreadPoolTaskBatch = interface
    ['{B593B216-E9A4-4242-9A6B-F3CB6B4AFBE8}']
    procedure Add(const ATask: IThreadPoolTask);
    procedure WaitFor; overload;
    function WaitFor(ATimeoutMS: Cardinal): Boolean; overload;
    function CancelPending: Integer;
    function GetCount: Integer;
    function GetFinishedCount: Integer;
    function GetFailedCount: Integer;
    function GetCancelledCount: Integer;
    function GetTask(AIndex: Integer): IThreadPoolTask;
    property Count: Integer read GetCount;
    property FinishedCount: Integer read GetFinishedCount;
    property FailedCount: Integer read GetFailedCount;
    property CancelledCount: Integer read GetCancelledCount;
    property Tasks[AIndex: Integer]: IThreadPoolTask read GetTask; default;
  end;

  { Optional v0.9 capability interface. IThreadPool remains unchanged so
    existing third-party implementations retain source compatibility. }
  IThreadPoolTaskSource = interface
    ['{2898DB24-5D5D-4CF3-9A1B-BDA89D4127A8}']
    function Submit(AProcedure: TThreadProcedure): IThreadPoolTask; overload;
    function Submit(AMethod: TThreadMethod): IThreadPoolTask; overload;
    function Submit(AProcedure: TThreadProcedureIndex;
      AIndex: Integer): IThreadPoolTask; overload;
    function Submit(AMethod: TThreadMethodIndex;
      AIndex: Integer): IThreadPoolTask; overload;
    function TrySubmit(AProcedure: TThreadProcedure; ATimeoutMS: Cardinal;
      out ATask: IThreadPoolTask): Boolean; overload;
    function TrySubmit(AMethod: TThreadMethod; ATimeoutMS: Cardinal;
      out ATask: IThreadPoolTask): Boolean; overload;
    function TrySubmit(AProcedure: TThreadProcedureIndex; AIndex: Integer;
      ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean; overload;
    function TrySubmit(AMethod: TThreadMethodIndex; AIndex: Integer;
      ATimeoutMS: Cardinal; out ATask: IThreadPoolTask): Boolean; overload;
    function SubmitRange(AProcedure: TThreadProcedureIndex;
      AFirstIndex, ALastIndex: Integer;
      AChunkSize: Integer = 0): IThreadPoolTaskBatch; overload;
    function SubmitRange(AMethod: TThreadMethodIndex;
      AFirstIndex, ALastIndex: Integer;
      AChunkSize: Integer = 0): IThreadPoolTaskBatch; overload;
  end;

  { Internal worker bridge. It is separate from IWorkItem so the v0.8
    interface and its GUID do not change. }
  IThreadPoolTrackedWorkItem = interface
    ['{9B013788-41B1-4C03-95E1-5DA8A9E80904}']
    function TryStart: Boolean;
    procedure MarkCompleted;
    procedure MarkFailed(const AMessage: string);
  end;

function NewThreadPoolTaskBatch: IThreadPoolTaskBatch;

{ Internal factories shared by the two pool implementations. }
function NewTrackedWorkItem(AProcedure: TThreadProcedure;
  out ATask: IThreadPoolTask): IWorkItem; overload;
function NewTrackedWorkItem(AMethod: TThreadMethod;
  out ATask: IThreadPoolTask): IWorkItem; overload;
function NewTrackedWorkItem(AProcedure: TThreadProcedureIndex;
  AIndex: Integer; out ATask: IThreadPoolTask): IWorkItem; overload;
function NewTrackedWorkItem(AMethod: TThreadMethodIndex;
  AIndex: Integer; out ATask: IThreadPoolTask): IWorkItem; overload;
function NewTrackedRangeWorkItem(AProcedure: TThreadProcedureIndex;
  AFirstIndex, ALastIndex: Integer;
  out ATask: IThreadPoolTask): IWorkItem; overload;
function NewTrackedRangeWorkItem(AMethod: TThreadMethodIndex;
  AFirstIndex, ALastIndex: Integer;
  out ATask: IThreadPoolTask): IWorkItem; overload;
function GetThreadPoolRangeChunkSize(AFirstIndex, ALastIndex,
  AThreadCount, ARequestedChunkSize: Integer): Int64;
procedure ExecuteThreadPoolWorkItem(const AWorkItem: IWorkItem;
  AOnError: TThreadPoolErrorEvent);
function NewThreadPoolRangeBatch(AProcedure: TThreadProcedureIndex;
  AFirstIndex, ALastIndex, AThreadCount, AChunkSize: Integer;
  ASubmit: TThreadPoolWorkItemSubmit): IThreadPoolTaskBatch; overload;
function NewThreadPoolRangeBatch(AMethod: TThreadMethodIndex;
  AFirstIndex, ALastIndex, AThreadCount, AChunkSize: Integer;
  ASubmit: TThreadPoolWorkItemSubmit): IThreadPoolTaskBatch; overload;

implementation

type
  TThreadPoolTaskControl = class(TInterfacedObject, IThreadPoolTask)
  private
    FLock: TCriticalSection;
    FCompletionEvent: TEvent;
    FState: TThreadPoolTaskState;
    FErrorMessage: string;
    FRunningThreadID: TThreadID;
    procedure SignalCompletion;
    class function IsTerminal(AState: TThreadPoolTaskState): Boolean; static;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WaitFor; overload;
    function WaitFor(ATimeoutMS: Cardinal): Boolean; overload;
    function Cancel: Boolean;
    function GetState: TThreadPoolTaskState;
    function GetErrorMessage: string;
    function GetIsFinished: Boolean;
    function TryStart: Boolean;
    procedure MarkCompleted;
    procedure MarkFailed(const AMessage: string);
  end;

  TTrackedWorkItem = class(TInterfacedObject, IWorkItem,
    IThreadPoolTrackedWorkItem)
  private
    FProcedure: TThreadProcedure;
    FMethod: TThreadMethod;
    FProcedureIndex: TThreadProcedureIndex;
    FMethodIndex: TThreadMethodIndex;
    FIndex: Integer;
    FFirstIndex: Integer;
    FLastIndex: Integer;
    FItemType: TWorkItemType;
    FIsRange: Boolean;
    FTask: IThreadPoolTask;
    FControl: TThreadPoolTaskControl;
  public
    constructor Create(AControl: TThreadPoolTaskControl;
      const ATask: IThreadPoolTask);
    procedure Execute;
    function GetItemType: Integer;
    function TryStart: Boolean;
    procedure MarkCompleted;
    procedure MarkFailed(const AMessage: string);
  end;

  TThreadPoolTaskBatch = class(TInterfacedObject, IThreadPoolTaskBatch)
  private
    FLock: TCriticalSection;
    FTasks: TThreadPoolTaskArray;
    FCount: Integer;
    function Snapshot: TThreadPoolTaskArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ATask: IThreadPoolTask);
    procedure WaitFor; overload;
    function WaitFor(ATimeoutMS: Cardinal): Boolean; overload;
    function CancelPending: Integer;
    function GetCount: Integer;
    function GetFinishedCount: Integer;
    function GetFailedCount: Integer;
    function GetCancelledCount: Integer;
    function GetTask(AIndex: Integer): IThreadPoolTask;
  end;

{ TThreadPoolTaskControl }

constructor TThreadPoolTaskControl.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCompletionEvent := nil;
  FState := ttsPending;
  FErrorMessage := '';
  FRunningThreadID := 0;
end;

destructor TThreadPoolTaskControl.Destroy;
begin
  FCompletionEvent.Free;
  FLock.Free;
  inherited;
end;

class function TThreadPoolTaskControl.IsTerminal(
  AState: TThreadPoolTaskState): Boolean;
begin
  Result := AState in [ttsCompleted, ttsFailed, ttsCancelled];
end;

procedure TThreadPoolTaskControl.SignalCompletion;
begin
  { FLock must be held so a waiter cannot miss the transition while lazily
    creating the event. }
  if Assigned(FCompletionEvent) then
    FCompletionEvent.SetEvent;
end;

procedure TThreadPoolTaskControl.WaitFor;
begin
  WaitFor(THREADPOOL_INFINITE);
end;

function TThreadPoolTaskControl.WaitFor(ATimeoutMS: Cardinal): Boolean;
var
  CompletionEvent: TEvent;
begin
  CompletionEvent := nil;
  FLock.Enter;
  try
    if IsTerminal(FState) then
      Exit(True);
    if (FState = ttsRunning) and
      (FRunningThreadID = GetCurrentThreadID) then
      raise EThreadPoolDeadlock.Create(
        'A task cannot wait for its own completion');
    if not Assigned(FCompletionEvent) then
      FCompletionEvent := TEvent.Create(nil, True, False, '');
    CompletionEvent := FCompletionEvent;
  finally
    FLock.Leave;
  end;

  Result := CompletionEvent.WaitFor(ATimeoutMS) = wrSignaled;
end;

function TThreadPoolTaskControl.Cancel: Boolean;
begin
  FLock.Enter;
  try
    Result := FState = ttsPending;
    if Result then
    begin
      FState := ttsCancelled;
      SignalCompletion;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadPoolTaskControl.GetState: TThreadPoolTaskState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

function TThreadPoolTaskControl.GetErrorMessage: string;
begin
  FLock.Enter;
  try
    Result := FErrorMessage;
  finally
    FLock.Leave;
  end;
end;

function TThreadPoolTaskControl.GetIsFinished: Boolean;
begin
  FLock.Enter;
  try
    Result := IsTerminal(FState);
  finally
    FLock.Leave;
  end;
end;

function TThreadPoolTaskControl.TryStart: Boolean;
begin
  FLock.Enter;
  try
    Result := FState = ttsPending;
    if Result then
    begin
      FState := ttsRunning;
      FRunningThreadID := GetCurrentThreadID;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolTaskControl.MarkCompleted;
begin
  FLock.Enter;
  try
    if FState = ttsRunning then
    begin
      FState := ttsCompleted;
      FRunningThreadID := 0;
      SignalCompletion;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolTaskControl.MarkFailed(const AMessage: string);
begin
  FLock.Enter;
  try
    if FState = ttsRunning then
    begin
      FErrorMessage := AMessage;
      FState := ttsFailed;
      FRunningThreadID := 0;
      SignalCompletion;
    end;
  finally
    FLock.Leave;
  end;
end;

{ TTrackedWorkItem }

constructor TTrackedWorkItem.Create(AControl: TThreadPoolTaskControl;
  const ATask: IThreadPoolTask);
begin
  inherited Create;
  FControl := AControl;
  FTask := ATask;
  FItemType := witProcedure;
  FIndex := 0;
  FFirstIndex := 0;
  FLastIndex := -1;
  FIsRange := False;
end;

procedure TTrackedWorkItem.Execute;
var
  CurrentIndex: Integer;
begin
  if FIsRange then
  begin
    CurrentIndex := FFirstIndex;
    while True do
    begin
      case FItemType of
        witProcedureIndex:
          if Assigned(FProcedureIndex) then
            FProcedureIndex(CurrentIndex);
        witMethodIndex:
          if Assigned(FMethodIndex) then
            FMethodIndex(CurrentIndex);
      end;
      if CurrentIndex = FLastIndex then
        Break;
      Inc(CurrentIndex);
    end;
    Exit;
  end;

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

function TTrackedWorkItem.GetItemType: Integer;
begin
  Result := Ord(FItemType);
end;

function TTrackedWorkItem.TryStart: Boolean;
begin
  Result := FControl.TryStart;
end;

procedure TTrackedWorkItem.MarkCompleted;
begin
  FControl.MarkCompleted;
end;

procedure TTrackedWorkItem.MarkFailed(const AMessage: string);
begin
  FControl.MarkFailed(AMessage);
end;

{ TThreadPoolTaskBatch }

constructor TThreadPoolTaskBatch.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  SetLength(FTasks, 8);
  FCount := 0;
end;

destructor TThreadPoolTaskBatch.Destroy;
begin
  SetLength(FTasks, 0);
  FLock.Free;
  inherited;
end;

procedure TThreadPoolTaskBatch.Add(const ATask: IThreadPoolTask);
var
  NewCapacity: Integer;
begin
  if ATask = nil then
    raise EArgumentException.Create('Task must not be nil');

  FLock.Enter;
  try
    if FCount = Length(FTasks) then
    begin
      NewCapacity := Length(FTasks) * 2;
      if NewCapacity = 0 then
        NewCapacity := 8;
      SetLength(FTasks, NewCapacity);
    end;
    FTasks[FCount] := ATask;
    Inc(FCount);
  finally
    FLock.Leave;
  end;
end;

function TThreadPoolTaskBatch.Snapshot: TThreadPoolTaskArray;
var
  I: Integer;
begin
  Result := nil;
  FLock.Enter;
  try
    SetLength(Result, FCount);
    for I := 0 to FCount - 1 do
      Result[I] := FTasks[I];
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolTaskBatch.WaitFor;
begin
  WaitFor(THREADPOOL_INFINITE);
end;

function TThreadPoolTaskBatch.WaitFor(ATimeoutMS: Cardinal): Boolean;
var
  TasksSnapshot: TThreadPoolTaskArray;
  StartedAt, Elapsed: QWord;
  Remaining: Cardinal;
  I: Integer;
begin
  TasksSnapshot := Snapshot;
  StartedAt := GetTickCount64;
  for I := 0 to High(TasksSnapshot) do
  begin
    if ATimeoutMS = THREADPOOL_INFINITE then
      Remaining := THREADPOOL_INFINITE
    else
    begin
      Elapsed := GetTickCount64 - StartedAt;
      if Elapsed >= ATimeoutMS then
        Remaining := 0
      else
        Remaining := ATimeoutMS - Cardinal(Elapsed);
    end;
    if not TasksSnapshot[I].WaitFor(Remaining) then
      Exit(False);
  end;
  Result := True;
end;

function TThreadPoolTaskBatch.CancelPending: Integer;
var
  TasksSnapshot: TThreadPoolTaskArray;
  I: Integer;
begin
  Result := 0;
  TasksSnapshot := Snapshot;
  for I := 0 to High(TasksSnapshot) do
    if TasksSnapshot[I].Cancel then
      Inc(Result);
end;

function TThreadPoolTaskBatch.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

function TThreadPoolTaskBatch.GetFinishedCount: Integer;
var
  TasksSnapshot: TThreadPoolTaskArray;
  I: Integer;
begin
  Result := 0;
  TasksSnapshot := Snapshot;
  for I := 0 to High(TasksSnapshot) do
    if TasksSnapshot[I].IsFinished then
      Inc(Result);
end;

function TThreadPoolTaskBatch.GetFailedCount: Integer;
var
  TasksSnapshot: TThreadPoolTaskArray;
  I: Integer;
begin
  Result := 0;
  TasksSnapshot := Snapshot;
  for I := 0 to High(TasksSnapshot) do
    if TasksSnapshot[I].State = ttsFailed then
      Inc(Result);
end;

function TThreadPoolTaskBatch.GetCancelledCount: Integer;
var
  TasksSnapshot: TThreadPoolTaskArray;
  I: Integer;
begin
  Result := 0;
  TasksSnapshot := Snapshot;
  for I := 0 to High(TasksSnapshot) do
    if TasksSnapshot[I].State = ttsCancelled then
      Inc(Result);
end;

function TThreadPoolTaskBatch.GetTask(AIndex: Integer): IThreadPoolTask;
begin
  FLock.Enter;
  try
    if (AIndex < 0) or (AIndex >= FCount) then
      raise EArgumentOutOfRangeException.CreateFmt(
        'Task index %d is outside the batch', [AIndex]);
    Result := FTasks[AIndex];
  finally
    FLock.Leave;
  end;
end;

function NewThreadPoolTaskBatch: IThreadPoolTaskBatch;
begin
  Result := TThreadPoolTaskBatch.Create;
end;

function NewTaskControl(out ATask: IThreadPoolTask): TThreadPoolTaskControl;
begin
  Result := TThreadPoolTaskControl.Create;
  ATask := Result;
end;

function NewTrackedWorkItem(AProcedure: TThreadProcedure;
  out ATask: IThreadPoolTask): IWorkItem;
var
  Control: TThreadPoolTaskControl;
  WorkItem: TTrackedWorkItem;
begin
  Control := NewTaskControl(ATask);
  WorkItem := TTrackedWorkItem.Create(Control, ATask);
  WorkItem.FProcedure := AProcedure;
  WorkItem.FItemType := witProcedure;
  Result := WorkItem;
end;

function NewTrackedWorkItem(AMethod: TThreadMethod;
  out ATask: IThreadPoolTask): IWorkItem;
var
  Control: TThreadPoolTaskControl;
  WorkItem: TTrackedWorkItem;
begin
  Control := NewTaskControl(ATask);
  WorkItem := TTrackedWorkItem.Create(Control, ATask);
  WorkItem.FMethod := AMethod;
  WorkItem.FItemType := witMethod;
  Result := WorkItem;
end;

function NewTrackedWorkItem(AProcedure: TThreadProcedureIndex;
  AIndex: Integer; out ATask: IThreadPoolTask): IWorkItem;
var
  Control: TThreadPoolTaskControl;
  WorkItem: TTrackedWorkItem;
begin
  Control := NewTaskControl(ATask);
  WorkItem := TTrackedWorkItem.Create(Control, ATask);
  WorkItem.FProcedureIndex := AProcedure;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witProcedureIndex;
  Result := WorkItem;
end;

function NewTrackedWorkItem(AMethod: TThreadMethodIndex;
  AIndex: Integer; out ATask: IThreadPoolTask): IWorkItem;
var
  Control: TThreadPoolTaskControl;
  WorkItem: TTrackedWorkItem;
begin
  Control := NewTaskControl(ATask);
  WorkItem := TTrackedWorkItem.Create(Control, ATask);
  WorkItem.FMethodIndex := AMethod;
  WorkItem.FIndex := AIndex;
  WorkItem.FItemType := witMethodIndex;
  Result := WorkItem;
end;

function NewTrackedRangeWorkItem(AProcedure: TThreadProcedureIndex;
  AFirstIndex, ALastIndex: Integer;
  out ATask: IThreadPoolTask): IWorkItem;
var
  Control: TThreadPoolTaskControl;
  WorkItem: TTrackedWorkItem;
begin
  Control := NewTaskControl(ATask);
  WorkItem := TTrackedWorkItem.Create(Control, ATask);
  WorkItem.FProcedureIndex := AProcedure;
  WorkItem.FFirstIndex := AFirstIndex;
  WorkItem.FLastIndex := ALastIndex;
  WorkItem.FItemType := witProcedureIndex;
  WorkItem.FIsRange := True;
  Result := WorkItem;
end;

function NewTrackedRangeWorkItem(AMethod: TThreadMethodIndex;
  AFirstIndex, ALastIndex: Integer;
  out ATask: IThreadPoolTask): IWorkItem;
var
  Control: TThreadPoolTaskControl;
  WorkItem: TTrackedWorkItem;
begin
  Control := NewTaskControl(ATask);
  WorkItem := TTrackedWorkItem.Create(Control, ATask);
  WorkItem.FMethodIndex := AMethod;
  WorkItem.FFirstIndex := AFirstIndex;
  WorkItem.FLastIndex := ALastIndex;
  WorkItem.FItemType := witMethodIndex;
  WorkItem.FIsRange := True;
  Result := WorkItem;
end;

function GetThreadPoolRangeChunkSize(AFirstIndex, ALastIndex,
  AThreadCount, ARequestedChunkSize: Integer): Int64;
var
  ItemCount, TargetTaskCount: Int64;
begin
  if ARequestedChunkSize < 0 then
    raise EArgumentOutOfRangeException.Create(
      'Range chunk size must be zero or greater');
  if AFirstIndex > ALastIndex then
    Exit(0);
  if ARequestedChunkSize > 0 then
    Exit(ARequestedChunkSize);

  ItemCount := Int64(ALastIndex) - Int64(AFirstIndex) + 1;
  TargetTaskCount := Int64(AThreadCount) * 4;
  if TargetTaskCount < 1 then
    TargetTaskCount := 1;
  Result := (ItemCount + TargetTaskCount - 1) div TargetTaskCount;
  if Result < 1 then
    Result := 1;
end;

procedure ExecuteThreadPoolWorkItem(const AWorkItem: IWorkItem;
  AOnError: TThreadPoolErrorEvent);
var
  TrackedWorkItem: IThreadPoolTrackedWorkItem;
  ErrorMessage: string;
begin
  TrackedWorkItem := nil;
  if Supports(AWorkItem, IThreadPoolTrackedWorkItem, TrackedWorkItem) then
  begin
    if not TrackedWorkItem.TryStart then
      Exit;
    try
      AWorkItem.Execute;
      TrackedWorkItem.MarkCompleted;
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        try
          if Assigned(AOnError) then
            AOnError(ErrorMessage);
        finally
          TrackedWorkItem.MarkFailed(ErrorMessage);
        end;
      end;
    end;
    Exit;
  end;

  try
    AWorkItem.Execute;
  except
    on E: Exception do
      if Assigned(AOnError) then
        AOnError(E.Message);
  end;
end;

function NewThreadPoolRangeBatch(AProcedure: TThreadProcedureIndex;
  AFirstIndex, ALastIndex, AThreadCount, AChunkSize: Integer;
  ASubmit: TThreadPoolWorkItemSubmit): IThreadPoolTaskBatch;
var
  Batch: IThreadPoolTaskBatch;
  Task: IThreadPoolTask;
  WorkItem: IWorkItem;
  EffectiveChunkSize, ChunkFirst, ChunkLast: Int64;
begin
  if not Assigned(ASubmit) then
    raise EArgumentException.Create('Range submit callback must not be nil');
  Batch := NewThreadPoolTaskBatch;
  EffectiveChunkSize := GetThreadPoolRangeChunkSize(AFirstIndex,
    ALastIndex, AThreadCount, AChunkSize);
  if EffectiveChunkSize = 0 then
    Exit(Batch);

  try
    ChunkFirst := AFirstIndex;
    while ChunkFirst <= Int64(ALastIndex) do
    begin
      ChunkLast := ChunkFirst + EffectiveChunkSize - 1;
      if ChunkLast > Int64(ALastIndex) then
        ChunkLast := ALastIndex;
      WorkItem := NewTrackedRangeWorkItem(AProcedure,
        Integer(ChunkFirst), Integer(ChunkLast), Task);
      if not ASubmit(WorkItem) then
      begin
        Task.Cancel;
        raise EThreadPoolTaskSubmission.Create(
          'Range chunk was not accepted');
      end;
      try
        Batch.Add(Task);
      except
        Task.Cancel;
        raise;
      end;
      WorkItem := nil;
      Task := nil;
      ChunkFirst := ChunkLast + 1;
    end;
  except
    Batch.CancelPending;
    raise;
  end;
  Result := Batch;
end;

function NewThreadPoolRangeBatch(AMethod: TThreadMethodIndex;
  AFirstIndex, ALastIndex, AThreadCount, AChunkSize: Integer;
  ASubmit: TThreadPoolWorkItemSubmit): IThreadPoolTaskBatch;
var
  Batch: IThreadPoolTaskBatch;
  Task: IThreadPoolTask;
  WorkItem: IWorkItem;
  EffectiveChunkSize, ChunkFirst, ChunkLast: Int64;
begin
  if not Assigned(ASubmit) then
    raise EArgumentException.Create('Range submit callback must not be nil');
  Batch := NewThreadPoolTaskBatch;
  EffectiveChunkSize := GetThreadPoolRangeChunkSize(AFirstIndex,
    ALastIndex, AThreadCount, AChunkSize);
  if EffectiveChunkSize = 0 then
    Exit(Batch);

  try
    ChunkFirst := AFirstIndex;
    while ChunkFirst <= Int64(ALastIndex) do
    begin
      ChunkLast := ChunkFirst + EffectiveChunkSize - 1;
      if ChunkLast > Int64(ALastIndex) then
        ChunkLast := ALastIndex;
      WorkItem := NewTrackedRangeWorkItem(AMethod,
        Integer(ChunkFirst), Integer(ChunkLast), Task);
      if not ASubmit(WorkItem) then
      begin
        Task.Cancel;
        raise EThreadPoolTaskSubmission.Create(
          'Range chunk was not accepted');
      end;
      try
        Batch.Add(Task);
      except
        Task.Cancel;
        raise;
      end;
      WorkItem := nil;
      Task := nil;
      ChunkFirst := ChunkLast + 1;
    end;
  except
    Batch.CancelPending;
    raise;
  end;
  Result := Batch;
end;

end.
