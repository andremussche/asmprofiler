unit uResultTypes;

interface

uses
  ContNrs, Generics.Collections;

type
  TFlatFunction = class;
  TFunctionLine = class;

  TThreadResultList = class;
  TModuleList       = class;
  TUnitList         = class;
  TFlatFunctionList = class;
  TFunctionLineList = class;
  TSubFunctionList  = class;

  TIntegerList      = class(TList<Integer>);

  TFlatFunction = class
  public
    ModuleName,
    UnitName,
    ClassName,
    FunctionName,
    CompleteName: String;

    Count: Integer;
    TotalTime,
    TotalOwnTime,
    TotalChildTime: Real;

    LastStackNr: Integer;

    FunctionLines: TFunctionLineList;
    ChildCalls, ParentCalls: TSubFunctionList;
//    Addresses: array of Pointer;

    SnapshotNrs: TIntegerList;

    procedure  AfterConstruction;override;
    destructor Destroy;override;
  end;

  TSubFunction = class
  public
    FunctionRef: TFlatFunction;
    Count: Integer;
    TotalTime,
    TotalOwnTime,
    TotalChildTime: Real;
  end;

  TFunctionLine = class
  public
    Address: Pointer;
    Line: Integer;
    LineString: String;
    Count: Integer;
    Time: Real;
    MinAddr, MaxAddr: Cardinal;
  end;

  TModule = class
    ModuleName: string;
    Units: TUnitList;

    Calls: integer;
    TotalTime, TotalOwnTime, TotalChildTime: Real;

    procedure AfterConstruction;override;
    destructor Destroy;override;
  end;

  TUnit = class
    UnitName: string;
    Functions: TFlatFunctionList;

    Calls: integer;
    TotalTime, TotalOwnTime, TotalChildTime: Real;

    procedure AfterConstruction;override;
    destructor Destroy;override;
  end;

  //-----------------------------------------------------------------------
  
  TFunctionLineList = class(TObjectList)
  private
    function GetItem(Index: Integer): TFunctionLine;
    procedure SetItem(Index: Integer; const Value: TFunctionLine);
  public
    function GetLineByNumber(const aLineNumber: integer; aAddress: Pointer): TFunctionLine;
    property Lines[Index: Integer]: TFunctionLine read GetItem write SetItem; default;
  end;

  TSubFunctionList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSubFunction;
    procedure SetItem(Index: Integer; const Value: TSubFunction);
  public
    function GetFunctionByRef(const aFunction: TFlatFunction): TSubFunction;
    function GetFunctionByName(const aFunctionName: string): TSubFunction;
    property Functions[Index: Integer]: TSubFunction read GetItem write SetItem; default;
  end;

  TFlatFunctionList = class(TObjectList)
  private
    function GetItem(Index: Integer): TFlatFunction;
    procedure SetItem(Index: Integer; const Value: TFlatFunction);
  public
    function GetFunctionByName(const aCompleteName: string): TFlatFunction;
    property Functions[Index: Integer]: TFlatFunction read GetItem write SetItem; default;
  end;

  TModuleList = class(TObjectList)
  private
    FProcessExe: string;
    function GetItem(Index: Integer): TModule;
    procedure SetItem(Index: Integer; const Value: TModule);
  public
    function GetModuleByName(const aModule: string): TModule;
    property Modules[Index: Integer]: TModule read GetItem write SetItem; default;

    property ProcessExe: string read FProcessExe write FProcessExe; 
  end;

  TUnitList = class(TObjectList)
  private
    function GetItem(Index: Integer): TUnit;
    procedure SetItem(Index: Integer; const Value: TUnit);
  public
    function GetUnitByName(const aUnit: string): TUnit;
    property Units[Index: Integer]: TUnit read GetItem write SetItem; default;
  end;

  //-----------------------------------------------------------------------

  TProcessResult = class
  public
    ProcessId: Cardinal;
    ProcessExe: string;
    ThreadResults: TThreadResultList;

    procedure AfterConstruction;override;
    destructor Destroy;override;
  end;

  TThreadResult = class
  public
    ThreadId: Cardinal;
    //SampleCount: integer;
    Duration: Double;

    ModuleResult: TModuleList;
    FlatFunctions: TFlatFunctionList;

    procedure  AfterConstruction;override;
    destructor Destroy;override;
  end;

  TThreadResultList = class(TObjectList)
  private
    function GetItem(Index: Integer): TThreadResult;
    procedure SetItem(Index: Integer; const Value: TThreadResult);
  public
    property ThreadResults[Index: Integer]: TThreadResult read GetItem write SetItem; default;
  end;

  //------------------------------------------------------------------

type
  TProcessHeader = record
    ProcessId: Cardinal;
    FirstSample,
    LastSample: TDatetime;
    SampleCount,
    ThreadCount: integer;
    CounterFrequency: Int64;
  end;

  TSampleTime = record
    Nr: Integer;
    Time: Int64;
  end;
  TSampleTimes = array of TSampleTime;

  TThreadHeader = record
    ThreadId: Cardinal;
    ThreadStackTop: Pointer;
    DumpCount: integer;
  end;

  TSnapshotHeader = record
    Nr: Integer;
    Size: Cardinal;
  end;
  

implementation

{ TFlatFunction }

procedure TFlatFunction.AfterConstruction;
begin
  inherited;
  FunctionLines := TFunctionLineList.Create(True);
  ChildCalls    := TSubFunctionList.Create(True);
  ParentCalls   := TSubFunctionList.Create(True);

  SnapshotNrs   := TIntegerList.Create;
end;

destructor TFlatFunction.Destroy;
begin
  FunctionLines.Free;
  ChildCalls.Free;
  ParentCalls.Free;
  SnapshotNrs.Free;
  inherited;
end;

{ TThreadResult }

procedure TThreadResult.AfterConstruction;
begin
  inherited;
  ModuleResult  := TModuleList.Create(True);
  FlatFunctions := TFlatFunctionList.Create(True);
end;

destructor TThreadResult.Destroy;
begin
  ModuleResult.Free;
  FlatFunctions.Free;
  inherited;
end;

{ TProcessResult }

procedure TProcessResult.AfterConstruction;
begin
  inherited;
  ThreadResults := TThreadResultList.Create(True);
end;

destructor TProcessResult.Destroy;
begin
  ThreadResults.Free;
  inherited;
end;

{ TFlatFunctionList }

function TFlatFunctionList.GetFunctionByName(const aCompleteName: string): TFlatFunction;
var
  i: Integer;
  ff: TFlatFunction;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    ff := TFlatFunction(Self.Items[i]);
    if ff.CompleteName = aCompleteName then
    begin
      Result := ff;
      Exit;
    end;
  end;
end;

function TFlatFunctionList.GetItem(Index: Integer): TFlatFunction;
begin
  Result := TFlatFunction(Items[Index]);
end;

procedure TFlatFunctionList.SetItem(Index: Integer; const Value: TFlatFunction);
begin
  Items[Index] := Value;
end;

{ TFunctionLineList }

function TFunctionLineList.GetItem(Index: Integer): TFunctionLine;
begin
  Result := TFunctionLine(Items[Index]);
end;

function TFunctionLineList.GetLineByNumber(const aLineNumber: integer; aAddress: Pointer): TFunctionLine;
var
  i: Integer;
  fl: TFunctionLine;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    fl := TFunctionLine(Self.Items[i]);

    if (aLineNumber = 0) then
    begin
      if fl.Address = aAddress then
      begin
        Result := fl;
        Exit;
      end;
    end
    else
    if fl.Line = aLineNumber then
    begin
      Result := fl;
      Exit;
    end;
  end;
end;

procedure TFunctionLineList.SetItem(Index: Integer; const Value: TFunctionLine);
begin
  Items[Index] := Value;
end;

{ TSubFunctionList }

function TSubFunctionList.GetFunctionByName(const aFunctionName: string): TSubFunction;
var
  i: Integer;
  sf: TSubFunction;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    sf := TSubFunction(Self.Items[i]);
    if sf.FunctionRef.FunctionName = aFunctionName then
    begin
      Result := sf;
      Exit;
    end;
  end;
end;

function TSubFunctionList.GetFunctionByRef(const aFunction: TFlatFunction): TSubFunction;
var
  i: Integer;
  sf: TSubFunction;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    sf := TSubFunction(Self.Items[i]);
    if sf.FunctionRef = aFunction then
    begin
      Result := sf;
      Exit;
    end;
  end;
end;

function TSubFunctionList.GetItem(Index: Integer): TSubFunction;
begin
  Result := TSubFunction(Items[Index]);
end;

procedure TSubFunctionList.SetItem(Index: Integer; const Value: TSubFunction);
begin
  Items[Index] := Value;
end;

{ TUnitList }

function TUnitList.GetItem(Index: Integer): TUnit;
begin
  Result := TUnit(Items[Index]);
end;

function TUnitList.GetUnitByName(const aUnit: string): TUnit;
var
  i: Integer;
  u: TUnit;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    u := TUnit(Self.Items[i]);
    if u.UnitName = aUnit then
    begin
      Result := u;
      Exit;
    end;
  end;
end;

procedure TUnitList.SetItem(Index: Integer; const Value: TUnit);
begin
  Items[Index] := Value;
end;

{ TModuleList }

function TModuleList.GetItem(Index: Integer): TModule;
begin
  Result := TModule(Items[Index]);
end;

function TModuleList.GetModuleByName(const aModule: string): TModule;
var
  i: Integer;
  m: TModule;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    m := TModule(Self.Items[i]);
    if m.ModuleName = aModule then
    begin
      Result := m;
      Exit;
    end;
  end;
end;

procedure TModuleList.SetItem(Index: Integer; const Value: TModule);
begin
  Items[Index] := Value;
end;

{ TModule }

procedure TModule.AfterConstruction;
begin
  inherited;
  Units := TUnitList.Create(True);
end;

destructor TModule.Destroy;
begin
  Units.Free;
  inherited;
end;

{ TUnit }

procedure TUnit.AfterConstruction;
begin
  inherited;
  Functions := TFlatFunctionList.Create(False);
end;

destructor TUnit.Destroy;
begin
  Functions.Free;
  inherited;
end;

{ TThreadResultList }

function TThreadResultList.GetItem(Index: Integer): TThreadResult;
begin
  Result := TThreadResult(Items[Index]);
end;

procedure TThreadResultList.SetItem(Index: Integer; const Value: TThreadResult);
begin
  Items[Index] := Value;
end;

end.
