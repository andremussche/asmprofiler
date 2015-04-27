unit mcSamplingResult2;

interface

uses
  Contnrs, Classes,
  mcProcessSampler, mcThreadSampler;

type
  TFlatFunction = class;
  TFunctionLine = class;

  TThreadResultList = class;
  TModuleList       = class;
  TUnitList         = class;
  TFlatFunctionList = class;
  TFunctionLineList = class;
  TSubFunctionList  = class;

  TFlatFunction = class
  public
    ModuleName,
    UnitName,
    ClassName,
    FunctionName,
    CompleteName: String;

    Count: Integer;
    TotalTime: Integer;
    TotalOwnTime: Integer;
    TotalChildTime: Integer;

    FunctionLines: TFunctionLineList;
    ChildCalls, ParentCalls: TSubFunctionList;

    procedure AfterConstruction;override;
    destructor Destroy;override;
  end;

  TSubFunction = class
  public
    FunctionRef: TFlatFunction;
    Count: Integer;
    TotalTime: Integer;
//    TotalOwnTime: Integer;
//    TotalChildTime: Integer;
  end;

  TFunctionLine = class
  public
    Address: Pointer;
    Line: Integer;
    LineString: String;
    Count: Integer;
    Time: Real;
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
    function GetLineByNumber(const aLineNumber: integer): TFunctionLine;
  end;

  TSubFunctionList = class(TObjectList)
    function GetFunctionByRef(const aFunction: TFlatFunction): TSubFunction;
    function GetFunctionByName(const aFunctionName: string): TSubFunction;
  end;

  TFlatFunctionList = class(TObjectList)
  public
    function GetFunctionByName(const aCompleteName: string): TFlatFunction;
  end;

  TModuleList = class(TObjectList)
  public
    function GetModuleByName(const aModule: string): TModule;
  end;

  TUnitList = class(TObjectList)
  public
    function GetUnitByName(const aUnit: string): TUnit;
  end;

  //-----------------------------------------------------------------------

  TProcessResult = class
  public
    ProcessId: Cardinal;
    ThreadResults: TThreadResultList;

    procedure AfterConstruction;override;
    destructor Destroy;override;
  end;

  TThreadResult = class
  public
    ThreadId: Cardinal;
    SampleCount: integer;

    ModuleResult: TModuleList;
    FlatFunctions: TFlatFunctionList;

    procedure AfterConstruction;override;
    destructor Destroy;override;
  end;

  TThreadResultList = class(TObjectList)
  end;

  //-----------------------------------------------------------------------
    
  TSamplingResult = class
  private
    FProcessObject: TProcessSampler;
    FProcessId: Cardinal;

    //FThreadList: TObjectlist;
    FProcessResult: TProcessResult;
    procedure MakeFunctionResult(aStackFunctions: TSnapshotArray; aThreadResult: TThreadResult);
    procedure MakeModuleResult(aThreadResult: TThreadResult);
  protected
    procedure MakeAllProfilingResults;
  public
    constructor Create(aProcessObject: TProcessSampler);

    function GetProcessResult: TProcessResult;
  end;


implementation

uses
  JclDebug, SysUtils, Windows, muExactTiming, DateUtils;

procedure IncF(var aFloat: Real; aIncrement: Real);
begin
  aFloat := aFloat + aIncrement;
end;
  
{ TSamplingResult }

constructor TSamplingResult.Create(aProcessObject: TProcessSampler);
begin
//  FThreadList    := TObjectList.Create;
  FProcessResult := TProcessResult.Create;
  FProcessObject := aProcessObject;
  //LoadAllSnapshots;
  MakeAllProfilingResults;
end;

function TSamplingResult.GetProcessResult: TProcessResult;
//var
//  i: Integer;
//  it: TInternalThread;
//  t: TDatetime;
begin
  //Result := TProcessResult.Create;
  Result := FProcessResult;

  {
  t := Now;


  for i := 0 to FThreadList.Count - 1 do
  begin
    it := FThreadList.Items[i] as TInternalThread;
//    Result[i].ThreadId     := it.ThreadId;
//    Result[i].ThreadResult := it.ProfileResult;
//    Result[i].SampleCount  := Length(it.Snapshots);
  end;

  OutputDebugString( PChar(Format('TSamplingResult.GetProcessResult: %4.2f ms',
                                  [MilliSecondSpan(Now, t)])) );
  }
end;

{
procedure TSamplingResult.LoadAllSnapshots;
var
  i: Integer;
  ts: TThreadSampler;
//  it: TInternalThread;
  t: TDatetime;
begin
  t := Now;

//  FThreadList.Clear;
  for i := 0 to FProcessObject.ThreadCount - 1 do
  begin
    ts := FProcessObject.Threads[i];

    it := TInternalThread.Create;
    it.ThreadId       := ts.ThreadId;
    it.Snapshots      := ts.GetAllSnapshotStacks;
    it.FProcessObject := Self.FProcessObject;

    FThreadList.Add(it);
  end;

  OutputDebugString( PChar(Format('TSamplingResult.LoadAllSnapshots: %4.2f ms',
                                  [MilliSecondSpan(Now, t)])) );
end;
}

procedure TSamplingResult.MakeAllProfilingResults;
var
  i: Integer;
  ts: TThreadSampler;
  //it: TInternalThread;
  tr: TThreadResult;
  t:  TDatetime;
begin
  t := Now;
  FProcessResult.ThreadResults.Clear;
  FProcessResult.ProcessId := FProcessObject.ProcessId;
  
  for i := 0 to FProcessObject.ThreadCount - 1 do
  begin
    ts := FProcessObject.Threads[i];

    tr := TThreadResult.Create;
    tr.ThreadId       := ts.ThreadId;

    MakeFunctionResult(ts.GetAllSnapshotStacks, tr);
    MakeModuleResult(tr);

    FProcessResult.ThreadResults.Add(tr);
  end;

  OutputDebugString( PChar(Format('TSamplingResult.MakeAllProfilingResults: %4.2f ms',
                                  [MilliSecondSpan(Now, t)])) );
end;

procedure TSamplingResult.MakeModuleResult(aThreadResult: TThreadResult);
var
  i: Integer;
  ff: TFlatFunction;
  m: TModule;
  u: TUnit;
begin
  aThreadResult.ModuleResult.Clear;

  for i := 0 to aThreadResult.FlatFunctions.Count - 1 do
  begin
    ff := TFlatFunction(aThreadResult.FlatFunctions.Items[i]);

    //module
    m := aThreadResult.ModuleResult.GetModuleByName(ff.ModuleName);
    if m = nil then
    begin
      m := TModule.Create;
      m.ModuleName := ff.ModuleName;
      aThreadResult.ModuleResult.Add(m);
    end;
    Inc (m.Calls,          ff.Count);
    IncF(m.TotalTime,      ff.TotalTime);
    IncF(m.TotalOwnTime,   ff.TotalOwnTime);
    IncF(m.TotalChildTime, ff.TotalChildTime);

    //unit
    u := m.Units.GetUnitByName(ff.UnitName);
    if u = nil then
    begin
      u := TUnit.Create;
      u.UnitName := ff.UnitName;
      m.Units.Add(u);
    end;
    Inc (u.Calls,          ff.Count);
    IncF(u.TotalTime,      ff.TotalTime);
    IncF(u.TotalOwnTime,   ff.TotalOwnTime);
    IncF(u.TotalChildTime, ff.TotalChildTime);

    //function
    if u.Functions.GetFunctionByName(ff.CompleteName) = nil then
      u.Functions.Add(ff);
  end;

end;

{ TSamplingResult}

procedure TSamplingResult.MakeFunctionResult(aStackFunctions: TSnapshotArray; aThreadResult: TThreadResult);
var
  isnapshot: Integer;
  stack: TFunctionArray;
  istack: Integer;
  info: TJclLocationInfo;
  ff: TFlatFunction;
  sfunction: string;
  fl: TFunctionLine;

  childfunction: TFlatFunction;
  parentsub, childsub: TSubFunction;
begin
  aThreadResult.FlatFunctions.Clear;

  for isnapshot := Low(aStackFunctions) to High(aStackFunctions) do
  begin
    stack := aStackFunctions[isnapshot];

    childfunction := nil;

    for istack := Low(stack) to High(stack) do
    begin
      if not FProcessObject.GetLocationInfoRecord(stack[istack], info) then Continue;

      sfunction := info.ProcedureName;
      if Pos(info.UnitName + '.', sfunction) = 1 then
        sfunction := Copy(sfunction, Length(info.UnitName) + 2,
                                     Length(sfunction) - Length(info.UnitName) - 1);
      if sfunction = '' then
        sfunction := IntToHex(Integer(info.Address), 8);

      //search function
      ff := aThreadResult.FlatFunctions.GetFunctionByName(sfunction);
      //add new function
      if ff = nil then
      begin
        ff := TFlatFunction.Create;
        ff.ModuleName   := info.BinaryFileName;
        ff.UnitName     := info.UnitName;
        //ProfileResult[ifunction].ClassName    := sfunction;
        ff.FunctionName := info.ProcedureName;
        ff.CompleteName := sfunction;
        aThreadResult.FlatFunctions.Add(ff);
      end;
      inc(ff.Count);
      inc(ff.TotalTime);
      if istack = 0 then
        inc(ff.TotalOwnTime)
      else
        inc(ff.TotalChildTime);

      //------------------------------------
      //search line counter of function
      fl := ff.FunctionLines.GetLineByNumber(info.LineNumber);
      //add line
      if fl = nil then
      begin
        fl := TFunctionLine.Create;
        fl.Address    := info.Address;
        fl.Line       := info.LineNumber;
        if info.LineNumber > 0 then
          fl.LineString := 'Line ' + IntToStr(info.LineNumber)
        else
          fl.LineString := '$' + IntToHex(Integer(info.Address), 8);
        ff.FunctionLines.Add(fl);
      end;
      //process line
      inc(fl.Count);
      incF(fl.Time, 1);

      if childfunction <> nil then
      begin
        //PARENT sub functions
        parentsub := childfunction.ParentCalls.GetFunctionByRef(ff);
        //add sub function
        if parentsub = nil then
        begin
          parentsub := TSubFunction.Create;
          parentsub.FunctionRef := ff;
          childfunction.ParentCalls.Add(parentsub);
        end;
        //process sub function
        inc(parentsub.Count);
        inc(parentsub.TotalTime, ff.TotalTime);

        //CHILD sub functions
        childsub := ff.ChildCalls.GetFunctionByRef(childfunction);
        //add sub function
        if childsub = nil then
        begin
          childsub := TSubFunction.Create;
          childsub.FunctionRef := childfunction;
          ff.ChildCalls.Add(childsub);
        end;
        //process sub function
        inc(childsub.Count);
        inc(childsub.TotalTime, childfunction.TotalTime);
      end;

      childfunction := ff;
    end;
  end;

end;

{ TFlatFunction }

procedure TFlatFunction.AfterConstruction;
begin
  inherited;
  ChildCalls  := TSubFunctionList.Create(True);
  ParentCalls := TSubFunctionList.Create(True);
end;

destructor TFlatFunction.Destroy;
begin
  ChildCalls.Free;
  ParentCalls.Free;
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

{ TFunctionLineList }

function TFunctionLineList.GetLineByNumber(const aLineNumber: integer): TFunctionLine;
var
  i: Integer;
  fl: TFunctionLine;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    fl := TFunctionLine(Self.Items[i]);
    if fl.Line = aLineNumber then
    begin
      Result := fl;
      Exit;
    end;
  end;
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

{ TUnitList }

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

{ TModuleList }

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

end.
