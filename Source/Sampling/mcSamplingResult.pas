unit mcSamplingResult;

interface

uses
  Contnrs, Classes,
  uResultTypes,
  mcProcessSampler, mcThreadSampler;

type
  TSamplingResult = class
  private
    FProcessObject: TProcessSampler;
//    FProcessId: Cardinal;

    //FThreadList: TObjectlist;
    FProcessResult: TProcessResult;
    procedure MakeFunctionResult(aStackFunctions: TSnapshotArray; aThreadResult: TThreadResult);
    procedure MakeFunctionSummary(const aSourceFunctions: TFlatFunctionList; var aDestFunctions: TFlatFunctionList);
    //procedure MakeModuleResult(aThreadResult: TThreadResult);
    procedure MakeModuleResult(const aFunctions: TFlatFunctionList; var aModules: TModuleList);
    function GetProcessResult: TProcessResult;
  protected
    procedure MakeAllProfilingResults;
  public
    constructor Create(aProcessObject: TProcessSampler);

    procedure LoadDumpsFromFile(const aFile: string);
    procedure SaveDumpsToFile(const aFile: string);

    property ProcessObject: TProcessSampler read FProcessObject;
    property ProcessResult: TProcessResult read GetProcessResult;
  end;

implementation

uses
  JclDebug, SysUtils, Windows, muExactTiming, DateUtils, Math, uOfflineDebugInfo;

procedure IncF(var aFloat: Real; aIncrement: Real);
begin
  aFloat := aFloat + aIncrement;
end;
  
{ TSamplingResult }

constructor TSamplingResult.Create(aProcessObject: TProcessSampler);
begin
//  FThreadList    := TObjectList.Create;
  FProcessObject := aProcessObject;
//  FProcessResult := TProcessResult.Create;
//  FProcessResult.ProcessExe := FProcessObject.ProcessExe;
  //LoadAllSnapshots;
  //MakeAllProfilingResults;
end;

function TSamplingResult.GetProcessResult: TProcessResult;
//var
//  i: Integer;
//  it: TInternalThread;
//  t: TDatetime;
begin
  //Result := TProcessResult.Create;
  if FProcessResult = nil then
  begin
    FProcessResult := TProcessResult.Create;
    FProcessResult.ProcessExe := FProcessObject.ProcessExe;
    
    MakeAllProfilingResults;
  end;

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

procedure TSamplingResult.LoadDumpsFromFile(const aFile: string);
var
//  ph: TProcessHeader;
//  st: TSampleTimes;
//  th: TThreadHeader;
//  sh: TSnapshotHeader;

//  si: TStackDumpInfo;
//  sd: TStackDump;
//  idumpsize: Integer;
//  ts: TThreadSampler;
//  ss: TSnapshotStack;
//  fa: TFunctionArray;

//  i: Integer;
//  j: Integer;

  strm: TMemoryStream;
begin
  strm := TMemoryStream.Create;
  try
    strm.LoadFromFile(aFile);
    strm.Position := 0;

    if FProcessObject = nil then
      FProcessObject := TProcessSampler.Create(0);
    ProcessObject.LoadDumpsFromStream(strm);

    if FProcessResult = nil then
      FProcessResult := TProcessResult.Create;
    if FProcessObject.OfflineInfo = nil then
      FProcessObject.OfflineInfo := TOfflineDebugInfo.Create(ProcessResult);
    ProcessObject.OfflineInfo.LoadDebugInfoFromFile(
      ChangeFileExt(aFile, '.debug'));
    ProcessObject.ProcessExe := ProcessResult.ProcessExe;

    MakeAllProfilingResults;
  finally
    strm.Free;
  end;
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
  trall,
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
    MakeModuleResult(tr.FlatFunctions, tr.ModuleResult);

    FProcessResult.ThreadResults.Add(tr);
  end;

  //make summary of all functions by module
  trall := TThreadResult.Create;
  trall.ThreadId := 0; //all
  for i := 0 to FProcessResult.ThreadResults.Count - 1 do
  begin
    tr := FProcessResult.ThreadResults[i];
    //trall.SampleCount := Max(trall.SampleCount, tr.SampleCount);
    trall.Duration := Max(trall.Duration, tr.Duration);
    MakeFunctionSummary(tr.FlatFunctions, trall.FlatFunctions);
  end;
  MakeModuleResult(trall.FlatFunctions, trall.ModuleResult);
  FProcessResult.ThreadResults.Insert(0, trall);

  OutputDebugString( PChar(Format('TSamplingResult.MakeAllProfilingResults: %4.2f ms',
                                  [MilliSecondSpan(Now, t)])) );
end;

procedure TSamplingResult.MakeModuleResult(const aFunctions: TFlatFunctionList; var aModules: TModuleList); //  aThreadResult: TThreadResult);
var
  i: Integer;
  ff: TFlatFunction;
  m: TModule;
  u: TUnit;
begin
  //ThreadResult.ModuleResult.Clear;

//  for i := 0 to aThreadResult.FlatFunctions.Count - 1 do
  for i := 0 to aFunctions.Count - 1 do
  begin
//    ff := TFlatFunction(aThreadResult.FlatFunctions.Items[i]);
    ff := aFunctions[i];

    //module
//    m := aThreadResult.ModuleResult.GetModuleByName(ff.ModuleName);
    m := aModules.GetModuleByName(ff.ModuleName);
    if m = nil then
    begin
      m := TModule.Create;
      m.ModuleName := ff.ModuleName;
//      aThreadResult.ModuleResult.Add(m);
      aModules.Add(m);
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
    //if u.Functions.GetFunctionByName(ff.CompleteName) = nil then
      u.Functions.Add(ff);
  end;

end;

procedure TSamplingResult.SaveDumpsToFile(const aFile: string);
var
  strm: TMemoryStream;
begin
  strm := TMemoryStream.Create;
  try
    strm.Size := (ProcessObject.StackDumpCount * SizeOf(TSampleTimes)) +
                 (ProcessObject.StackDumpCount *    //1000 dumps
                  ProcessObject.ThreadCount *       //3 threads
                  SizeOf(TThreadHeader) *
                  25);                             //about 25 functions per stack?

    ProcessObject.SaveDumpsToStream(strm);
    strm.SaveToFile(aFile);

    if ProcessObject.OfflineInfo = nil then
      ProcessObject.OfflineInfo := TOfflineDebugInfo.Create(ProcessResult);
    ProcessObject.OfflineInfo.SaveDebugInfoToFile(
      ChangeFileExt(aFile, '.debug'));
  finally
    strm.Free;
  end;
end;

{ TSamplingResult}

procedure TSamplingResult.MakeFunctionResult(aStackFunctions: TSnapshotArray; aThreadResult: TThreadResult);
var
  isnapshot: Integer;
  stack: TSnapshotStack;
  istack: Integer;
  info: TJclLocationInfo;
  ff: TFlatFunction;
  sfunction: string;
  fl: TFunctionLine;
  dumpinfo: TStackDumpInfo;

  childfunction: TFlatFunction;
  parentsub, childsub: TSubFunction;
  I: Integer;
  iprevtime, idifftime: int64;
  ftime: Double;
begin
  aThreadResult.FlatFunctions.Clear;
  //aThreadResult.SampleCount := Length(aStackFunctions);
  aThreadResult.Duration := MilliSecondSpan(ProcessObject.LastSample, ProcessObject.FirstSample); 
  iprevtime     := 0;

  for isnapshot := Low(aStackFunctions) to High(aStackFunctions) do
  begin
    stack := aStackFunctions[isnapshot];
    if (Length(stack.Functions)=0) then Continue;    

    childfunction := nil;
    //search time info
    dumpinfo := nil;
    if ProcessObject.StackDumpInfo[isnapshot].Nr = stack.SnapshotNr then
      dumpinfo := ProcessObject.StackDumpInfo[isnapshot]
    else
    begin
      for I := 0 to ProcessObject.StackDumpCount - 1 do
      if ProcessObject.StackDumpInfo[i].Nr = stack.SnapshotNr then
      begin
        dumpinfo := ProcessObject.StackDumpInfo[i];
        Break;
      end;
    end;
    idifftime := 0;
    if dumpinfo <> nil then
    begin
      if iprevtime > 0 then
        idifftime := dumpinfo.Time - iprevtime;
      iprevtime := dumpinfo.Time;
    end;
    //calc time
    if ProcessObject.CounterFrequency > 0 then
      ftime := (idifftime / ProcessObject.CounterFrequency) * 1000    //ticks / tick per second = ys * 1000 = x ms
    else
      ftime := 0;

    //process stack functions
    for istack := Low(stack.Functions) to High(stack.Functions) do
    begin
      if not FProcessObject.GetLocationInfoRecord(stack.Functions[istack], info) then
      begin
        Continue;
      end;

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
        ff.LastStackNr  := -1;
        aThreadResult.FlatFunctions.Add(ff);
      end;
      {
      incF(ff.TotalTime, 1);
      if istack = 0 then
        incF(ff.TotalOwnTime, 1)
      else
        incF(ff.TotalChildTime, 1);
      }

      if ff.LastStackNr <> isnapshot then
      begin
        inc(ff.Count);
        //write time
        incF(ff.TotalTime, ftime);
        if istack = 0 then
          incF(ff.TotalOwnTime, ftime)
        else
          incF(ff.TotalChildTime, ftime);
        ff.SnapshotNrs.Add(stack.SnapshotNr);
      end;

      //------------------------------------
      //search line counter of function
      fl := ff.FunctionLines.GetLineByNumber(info.LineNumber, info.Address);
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
        fl.MinAddr := Cardinal(info.Address);
        fl.MaxAddr := Cardinal(info.Address);
      end;
      //process line
      inc(fl.Count);
      incF(fl.Time, 1);
      fl.MinAddr := Min(fl.MinAddr, Cardinal(info.Address));
      fl.MaxAddr := Max(fl.MaxAddr, Cardinal(info.Address));

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
        incF(parentsub.TotalTime,      ff.TotalTime);
        incF(parentsub.TotalOwnTime,   ff.TotalOwnTime);
        incF(parentsub.TotalChildTime, ff.TotalChildTime);

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
        incF(childsub.TotalTime,      childfunction.TotalTime);
        incF(childsub.TotalOwnTime,   childfunction.TotalOwnTime);
        incF(childsub.TotalChildTime, childfunction.TotalChildTime);
      end;

      ff.LastStackNr := isnapshot;
      childfunction := ff;
    end;
  end;

end;

procedure TSamplingResult.MakeFunctionSummary(const aSourceFunctions: TFlatFunctionList;
  var aDestFunctions: TFlatFunctionList);
var
  i: Integer;
  sf, df: TFlatFunction;
  sl, dl: TFunctionLine;
  j: Integer;
begin
  for i := 0 to aSourceFunctions.Count - 1 do
  begin
    sf := aSourceFunctions[i];

    df := aDestFunctions.GetFunctionByName(sf.CompleteName);
    if df = nil then
    begin
      df := TFlatFunction.Create;
      df.ModuleName       := sf.ModuleName;
      df.UnitName         := sf.UnitName;
      df.ClassName        := sf.ClassName;
      df.FunctionName     := sf.FunctionName;
      df.CompleteName     := sf.CompleteName;
      aDestFunctions.Add(df);
    end;
    Inc(df.Count, sf.Count);
    IncF(df.TotalTime, sf.TotalTime);
    IncF(df.TotalOwnTime, sf.TotalOwnTime);
    IncF(df.TotalChildTime, sf.TotalChildTime);

    for j := 0 to sf.FunctionLines.Count - 1 do
    begin
      sl := sf.FunctionLines[j];

      dl := df.FunctionLines.GetLineByNumber(sl.Line, sl.Address);
      if dl = nil then
      begin
        dl := TFunctionLine.Create;
        dl.Address     := sl.Address;
        dl.Line        := sl.Line;
        dl.LineString  := sl.LineString;
        df.FunctionLines.Add(dl);
//        dl.MinAddr     := sl.MinAddr;
//        dl.MaxAddr     := sl.MaxAddr;
        dl.MinAddr := Cardinal(sl.Address);
        dl.MaxAddr := Cardinal(sl.Address);
      end;
      Inc(dl.Count, sl.Count);
      IncF(dl.Time, sl.Time);
      dl.MinAddr := Min(dl.MinAddr, Cardinal(dl.Address));
      dl.MaxAddr := Max(dl.MaxAddr, Cardinal(dl.Address));
    end;

//    todo:
//      df.ChildCalls
//      df.ParentCalls
  end;
end;

end.
