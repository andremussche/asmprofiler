unit _uDllImplementation;

interface

uses
  Windows;

  procedure ShowProfiler;stdcall;
  procedure HideProfiler;stdcall;
  procedure StartProfiler(aAskForConfirmation: boolean = true);stdcall;
  procedure StopProfiler;stdcall;
  procedure ShowSelectItemsForm;stdcall;
  procedure ShowResultsForm;stdcall;

  function  SelectFunction(aUnit, aProcedure: pansichar): Integer; stdcall;

  procedure MyDLLProc(Reason: Integer);
  //
  function  AddCustomUnit(aUnit: pansichar): integer; stdcall;
  procedure AddCustomFunction(aUnit, aProcedure: pansichar; aFuntionPointer: pointer); stdcall;
  procedure AddCustomFunctionByIndex(aUnitIndex: integer; aProcedure: pansichar; aFuntionPointer: pointer); stdcall;

implementation

uses _frmProfileMain, _uAsmProfiler, _uProfilerManager, _AsmDllInjectionHook,
  _uProfileClasses;

procedure ShowProfiler;stdcall;
begin
  ProfilerManager.ShowProfileForm;
end;

procedure HideProfiler;stdcall;
begin
  ProfilerManager.DestroyProfileForm;
end;

procedure StartProfiler(aAskForConfirmation: boolean = true);stdcall;
begin
  ProfilerManager.StartProfiling(aAskForConfirmation);

  {
  if frmProfileMain = nil then CreateProfileForm;
  frmProfileMain.Show;
  frmProfileMain.btnStart.Click;
  }
end;

procedure StopProfiler;stdcall;
begin
  if ProfilerManager.Started then
    ProfilerManager.StopProfiling;

  //if frmProfileMain = nil then CreateProfileForm;
  //frmProfileMain.Show;
  //frmProfileMain.btnstop.Click;
end;

procedure ShowSelectItemsForm;stdcall;
begin
  ProfilerManager.ShowSelectItemsForm;
end;

procedure ShowResultsForm;stdcall;
begin
  ProfilerManager.ShowResultsForm;
end;

function SelectFunction(aUnit, aProcedure: pansichar): Integer; stdcall;

  function __SelectFunction(aDebuginfo: TDebugInfoStorage; aUnit, aProcedure: pansichar): Integer;
  var
    iUnit, iProc, i: Integer;
  begin
    Result := -1;
    with aDebuginfo do
    begin
      iUnit := GetSegmentByName(string(aUnit));    //Windows
      if iUnit >= 0 then
      begin
        {add all procedure of unit}
        if aProcedure = '*' then
        begin
          iProc := -1;
          for i := low(UnitNames[iUnit].Procs) to high(UnitNames[iUnit].Procs) do
          begin
            iProc := GetProcByName(String(UnitNames[iUnit].Procs[i].ProcName), iUnit);
            AddSelectedProcedure(iUnit, iProc);
          end;
          Result := iProc;
        end
        else
        begin
          iProc := GetProcByName(string(aUnit + '.' + aProcedure), iUnit);  //Windows.Sleep
          if iProc >= 0 then
          begin
            AddSelectedProcedure(iUnit, iProc);
            Result := iProc;
            Exit;
          end;
        end;
      end;
    end;
  end;

begin
  Result   := __SelectFunction(ProfilerManager.MainMapFile, aUnit, aProcedure);
  if Result < 0 then
    Result := __SelectFunction(ProfilerManager.InternalDebugInfo, aUnit, aProcedure);
  if Result < 0 then
    Result := __SelectFunction(ProfilerManager.CustomDebugInfo, aUnit, aProcedure);
end;

function AddCustomUnit(aUnit: pansichar): integer; stdcall;
begin
  with ProfilerManager.CustomDebugInfo do
  begin
    Result := GetSegmentByName(String(aUnit));
    if Result < 0 then
      Result := AddSegment(String(aUnit));
  end;
end;

procedure AddCustomFunction(aUnit, aProcedure: pansichar; aFuntionPointer: pointer); stdcall;
var
  iUnitIndex: integer;
begin
  with ProfilerManager.CustomDebugInfo do
  begin
    iUnitIndex := GetSegmentByName(String(aUnit));
    if iUnitIndex < 0 then
      iUnitIndex := AddSegment(String(aUnit));
    AddCustomFunctionByIndex(iUnitIndex, aProcedure, aFuntionPointer);
  end;
end;

procedure AddCustomFunctionByIndex(aUnitIndex: integer; aProcedure: pansichar; aFuntionPointer: pointer); stdcall;
begin
  with ProfilerManager.CustomDebugInfo do
  begin
    AddProcedure(aUnitIndex, String(aProcedure), Cardinal(aFuntionPointer) );
  end;
end;

procedure MyDLLProc(Reason: Integer);
begin
  case reason of
    DLL_PROCESS_DETACH :
    begin
      StopProfiler;
      DestroyProfileForm;
    end;
    DLL_PROCESS_ATTACH :
    begin
      //ShowProfiler;
      StartDllInjectionHook;
    end;
  end;
end;

end.
