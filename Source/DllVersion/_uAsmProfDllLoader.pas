unit _uAsmProfDllLoader;

interface

uses
  Windows;

  function  LoadProfilerDll: boolean;
  function  IsProfilerDllLoaded: boolean;
  procedure UnLoadProfilerDll;

  procedure CreateProfileForm;
  procedure ShowProfileForm;
  //
  procedure StartProfiler(aAskForConfirmation: boolean = true);
  procedure StopProfiler;
  //
  procedure AddCustomFunction(aUnit, aProcedure: pchar; aFuntionPointer: pointer);
  function  AddCustomUnit(aUnit: pchar): integer;
  procedure AddCustomFunctionByIndex(aUnitIndex: integer; aProcedure: pchar; aFuntionPointer: pointer);

implementation

const
  C_AsmProfiler_dll       = 'AsmProfiler.dll';
  //
  C_CreateProfileForm     = 'CreateProfileForm';
  C_ShowProfileForm       = 'ShowProfileForm';
  //
  C_StartProfiler          = 'StartProfiler';
  C_StopProfiler           = 'StopProfiler';
  //
  C_AddCustomUnit             = 'AddCustomUnit';
  C_AddCustomFunction         = 'AddCustomFunction';
  C_AddCustomFunctionByIndex  = 'AddCustomFunctionByIndex';
  //
  C_StartDllInjectionHook     = 'StartDllInjectionHook';

type
  TCreateProfileForm  = procedure;stdcall;
  TShowProfileForm    = procedure;stdcall;
  //
  TStartProfiler      = procedure(aAskForConfirmation: boolean = true);stdcall;
  TStopProfiler       = procedure;stdcall;
  //
  TAddCustomFunction  = procedure(aUnit, aProcedure: pchar; aFuntionPointer: pointer); stdcall;
  //
  TAddCustomUnit            = function(aUnit: pchar): integer; stdcall;
  TAddCustomFunctionByIndex = procedure(aUnitIndex: integer; aProcedure: pchar; aFuntionPointer: pointer); stdcall;
  //
  TStartDllInjectionHook    = procedure;stdcall;

var
  hAsmProfilerDll: THandle;
  //
  pCreateProfileForm:TCreateProfileForm;
  pShowProfileForm:TShowProfileForm;
  //
  pStartProfiler: TStartProfiler;
  pStopProfiler: TStopProfiler;
  //
  pAddCustomFunction: TAddCustomFunction;
  pAddCustomUnit: TAddCustomUnit;
  pAddCustomFunctionByIndex: TAddCustomFunctionByIndex;

function LoadProfilerDll: boolean;
begin
  Result := (hAsmProfilerDll > 0);
  if hAsmProfilerDll > 0 then exit;
  hAsmProfilerDll := LoadLibrary(C_AsmProfiler_dll);

  if hAsmProfilerDll > 0 then
  begin
    pCreateProfileForm     := GetProcAddress(hAsmProfilerDll,C_CreateProfileForm);
    pShowProfileForm       := GetProcAddress(hAsmProfilerDll,C_ShowProfileForm);
    //
    pStartProfiler         := GetProcAddress(hAsmProfilerDll,C_StartProfiler);
    pStopProfiler          := GetProcAddress(hAsmProfilerDll,C_StopProfiler);
    //
    pAddCustomFunction          := GetProcAddress(hAsmProfilerDll,C_AddCustomFunction);
    pAddCustomUnit              := GetProcAddress(hAsmProfilerDll,C_AddCustomUnit);
    pAddCustomFunctionByIndex   := GetProcAddress(hAsmProfilerDll,C_AddCustomFunctionByIndex);
  end
  else
    UnloadProfilerDll;

  Result := (hAsmProfilerDll > 0);
end;

function IsProfilerDllLoaded: boolean;
begin
  Result := (hAsmProfilerDll > 0);
end;

procedure UnLoadProfilerDll;
begin
  FreeLibrary(hAsmProfilerDll);

  pCreateProfileForm     := nil;
  pShowProfileForm       := nil;
  //
  pStartProfiler    := nil;
  pStopProfiler     := nil;
  //
  pAddCustomFunction          := nil;
  pAddCustomUnit              := nil;
  pAddCustomFunctionByIndex   := nil;
end;

procedure CreateProfileForm;
begin
  assert( Assigned(pCreateProfileForm), '"CreateProfileForm" not assigned! ProfilerDll not loaded?');
  pCreateProfileForm;
end;

procedure ShowProfileForm;
begin
  assert( Assigned(pShowProfileForm), '"ShowProfileForm" not assigned! ProfilerDll not loaded?');
  pShowProfileForm;
end;

procedure StartProfiler(aAskForConfirmation: boolean = true);
begin
  assert( Assigned(pStartProfiler), '"StartProfiler" not assigned! ProfilerDll not loaded?');
  pStartProfiler(aAskForConfirmation);
end;

procedure StopProfiler;
begin
  assert( Assigned(pStopProfiler), '"StopProfiler" not assigned! ProfilerDll not loaded?');
  pStopProfiler;
end;

procedure AddCustomFunction(aUnit, aProcedure: pchar; aFuntionPointer: pointer);
begin
  assert( Assigned(pAddCustomFunction), '"AddCustomFunction" not assigned! ProfilerDll not loaded?');
  pAddCustomFunction(aUnit, aProcedure, aFuntionPointer);
end;

function  AddCustomUnit(aUnit: pchar): integer;
begin
  assert( Assigned(pAddCustomUnit), '"AddCustomUnit" not assigned! ProfilerDll not loaded?');
  Result := pAddCustomUnit(aUnit);
end;

procedure AddCustomFunctionByIndex(aUnitIndex: integer; aProcedure: pchar; aFuntionPointer: pointer);
begin
  assert( Assigned(pAddCustomFunctionByIndex), '"AddCustomFunctionByIndex" not assigned! ProfilerDll not loaded?');
  pAddCustomFunctionByIndex(aUnitIndex, aProcedure, aFuntionPointer);
end;

procedure Initialize;
var
  i:integer;
begin
  for i := 0 to ParamCount do
  begin
    if ParamStr(i) = '-profile' then
    begin
      if LoadProfilerDll then
      begin
        {$ifndef NOT_VISIBLE}
        ShowProfileForm;
        {$endif}
      end;
    end;

    if (ParamStr(i) = '-profilestart') then
    begin
      if LoadProfilerDll then
      begin
        {$ifndef NOT_VISIBLE}
        ShowProfileForm;
        {$endif}
        StartProfiler(false);
      end;
    end;
  end;
end;

initialization
  Initialize;

finalization
  UnLoadProfilerDll;

end.
