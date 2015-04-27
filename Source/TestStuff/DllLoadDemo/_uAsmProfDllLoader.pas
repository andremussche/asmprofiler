unit _uAsmProfDllLoader;

interface

uses
  _uAsmProfDllInterface, Windows;

  function LoadProfilerDll: boolean;
  procedure UnLoadProfilerDll;

var
  CreateProfileForm: TCreateProfileForm;
  ShowProfileForm: TShowProfileForm;
  HideProfileForm: THideProfileForm;
  //
  ShowSelectItemsForm: TShowSelectItemsForm;
  ShowResultsForm: TShowResultsForm;
  SelectFunction: TSelectFunction;
  //
  StartProfiler: TStartProfiler;
  StopProfiler: TStopProfiler;
  //
  AddCustomFunction: TAddCustomFunction;
  AddCustomUnit: TAddCustomUnit;
  AddCustomFunctionByIndex: TAddCustomFunctionByIndex;

implementation

var
  hAsmProfilerDll: THandle;

function LoadProfilerDll: boolean;
begin
  Result := (hAsmProfilerDll > 0);
  if hAsmProfilerDll > 0 then exit;
  hAsmProfilerDll := LoadLibrary(C_AsmProfiler_dll);

  if hAsmProfilerDll > 0 then
  begin
    CreateProfileForm     := GetProcAddress(hAsmProfilerDll,C_CreateProfileForm);
    ShowProfileForm       := GetProcAddress(hAsmProfilerDll,C_ShowProfileForm);
    HideProfileForm       := GetProcAddress(hAsmProfilerDll,C_HideProfileForm);
    //
    ShowSelectItemsForm   := GetProcAddress(hAsmProfilerDll,C_ShowSelectItemsForm);
    ShowResultsForm       := GetProcAddress(hAsmProfilerDll,C_ShowResultsForm);
    SelectFunction        := GetProcAddress(hAsmProfilerDll,C_SelectFunction);
    //
    StartProfiler    := GetProcAddress(hAsmProfilerDll,C_StartProfiler);
    StopProfiler     := GetProcAddress(hAsmProfilerDll,C_StopProfiler);
    //
    AddCustomFunction          := GetProcAddress(hAsmProfilerDll,C_AddCustomFunction);
    AddCustomUnit              := GetProcAddress(hAsmProfilerDll,C_AddCustomUnit);
    AddCustomFunctionByIndex   := GetProcAddress(hAsmProfilerDll,C_AddCustomFunctionByIndex);
  end
  else
    UnloadProfilerDll;

  Result := (hAsmProfilerDll > 0);
end;

procedure UnLoadProfilerDll;
begin
  FreeLibrary(hAsmProfilerDll);

  CreateProfileForm     := nil;
  ShowProfileForm       := nil;
  //
  StartProfiler    := nil;
  StopProfiler     := nil;
  //
  AddCustomFunction          := nil;
  AddCustomUnit              := nil;
  AddCustomFunctionByIndex   := nil;
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
