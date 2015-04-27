unit _uAsmProfDllInterface;

interface

const
  C_AsmProfiler_dll           = 'AsmProfiler.dll';
  //
  C_CreateProfileForm         = 'CreateProfileForm';
  C_ShowProfileForm           = 'ShowProfileForm';
  C_HideProfileForm           = 'HideProfileForm';
  //
  C_StartProfiler             = 'StartProfiler';
  C_StopProfiler              = 'StopProfiler';
  //
  C_SelectFunction            = 'SelectFunction';
  C_ShowSelectItemsForm       = 'ShowSelectItemsForm';
  C_ShowResultsForm           = 'ShowResultsForm';
  //
  C_AddCustomUnit             = 'AddCustomUnit';
  C_AddCustomFunction         = 'AddCustomFunction';
  C_AddCustomFunctionByIndex  = 'AddCustomFunctionByIndex';
  //
  C_StartDllInjectionHook     = 'StartDllInjectionHook';

type
  TCreateProfileForm   = procedure;
  TShowProfileForm     = procedure;
  THideProfileForm     = procedure;

  TShowSelectItemsForm = procedure;
  TShowResultsForm     = procedure;
  TSelectFunction      = function(aUnit, aProcedure: pansichar {use "*""for all functions}): Integer; stdcall;
  //
  TStartProfiler      = procedure(aAskForConfirmation: boolean = true);
  TStopProfiler       = procedure;
  //
  TAddCustomFunction = procedure(aUnit, aProcedure: pchar; aFuntionPointer: pointer); stdcall;
  //
  TAddCustomUnit            = function(aUnit: pchar): integer; stdcall;
  TAddCustomFunctionByIndex = procedure(aUnitIndex: integer; aProcedure: pchar; aFuntionPointer: pointer); stdcall;
  //
  TStartDllInjectionHook    = procedure;stdcall;

implementation

end.
