library AsmProfiler_inject;

{
}

uses
  FastMM4,
  SysUtils,
  Classes,
  Windows,
  _uAsmProfiler in '..\SRC\_uAsmProfiler.pas',
  _frmProfileMain in '..\SRC\_frmProfileMain.pas' {frmProfileMain},
  _uAsmProfDllInterface in '_uAsmProfDllInterface.pas',
  _framUnitTreeview in '..\SRC\_framUnitTreeview.pas' {framUnitTreeview: TFrame},
  _frmSelectItems in '..\SRC\_frmSelectItems.pas' {frmSelectItems},
  _uDllImplementation in '_uDllImplementation.pas',
  _frmResults in '..\SRC\_frmResults.pas' {frmResults},
  _uProfileTypes in '..\SRC\_uProfileTypes.pas',
  AsmDetours in '..\SRC\AsmDetours.pas',
  _uProfileClasses in '..\SRC\_uProfileClasses.pas',
  _framProfileResultTree in '..\SRC\_framProfileResultTree.pas' {framProfileResultTree: TFrame},
  _AsmDllInjectionHook in '_AsmDllInjectionHook.pas',
  _uProfilerManager in '..\SRC\_uProfilerManager.pas',
  _uProfileTools in '..\SRC\_uProfileTools.pas',
  GpLists in '..\SRC\GpLists.pas',
  JclDebug in '..\SRC\jclDebug.pas',
  _framProfileTraceTree in '..\SRC\_framProfileTraceTree.pas' {framProfileTraceTree: TFrame};
  //ToolsAPI in '..\..\..\..\program files\borland\delphi7\Source\ToolsAPI\ToolsAPI.pas',
  //DesignIntf in '..\..\..\..\Program Files\Borland\Delphi7\Source\ToolsAPI\DesignIntf.pas';

{$R *.res}

exports
  CreateProfileForm name C_CreateProfileForm,
  ShowProfileForm name C_ShowProfileForm,
  //
  StartProfiler name C_StartProfiler,
  StopProfiler name C_StopProfiler,
  //
  AddCustomUnit name C_AddCustomUnit,
  AddCustomFunction name C_AddCustomFunction,
  AddCustomFunctionByIndex name C_AddCustomFunctionByIndex;

begin
  {$ifndef dllinjection}
  Please add "dllinjection" to Project Options -> Defines
  {$endif}

  DllProc := @MyDLLProc;
end.




