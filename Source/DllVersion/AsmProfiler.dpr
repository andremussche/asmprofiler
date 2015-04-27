 library AsmProfiler;

uses
  SysUtils,
  Classes,
  Windows,
  _uAsmProfiler in '..\SRC\_uAsmProfiler.pas',
  _frmProfileMain in '..\SRC\_frmProfileMain.pas' {frmProfileMain},
  _framUnitTreeview in '..\SRC\_framUnitTreeview.pas' {framUnitTreeview: TFrame},
  _frmSelectItems in '..\SRC\_frmSelectItems.pas' {frmSelectItems},
  _uDllImplementation in '_uDllImplementation.pas',
  _frmResults in '..\SRC\_frmResults.pas' {frmResults},
  _uProfileTypes in '..\SRC\_uProfileTypes.pas',
  AsmDetours in '..\SRC\AsmDetours.pas',
  _uProfileClasses in '..\SRC\_uProfileClasses.pas',
  _framProfileResultTree in '..\SRC\_framProfileResultTree.pas' {framProfileResultTree: TFrame},
  JwaWinNT in '..\SRC\JwaWinNT.pas',
  _AsmDllInjectionHook in '_AsmDllInjectionHook.pas',
  _uProfilerManager in '..\SRC\_uProfilerManager.pas',
  _uProfileTools in '..\SRC\_uProfileTools.pas',
  jclDebug in '..\SRC\jclDebug.pas',
  JwaWinUser in '..\EXT\win32api\JwaWinUser.pas',
  _uRemoteControlThread in '..\SRC\_uRemoteControlThread.pas',
  _framProfileTraceTree in '..\SRC\_framProfileTraceTree.pas' {framProfileTraceTree: TFrame},
  _frmCallChart in '..\ProfilerResultViewer\_frmCallChart.pas' {frmCallChart},
  _uAsmProfDllInterface in '_uAsmProfDllInterface.pas';

{$E .dll}
{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

exports
  _uAsmProfiler.CreateProfileForm name C_CreateProfileForm,
  _uAsmProfiler.ShowProfileForm name C_ShowProfileForm,
  _uAsmProfiler.DestroyProfileForm name C_HideProfileForm,
  //
  _uDllImplementation.StartProfiler name C_StartProfiler,
  _uDllImplementation.StopProfiler name C_StopProfiler,
  //
  _uDllImplementation.ShowSelectItemsForm name C_ShowSelectItemsForm,
  _uDllImplementation.ShowResultsForm name C_ShowResultsForm,
  _uDllImplementation.SelectFunction name C_SelectFunction,
  //
  _uDllImplementation.AddCustomUnit name C_AddCustomUnit,
  _uDllImplementation.AddCustomFunction name C_AddCustomFunction,
  _uDllImplementation.AddCustomFunctionByIndex name C_AddCustomFunctionByIndex,
  //
  _AsmDllInjectionHook.StartDllInjectionHook name C_StartDllInjectionHook;

begin
  DllProc := @MyDLLProc;
end.




