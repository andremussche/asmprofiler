program ProfTestApp;

{%File 'Virtual Treeview\TMSourceOnly\'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  _frmProfileMain in '..\SRC\_frmProfileMain.pas' {frmProfileMain},
  _frmSelectItems in '..\SRC\_frmSelectItems.pas' {frmSelectItems},
  _uProfileTypes in '..\SRC\_uProfileTypes.pas',
  _frmResults in '..\SRC\_frmResults.pas' {frmResults},
  AsmDetours in '..\SRC\AsmDetours.pas',
  _uAsmProfiler in '..\SRC\_uAsmProfiler.pas',
  _framUnitTreeview in '..\SRC\_framUnitTreeview.pas' {Frame2: TFrame},
  _uProfileClasses in '..\SRC\_uProfileClasses.pas',
  _uDllImplementation in '..\DllVersion\_uDllImplementation.pas',
  _framProfileResultTree in '..\SRC\_framProfileResultTree.pas' {framProfileResultTree: TFrame},
  _framProfileResultProcsTree in '..\SRC\_framProfileResultProcsTree.pas' {framProfileResultProcsTree: TFrame},
  _uProfilerManager in '..\SRC\_uProfilerManager.pas',
  _uProfileTools in '..\SRC\_uProfileTools.pas',
//  _uDbgLoader in '..\DllVersion\_uDbgLoader.pas',
  _uAsmProfDllInterface in '..\DllVersion\_uAsmProfDllInterface.pas',
  _uTestFunctions in '_uTestFunctions.pas',
  _AsmDllInjectionHook in '..\DllVersion\_AsmDllInjectionHook.pas',
  _uTestThread in '_uTestThread.pas',
  GpLists in '..\SRC\GpLists.pas',
  _uRemoteControlThread in '..\SRC\_uRemoteControlThread.pas',
  _framProfileTraceTree in '..\SRC\_framProfileTraceTree.pas' {framProfileTraceTree: TFrame},
  _frmCallChart in '..\ProfilerResultViewer\_frmCallChart.pas' {frmCallChart};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
