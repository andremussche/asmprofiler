program ProfilerResultViewer;

uses
  //FastMM4,
  Forms,
  _frmMain in '_frmMain.pas' {frmMain},
  _frmResults in '..\SRC\_frmResults.pas' {frmResults},
  _uProfilerManager in '..\SRC\_uProfilerManager.pas',
  _framProfileResultTree in '..\SRC\_framProfileResultTree.pas' {framProfileResultTree: TFrame},
  _framProfileTraceTree in '..\SRC\_framProfileTraceTree.pas' {framProfileTraceTree: TFrame},
  _frmCallChart in '_frmCallChart.pas' {frmCallChart};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Profiler Result Viewer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
