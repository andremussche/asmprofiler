program DelphiStackViewer;

uses
  Forms,
  mfMain in 'mfMain.pas' {frmMain},
  mcThreadUtils in 'mcThreadUtils.pas',
  mcThreadSampler in 'mcThreadSampler.pas',
  mcProcessSampler in 'mcProcessSampler.pas',
  mcSamplingThread in 'mcSamplingThread.pas',
  mcTestThread in 'mcTestThread.pas',
  mcSamplingResult in 'mcSamplingResult.pas',
  MainForm in 'MainForm.pas' {frmMainForm},
  ProcessesFrame in 'ProcessesFrame.pas' {framProcesses: TFrame},
  LiveViewFrame in 'LiveViewFrame.pas' {framLiveView: TFrame},
  ProfilingFrame in 'ProfilingFrame.pas' {framProfiling: TFrame},
  SamplingResultsFrame in 'SamplingResultsFrame.pas' {framSamplingResults: TFrame},
  muExactTiming in 'muExactTiming.pas',
  ResultsForm in 'ResultsForm.pas' {frmSamplingResults},
  BaseTreeFrame in 'BaseTreeFrame.pas' {framBaseTree: TFrame},
  ResultsFunctionOverviewFrame in 'ResultsFunctionOverviewFrame.pas' {framFunctionOverview: TFrame},
  ResultsDetailedViewFrame in 'ResultsDetailedViewFrame.pas' {framDetailedView: TFrame},
  FunctionListFrame in 'FunctionListFrame.pas' {framFunctionList: TFrame},
  ChoiceMainForm in 'ChoiceMainForm.pas' {frmChoiceMainForm},
  BaseActionForm in 'BaseActionForm.pas' {frmAction};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
//  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
