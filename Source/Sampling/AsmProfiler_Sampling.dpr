program AsmProfiler_Sampling;

uses
  //FastMM4,
  //VCLFixPack,
  Windows,
  Forms,
  mcThreadUtils in 'mcThreadUtils.pas',
  mcThreadSampler in 'mcThreadSampler.pas',
  mcProcessSampler in 'mcProcessSampler.pas',
  mcSamplingThread in 'mcSamplingThread.pas',
  mcTestThread in 'mcTestThread.pas',
  mcSamplingResult in 'mcSamplingResult.pas',
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
  BaseActionForm in 'BaseActionForm.pas' {frmAction},
  uOfflineDebugInfo in 'uOfflineDebugInfo.pas',
  uResultTypes in 'uResultTypes.pas',
  frThreadCharts in 'frThreadCharts.pas' {framThreadCharts: TFrame},
  uDllInjection in 'uDllInjection.pas',
  _uAsmProfDllInterface in '..\DllVersion\_uAsmProfDllInterface.pas',
  uDebugUtils in 'uDebugUtils.pas',
  RawStacksFrame in 'RawStacksFrame.pas' {framRawStacks: TFrame},
  VirtualTrees in '..\EXT\Virtual Treeview\Source\VirtualTrees.pas';

{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
//  Application.CreateForm(TfrmMain, frmMain);
//  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Title := 'AsmProfiler (Sampling mode)';
  Application.CreateForm(TfrmChoiceMainForm, frmChoiceMainForm);
  Application.Run;
end.
