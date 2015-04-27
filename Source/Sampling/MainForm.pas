unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, LiveViewFrame, ProcessesFrame, mcSamplingThread, ProfilingFrame, SamplingResultsFrame;

type
  TfrmMainForm = class(TForm)
    PageControl1: TPageControl;
    tsProcessesView: TTabSheet;
    tsLiveView: TTabSheet;
    tsProfiling: TTabSheet;
    framProcesses1: TframProcesses;
    framLiveView1: TframLiveView;
    tsProfilingResults: TTabSheet;
    framProfiling1: TframProfiling;
    framSamplingResults1: TframSamplingResults;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure LiveViewClicked(aProcessId: Integer);
    procedure ProfileItClicked(aProcessId: Integer);
    procedure ShowResultsClicked(aSamplingThread: TSamplingThread);
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

implementation

uses
  mcProcessSampler;

{$R *.dfm}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  framProcesses1.OnLiveViewClick  := Self.LiveViewClicked;
  framProcesses1.OnProfileItClick := Self.ProfileItClicked;

  framLiveView1.OnProfileItClick  := Self.ProfileItClicked;

  framProfiling1.OnResultsClick   := Self.ShowResultsClicked;

  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmMainForm.LiveViewClicked(aProcessId: Integer);
begin
  if aProcessId > 0 then
  begin
    framLiveView1.ProcessObject := TProcessSampler.Create(aProcessId);
    PageControl1.ActivePage     := tsLiveView;
  end;
end;

procedure TfrmMainForm.ProfileItClicked(aProcessId: Integer);
begin
  if aProcessId > 0 then
  begin
    //create new object for profiling
    framProfiling1.ProcessObject := TProcessSampler.Create(aProcessId);
    PageControl1.ActivePage      := tsProfiling;
  end;
end;

procedure TfrmMainForm.ShowResultsClicked(aSamplingThread: TSamplingThread);
begin
  if aSamplingThread <> nil then
  begin
//    framSamplingResults1.ProcessObject := aSamplingThread.ProcessObject;
    PageControl1.ActivePage            := tsProfilingResults;
  end;
end;

end.
