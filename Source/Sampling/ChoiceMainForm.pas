unit ChoiceMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, mcSamplingThread, mcSamplingResult;

type
  TfrmChoiceMainForm = class(TForm)
    PageControl1: TPageControl;
    tsChoice: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    btnViewProcesses: TBitBtn;
    btnViewResults: TBitBtn;
    btnStartProfiling: TBitBtn;
    procedure btnStartProfilingClick(Sender: TObject);
    procedure btnViewResultsClick(Sender: TObject);
    procedure btnViewProcessesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure LiveViewClicked(aProcessId: Integer);
    procedure ProfileItClicked(aProcessId: Integer);
    procedure ShowResultsClicked(aSamplingThread: TSamplingThread; aSamplingResult: TSamplingResult = nil);
  public
    { Public declarations }
  end;

var
  frmChoiceMainForm: TfrmChoiceMainForm;

implementation

uses
  ProfilingFrame, BaseActionForm, ResultsForm, ProcessesFrame, mcProcessSampler, LiveViewFrame,
  uDebugUtils;

{$R *.dfm}

procedure TfrmChoiceMainForm.btnStartProfilingClick(Sender: TObject);
var fram: TframProfiling;
begin
  fram := TframProfiling.Create(nil);
  fram.Name := fram.Name + IntToHex(Integer(fram),8);

  fram.OnResultsClick   := Self.ShowResultsClicked;

  TfrmAction.ShowFormWithFrame(fram, Format('Profiling',[]));
end;

procedure TfrmChoiceMainForm.btnViewProcessesClick(Sender: TObject);
var fram: TframProcesses;
begin
  fram := TframProcesses.Create(nil);
  fram.Name := fram.Name + IntToHex(Integer(fram),8);

  fram.OnLiveViewClick  := Self.LiveViewClicked;
  fram.OnProfileItClick := Self.ProfileItClicked;

  TfrmAction.ShowFormWithFrame(fram, Format('View running processes',[]));
end;

procedure TfrmChoiceMainForm.LiveViewClicked(aProcessId: Integer);
var fram: TframLiveView;
begin
  fram := TframLiveView.Create(nil);
  fram.Name := fram.Name + IntToHex(Integer(fram),8);

  if aProcessId > 0 then
    fram.ProcessObject  := TProcessSampler.Create(aProcessId);
  fram.OnProfileItClick := Self.ProfileItClicked;

  TfrmAction.ShowFormWithFrame(fram, Format('Live View (PID: %d)',[aProcessId]));
end;

procedure TfrmChoiceMainForm.ProfileItClicked(aProcessId: Integer);
var fram: TframProfiling;
begin
  fram := TframProfiling.Create(nil);
  fram.Name := fram.Name + IntToHex(Integer(fram),8);

  fram.OnResultsClick   := Self.ShowResultsClicked;
  if aProcessId > 0 then
    //create new object for profiling
    fram.ProcessObject := TProcessSampler.Create(aProcessId);

  TfrmAction.ShowFormWithFrame(fram, Format('Profiling (PID: %d)',[aProcessId]));
end;

procedure TfrmChoiceMainForm.btnViewResultsClick(Sender: TObject);
begin
  ShowResultsClicked(nil);
end;

procedure TfrmChoiceMainForm.FormCreate(Sender: TObject);
begin
  uDebugUtils.GetDebugPrivs;
  //EnableThreadPrivilege(True, 'SeDebugPrivilege');
end;

procedure TfrmChoiceMainForm.ShowResultsClicked(aSamplingThread: TSamplingThread; aSamplingResult: TSamplingResult = nil);
var frm: TfrmSamplingResults;
begin
  frm := TfrmSamplingResults.Create(Self);
  frm.Name := frm.Name + IntToHex(Integer(frm),8);

  if aSamplingResult <> nil then
    frm.SamplingResult := aSamplingResult
  else if aSamplingThread <> nil then
    frm.ProcessObject := aSamplingThread.ProcessObject;

  frm.Position := poDesigned;
  frm.Top  := Screen.ActiveCustomForm.Top + 25;
  frm.Left := Screen.ActiveCustomForm.Left + 25;

  frm.Show;
end;

end.
