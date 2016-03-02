unit ProfilingFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, mcProcessSampler, ExtCtrls, Spin, Buttons, ActnList, mcSamplingThread, mcSamplingResult,
  System.Actions;

type
  TResultsNotify = procedure(aSamplingThread: TSamplingThread; aSamplingResult: TSamplingResult = nil) of object;

  TframProfiling = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edtFile: TEdit;
    edtPID: TEdit;
    GroupBox2: TGroupBox;
    pnlProcess: TPanel;
    Label3: TLabel;
    edtSamplingInterval: TSpinEdit;
    Label4: TLabel;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    btnShowResults: TBitBtn;
    ActionList1: TActionList;
    actStart: TAction;
    actStop: TAction;
    actResults: TAction;
    Timer1: TTimer;
    Label7: TLabel;
    cmbxPrio: TComboBox;
    Panel2: TPanel;
    btnSelectProcess: TBitBtn;
    GroupBox3: TGroupBox;
    Label8: TLabel;
    edtProcessExe: TEdit;
    BitBtn5: TBitBtn;
    chkStopAfterProfiling: TCheckBox;
    btnStartNow: TBitBtn;
    GroupBox4: TGroupBox;
    Panel3: TPanel;
    Label6: TLabel;
    Label5: TLabel;
    lblSamplesTaken: TLabel;
    lblSampleRate: TLabel;
    Label9: TLabel;
    lblActualInterval: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    OpenDialog1: TOpenDialog;
    Label10: TLabel;
    edtParams: TEdit;
    btnStopProcess: TBitBtn;
    chkSampleWhenUIisBusy: TCheckBox;
    edtUIWaitTime: TSpinEdit;
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actResultsExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure btnSelectProcessClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure btnStartNowClick(Sender: TObject);
    procedure btnStopProcessClick(Sender: TObject);
    procedure edtUIWaitTimeChange(Sender: TObject);
    procedure chkSampleWhenUIisBusyClick(Sender: TObject);
  private
    FProcessObject: TProcessSampler;
    FOnResultsClick: TResultsNotify;
    procedure SetProcessObject(const Value: TProcessSampler);
  protected
    FSamplingThread: TSamplingThread;
    FSamplingResult : TSamplingResult;
    FPrevSampleCount: Integer;
    procedure ProfileItClicked(aProcessId: Integer);
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    property ProcessObject: TProcessSampler read FProcessObject write SetProcessObject;

    property OnResultsClick: TResultsNotify read FOnResultsClick write FOnResultsClick;  
  end;

implementation

uses
  ProcessesFrame, BaseActionForm;

{$R *.dfm}

{ TframProfiling }

procedure TframProfiling.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actStart.Enabled   := (ProcessObject <> nil) and
                        ( (FSamplingThread = nil) or
                          FSamplingThread.Terminated);
  actStop.Enabled    := (ProcessObject <> nil) and
                        ( (FSamplingThread <> nil) and
                          not FSamplingThread.Terminated);
  actResults.Enabled := (ProcessObject <> nil) and
                        ( (FSamplingThread <> nil) and
                          FSamplingThread.Terminated);
end;

procedure TframProfiling.actResultsExecute(Sender: TObject);
begin
  if FSamplingThread <> nil then
  begin
    if Assigned(OnResultsClick) then
      OnResultsClick(FSamplingThread, FSamplingResult);
  end;
end;

procedure TframProfiling.actStartExecute(Sender: TObject);
//var
//  i: Integer;
begin
  FreeAndNil(FSamplingResult);

  if ProcessObject = nil then Exit;
  ProcessObject.ClearAllStackdumps;
  ProcessObject.RefreshThreads;

  if FSamplingThread <> nil then
  begin
    FSamplingThread.Terminate;
    FSamplingThread.WaitFor;
    FSamplingThread.Free;
  end;

  if edtProcessExe.Text <> '' then
    ProcessObject.Params := edtParams.Text;

  FSamplingThread                  := TSamplingThread.Create(True);
  FSamplingThread.ProcessObject    := ProcessObject;
  FSamplingThread.SamplingInterval := edtSamplingInterval.Value; //default 1 ms
  FSamplingThread.Priority         := TThreadPriority(cmbxPrio.ItemIndex);
  FSamplingThread.OnlySampleWhenUIisBusy := chkSampleWhenUIisBusy.Checked;
  FSamplingThread.UIbusyWaitTimeMsec     := edtUIWaitTime.Value;

  FSamplingThread.Start;

  Timer1.Enabled := True;
end;

procedure TframProfiling.actStopExecute(Sender: TObject);
begin
//  Timer1.Enabled := False;

  if FSamplingThread <> nil then
  begin
    FSamplingThread.Terminate;
    FSamplingThread.WaitFor;

    FSamplingResult := TSamplingResult.Create(FProcessObject);
    FSamplingResult.ProcessResult; //get and make result

    if ProcessObject.ManualStarted and
       chkStopAfterProfiling.Checked then
    begin
      //actResults.Execute;  //first get all debug info etc of running process (!)
      ProcessObject.ManualStopExe;
    end;
  end;
end;

procedure TframProfiling.AfterConstruction;
begin
  inherited;
{
0 IDLE_PRIORITY_CLASS
1 BELOW_NORMAL_PRIORITY_CLASS
2 NORMAL_PRIORITY_CLASS
3 ABOVE_NORMAL_PRIORITY_CLASS
4 HIGH_PRIORITY_CLASS
}
  cmbxPrio.Items.Clear;
  cmbxPrio.Items.Add('0: Idle');
  cmbxPrio.Items.Add('1: Below normal');
  cmbxPrio.Items.Add('2: Normal');
  cmbxPrio.Items.Add('3: Above normal');
  cmbxPrio.Items.Add('4: High');
  cmbxPrio.ItemIndex := 2; {default normal}
end;

procedure TframProfiling.BitBtn5Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edtProcessExe.Text := OpenDialog1.FileName;
    Self.ProcessObject := TProcessSampler.Create(OpenDialog1.FileName);
  end;
end;

procedure TframProfiling.btnStartNowClick(Sender: TObject);
begin
  if FProcessObject <> nil then
  begin
    ProcessObject.Params := edtParams.Text;
    FProcessObject.ManualStartExe;
    edtPID.Text  := IntToStr(ProcessObject.ProcessId);
  end;
end;

procedure TframProfiling.btnStopProcessClick(Sender: TObject);
begin
  if FProcessObject <> nil then
  begin
    ProcessObject.ManualStopExe;
    edtPID.Text  := IntToStr(ProcessObject.ProcessId);
  end;
end;

procedure TframProfiling.chkSampleWhenUIisBusyClick(Sender: TObject);
begin
  if FSamplingThread <> nil then
    FSamplingThread.OnlySampleWhenUIisBusy := chkSampleWhenUIisBusy.Checked;
end;

destructor TframProfiling.Destroy;
begin
  ProcessObject.Free;
  inherited;
end;

procedure TframProfiling.edtUIWaitTimeChange(Sender: TObject);
begin
  if FSamplingThread <> nil then
    FSamplingThread.UIbusyWaitTimeMsec     := edtUIWaitTime.Value;
end;

procedure TframProfiling.btnSelectProcessClick(Sender: TObject);
var fram: TframProcesses;
begin
  fram := TframProcesses.Create(nil);
  fram.Name := fram.Name + IntToHex(Integer(fram),8);

  fram.OnProfileItClick := Self.ProfileItClicked;

  TfrmAction.ShowModalFormWithFrame(fram, Format('Select process for profiling',[]));
end;

procedure TframProfiling.ProfileItClicked(aProcessId: Integer);
begin
  Self.ProcessObject := TProcessSampler.Create(aProcessId);
end;

procedure TframProfiling.SetProcessObject(const Value: TProcessSampler);
begin
  if FProcessObject <> nil then
    FProcessObject.Free;

  FProcessObject := Value;

  if ProcessObject <> nil then
  begin
    edtPID.Text  := IntToStr(ProcessObject.ProcessId);
    edtFile.Text := ProcessObject.ProcessExe;
  end;
end;

procedure TframProfiling.Timer1Timer(Sender: TObject);
var
  iCount, iDiv: Integer;
begin
  iCount := FSamplingThread.ProcessObject.StackDumpCount;
  lblSamplesTaken.Caption := IntToStr(iCount);

  iDiv  := Abs(FPrevSampleCount - iCount);
  lblSampleRate.Caption := Format('%d / s', [iDiv]);

  edtPID.Text  := IntToStr(ProcessObject.ProcessId);

  FPrevSampleCount := iCount;

  Timer1.Enabled := (FSamplingThread <> nil) and
                    not FSamplingThread.Terminated;
end;

end.
