unit LiveViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, Buttons, mcProcessSampler, ActnList;

type
  TProfileNotify  = procedure(aProcessId: Integer) of object;

  TframLiveView = class(TFrame)
    GroupBox1: TGroupBox;
    pnlThreads: TPanel;
    lbThreads: TListBox;
    Splitter1: TSplitter;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    pnlStack: TPanel;
    chkAutoRefresh: TCheckBox;
    chkRaw: TCheckBox;
    btnRefreshThreads: TBitBtn;
    btnRefreshStack: TBitBtn;
    Button1: TBitBtn;
    GroupBox3: TGroupBox;
    edtFile: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtPID: TEdit;
    Panel1: TPanel;
    Label1: TLabel;
    lblDuration: TLabel;
    ActionList1: TActionList;
    actRefreshThreads: TAction;
    actRefreshStack: TAction;
    actTryStackwalk64API: TAction;
    btnProfileIt: TBitBtn;
    tmrRefresh: TTimer;
    procedure actRefreshThreadsExecute(Sender: TObject);
    procedure actRefreshStackExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actTryStackwalk64APIExecute(Sender: TObject);
    procedure lbThreadsClick(Sender: TObject);
    procedure btnProfileItClick(Sender: TObject);
    procedure chkAutoRefreshClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure chkRawClick(Sender: TObject);
  private
    FProcessObject: TProcessSampler;
    FOnProfileItClick: TProfileNotify;
    procedure SetProcessObject(const Value: TProcessSampler);
  public
    destructor Destroy;override;

    property ProcessObject: TProcessSampler read FProcessObject write SetProcessObject;
    property OnProfileItClick: TProfileNotify read FOnProfileItClick write FOnProfileItClick;
  end;

implementation

uses
  mcThreadSampler, mcThreadUtils;

{$R *.dfm}

procedure TframLiveView.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actRefreshThreads.Enabled := (ProcessObject <> nil);
  actRefreshStack.Enabled   := (ProcessObject <> nil) and (lbThreads.ItemIndex >= 0);
end;

procedure TframLiveView.actRefreshStackExecute(Sender: TObject);
var
  ts: TThreadSampler;
begin
  Screen.Cursor := crHourGlass;
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Clear;
    if lbThreads.ItemIndex >= 0 then
    begin
      ts := lbThreads.Items.Objects[lbThreads.ItemIndex] as TThreadSampler;
      ts.MakeStackDump(-1);
      lblDuration.Caption := Format('%4.3fms',[ts.GetDumpMakingDuration * MSecsPerDay]);
      Memo1.Lines.Text    := ts.TraceLastDump(chkRaw.Checked);
    end;
  finally
    Memo1.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TframLiveView.actRefreshThreadsExecute(Sender: TObject);
var
  i: Integer;
  ts: TThreadSampler;
begin
  if ProcessObject = nil then Exit;

  lbThreads.Items.BeginUpdate;
  try

    ProcessObject.RefreshThreads;

    lbThreads.Clear;
    for i := 0 to ProcessObject.ThreadCount - 1 do
    begin
      ts := ProcessObject.Threads[i];
      lbThreads.Items.AddObject( IntToStr(ts.ThreadId), ts);
    end;

  finally
    lbThreads.Items.EndUpdate;
  end;
end;

procedure TframLiveView.actTryStackwalk64APIExecute(Sender: TObject);
var
  ts: TThreadSampler;
  pa: TPointerArray;
  i: Integer;
begin
  Screen.Cursor := crHourGlass;
  Memo1.Lines.BeginUpdate;
  try
    ts := lbThreads.Items.Objects[lbThreads.ItemIndex] as TThreadSampler;
    if ts.ThreadId = GetCurrentThreadId then
    begin
      MessageDlg('Stackwalk64 API does not work on the same thread!', mtError, [mbOK], 0);
      Exit;
    end;

    pa := mcThreadUtils.GetCompleteThreadStack_64(
                                   TProcessSampler(ts.ProcessObject).ProcessHandle,
                                   ts.ThreadId);
    Memo1.Lines.Clear;
    for i := 0 to High(pa) do
      Memo1.Lines.Add( TProcessSampler(ts.ProcessObject).GetLocationInfo(pa[i]) );

    lblDuration.Caption := Format('%4.3fms',[GetSnapshotMakingDuration * MSecsPerDay]);
  finally
    Memo1.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TframLiveView.btnProfileItClick(Sender: TObject);
begin
  if ProcessObject <> nil then
  begin
    if Assigned(OnProfileItClick) then
      OnProfileItClick(ProcessObject.ProcessId);
  end;
end;

procedure TframLiveView.chkAutoRefreshClick(Sender: TObject);
begin
  tmrRefresh.Enabled := chkAutoRefresh.Checked;
end;

procedure TframLiveView.chkRawClick(Sender: TObject);
begin
  actRefreshStack.Execute;
end;

destructor TframLiveView.Destroy;
begin
  ProcessObject.Free;
  inherited;
end;

procedure TframLiveView.lbThreadsClick(Sender: TObject);
begin
  actRefreshStack.Execute;
end;

procedure TframLiveView.SetProcessObject(const Value: TProcessSampler);
begin
  if FProcessObject <> nil then
    FProcessObject.Free;

  FProcessObject := Value;

  if ProcessObject <> nil then                       
  begin
    edtPID.Text  := IntToStr(ProcessObject.ProcessId);
    edtFile.Text := ProcessObject.ProcessExe;

    actRefreshThreads.Execute;   //load threads
    lbThreads.ItemIndex := 0;
    actRefreshStack.Execute;     //show stack of first thread
  end;
end;

procedure TframLiveView.tmrRefreshTimer(Sender: TObject);
begin
  tmrRefresh.Enabled := False;
  try
    actRefreshStack.Execute;
  finally
    tmrRefresh.Enabled := True;
  end;
end;

end.
