unit mfMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  mcSamplingThread;

type
  TfrmMain = class(TForm)
    tmrRefresh: TTimer;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lbProcesses: TListBox;
    btnRefreshProcesses: TButton;
    lbThreads: TListBox;
    btnRefreshThreads: TButton;
    GroupBox3: TGroupBox;
    Memo1: TMemo;
    btnRefreshStack: TButton;
    chkAutoRefresh: TCheckBox;
    Label1: TLabel;
    lblDuration: TLabel;
    chkRaw: TCheckBox;
    Button1: TButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pnlProcess: TPanel;
    pnlThreads: TPanel;
    pnlStack: TPanel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure btnRefreshProcessesClick(Sender: TObject);
    procedure lbProcessesClick(Sender: TObject);
    procedure lbThreadsClick(Sender: TObject);
    procedure btnRefreshStackClick(Sender: TObject);
    procedure chkAutoRefreshClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnRefreshThreadsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FSamplingThread: TSamplingThread;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JclDebug, JclHookExcept, mcThreadUtils, JwaWinBase, PsAPI, TlHelp32, JwaWinNT, mcThreadSampler, mcProcessSampler,
  DateUtils, mcSamplingResult;

{$R *.dfm}

procedure TfrmMain.btnRefreshProcessesClick(Sender: TObject);
var str: TStrings;
begin
  str := nil;
  Screen.Cursor := crHourGlass;
  lbProcesses.Items.BeginUpdate;
  try
    lbProcesses.Items.Clear;
    str := mcThreadUtils.GetAllRunningProcesses;
    lbProcesses.Items.AddStrings(str);
  finally
    lbProcesses.Items.EndUpdate;
    Screen.Cursor := crDefault;
    str.Free;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  ms: TThreadSampler;
  pa: TPointerArray;
  i: Integer;
begin
  ms := lbThreads.Items.Objects[lbThreads.ItemIndex] as TThreadSampler;
  if ms.ThreadId = GetCurrentThreadId then
  begin
    MessageDlg('Stackwalk64 does not work on the same thread!', mtError, [mbOK], 0);
    Exit;
  end;

  Memo1.Lines.Clear;
  pa := GetCompleteThreadStack_64( TProcessSampler(ms.ProcessObject).ProcessHandle,
                                 ms.ThreadId);
  Memo1.Lines.Clear;
  for i := 0 to High(pa) do
    Memo1.Lines.Add( TProcessSampler(ms.ProcessObject).GetLocationInfo(pa[i]) );

  lblDuration.Caption := Format('%4.3fms',[GetSnapshotMakingDuration * MSecsPerDay]);
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
//  ai: TIntegerArray;
  iprocessid: Cardinal;
//  i: Integer;
  ps: TProcessSampler;
begin
  iprocessid := THandle(lbProcesses.Items.Objects[lbProcesses.ItemIndex]);
  ps         := TProcessSampler.Create(iprocessid);
  ps.RefreshThreads;

  if FSamplingThread <> nil then
  begin
    FSamplingThread.Terminate;
    FSamplingThread.WaitFor;
    FSamplingThread.Free;
  end;

  FSamplingThread := TSamplingThread.Create(True);
  FSamplingThread.ProcessObject := ps;
  FSamplingThread.SamplingInterval := 1; //1ms
  FSamplingThread.Resume;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  if FSamplingThread <> nil then
    FSamplingThread.Terminate;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
{
var
  sr: TSamplingResult;
  pr: TThreadItemArray;
  ti: TThreadItem;
  tr: TFunctionItemArray;
  fi: TFunctionItem;
  i: Integer;
  j: Integer;
  k: Integer;
begin
  Memo1.Clear;

  if FSamplingThread <> nil then
  begin
    sr := FSamplingThread.GetResults;
    pr := sr.GetProcessResult;

    //all threads
    for i := 0 to High(pr) do
    begin
      ti := pr[i];
      Memo1.Lines.Add('ThreadID: ' + IntToStr(ti.ThreadId));
      Memo1.Lines.Add('--------------------');

      //all functions
      tr := ti.ThreadResult;
      for j := 0 to High(tr) do
      begin
        fi := tr[j];
        Memo1.Lines.Add( Format('  Function: %s - Count: %d - Time: %d',
                                [fi.CompleteName, fi.Count, fi.TotalTime]) );

        //lines
        for k := 0 to High(fi.FunctionLines) do
          Memo1.Lines.Add( Format('    Line: %d - Count: %d - Time: %d',
                                  [fi.FunctionLines[k].Line,
                                   fi.FunctionLines[k].Count,
                                   fi.FunctionLines[k].Time]) );
      end;
      Memo1.Lines.Add('--------------------');
      Memo1.Lines.Add('');
    end;
  end;
  }
end;

procedure TfrmMain.btnRefreshStackClick(Sender: TObject);
begin
  lbThreadsClick(nil);
end;

procedure TfrmMain.btnRefreshThreadsClick(Sender: TObject);
begin
  lbProcessesClick(nil);
end;

procedure TfrmMain.chkAutoRefreshClick(Sender: TObject);
begin
  tmrRefresh.Enabled := chkAutoRefresh.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  btnRefreshProcesses.Click;
end;

procedure TfrmMain.lbProcessesClick(Sender: TObject);
var
//  ai: TIntegerArray;
  iprocessid: Cardinal;
  i: Integer;
  ps: TProcessSampler;
begin
  lbThreads.Items.BeginUpdate;
  try
    lbThreads.Clear;

    if lbProcesses.ItemIndex >= 0 then
    begin
      iprocessid := THandle(lbProcesses.Items.Objects[lbProcesses.ItemIndex]);

      ps         := TProcessSampler.Create(iprocessid);
      ps.RefreshThreads;

      for i := 0 to ps.ThreadCount - 1 do
        lbThreads.Items.AddObject( IntToStr(ps.Threads[i].ThreadId), ps.Threads[i]);
      //ai         := GetAllThreadIDsOfProcess(iprocessid);
      //for i := 0 to High(ai) do
      //  ListBox2.Items.AddObject( IntToStr(ai[i]), TObject(ai[i]) );
    end;

  finally
    lbThreads.Items.EndUpdate;
  end;
end;

procedure TfrmMain.lbThreadsClick(Sender: TObject);
var
//  iprocessid,
//  ithreadid: Cardinal;
//  hthread: THandle;
  list: TJclStackInfoList;
//  str: tstrings;

  ms: TThreadSampler;
begin

//  hthread := 0;
  list    := nil;
  Screen.Cursor := crHourGlass;
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Clear;
    if lbThreads.ItemIndex >= 0 then
    begin
//      iprocessid := THandle(ListBox1.Items.Objects[ListBox1.ItemIndex]);
//      ithreadid  := THandle(ListBox2.Items.Objects[ListBox2.ItemIndex]);
//      ms := TThreadSampler.Create(iprocessid, ithreadid);
      ms := lbThreads.Items.Objects[lbThreads.ItemIndex] as TThreadSampler;
      ms.MakeStackDump(-1);
      lblDuration.Caption := Format('%4.3fms',[ms.GetDumpMakingDuration * MSecsPerDay]);
      Memo1.Lines.Text := ms.TraceLastDump(chkRaw.Checked);
//      ms.Free;

//      list := GetThreadStackOfProcess(iprocessid, ithreadid);

//      hthread   := OpenThread(THREAD_ALL_ACCESS, False, ithreadid);
      //list := jcldebug.JclCreateThreadStackTraceFromId(False, ithreadid);
//      list := JclCreateStackList(False, 0, nil, False);
//      if list <> nil then
//        list.AddToStrings(Memo1.Lines, True, True, True, True);

      {
      str := GetCompleteThreadStack2(ithreadid);
      Memo1.Lines.Add('');
      Memo1.Lines.Add('===============');
      Memo1.Lines.AddStrings(str);
      }
    end;
  finally
    Memo1.Lines.EndUpdate;
    Screen.Cursor := crDefault;
    list.Free;
//    CloseHandle(hthread);
  end;
end;

procedure TfrmMain.tmrRefreshTimer(Sender: TObject);
begin
  tmrRefresh.Enabled := False;
  try
    lbThreadsClick(nil);
  finally
    tmrRefresh.Enabled := chkAutoRefresh.Checked;
  end;
end;

end.
