unit ResultsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, mcProcessSampler, ResultsFunctionOverviewFrame,
  mcSamplingResult, BaseTreeFrame, ResultsDetailedViewFrame, SamplingResultsFrame,
  uResultTypes, frThreadCharts, RawStacksFrame, ExtCtrls, Menus;

type
  TfrmSamplingResults = class(TForm)
    pgMain: TPageControl;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    GroupBox1: TGroupBox;
    lbThreads: TListBox;
    pgResults: TPageControl;
    TabSheet1: TTabSheet;
    framFunctionOverview1: TframFunctionOverview;
    TabSheet2: TTabSheet;
    framDetailedView1: TframDetailedView;
    framSamplingResults1: TframSamplingResults;
    tsThreadChart: TTabSheet;
    framThreadCharts1: TframThreadCharts;
    tsRawStackTraces: TTabSheet;
    framRawStacks1: TframRawStacks;
    Splitter1: TSplitter;
    Panel1: TPanel;
    chkRawStackFilter: TCheckBox;
    Label1: TLabel;
    lblRawStackFilter: TLabel;
    N1: TMenuItem;
    SavetoAsmProfilerSelectionFile1: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure lbThreadsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure framFunctionOverview1vtreeItemsDblClick(Sender: TObject);
    procedure chkRawStackFilterClick(Sender: TObject);
    procedure SavetoAsmProfilerSelectionFile1Click(Sender: TObject);
  private
    FProcessObject: TProcessSampler;
    procedure SetProcessObject(const Value: TProcessSampler);
    procedure SetSamplingResult(const Value: TSamplingResult);
    { Private declarations }
  protected
    FSamplingResult: TSamplingResult;
    //FProcessResult: TProcessResult;
    procedure LoadResults;
    procedure SamplingResultLoaded(Sender: TObject);
  public
    { Public declarations }
    property ProcessObject : TProcessSampler read FProcessObject write SetProcessObject;
    property SamplingResult: TSamplingResult read FSamplingResult write SetSamplingResult;
  end;

var
  frmSamplingResults: TfrmSamplingResults;

implementation

uses
  mcThreadSampler, VirtualTrees, _uProfileTypes;

{$R *.dfm}

{ TForm1 }

procedure TfrmSamplingResults.chkRawStackFilterClick(Sender: TObject);
begin
  if not chkRawStackFilter.Checked then
  begin
    framRawStacks1.FilterStackNrs(nil);
    lblRawStackFilter.Caption := '';
  end;
end;

procedure TfrmSamplingResults.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Action = caHide then
    Action := caFree;
end;

procedure TfrmSamplingResults.FormCreate(Sender: TObject);
begin
  pgMain.ActivePageIndex := 0;
  pgResults.ActivePageIndex := 0;

  framSamplingResults1.OnSamplingResultLoaded := Self.SamplingResultLoaded;
end;

procedure TfrmSamplingResults.framFunctionOverview1vtreeItemsDblClick(
  Sender: TObject);
var
  nodes: TNodeArray;
  data: PTreeFunctionRec;
begin
  Nodes := framFunctionOverview1.vtreeItems.GetSortedSelection(True);
  if Length(Nodes) <= 0 then Exit;

  data := framFunctionOverview1.GetTreeNodeData(nodes[0]);

  if (data <> nil) and (data.ProfiledFunction <> nil) then
  begin
    lblRawStackFilter.Caption := data.ModuleName + '.' + data.FunctionName;
    chkRawStackFilter.Checked := True;

    pgResults.ActivePage := tsRawStackTraces;
    framRawStacks1.FilterStackNrs(data.ProfiledFunction.SnapshotNrs, data.FunctionName);
  end;
end;

procedure TfrmSamplingResults.lbThreadsClick(Sender: TObject);
var
//  ti: PThreadItem;
  tr: TThreadResult;
  ts: TThreadSampler;
//  o: TObject;
//  iThreadid: integer;
  i: Integer;
begin
  if lbThreads.ItemIndex >= 0 then
  begin
    tr := lbThreads.Items.Objects[lbThreads.ItemIndex] as TThreadResult;

    framFunctionOverview1.ThreadItem := tr;
    framDetailedView1.ThreadItem     := tr;

    for i := 0 to FSamplingResult.ProcessObject.ThreadCount-1 do
    begin
      ts := FSamplingResult.ProcessObject.Threads[i];
      if ts.ThreadId = tr.ThreadId then
      begin
        framRawStacks1.ProcessObject := SamplingResult.ProcessObject;
        framRawStacks1.ThreadSampler := ts;
        Break;
      end;
    end;

    if tr <> nil then
      framThreadCharts1.SelectedThreadId := tr.ThreadId
    else
      framThreadCharts1.SelectedThreadId := -1;
  end;
end;

procedure TfrmSamplingResults.LoadResults;
var
  tr: TThreadResult;
  i: Integer;
  j: Integer;
  ts: TThreadSampler;
  sthread: string;
begin
  Screen.Cursor := crHourGlass;
  try
    lbThreads.Clear;

//    FProcessResult := FSamplingResult.ProcessResult;
//    framSamplingResults1.ProcessResult := pr;
    framSamplingResults1.SamplingResult := FSamplingResult;

    //all threads
    for i := 0 to FSamplingResult.ProcessResult.ThreadResults.Count-1 do
    begin
      tr := FSamplingResult.ProcessResult.ThreadResults[i];
      sthread := IntToStr(tr.ThreadId);
      if tr.ThreadId = 0 then
        sthread := sthread + ' (all threads)';

      //lbThreads.Items.AddObject(IntToStr(tr.ThreadId), tr);

      for j := 0 to FSamplingResult.ProcessObject.ThreadCount-1 do
      begin
        ts := FSamplingResult.ProcessObject.Threads[j];
        if ts.ThreadId = tr.ThreadId then
        begin
          sthread := sthread + ' (cpu: ' + FormatDateTime('hh:nn:ss.zzz', ts.Usertime) + ')';
          Break;
        end;
      end;

      lbThreads.Items.AddObject(sthread, tr);
    end;

    if lbThreads.Count > 1 then
      lbThreads.ItemIndex := 1  //item 0 is "0 all threads", so select first real thread
    else
      lbThreads.ItemIndex := 0;
    lbThreadsClick(nil);      //select first thread

    framThreadCharts1.SamplingResult := FSamplingResult;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSamplingResults.SamplingResultLoaded(Sender: TObject);
begin
  FSamplingResult := framSamplingResults1.SamplingResult;
  //FProcessObject  := framSamplingResults1.SamplingResult.ProcessObject;
  LoadResults;
end;

procedure TfrmSamplingResults.SavetoAsmProfilerSelectionFile1Click(
  Sender: TObject);
var
  items: TSelectedUnitArray;
  i,j: Integer;
  fi: TFlatFunction;
  iFound, iProc: Integer;
begin
  //SetLength(items, framFunctionOverview1.ThreadItem.FlatFunctions.Count);

  SaveDialog1.InitialDir := ExtractFilePath(Self.ProcessObject.ProcessExe) + 'AsmProfiler';
  ForceDirectories(SaveDialog1.InitialDir);
  SaveDialog1.FileName   := ExtractFilePath(Self.ProcessObject.ProcessExe) + 'AsmProfiler\SelectedProcs.pselected';
  if not SaveDialog1.Execute then Exit;

  for i := 0 to framFunctionOverview1.ThreadItem.FlatFunctions.Count-1 do
  begin
    fi := framFunctionOverview1.ThreadItem.FlatFunctions.Functions[i];
    if fi.UnitName = '' then Continue;
    { TODO : only the same exe, dll's etc must be placed in seperate file!}
    if not SameText(Self.ProcessObject.ProcessExe, fi.ModuleName) then Continue;

    iFound := -1;
    for j := 0 to High(items) do
    begin
      if items[j].UnitName = fi.UnitName then
        iFound := j;
    end;

    if iFound < 0 then
    begin
      SetLength(items, Length(items)+1);
      iFound := High(items);
      items[iFound].UnitName := fi.UnitName;
    end;

    SetLength(items[iFound].SelectedProcs, Length(items[iFound].SelectedProcs)+1);
    iProc := High(items[iFound].SelectedProcs);
    items[iFound].SelectedProcs[iProc].ProcName := AnsiString(fi.UnitName + '.' + fi.CompleteName);
    Inc(items[iFound].SelectedProcsCount);
    //Inc(items[iFound].TotalProcsCount); not "all"?
    items[iFound].TotalProcsCount := items[iFound].SelectedProcsCount + 1; //one more so not "all"
  end;
  _uProfileTypes.Save_TSelectedUnitArray_ToFile(items, SaveDialog1.FileName);
end;

procedure TfrmSamplingResults.SetProcessObject(const Value: TProcessSampler);
begin
  FProcessObject := Value;

  //general tab
  //framSamplingResults1.ProcessObject := FProcessObject;

  //results tab
  if FProcessObject <> nil then
    FSamplingResult := TSamplingResult.Create(FProcessObject);

  LoadResults;
end;

procedure TfrmSamplingResults.SetSamplingResult(const Value: TSamplingResult);
begin
  FSamplingResult := Value;
  if Value <> nil then
    FProcessObject := Value.ProcessObject;

  LoadResults;
end;

end.
