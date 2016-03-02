unit SamplingResultsFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList,
  mcProcessSampler, mcSamplingResult, uResultTypes,
  ResultsDetailedViewFrame, System.Actions;

type
  TframSamplingResults = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edtFile: TEdit;
    edtPID: TEdit;
    GroupBox2: TGroupBox;
    pnlProcess: TPanel;
    edtStarted: TEdit;
    edtStopped: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtDuration: TEdit;
    edtSamples: TEdit;
    Label6: TLabel;
    btnRefreshStack: TBitBtn;
    btnLoad: TBitBtn;
    btnSave: TBitBtn;
    ActionList1: TActionList;
    actLoad: TAction;
    actSave: TAction;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    procedure btnRefreshStackClick(Sender: TObject);
    procedure actLoadExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
  private
    FProcessObject: TProcessSampler;
    FProcessResult: TProcessResult;
    FSamplingResult: TSamplingResult;
    FOnSamplingResultLoaded: TNotifyEvent;
    procedure SetProcessObject(const Value: TProcessSampler);
    procedure SetProcessResult(const Value: TProcessResult);
    procedure SetSamplingResult(const Value: TSamplingResult);
  protected
    property ProcessObject: TProcessSampler read FProcessObject write SetProcessObject;
    property ProcessResult: TProcessResult read FProcessResult write SetProcessResult;
  public
    { Public declarations }
    property SamplingResult: TSamplingResult read FSamplingResult write SetSamplingResult;
    property OnSamplingResultLoaded: TNotifyEvent read FOnSamplingResultLoaded write FOnSamplingResultLoaded;
  end;

implementation

uses
  ResultsForm, mcThreadSampler, uOfflineDebugInfo;

{$R *.dfm}

{ TframSamplingResults }

procedure TframSamplingResults.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actSave.Enabled := (ProcessObject <> nil) and (ProcessResult <> nil);
end;

procedure TframSamplingResults.actLoadExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if SamplingResult = nil then
      FSamplingResult := TSamplingResult.Create(nil);
    SamplingResult.LoadDumpsFromFile(OpenDialog1.FileName);

    Self.ProcessObject := SamplingResult.ProcessObject;
    Self.ProcessResult := SamplingResult.ProcessResult;

    if Assigned(OnSamplingResultLoaded) then
      OnSamplingResultLoaded(Self);

    {
    if ProcessObject = nil then
      ProcessObject := TProcessSampler.Create(0);
    ProcessObject.LoadDumpsFromFile(OpenDialog1.FileName);

    if ProcessResult = nil then
      ProcessResult := TProcessResult.Create;
    if ProcessObject.OfflineInfo = nil then
      ProcessObject.OfflineInfo := TOfflineDebugInfo.Create(ProcessResult);

    ProcessObject.OfflineInfo.LoadDebugInfoFromFile(
      ChangeFileExt(OpenDialog1.FileName, '.debug'));
    }
  end;
end;

procedure TframSamplingResults.actSaveExecute(Sender: TObject);
begin
  //FastMM4.ScanMemoryPoolForCorruptions;

  if SaveDialog1.Execute then
  begin
    SamplingResult.SaveDumpsToFile(SaveDialog1.FileName);

    {
    ProcessObject.SaveDumpsToFile(SaveDialog1.FileName);

    if ProcessObject.OfflineInfo = nil then
      ProcessObject.OfflineInfo := TOfflineDebugInfo.Create(ProcessResult);
    ProcessObject.OfflineInfo.SaveDebugInfoToFile(
      ChangeFileExt(SaveDialog1.FileName, '.debug'));

    //SaveDumpsToFile(ChangeFileExt(SaveDialog1.FileName, '.sampling'));
//    SaveDumpsToFile(SaveDialog1.FileName);
//    SaveDebugInfoToFile(m, ChangeFileExt(SaveDialog1.FileName, '.debug'));
    }
  end;
end;

procedure TframSamplingResults.btnRefreshStackClick(Sender: TObject);
//var
//  sr: TSamplingResult;
//  pr: TThreadItemArray;
//  ti: TThreadItem;
//  tr: TFunctionItemArray;
//  fi: TFunctionItem;
//  i: Integer;
//  j: Integer;
//  k: Integer;
begin
  with TfrmSamplingResults.Create(nil) do
  begin
    ProcessObject := Self.ProcessObject;
    { TODO : make as popup }
    ShowModal;
    Free;
    Exit;
  end;

  (*
  Screen.Cursor := crHourGlass;
  Memo1.Clear;
  Memo1.Lines.BeginUpdate;
  try

    sr := TSamplingResult.Create(FProcessObject);
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

  finally
    Memo1.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
  *)
end;

procedure TframSamplingResults.SetProcessObject(const Value: TProcessSampler);
begin
//  if FProcessObject <> nil then
//    FProcessObject.Free;

  FProcessObject := Value;

  if ProcessObject <> nil then
  begin
    edtPID.Text  := IntToStr(ProcessObject.ProcessId);
    edtFile.Text := ProcessObject.ProcessExe;

    edtStarted.Text  := DateTimeToStr(ProcessObject.FirstSample);
    edtStopped.Text  := DateTimeToStr(ProcessObject.LastSample);
    edtDuration.Text := TimeToStr(ProcessObject.LastSample - ProcessObject.FirstSample);
    edtSamples.Text  := IntToStr(ProcessObject.StackDumpCount);
  end;
end;

procedure TframSamplingResults.SetProcessResult(const Value: TProcessResult);
begin
  FProcessResult := Value;
end;

procedure TframSamplingResults.SetSamplingResult(const Value: TSamplingResult);
begin
  FSamplingResult := Value;
  if FSamplingResult <> nil then
  begin
    Self.ProcessObject := FSamplingResult.ProcessObject;
    Self.ProcessResult := FSamplingResult.ProcessResult;
  end;
end;

end.
