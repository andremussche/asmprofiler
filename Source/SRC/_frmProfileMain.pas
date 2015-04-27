unit _frmProfileMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmProfileMain = class (TForm)
    btnClear: TBitBtn;
    btnFlush: TBitBtn;
    btnSave: TBitBtn;
    btnSelectItems: TBitBtn;
    btnShowResults: TBitBtn;
    btnStart: TBitBtn;
    btnstop: TBitBtn;
    edtSessionName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    lblItemCount: TLabel;
    lblState: TLabel;
    procedure btnClearClick(Sender: TObject);
    procedure btnFlushClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSelectItemsClick(Sender: TObject);
    procedure btnShowResultsClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnstopClick(Sender: TObject);
    procedure edtSessionNameEnter(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateEnabledState;
  end;

var
  frmProfileMain: TfrmProfileMain;

implementation

uses
  {$if CompilerVersion >= 24}  //Delphi XE3
  UITypes,
  {$ifend}
  _uProfilerManager, JclFileUtils, _uAsmProfDllInterface;

{$R *.dfm}

{
******************************* TfrmProfileMain ********************************
}
procedure TfrmProfileMain.btnClearClick(Sender: TObject);
begin
  ProfilerManager.Clear;
  btnstop.Click;
end;

procedure TfrmProfileMain.btnFlushClick(Sender: TObject);
begin
  MessageDlg('Not implemented yet...', mtInformation, [mbOK], 0);
  //todo: lock all profile arrays, save to file, clear profile arrays
  //SaveProfileTimesToFile;
end;

procedure TfrmProfileMain.btnSaveClick(Sender: TObject);
begin
  ProfilerManager.SaveAndClear;
end;

procedure TfrmProfileMain.btnSelectItemsClick(Sender: TObject);
begin
  ProfilerManager.ShowSelectItemsForm;
  lblItemCount.Caption :=  IntToStr(ProfilerManager.SelectedItemsCount);
end;

procedure TfrmProfileMain.btnShowResultsClick(Sender: TObject);
begin
  ProfilerManager.ShowResultsForm;
end;

procedure TfrmProfileMain.btnStartClick(Sender: TObject);
begin
  if ProfilerManager.SelectedItemsCount <= 0 then
  begin
    MessageDlg('No items selected for profiling!'+#13#10+
               'Please select one or more items before starting.',
               mtError, [mbOK], 0);
    Exit;
  end;

  lblState.Caption := 'Starting...';
  ProfilerManager.StartProfiling;

  UpdateEnabledState;
  lblState.Caption := 'Started at: ' + TimeToStr(ProfilerManager.StartTime);
end;

procedure TfrmProfileMain.btnstopClick(Sender: TObject);
begin
  lblState.Caption := 'Saving...';
  ProfilerManager.StopProfiling;

  lblState.Caption := 'Stopped.';
  UpdateEnabledState;
end;

procedure TfrmProfileMain.edtSessionNameEnter(Sender: TObject);
begin
  edtSessionName.Font.Color := clWindowText;
end;

procedure TfrmProfileMain.FormCreate(Sender: TObject);
var
  fviApp : TJclFileVersionInfo;
begin
  if fileexists(C_AsmProfiler_dll) then
  begin
    fviApp  := TJclFileVersionInfo.Create(C_AsmProfiler_dll);
    Caption := Caption + Format(' (%s)',[fviApp.BinFileVersion]);
    fviApp.Free;
  end;

  lblItemCount.Caption :=  IntToStr(ProfilerManager.SelectedItemsCount);
end;

procedure TfrmProfileMain.FormDestroy(Sender: TObject);
begin
  frmProfileMain := nil;
  inherited;
end;

procedure TfrmProfileMain.UpdateEnabledState;
begin
  btnstop.Enabled  := ProfilerManager.Started; 
  btnStart.Enabled := not ProfilerManager.Started;

  btnClear.Enabled  := ProfilerManager.Started;
  //btnFlush.Enabled  := not GProfileStarted;
  //btnSave.Enabled   := not GProfileStarted;

  btnSelectItems.Enabled := not ProfilerManager.Started;
  btnShowResults.Enabled := not ProfilerManager.Started;
end;

end.
