unit _frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  GpSync;

const
  WM_NEW_MESSAGE = WM_USER;

type
  TfrmMain = class(TForm)
    btnStart: TBitBtn;
    btnStop: TBitBtn;
    btnSelect: TBitBtn;
    btnShowResults: TBitBtn;
    procedure btnSelectClick(Sender: TObject);
    procedure btnShowResultsClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    //FCommandReader: TGpMessageQueueReader;
    FCommandWriter: TGpMessageQueueWriter;
  protected
    procedure CheckProfilerActive;
    procedure WMNewMessage(var msg: TMessage); message WM_NEW_MESSAGE;

    function IsProfilerStarted: boolean;
    function IsProfilerActive: boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
  CMessageQueueSize = 1024;

procedure TfrmMain.btnSelectClick(Sender: TObject);
begin
  CheckProfilerActive;

  if IsProfilerStarted then
  begin
    MessageDlg('Profiler started, please stop first', mtError, [mbOK], 0);
    Abort;
  end;

  FCommandWriter.PostMessage(1000, 'SelectItems');
end;

procedure TfrmMain.btnShowResultsClick(Sender: TObject);
begin
  CheckProfilerActive;

  if IsProfilerStarted then
  begin
    MessageDlg('Profiler started, please stop first', mtError, [mbOK], 0);
    Abort;
  end;

  FCommandWriter.PostMessage(1000, 'ShowResults');
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  CheckProfilerActive;

  if IsProfilerStarted then
  begin
    MessageDlg('Profiler already started', mtWarning, [mbOK], 0);
    Abort;
  end;

  FCommandWriter.PostMessage(1000, 'ProfileStart');
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  CheckProfilerActive;

  if not IsProfilerStarted then
  begin
    MessageDlg('Profiler not started', mtWarning, [mbOK], 0);
    Abort;
  end;

  FCommandWriter.PostMessage(1000, 'ProfileStop');
end;

procedure TfrmMain.CheckProfilerActive;
begin
  if not IsProfilerActive then
  begin
    MessageDlg('Profiler not loaded!', mtError, [mbOK], 0);
    Abort;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  //FCommandReader := TGpMessageQueueReader.Create('Global/AsmProfiler/RemoteCommands', CMessageQueueSize, Handle, WM_NEW_MESSAGE);
  FCommandWriter := TGpMessageQueueWriter.Create('Global/AsmProfiler/RemoteCommands', CMessageQueueSize);
end;

function TfrmMain.IsProfilerActive: boolean;
begin
  Result := TGpFlag.IsFlagSet('Global/AsmProfiler/FlagIsActive');
end;

function TfrmMain.IsProfilerStarted: boolean;
begin
  Result := TGpFlag.IsFlagSet('Global/AsmProfiler/FlagIsStarted');
end;

procedure TfrmMain.WMNewMessage(var msg: TMessage);
var
  lParam : Windows.LPARAM;
  msgData: string;
  msgmsg : UINT;
  wParam : Windows.WPARAM;
begin
  {
  while mqr.PeekMessage(1000, msgData) = mqgOK do begin
    Log('Message previewed: '+msgData);
    if mqr.GetMessage(1000, msgData) = mqgOK then
      Log('Message received: '+msgData)
    else
      Log('Retrieve failed');
    if mqr.GetMessage(1000, msgmsg, msgData) <> mqgOK then
      Log('Retrieve failed')
    else
      Log('Message received: '+IntToStr(msgmsg)+' '+msgData);
    if mqr.GetMessage(1000, msgmsg, wParam, lParam) <> mqgOK then
      Log('Retrieve failed')
    else
      Log('Message received: '+IntToStr(msgmsg)+' '+IntToStr(wParam)+' '+IntToStr(lParam));
  end;
  }
end; { TfrmMain.WMNewMessage }

var pb: PBoolean;
    p: pointer;
initialization
  pb  := GetMemory(SizeOf(Boolean));
  pb^ := True;
//  if pb^ then
//    pb^ := False;

  p := pb;
  if PBoolean(p)^ then
    PBoolean(p)^ := True;

end.
