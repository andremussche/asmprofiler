unit _uRemoteControlThread;

interface

uses
  GPSync,
  Windows, Messages,
  Classes;

type
  TRemoteControlThread = class(TThread)
  private
    { Private declarations }
    FCommandReader: TGpMessageQueueReader;
    FFlagActive: TGpFlag;
  protected
    procedure Execute; override;
  public
  end;

implementation

uses _uProfilerManager;

var
  GRemoteControlThread: TRemoteControlThread;

const
  CMessageQueueSize = 1024;

{ TRemoteControlThread }

procedure TRemoteControlThread.Execute;
var sMsg: string;
begin
//  MainThreadID := 123;

  FFlagActive := TGpFlag.Create('Global/AsmProfiler/FlagIsActive');
  FFlagActive.SetFlag;
  //FCommandReader := TGpMessageQueueReader.Create('Global/AsmProfiler/RemoteCommands', CMessageQueueSize, Handle, WM_USER);
  FCommandReader := TGpMessageQueueReader.Create('Global/AsmProfiler/RemoteCommands', CMessageQueueSize, 0);

  repeat
    if FCommandReader <> nil then
    if FCommandReader.GetMessage(1000, sMsg) = mqgOK then
    begin
      if (sMsg = 'ProfileStart') and
         not ProfilerManager.Started
      then
        ProfilerManager.StartProfiling(false)
      else if (sMsg = 'ProfileStop') and
           ProfilerManager.Started
      then
        ProfilerManager.StopProfiling
      else if (sMsg = 'SelectItems') and
           not ProfilerManager.Started
      then
        //Synchronize( ProfilerManager.ShowSelectItemsForm )
        ProfilerManager.ShowSelectItemsForm
      else if (sMsg = 'ShowResults') and
           not ProfilerManager.Started
      then
        //Synchronize( ProfilerManager.ShowResultsForm );
        ProfilerManager.ShowResultsForm;
    end;
    sleep(100 * 100);
  until Terminated;
end;

initialization
  GRemoteControlThread := TRemoteControlThread.Create(true);
  GRemoteControlThread.Resume;

finalization
  GRemoteControlThread.Terminate;
  //GRemoteControlThread.WaitFor;
  //GRemoteControlThread.Free;

end.
