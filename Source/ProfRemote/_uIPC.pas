unit _uIPC;

interface

uses
  Windows,
  Classes;

type
  TIPCThread = class(TThread)
  private
    FSharedBuffer,        //main shared mem
    FGlobalMutex,         //global mutex for write lock
    FGlobalWriteEvent,    //global event to notify new data
    FGlobalReadEvent,     //global event to notify data is read
    FLocalSendEvent       //local event to notfiy new data in input buffer to send
    : THandle;
  protected
    procedure InitializeIPC;

    procedure SendLocalData;
    procedure ReceiveRemoteData;
  public
    constructor Create;
    procedure Execute; override;
  end;

const
  C_PROFILER_SHARED_DATA   = 'Global\ASMPROFILER_SHARED_DATA';
  C_PROFILER_LOCK_MUTEX    = 'Global\ASMPROFILER_LOCK_MUTEX';
  C_PROFILER_WRITE_EVENT   = 'Global\ASMPROFILER_WRITE_EVENT';
  C_PROFILER_READ_EVENT    = 'Global\ASMPROFILER_READ_EVENT';

implementation

{ TIPCThread }

constructor TIPCThread.Create;
begin
  //
end;

procedure TIPCThread.Execute;
var aWaitEvents : array[0..1] of THandle;
    cWaitResult : Cardinal;
begin
  InitializeIPC;

  aWaitEvents[0] := FLocalSendEvent;
  aWaitEvents[1] := FGlobalWriteEvent;

  //wait for:
  //- new local input data to send
  //- new remote data to receive
  while not terminated do
  begin
    cWaitResult := WaitForMultipleObjects(2, @aWaitEvents, false, 1000);
    if cWaitResult = 0 then
      SendLocalData
    else if cWaitResult = 1 then
      ReceiveRemoteData;
  end;

end;

procedure TIPCThread.InitializeIPC;
var
  Sa: TSecurityAttributes;
  Sd: TSecurityDescriptor;
begin
  { Setup Secureity so anyone can connect to the MMF/Mutex/Events.  This is
    needed when for Services. }
  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;
  
  FSharedBuffer := CreateFileMapping($FFFFFFFF, @sa, PAGE_READWRITE,
                       0, 1024*1024,     //1Mb size
                       C_PROFILER_SHARED_DATA);

  FGlobalMutex  := Windows.CreateMutex(@sa, false, C_PROFILER_LOCK_MUTEX);

  FGlobalWriteEvent := Windows.CreateEvent(@sa, true, false, C_PROFILER_WRITE_EVENT);
  FGlobalReadEvent  := Windows.CreateEvent(@sa, true, false, C_PROFILER_READ_EVENT);
  FLocalSendEvent   := Windows.CreateEvent(@sa, true, false, '');   //local, no name needed
end;

procedure TIPCThread.ReceiveRemoteData;
begin
  ResetEvent(FGlobalWriteEvent);
  try
    //read data
  finally
    SetEvent(FGlobalReadEvent);
  end;
end;

procedure TIPCThread.SendLocalData;
var aWaitEvents : array[0..1] of THandle;
    cWaitResult : Cardinal;
begin
  aWaitEvents[0] := FGlobalMutex;
  aWaitEvents[1] := FGlobalWriteEvent;
  repeat
    cWaitResult := WaitForMultipleObjects(2, @aWaitEvents, False, 1000);
    if cWaitResult = 0 then
      //send data
    else
      ReceiveRemoteData;

  until true = false
end;

end.
