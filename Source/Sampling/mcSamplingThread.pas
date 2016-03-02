unit mcSamplingThread;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF},
  mcSamplingResult,
  mcProcessSampler;

type
  TSamplingThread = class(TThread)
  private
    FSamplingInterval: Cardinal;
    FProcessObject: TProcessSampler;

    FOnlySampleWhenUIisBusy: boolean;
    FUIbusyWaitTimeMsec: Integer;
    FUIHandle: THandle;
    procedure SearchUIHandle;

    procedure SetName;
  protected
    procedure Execute; override;
  public
    property Terminated;

    property ProcessObject: TProcessSampler read FProcessObject write FProcessObject;
    property SamplingInterval: Cardinal read FSamplingInterval write FSamplingInterval;

    property OnlySampleWhenUIisBusy: boolean read FOnlySampleWhenUIisBusy write FOnlySampleWhenUIisBusy;
    property UIbusyWaitTimeMsec: Integer read FUIbusyWaitTimeMsec write FUIbusyWaitTimeMsec;

    function GetResults: TSamplingResult;
  end;

implementation

uses
  MMSystem, SysUtils, DateUtils, Winapi.Messages;

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TSamplingThread }

function TSamplingThread.GetResults: TSamplingResult;
begin
  Result := TSamplingResult.Create(FProcessObject);
end;

procedure TSamplingThread.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'TSamplingThread';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;

procedure TSamplingThread.Execute;
var t: TDatetime;
    iNr: integer;
    res: Cardinal;
begin
  SetName;
  iNr := 0;
  t   := Now;
  if (ProcessObject.ProcessId = 0) and
     (ProcessObject.ProcessExe <> '') then
  begin
    t := Now;
    ProcessObject.ManualStartExe;
  end;

  //init cpu times of all threads
  ProcessObject.RefreshThreads;

  while not Terminated do
  begin
    if ProcessObject.ManualStarted and
       (MilliSecondsBetween(Now, t) > 500)
       //(iNr mod 100 = 0)
    then
    begin
      t := Now;
      ProcessObject.RefreshThreads;
    end;

    if Self.OnlySampleWhenUIisBusy then
    begin
      if FUIHandle = 0 then
        SearchUIHandle;

      //if UI responds within waittime, we do not collect stacktraces
      if SendMessageTimeOut(Self.FUIHandle, WM_NULL, 0, 0, SMTO_NORMAL, Self.UIbusyWaitTimeMsec, res) <> 0 then
      begin
        sleep(1); //prevent 100% cpu
        Continue;
      end;

      //refresh per 1000 samples
      if (iNr mod 1000 = 0) then
        SearchUIHandle;
    end;

    ProcessObject.StackDumpAllThreads(iNr);
    inc(iNr);

    //if FSamplingInterval >= 0 then
    Sleep(FSamplingInterval);

//    OutputDebugString( PChar(Format('TSamplingThread.Total: %4.2f ms',
//                                    [MilliSecondSpan(Now, t)])) );
  end;

  //load cpu times of all threads
  ProcessObject.RefreshThreads;
end;

type
  TFindMainWindow = class
  protected
    PID: NativeUInt;
    Handle: THandle;
  end;

function CheckWindow(aWnd: HWnd; aInfo: LParam): Bool; stdCall;
var
  fmw: TFindMainWindow;
  pid: Cardinal;
begin
  fmw := TFindMainWindow(aInfo);
  Result := TRUE;

  //get pid of window
  GetWindowThreadProcessId(aWnd, @pid);

  //same pid we are searching for?
  if fmw.PID = pid then
  begin
    fmw.Handle := aWnd;
    //SetLastError(ERROR_SUCCESS);
    Result := FALSE;
  end;
end;

procedure TSamplingThread.SearchUIHandle;
var
  fmw: TFindMainWindow;
begin
  fmw := TFindMainWindow.Create;
  try
    fmw.PID := Self.ProcessObject.ProcessId;
    fmw.Handle := 0;
    EnumWindows(@CheckWindow, NativeInt(fmw));

    if fmw.Handle <> 0 then
      Self.FUIHandle := fmw.Handle;
  finally
    fmw.Free;
  end;
end;

initialization
  timeBeginPeriod(1); //1ms resolution

end.
