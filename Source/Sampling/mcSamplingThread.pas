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
    procedure SetName;
  protected
    procedure Execute; override;
  public
    property Terminated;

    property ProcessObject: TProcessSampler read FProcessObject write FProcessObject;
    property SamplingInterval: Cardinal read FSamplingInterval write FSamplingInterval;

    function GetResults: TSamplingResult;
  end;

implementation

uses
  MMSystem, SysUtils, DateUtils;

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
      ProcessObject.RefreshThreads;

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

initialization
  timeBeginPeriod(1); //1ms resolution

end.
