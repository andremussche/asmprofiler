unit _uProfileTools;

interface

  function GetCPUSpeed: Extended;
  function RDTSC: Int64;

  function ClockType: String;
  function GetClockSpeed: Extended;
  function ClockTicks: Int64;inline;

implementation

uses
  SysUtils, Dateutils, Windows;
  //JwaWinNT;

function ClockType: String;
begin
  {$ifdef USE_CPU_CLOCK}
  Result := 'CPU';
  {$ELSE}
  Result := 'Hardware (Windows performance counter)';
  {$ENDIF}
end;

function GetClockSpeed: Extended;
{$ifdef USE_CPU_CLOCK}
begin
  Result := GetCPUSpeed;
{$ELSE}
var
  iFreq: int64;
begin
  if QueryPerformanceFrequency(iFreq) then
    Result := iFreq
  else
    Result := -1;
{$ENDIF}
end;

function ClockTicks: Int64;
{$ifdef USE_CPU_CLOCK}
begin
  Result := RDTSC;
end
{$ELSE}
begin
  QueryPerformanceCounter(Result);
end;
{$ENDIF}

function RDTSC: Int64;
asm
  rdtsc
end;

function GetCPUSpeed: Extended;
var
  iTime1, iTime2, iTimeOverhead: int64;

  cOldPrioClass: Cardinal;
  iOldThreadPrio: Integer;

  tDiff: Extended;
  iDiff,
  i1,i2,iFreq,iOverhead: int64;

  procedure DummyWait(const aTimeMsec:integer);
  var tStart: Tdatetime;
  begin
    tStart := now; 
    while MilliSecondsBetween(now,tStart) < aTimeMsec do
      sleep(0);
  end;

begin
  cOldPrioClass  := GetPriorityClass(GetCurrentProcess);
  iOldThreadPrio := GetThreadPriority(GetCurrentThread);

  //set high prio for good results
  try
    SetPriorityClass( GetCurrentProcess, REALTIME_PRIORITY_CLASS );
    SetThreadPriority( GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL );
  except
    SetPriorityClass( GetCurrentProcess, HIGH_PRIORITY_CLASS );
    SetThreadPriority( GetCurrentThread, THREAD_PRIORITY_HIGHEST );
  end;

  {calc overhead of speed independent timers}
  QueryPerformanceFrequency(iFreq);
  QueryPerformanceCounter(i1);
  QueryPerformanceCounter(i2);
  iOverhead := i2 - i1;
  {calc overhead of high freq. timers}
  iTime1 := ClockTicks;
  iTime2 := ClockTicks;
  iTimeOverhead := iTime2 - iTime1;

  {sleep for 500 and measure time difference}
  iTime1 := ClockTicks;
  QueryPerformanceCounter(i1);
  //Sleep(500);
  DummyWait(250);                     //high cpu usage, so cpu will not sleep
  QueryPerformanceCounter(i2);
  iTime2 := ClockTicks;

  {exact cycle difference}
  iTime1 := iTime2 - iTime1 - iTimeOverhead;

  {real time elapsed, can be lower/higher than 500msec!}
  iDiff := i2 - i1 - iOverhead;
  tDiff := (iDiff / iFreq);

  {calc speed in Mhz = cycles / time}
  Result := iTime1 / tDiff;

  //restore
  SetPriorityClass( GetCurrentProcess, cOldPrioClass );
  SetThreadPriority( GetCurrentThread, iOldThreadPrio );
end;

(*
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;

procedure SetThreadDebugName(const aThreadName:string);
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PChar(aThreadName);
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    {send structure to debugger, so these names are only visible in Delphi Debugger!}
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
end;
*)

end.
