unit _uProfileTools;

interface

  function GetCPUSpeed: Extended;
  function RDTSC: Int64;

  function ClockType: String;
  function GetClockSpeed: Extended;
  function ClockTicks: Int64;inline;

  function CheckASLR(const FileName: string): Boolean;

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


{************************************
* Coded by Agmcz                    *
* Hints by naquadria                *
* Date: 2018-01-07                  *
************************************}

const
  IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE = $0040;
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5;

function CheckASLR(const FileName: string): Boolean;
var
  hFile: THandle;
  hMapping: DWORD;
  pMap: Pointer;
  dwSize: DWORD;
  IDH: PImageDosHeader;
  INH: PImageNtHeaders;
  ISH: PImageSectionHeader;
  n: Word;
  dwRelocAddr, dwRelocSize: DWORD;
begin
  Result := False;
  hFile := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if hFile <> INVALID_HANDLE_VALUE then
  begin
    dwSize := GetFileSize(hFile, nil);
    hMapping := CreateFileMapping(hFile, nil, PAGE_READONLY, 0, dwSize, nil);
    if hMapping <> 0 then
    begin
      pMap := MapViewOfFile(hMapping, FILE_MAP_READ, 0, 0, 0);
      if pMap <> nil then
      begin
        IDH := PImageDosHeader(pMap);
        if IDH.e_magic = IMAGE_DOS_SIGNATURE then
        begin
          INH := PImageNtHeaders(DWORD(pMap) + LongWord(IDH._lfanew));
          if INH.Signature = IMAGE_NT_SIGNATURE then
          begin
            if (INH.OptionalHeader.DllCharacteristics and IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE) = IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE then
            begin
              ISH := PImageSectionHeader(DWORD(pMap) + LongWord(IDH._lfanew) + SizeOf(DWORD) + SizeOf(INH.FileHeader) + INH.FileHeader.SizeOfOptionalHeader);
              dwRelocAddr := INH.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress;
              dwRelocSize := INH.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size;
              if (dwRelocAddr <> 0) and (dwRelocSize <> 0) then
              begin
                for n := 0 to INH.FileHeader.NumberOfSections - 1 do
                begin
                  if ISH.VirtualAddress = dwRelocAddr then
                  begin
                    if (ISH.Misc.VirtualSize <> 0) and  (ISH.PointerToRawData <> 0) and (ISH.SizeOfRawData <> 0) then
                      Result := True;
                    Break;
                  end;
                  Inc(ISH);
                end;
              end;
            end;
          end;
        end;
        UnmapViewOfFile(pMap);
      end;
      CloseHandle(hMapping);
    end;
    CloseHandle(hFile);
  end;
end;


end.
