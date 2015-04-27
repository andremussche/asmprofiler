unit _uAsmProfiler;

interface

{$OVERFLOWCHECKS OFF}

uses
  Windows, SysUtils, Forms, classes,
  _uProfileTools,
  _uProfileTypes;

  procedure RemoveAllProfileIntercepts;
  function RemoveProfileIntercept(TargetProc: Pointer): Pointer;
  function CreateProfileIntercept(TargetProc: Pointer; const aName:string; out aErrorReason: string): Pointer;
  function DummyProfileMarker: int64;
  procedure ClearAllProfileTimes;

  procedure CreateProfileForm;stdcall;
  procedure ShowProfileForm;stdcall;
  procedure DestroyProfileForm;stdcall;

var
  GProfileStartedMarker: pointer;
  GDummyProfileMarker: Pointer = Pointer(12345678);
  //
  GProfileTimeArrayCount: integer;
  GProfileTimes: TProfileRunRecordArray;
  GProfileStarted: PBoolean;
  GCPUSpeed: Extended;
  //
  GThreadNameCount: integer;
  GThreadNames: TThreadNameArray;
  GProfileOverhead: integer;

implementation

uses
  {$IFNDEF PROF_NO_THREAD_NAMES}
  //used for collecting thread names
  JclHookExcept,
  {$ENDIF}
  AsmDetours, Math,
  _uProfilerManager, _AsmDllInjectionHook, JwaWinNT;

const
  C_SecondStackSize = 100000;

type
  TStackRecord = packed record
    Call: Byte;                 //1
    ProfileEndFunc: pointer;    //4
    Jmp: Byte;                  //1
    OriginallEIP: pointer;      //4
    Callee: Pointer;
    Filler: Word;               //2 = make 16byte alignment for fast CPU speed
  end;
  PStackRecord      = ^TStackRecord;
  TStackRecordArray = array[0..C_SecondStackSize] of TStackRecord;
  PStackRecordArray = ^TStackRecordArray;

  TProfileStartInfo = record
    ProfileFuncSize: Cardinal;
    OffsetToProfileStartedMarker: Cardinal;
    OffsetToDummyProfileMarker1: Cardinal;
    OffsetToDummyProfileMarker2: Cardinal;
  end;
  PProfileStartInfo = ^TProfileStartInfo;

  TThreadRecord = record
    ThreadID: Cardinal;
    CurrentProfileTimeArray: PProfileTimeArray;
    CurrentProfileTimeCount: Cardinal;
    Recursive: boolean;

    SecondStackPos  : PStackRecord;
    SecondStackStart: PStackRecord;
    SecondStackEnd  : PStackRecord;
    SecondStack     : TStackRecordArray;

    procedure AddCounter(aCallee: Cardinal);inline; //inline, somewhat faster
    procedure CloseSecondStack;//inline; no inline, makes it slower

    class function GetRecord: Pointer; static;  //PThreadRecord
  end;
  PThreadRecord = ^TThreadRecord;

threadvar
//var
  GThreadRecord : PThreadRecord;
var
  GOwnTLSIndex: Integer;
  GThreadListCount:integer;
  GThreadList: array[1..C_MAX_THREADS] of TThreadRecord;

  GProfileStartInfo : TProfileStartInfo;
  //pointer to functions, to use in assembly
  GProcessProfileStart,
  GProfileEndPointer: pointer;
  //GDummyProfileMarker: Pointer;
  //GTrampoline_ProfileTest: pointer;

  GCriticalSection: Windows.TRTLCriticalSection;

function GetNewProfileTimeArray: PProfileTimeArray;
var
  i:integer;
  pta: TProfileTimeArray;
  //ppta: PProfileTimeArray;
begin
  if GProfileTimeArrayCount > 750 then         //1500mb / 2mb = 750
  begin
    ProfilerManager.StopProfiling;
    raise exception.Create('More than 750 buffers used! Profiler will stop now.');
  end;

  {threadsafe new counter}
  i := InterlockedIncrement(GProfileTimeArrayCount);
  EnterCriticalSection(GCriticalSection);
  try
    if Length(GProfileTimes) = 0 then
      SetLength(GProfileTimes,750);
    //SetLength(GProfileTimes,i);
  finally
    LeaveCriticalSection(GCriticalSection);
  end;

  SetLength(pta, C_PROFILETIMEARRAY_SIZE);                    //+- 16mb
  //setLength(Result, 10);
  GProfileTimes[i-1].ProfileTimeArray := pta;
  GProfileTimes[i-1].ThreadID         := GetCurrentThreadId;
  GProfileTimes[i-1].Time             := now;

  //does not work, gives nil pointer!
  //Result := @pta;
  Result := @GProfileTimes[i-1].ProfileTimeArray;
end;

procedure ClearAllProfileTimes;
var
  i: integer;
begin
  for i := 1 to C_MAX_THREADS do
  begin
    //GThreadList[i].ThreadID := 0;
    //GThreadList[i].SecondStackCount := 0;
    GThreadList[i].CurrentProfileTimeCount := 0;
    GThreadList[i].CurrentProfileTimeArray := nil;
    //GThreadList[i].SecondStack := nil;
  end;
  //GThreadListCount := 0;

  if Length(GProfileTimes) > 0 then
  for i := 0 to GProfileTimeArrayCount-1 do
  begin
    SetLength(GProfileTimes[i].ProfileTimeArray,0);
  end;
  SetLength(GProfileTimes,0);
  GProfileTimeArrayCount := 0;

  //GThreadNameCount := 0;
end;

function _GetOwnTls: Pointer;
asm
//      MOV     CL,ModuleIsLib
  MOV   EAX, GOwnTlsIndex
  mov   ecx, fs:[$00000018]
//  cmp ecx,$40     why is that? not more than $40 items?
//  jnb $7c844acc
//  and   dword ptr [ecx+$34],$00      corrupts memory, but why? is not really needed?
  mov   eax,[ecx+eax*4+$0e10]
  RET
end;

function ProcessProfileStart(const aCaller, aCallee: Cardinal): Pointer;
var
  tr: PThreadRecord;
begin
  Result := nil;
//  if Get8087CW = $27F then    //1372
//    if MainThreadID = GetCurrentThreadId then
//      DebugBreak;

//  tr := GThreadRecord;
  tr := _GetOwnTls;
//  tr := TlsGetValue(GOwnTlsIndex);
  if tr = nil then
  begin
    tr := TThreadRecord.GetRecord;
    if tr = nil then Exit;  //recursive call
    TlsSetValue(GOwnTLSIndex, tr);
  end;

  with tr^ do
  begin
    if Recursive then
      Exit;
    Recursive := True;

    with SecondStackPos^ do
    begin
      Call           := $E8;
      ProfileEndFunc := Pointer( Cardinal(GProfileEndPointer) - (Cardinal(SecondStackPos) + 5) ); //relative
      //ProfileEndFunc := GProfileEndPointer; // @ProfileEnd;
      Jmp            := opJmpRelative;
      OriginallEIP   := Pointer( aCaller - (Cardinal(SecondStackPos) + 5 + 5) );                  //relative

      Callee         := Pointer(aCallee);
    end;
    Result := SecondStackPos;  //= CALL GProfileEndPointer

    AddCounter(aCallee);

    //next
    inc(SecondStackPos, SizeOf(TStackRecord));
    if Cardinal(SecondStackPos) >= Cardinal(SecondStackEnd) then
      raise Exception.Create('Second stack overflow!');

    Recursive := False;
  end;

//  if Get8087CW = $27F then    //1372
//    if MainThreadID = GetCurrentThreadId then
//      DebugBreak;
end;

procedure ProcessProfileEnd(const aStackRecord: PStackRecord);
var
  tr: PThreadRecord;
begin
//  if Get8087CW <> $27F then
//    DebugBreak;
//  if Get8087CW = $27F then    //1372
//    if MainThreadID = GetCurrentThreadId then
//      DebugBreak;

  tr     := _GetOwnTls;
  if tr = nil then
  begin
    tr := TThreadRecord.GetRecord;
    if tr = nil then
    begin
      DebugBreak;
      Exit;  //recursive call
    end;
    TlsSetValue(GOwnTLSIndex, tr);
  end;

  with tr^ do
  begin
    if Recursive then
    begin
      DebugBreak;
      Exit;  //recursive call
    end;
    Recursive := True;

    //no check if exception occured (so pending for close), we do that in result viewer
    SecondStackPos := aStackRecord;
    //dec(SecondStackPos, SizeOf(TStackRecord));

    CloseSecondStack;

    Recursive := False;
  end;

//  if Get8087CW = $27F then    //1372
//    if MainThreadID = GetCurrentThreadId then
//      DebugBreak;
end;

{$O-} //no optimization
procedure ProfileStart;
//var
//DO NOT USE LOCAL VARIABLES! BECAUSE THEY WILL USE THE STACK!
//We cannot use the stack, it will corrupt the stack for the original/profiled function!
asm
    //if not GProfileStarted^ then exit;
//    mov  eax, GProfileStartedMarker          //pointer of GProfileStarted^, will be overwritten with real memory pointer at runtime
//    cmp  byte ptr eax,$00
//    cmp  byte ptr GProfileStartedMarker,$00
//    cmp byte ptr [GProfileStarted], $00   //if not GProfileStarted then exit;
//    jz @@Exit

    {save compare flags}
    pushfd;

    cmp  byte ptr GProfileStartedMarker,$00
    jz @@RestoreF

    {save the registers which are used for the function params
     because we don't know if all are used, we save them all on the stack}
    pushad;

    //{get address of original caller stack value}
    {store original caller stack value}
    mov  EAX, [ESP + 16+16+4];            //16 = 4*4 pushed registers, so before those is the caller value
    {callee address}
    mov  ECX, GDummyProfileMarker
    mov  EDX, GDummyProfileMarker
    //mov  EDX, ECX                       //second param?
                                  //     eax      edx
    //function ProcessProfileStart(const aCaller, aCallee): boolean;
    mov  EBX, GProcessProfileStart;
    call EBX
    {if ProcessProfileStart not succeeded, just restore and quit}
    test al, al
    jz @@Restore

    {overwrite caller with "ProfileEnd" to trap RET}
    //mov EAX, GProfileEndPointer;
    mov [ESP+16+16+4], EAX;

@@Restore:
    popad;

@@RestoreF:
    {restore saved registers}
    popfd;

//@@Exit:
    {write dummy marker, must be overwritten with original function pointer}
    //jmp dword ptr [$12345678];
    jmp dword ptr GDummyProfileMarker
    //jmp  GTrampoline_ProfileTest;
end;

procedure ProfileEnd;
asm
    {save the registers again, can be used from return values of functions}
    pushad;
    pushfd;

    mov EAX, [ESP+16+16+4]  //+4 = 1 stack pointer back
    sub EAX, 5              //5 bytes back = TStackRecord
    //                                  eax   edx            eax
    //proceure ProcessProfileEnd(const aStackRecord: PStackRecord);
    call ProcessProfileEnd
    {if ProcessProfileEnd not succeeded, just restore and quit}
//    test EAX, EAX
//    jz @@IgnoreOriginalCaller

    {overwrite 1 previous stack (dummy EAX push) with EAX value (=original caller)}
    {this will resume normal execution after our "RET"}
//    mov [ESP+16+16+4], EAX;              //+4 = 1 stack pointer back

    {restore saved registers}
    popfd;
    popad;

    {return, goes to original call}
    ret;

//@@IgnoreOriginalCaller:
//    {restore saved registers}
//    popfd;
//    popad;
//    {return, goes to original call}
//    ret;
end;

function DummyProfileMarker: int64;
begin
  Result := ClockTicks;
end;

function GetProfileStartInfo: PProfileStartInfo;
var
  p, p2, pStart:pointer;
  //pDummyPos:pointer;
  //proc: TProcedure;
  //i:integer;
begin
  Result := @GProfileStartInfo;

  if Result.OffsetToDummyProfileMarker2 = 0 then
  begin
    //pDummy := @DummyProfileMarker;
    pStart := @ProfileStart;
    Result.ProfileFuncSize := 0;

    p := pStart;
    //pDummyPos := nil;
    repeat
      if PByte(p)^ in [opRetPop,opRet] then
      if Result.ProfileFuncSize > 20 then
        Break;

      if //(PByte(p)^ = opMovEAX) and                   //is first statement of the proc so no check needed
         (Result.OffsetToProfileStartedMarker = 0) then
      begin
        if (PByte(p)^ = opPushFd) then //  pushfd
        begin
          Inc(PByte(p));
          inc(Result.ProfileFuncSize,1);
        end;

        //803D 20591450 00   cmp byte ptr [$50145920],$00
        Inc(PWord(p));
        inc(Result.ProfileFuncSize,2);

        p2 := @GProfileStartedMarker;

        if Pointer(p^) = p2 then
        begin
          Result.OffsetToProfileStartedMarker := Result.ProfileFuncSize;
          Inc(PByte(p),4);
          inc(Result.ProfileFuncSize,4);
          Continue;
        end;
      end;

      if PWord(p)^ = opMovEDX then
//      if PByte(p)^ = opMovEDX then
      begin
        Inc(PWord(p));
        inc(Result.ProfileFuncSize,2);
//        Inc(PByte(p));
//        inc(Result.ProfileFuncSize);

        p2 := @GDummyProfileMarker;

        if Pointer(p^) = p2 then
          Result.OffsetToDummyProfileMarker1 := Result.ProfileFuncSize;
      end;

      if PWord(p)^ = opJmpIndirect then
      begin
        Inc(PWord(p));
        inc(Result.ProfileFuncSize,2);

        //p2 := Pointer($12345678);
        p2 := @GDummyProfileMarker;

        if Pointer(p^) = p2 then
        begin
          Result.OffsetToDummyProfileMarker2 := Result.ProfileFuncSize;
          Inc(PByte(p),4);
          inc(Result.ProfileFuncSize,4);
          Continue;
        end;
      end;

      Inc(PByte(p));
      inc(Result.ProfileFuncSize);
    until True = False;

    assert( Result.ProfileFuncSize >= 45);
  end;
end;

function SetPermission(const Code: Pointer; const Size: Integer; const Permission: Longword): Longword;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective
    immediately }
  if Permission <> 0 then
    if FlushInstructionCache(GetCurrentProcess,Code,Size) then
      VirtualProtect(Code,Size,Permission,Longword(Result));
end;

type
  TInterceptedProc = record
    OriginalProc,
    //POriginalProc,
    InterceptProc,
    TrampolineProc: pointer;
    Name: string;
    //PTrampolineProc: pointer;
  end;
  PInterceptedProc = ^TInterceptedProc;

var
  GInterceptedProcs: array of PInterceptedProc;
  iLastFreeRecord,
  iTrampolineCount: integer;

function CreateProfileIntercept(TargetProc: Pointer; const aName:string; out aErrorReason: string): Pointer;
var
  i,
  iProfileSize,
  iDummyPos:Cardinal;
//  OrigTrampolineAccess,
//  OrigTargetProcAccess: LongWord;
  pProfileStart: Pointer;
  pDummy,
  pNewTrampoline: pointer;
  pTrampoline: Pointer;
  ppTrampoline: pointer;
  pi: PInterceptedProc;
  sError: string;
begin
  Result := nil;
  if Pos('..', aName) > 0 then                                     //sometime weird classnames as functions...
    Exit;

  {Check if it already profiled' }
  pDummy := AsmDetours.TIntercepts.GetFinalCode(TargetProc);
  if PByte(pDummy)^ = opJmpRelative then
  begin
    aErrorReason := 'Has relative jump at start of procedure, already profiled?';
    Exit;
  end;

  GDummyProfileMarker := TargetProc;  
  try
    iProfileSize  := GetProfileStartInfo.ProfileFuncSize + 1;
    pProfileStart := @ProfileStart;
    //alloc mem for complete function copy
    GetMem(pNewTrampoline,iProfileSize);
    //copy function
    //OrigTrampolineAccess :=
    SetPermission(pNewTrampoline,iProfileSize,PAGE_EXECUTE_READWRITE);
    Move(pProfileStart^,pNewTrampoline^,iProfileSize);
    SetPermission(pNewTrampoline,iProfileSize,PAGE_EXECUTE_READWRITE);  //flush

    pi := nil;
    {search for free slot}
    if length(GInterceptedProcs) > 0 then
    for i := iLastFreeRecord to high(GInterceptedProcs) do
    begin
      if (GInterceptedProcs[i] <> nil) and (GInterceptedProcs[i].OriginalProc = nil) then
      begin
        pi := GInterceptedProcs[i];
        iLastFreeRecord := i;
      end;
    end;
    {make new slot}
    if pi = nil then
    begin
      SetLength(GInterceptedProcs,iTrampolineCount+1);
      if GInterceptedProcs[iTrampolineCount] = nil then
      begin
        new(pi);
        GInterceptedProcs[iTrampolineCount] := pi;
      end;
      iLastFreeRecord := iTrampolineCount;
      inc(iTrampolineCount);
    end;

    //create intercepts
    pTrampoline   := AsmDetours.InterceptCreate(TargetProc, pNewTrampoline, sError);
    if pTrampoline = nil then
    begin
      aErrorReason := 'Intercept could not be added: ' + sError;
      exit;
    end;
    Result := pTrampoline;

    //save pointers
    pi.OriginalProc    := TargetProc;
    //pi.POriginalProc   := @pi.OriginalProc;
    pi.InterceptProc   := pNewTrampoline;
    pi.TrampolineProc  := pTrampoline;
    //pi.PTrampolineProc := @pi.TrampolineProc;
    pi.Name := aName;

    //fill pointer to GProfileStarted memory
    iDummyPos    := GetProfileStartInfo.OffsetToProfileStartedMarker;
    pDummy       := pNewTrampoline;
    //move dummy pointer to offset
    inc(pByte(pDummy),iDummyPos);
    //fill GProfileStarted pointer at marker position, so GProfileStarted can be used even after dll is unloaded
    Move(GProfileStarted,pDummy^,4);
    SetPermission(pDummy,4,PAGE_EXECUTE_READWRITE);  //flush

    ppTrampoline := @pi.TrampolineProc;
    iDummyPos    := GetProfileStartInfo.OffsetToDummyProfileMarker2;
    pDummy       := pNewTrampoline;
    //move dummy pointer to offset
    inc(pByte(pDummy),iDummyPos);
    //OrigTargetProcAccess :=
    //SetPermission(pDummy,4,PAGE_EXECUTE_READWRITE);
    //fill trampoline pointer at marker position, so after our function, the original function is called via trampoline
    Move(ppTrampoline,pDummy^,4);
    SetPermission(pDummy,4,PAGE_EXECUTE_READWRITE);  //flush

    iDummyPos  := GetProfileStartInfo.OffsetToDummyProfileMarker1;
    if iDummyPos > 0 then
    begin
      pDummy     := pNewTrampoline;
      //move dummy pointer to offset
      inc(pByte(pDummy),iDummyPos);
      //OrigTargetProcAccess :=
      //SetPermission(pDummy,4,PAGE_EXECUTE_READWRITE);
      //fill original proc value at marker position, so we know which function was called
      ppTrampoline := @pi.OriginalProc;
      Move(ppTrampoline,pDummy^,4);
      SetPermission(pDummy,4,PAGE_EXECUTE_READWRITE);  //flush
    end;
  finally
    GDummyProfileMarker := @DummyProfileMarker;
  end;
end;

function RemoveProfileIntercept(TargetProc: Pointer): Pointer;
var
  i:integer;
  //iProfileSize: Cardinal;
  pIntercept, pTrampoline: pointer;
begin
  Result := nil;
  for i := low(GInterceptedProcs) to high(GInterceptedProcs) do
  begin
    if (GInterceptedProcs[i] <> nil) and
       (GInterceptedProcs[i].OriginalProc = TargetProc) then
    begin
      pTrampoline := GInterceptedProcs[i].TrampolineProc;
      pIntercept  := GInterceptedProcs[i].InterceptProc;
      AsmDetours.InterceptRemove(pTrampoline, pIntercept);
      GInterceptedProcs[i].OriginalProc := nil;

      iLastFreeRecord := i;
      FreeMem(pIntercept);
      FreeMem(GInterceptedProcs[i]);
      GInterceptedProcs[i] := nil;
      exit;
    end;
  end;
end;

procedure RemoveAllProfileIntercepts;
var
  i:integer;
  pIntercept, pTrampoline: pointer;
const
  DebugFillByte = $80;
  DebugFillDWord = $01010101 * Cardinal(DebugFillByte);
begin
  for i := low(GInterceptedProcs) to high(GInterceptedProcs) do
  begin
    if (GInterceptedProcs[i] <> nil) and
       (GInterceptedProcs[i].OriginalProc <> nil) and
       (GInterceptedProcs[i].InterceptProc <> pointer(DebugFillDWord)) and
       (GInterceptedProcs[i].InterceptProc <> pointer($80808080)) then
    begin
      pTrampoline := GInterceptedProcs[i].TrampolineProc;
      pIntercept  := GInterceptedProcs[i].InterceptProc;
      try
        AsmDetours.InterceptRemove(pTrampoline, pIntercept);

        GInterceptedProcs[i].OriginalProc := nil;
        //iLastFreeRecord := i;
        FreeMem(pIntercept);
        FreeMem(GInterceptedProcs[i]);
        GInterceptedProcs[i] := nil;
      except
        GInterceptedProcs[i].OriginalProc := nil;
      end;
    end;
  end;
  iLastFreeRecord := 0;

  SetLength(GInterceptedProcs, 0);
end;

procedure AnyExceptionNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
var
  p:pchar;
  i:integer;
  c:cardinal;
begin
  if Application.Terminated then exit;

  {$WARN SYMBOL_PLATFORM OFF}

  if (ExceptObj is EExternalexception) and
     (EExternalexception(ExceptObj).Exceptionrecord^.ExceptionCode = 1080890248) then
  begin
    //delphi thread name
    c := (ExceptObj as EExternalexception).Exceptionrecord^.ExceptionInformation[1];
    p := pchar(c);

    c := GetCurrentThreadId;
    {if already used previously (thread is freed) }
    for i := 0 to GThreadNameCount-1 do
    begin
      if GThreadNames[i].ThreadID = c then
      begin
         GThreadNames[i].Name     := Ansistring(p);
        exit;
      end;
      {cannot store more thread names!}
      if i >= C_MAX_THREADS then exit;
    end;

    {else add new}
    i := InterlockedIncrement(GThreadNameCount);
    GThreadNames[i].ThreadID := c;
    GThreadNames[i].Name     := Ansistring(p);
  end;
end;

procedure CreateProfileForm;
begin
  ProfilerManager.CreateProfileForm;
end;

procedure ShowProfileForm;
begin
  CreateProfileForm;
  ProfilerManager.ShowProfileForm;
end;

procedure DestroyProfileForm;
begin
  ProfilerManager.DestroyProfileForm;
end;

function TerminateProc: boolean;
begin
  RemoveAllProfileIntercepts;
  Result := True;
end;

//var
//  itest: integer = 1;

procedure DummyProcForOverheadTest;
asm
  nop;
  nop;
  nop;
  nop;
  nop;
  nop;
//  Result := itest;
//  Result := Result + 2;
end;

procedure SubSubTest;
begin
  sleep(0);
end;

procedure SubTest;
begin
  SubSubTest;
end;

procedure Test;
begin
  SubTest;
end;


function CalcProfileOverhead: integer;
var
  i:integer;
  i1,i2, iNormal, iProfile: int64;
  s: string;
  cOldPrioClass: Cardinal;
  iOldThreadPrio: Integer;
begin
  cOldPrioClass  := GetPriorityClass(GetCurrentProcess);
  iOldThreadPrio := GetThreadPriority(GetCurrentThread);

  //set high prio for good results
  SetPriorityClass( GetCurrentProcess, REALTIME_PRIORITY_CLASS );
  SetThreadPriority( GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL );

  i1 := ClockTicks;
  for i := 1 to 1000 do
    DummyProcForOverheadTest;
  i2 := ClockTicks;
  iNormal := i2 - i1;

  if CreateProfileIntercept(@DummyProcForOverheadTest, 'DummyProcForOverheadTest',s) = nil then
  begin
    Result := -1;
    Exit;
  end;
  CreateProfileIntercept(@GetLastError, 'GetLastError',s);

  GProfileStarted^ := True;
  try
    //call it one time, to initialize profiler
    DummyProcForOverheadTest;

    GetLastError;

    i1 := ClockTicks;
    for i:= 1 to 1000 do
      DummyProcForOverheadTest;
    i2 := ClockTicks;
    iProfile := i2 - i1;
    Result   := round( (iProfile - iNormal) / 1000 );

//    CreateProfileIntercept(@Test, 'Test', s);
//    CreateProfileIntercept(@SubTest, 'SubTest', s);
//    CreateProfileIntercept(@SubSubTest, 'SubSubTest', s);
//    Test;
  finally
    GProfileStarted^ := False;
  end;

  //restore
  SetPriorityClass( GetCurrentProcess, cOldPrioClass );
  SetThreadPriority( GetCurrentThread, iOldThreadPrio );

  RemoveProfileIntercept(@DummyProcForOverheadTest);
end;

{ TThreadRecord }

procedure TThreadRecord.AddCounter(aCallee: Cardinal);
begin
//  if Recursive then Exit;
//  Recursive := True;

  if CurrentProfileTimeArray = nil then
  begin
    CurrentProfileTimeArray := GetNewProfileTimeArray;
  end;

  {get new record}
  {start time in record}
  with CurrentProfileTimeArray^[CurrentProfileTimeCount] do
  begin
    Time        := ClockTicks;
    ProfileType := ptEnter;
    Address     := Pointer(aCallee);
  end;

  inc(CurrentProfileTimeCount);
  if CurrentProfileTimeCount >= C_PROFILETIMEARRAY_SIZE then
  begin
//    with GThreadList[iThreadIndex] do
//    begin
//      Recursive := True;
//      try
//        pta := GetNewProfileTimeArray;
//        CurrentProfileTimeArray := pta;
        CurrentProfileTimeArray := GetNewProfileTimeArray;
//      finally
//        Recursive := False;
//      end;
//    end;
    CurrentProfileTimeCount := 0;
  end;
//  Recursive := False;
end;

procedure TThreadRecord.CloseSecondStack;
begin
  {save end time, only if still running}
  if GProfileStarted^ then
  begin
    {get new record}
    {save profile end in record}
    with CurrentProfileTimeArray^[CurrentProfileTimeCount] do
    begin
      ProfileType := ptLeave;
      {stored callee}
      Address     := SecondStackPos.Callee;
      Time        := ClockTicks;
    end;

    inc(CurrentProfileTimeCount);
    if CurrentProfileTimeCount >= C_PROFILETIMEARRAY_SIZE then
    begin
      CurrentProfileTimeArray := GetNewProfileTimeArray;
      CurrentProfileTimeCount := 0;
    end;
  end;
end;

class function TThreadRecord.GetRecord: Pointer;
var
  iThreadID: Cardinal;
  iThreadIndex: Integer;
begin
  Result := nil;

  iThreadID := GetCurrentThreadId;
  {search record for this thread}
  for iThreadIndex := 1 to GThreadListCount do
  begin
    with GThreadList[iThreadIndex] do
    if ThreadID = iThreadID then
    begin
      if Recursive then exit;
      Recursive := True;

//      if CurrentProfileTimeCount <> 0 then
//        assert(CurrentProfileTimeCount = 0);
      CurrentProfileTimeCount := 0;
      if CurrentProfileTimeArray = nil then
        CurrentProfileTimeArray := GetNewProfileTimeArray;
      Result        := @GThreadList[iThreadIndex];

      SecondStackStart := @SecondStack[0];
      SecondStackEnd   := @SecondStack[High(SecondStack)];
      SecondStackPos   := SecondStackStart;

      Recursive     := False;
      Break;
    end;
  end;
  {if it is the first time for this thread_id, make new record}
  if (Result = nil) then
  begin
    if C_MAX_THREADS = GThreadListCount then
      raise Exception.CreateFmt('More than %d threads cannot be profiled!',[C_MAX_THREADS]);
    iThreadIndex := InterlockedIncrement(GThreadListCount);
    Result       := @GThreadList[iThreadIndex];
    with GThreadList[iThreadIndex] do
    begin
      if Recursive then exit;
      ThreadID  := iThreadID;
      Recursive := True;

      CurrentProfileTimeArray := GetNewProfileTimeArray;
      SecondStackStart := @SecondStack[0];
      SecondStackEnd   := @SecondStack[High(SecondStack)];
      SecondStackPos   := SecondStackStart;

      Recursive := False;
    end;
  end;

//  if (Result <> nil) and
//     (getcurrentthreadid = mainthreadid)
//  then
//    GThreadRecord := Result;
end;

procedure InitializeProfiling;
var
  i:integer;
begin
  //AddTerminateProc(TerminateProc);
  //if forms.Application = nil then sleep(0);

  SetPermission(@GThreadList[1], SizeOf(GThreadList), PAGE_EXECUTE_READWRITE);

  {$ifdef USE_CPU_CLOCK}
  //to avoid timing issues, only use one CPU!
  SetProcessAffinityMask( GetCurrentProcess, 1);
  //calc avg. cpu speed (hz)
  GCPUSpeed := mean([GetClockSpeed,GetClockSpeed,GetClockSpeed]);
  {$ELSE}
  GCPUSpeed := GetClockSpeed;
  {$ENDIF}

  GOwnTLSIndex                := TlsAlloc;
  if DWORD(GOwnTLSIndex) = TLS_OUT_OF_INDEXES then
    RaiseLastOSError;
  assert(GOwnTLSIndex >= 0, Format('GOwnTLSIndex = %d',[GOwnTLSIndex]));
  //assert(GOwnTLSIndex < $40, Format('GOwnTLSIndex = %d',[GOwnTLSIndex]));    //see _GetTls
  GProcessProfileStart        := @ProcessProfileStart;
  GProfileEndPointer          := @ProfileEnd;
  //GSaveRegistersToSecondStack := @SaveRegistersToSecondStack;
  //GCalcStartTime              := @CalcStartTime;
  //GDummyProfileMarker         := @DummyProfileMarker;
  GDummyProfileMarker         := Pointer($12345678);
  GetProfileStartInfo;

  //get mem for boolean, so "GProfileStarted" can still be "used" after dll is unloaded
  GetMem(GProfileStarted, SizeOf(Boolean));
  GProfileStartedMarker       := GProfileStarted;

  InitializeCriticalSection(GCriticalSection);
  SetCriticalSectionSpinCount(GCriticalSection, 4000);

  ProfilerManager := TProfilerManager.Create;

  for i := 0 to ParamCount do
  begin
    if ParamStr(i) = '-profile' then
    begin
      CreateProfileForm;
    end;

    if ParamStr(i) = '-profileshow' then
    begin
      ShowProfileForm;
    end;

    if (ParamStr(i) = '-profilestart') then
    begin
      ShowProfileForm;
      ProfilerManager.StartProfiling(false);
    end;
  end;

  {$ifdef NOT_VISIBLE}
  //does not show any visible controls
  {$endif}

  {$IFDEF profile}
  CreateProfileForm;
  {$ENDIF}
  {$IFDEF profileshow}
  ShowProfileForm;
  {$ENDIF}
  {$IFDEF profilestart}
  ShowProfileForm;
  {$ENDIF}

  {$IFDEF dllinjection}
  StartDllInjectionHook;               
  {$ENDIF}

  {$IFNDEF PROF_NO_THREAD_NAMES}
  // Assign notification procedure for hooked RaiseException API call. This
  // allows being notified of any exception
  //Note: used for collecting thread names
  JclAddExceptNotifier(AnyExceptionNotify);
  {$ENDIF}

  GProfileOverhead := CalcProfileOverhead;
end;

procedure FinalizeProfiling;
begin
  //stop profiling
  GProfileStarted^ := False;

  RemoveAllProfileIntercepts;
  DeleteCriticalSection(GCriticalSection);

  ProfilerManager.Free;
end;

initialization
  try
    InitializeProfiling;
  except
    on e:exception do
      Windows.MessageBox(0, pchar('initialization error:' + e.Message),'AsmProfiler Initialization error', mb_ok)

  end;

finalization
  FinalizeProfiling;

end.
