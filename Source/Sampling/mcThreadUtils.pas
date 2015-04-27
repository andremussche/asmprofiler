unit mcThreadUtils;

interface

uses
  jclDebug,
  Classes;

type
  TIntegerArray = array of integer;
  TPointerArray = array of Pointer;

  function GetAllRunningProcesses: TStrings;
  function GetAllThreadIDsOfProcess(hProcessId: Cardinal = 0): TIntegerArray;

  function GetThreadStackOfProcess(const aProcessID: Cardinal; aThreadID: Cardinal): TJclStackInfoList;
  function GetCompleteThreadStack_64(const hProcess: THandle; aThreadID: cardinal): TPointerArray;

  function GetSnapshotMakingDuration: TDatetime;
  
implementation

uses
  PsAPI, TlHelp32,
  JwaImageHlp, JwaWinBase, JwaNative, JwaWinNT, Windows, SysUtils, JclWin32, JclSysUtils,
  muExactTiming, DateUtils, Dialogs;

var
  FHardwareCounter,
  FMakeSnapshotStart, FMakeSnapshotEnd: int64;

function GetSnapshotMakingDuration: TDatetime;
var idiff: Int64;
    fdiffsec: Double;
const
  cSecPerDay = 24 * 60 * 60;
begin
  Result := -1;

  idiff  := TLargeInteger(FMakeSnapshotEnd) - TLargeInteger(FMakeSnapshotStart);
  if idiff > 0 then
  begin
    fdiffsec := (idiff / TLargeInteger(FHardwareCounter));  //1 = 1s
    Result   := (fdiffsec/cSecPerDay);     //convert to tdatetime
  end;
end;


function GetAllRunningProcesses: TStrings;
var
  h: THandle;
  process: TProcessEntry32;

  function _GetName: string;
  begin
    Result := Format('%s (PID: %d, Threads: %d, Path: %s)',
                     [Copy(process.szExeFile, 0, MAX_PATH - 1),
                      process.th32ProcessID,
                      process.cntThreads,
                      process.szExeFile]);
   end;

var t: TDatetime;
    str: Tstrings;
begin
  t := Now;

  Result := TStringList.Create;
  try
    h := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    try
      process.dwSize := SizeOf(process);
      if (h <> Thandle(-1)) and
         Process32First(h, process) then
      repeat
        Result.AddObject(_GetName, TObject(process.th32ProcessID));
      until not Process32Next(h, process);

    finally
      CloseHandle(h);
    end;
  except
    on e: exception do
    begin
      str := TStringList.Create;
      jcldebug.JclCreateStackList(False, 0, nil).AddToStrings(str, True, True, True, True);
      MessageDlg(e.Message + #13#13 + 'Stack:' + #13 + str.Text, mtError, [mbOK], 0);
    end;
  end;

  OutputDebugString( pchar(Format('GetAllRunningProcesses: %d ms',[MilliSecondsBetween(Now, t)])) );
end;

procedure CreateWinNTProcessList(List: TstringList);
var
  PIDArray: array [0..1023] of DWORD;
  cb: DWORD;
  I: Integer;
  ProcCount: Integer;
  hMod: HMODULE;
  hProcess: THandle;
  ModuleName: array [0..300] of Char;
begin
  if List = nil then Exit;
  EnumProcesses(@PIDArray, SizeOf(PIDArray), cb);
  ProcCount := cb div SizeOf(DWORD);
  for I := 0 to ProcCount - 1 do
  begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or
      PROCESS_VM_READ,
      False,
      PIDArray[I]);
    if (hProcess <> 0) then
    begin
      EnumProcessModules(hProcess, @hMod, SizeOf(hMod), cb);
      GetModuleFilenameEx(hProcess, hMod, ModuleName, SizeOf(ModuleName));
      List.Add(ModuleName);
      CloseHandle(hProcess);
    end;
  end;
end;

function GetAllThreadIDsOfProcess(hProcessId: Cardinal = 0): TIntegerArray;
var
  hSnapProc: THandle;
  threadentry: TThreadEntry32;
  bNext: Boolean;
  iCount: integer;
  
var t: TDatetime;
begin
  t := Now;

  iCount := 0;
  SetLength(Result, 50);   //initial buffer

  hSnapProc := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, hProcessId);
  try

    if hSnapProc <> THandle(-1) then
    begin
      threadentry.dwSize := Sizeof(threadentry);
      bNext := Thread32First(hSnapProc, threadentry);
      while bNext do
      begin
        if ThreadEntry.th32OwnerProcessID = hProcessID then
        begin
          if iCount >= 50 then
            SetLength(Result, iCount+1);
          Result[iCount] := threadentry.th32ThreadID;
          inc(iCount);
        end;
        bNext := Thread32Next(hSnapProc, threadentry);
      end;
    end;

  finally
    CloseHandle(hSnapProc);
  end;
  SetLength(Result, iCount);

  OutputDebugString( pchar(Format('GetAllThreadIDsOfProcess: %d ms',[MilliSecondsBetween(Now, t)])) );
end;

function GetThreadTopOfStack(aThreadHandle: THandle; aProcess: THandle): Pointer;
var
  TBI: THREAD_BASIC_INFORMATION;
  ReturnedLength: ULONG;
  isize, iread: {$if CompilerVersion >= 23} (*Delphi XE2*) NativeUInt {$else} Cardinal {$ifend};
  pbuffer: Pointer;
begin
  Result := nil;
  ReturnedLength := 0;
  if (NtQueryInformationThread(aThreadHandle, ThreadBasicInformation, @TBI, SizeOf(TBI), @ReturnedLength) < $80000000) and
     //(ReturnedLength > 0)
     (ReturnedLength = SizeOf(TBI)) then
  begin
//    {$IFDEF CPU32}
    if aProcess = 0 then
      Result := Pointer(PNT_TIB32(TBI.TebBaseAddress)^.StackBase)
    else
    begin
      isize    := SizeOf(NT_TIB32);
      pbuffer  := GetMemory(isize);
      ReadProcessMemory(aProcess, TBI.TebBaseAddress, pbuffer, isize, iread);
      Result   := Pointer(PNT_TIB32(pbuffer)^.StackBase);
      FreeMemory(pbuffer);
    end;

//    {$ENDIF CPU32}
//    {$IFDEF CPU64}
//    Result := TJclAddr(PNT_TIB64(TBI.TebBaseAddress)^.StackBase)
//    {$ENDIF CPU64}
  end
  else
    RaiseLastOSError;
end;


{$STACKFRAMES OFF}

function GetEBP: Pointer;
asm
        MOV     EAX, EBP
end;

function GetESP: Pointer;
asm
        MOV     EAX, ESP
end;

function GetFS: Pointer;
asm
        XOR     EAX, EAX
        MOV     EAX, FS:[EAX]
end;

// Reference: Matt Pietrek, MSJ, Under the hood, on TIBs:
// http://www.microsoft.com/MSJ/archive/S2CE.HTM

function GetStackTop: DWORD;
asm
  // TODO: 64 bit version
        MOV     EAX, FS:[0].NT_TIB32.StackBase
end;

{$IFDEF STACKFRAMES_ON}
{$STACKFRAMES ON}
{$ENDIF STACKFRAMES_ON}


function GetThreadStackOfProcess(const aProcessID: Cardinal; aThreadID: cardinal): TJclStackInfoList;
var
  C: CONTEXT;
  hprocess,
  hthread: THandle;
  pTop, pBase: Pointer;
  iSize, iread: {$if CompilerVersion >= 23} (*Delphi XE2*) NativeUInt {$else} Cardinal {$ifend};
  pbuffer: Pointer;
//  list: TJclStackInfoList;
//var
//  StackFrame: PStackFrame;
//  StackFrame2: PStackFrame;
var
  StackPtr: PDWORD;
  StackDataSize: Cardinal;
  BaseOfStack, TopOfStack: Cardinal;
  FStackData: PPointer;

var t: TDatetime;
begin
  Result := nil;
  hthread        := OpenThread(THREAD_ALL_ACCESS, False, aThreadID);
    TopOfStack  := GetStackTop;

  t := Now;
  if aThreadID = GetcurrentThreadid then
  begin
    FillChar(C, SizeOf(C), 0);
    C.ContextFlags := CONTEXT_FULL;
    GetThreadContext(hthread, C);
    BaseOfStack := c.Ebp;

//    BaseOfStack := Cardinal(GetEBP);
//    TopOfStack  := GetStackTop;

    // Get a pointer to the current bottom of the stack
    StackPtr := PDWORD(BaseOfStack);
    if Cardinal(StackPtr) < TopOfStack then
    begin
      StackDataSize := TopOfStack - Cardinal(StackPtr);
      GetMem(FStackData, StackDataSize);
      System.Move(StackPtr^, FStackData^, StackDataSize);
      //CopyMemory(FStackData, StackPtr, StackDataSize);
    end;

//    Result := JclCreateStackList(False, 0, nil, True, GetEBP, Pointer(GetStackTop))
  end
  else
    Result := JclCreateThreadStackTraceFromID(False, aThreadID);
  OutputDebugString( pchar(Format('GetThreadStackOfProcess: %8.3f ms',[MilliSecondSpan(Now, t)])) );
  Exit;

//  list   := nil;
  ZeroMemory( @C, sizeof(C) );
//  ResetMemory(C, SizeOf(C));
  C.ContextFlags := CONTEXT_FULL;
  hprocess       := OpenProcess( PROCESS_ALL_ACCESS, False, aProcessID);
  hthread        := OpenThread(THREAD_ALL_ACCESS, False, aThreadID);
  try
    if GetThreadContext(hthread, C) then
    begin
      pTop    := GetThreadTopOfStack(hthread, hprocess);   //top   $130000
      pBase   := Pointer(C.Ebp);                           //base  $123456

      isize   := Cardinal(pTop) - Cardinal(pBase);
      pbuffer := GetMemory(isize);
      ReadProcessMemory(hprocess, pBase, pbuffer, isize, iread);

//      StackFrame  := PStackFrame(pBase);
//      StackFrame2 := PStackFrame(pbuffer);

      Result := JclCreateThreadStackTraceFromID(False, aThreadID);

//      Result := JclCreateStackList(False, DWORD(-1), nil, False, pBase,   ptop);
//      list := JclCreateStackList(True, DWORD(-1), nil, False, pbuffer, pointer(Cardinal(pbuffer) + isize));
//      Result  := TStringList.Create;
//      list.AddToStrings(Result, True, True, True, True);
    end;
  finally
    CloseHandle(hthread);
    CloseHandle(hprocess);
//    list.Free;
  end;

  OutputDebugString( pchar(Format('GetThreadStackOfProcess: %d ms',[MilliSecondsBetween(Now, t)])) );
end;

(*
function GetThreadStackOfProcess2(const aProcessID: Cardinal; aThreadID: cardinal): TJclStackInfoList;
var
  C: CONTEXT;
  hprocess,
  hthread: THandle;
  pTop, pBase: Pointer;
  iSize, iread: Cardinal;
  pbuffer: Pointer;
//  list: TJclStackInfoList;
var
  StackFrame: PStackFrame;
//  StackFrame2: PStackFrame;
//var
//  StackPtr: PDWORD;
//  StackDataSize: Cardinal;
//  BaseOfStack, TopOfStack: Cardinal;
//  FStackData: PPointer;

var t: TDatetime;
begin
  Result  := nil;
  hthread := OpenThread(THREAD_ALL_ACCESS, False, aThreadID);
  pTop    := GetThreadTopOfStack(hthread, hprocess);   //top   $130000

  t := Now;
  ZeroMemory( @C, sizeof(C) );
  C.ContextFlags := CONTEXT_FULL;

  if GetThreadContext(hthread, C) then
  begin
    pBase   := Pointer(C.Ebp);                           //base  $123456

    isize   := Cardinal(pTop) - Cardinal(pBase);
    pbuffer := GetMemory(isize);
    ReadProcessMemory(hprocess, pBase, pbuffer, isize, iread);

//    StackFrame  := PStackFrame(pBase);

    Result := JclCreateThreadStackTraceFromID(False, aThreadID);
  end;

  OutputDebugString( pchar(Format('GetThreadStackOfProcess: %d ms',[MilliSecondsBetween(Now, t)])) );
end;
*)


function GetCompleteThreadStack(const aThreadID: cardinal): TStrings;
var
  hProcess,
  hThread: Cardinal;
  Cxt : Context;
  Sf : JwaImageHlp.TStackFrame;
  s:string;
//  bJclExceptTrackingActive: boolean;
  sTempFile:string;
begin
  Result := TStringList.Create;
  if aThreadID = GetCurrentThreadId then
  begin
    Result.Text := 'Cannot dump in the same thread!';
    exit;
  end;

  {disable exception tracking during dump}
  //bJclExceptTrackingActive := JclExceptionTrackingActive;
  //if bJclExceptTrackingActive then
  //  bJclExceptTrackingActive := JclStopExceptionTracking;
//  bJclExceptTrackingActive := JclExceptionsHooked;
//  if bJclExceptTrackingActive then
//    bJclExceptTrackingActive := JclUnhookExceptions;

  {
  NtQuerySystemInformation
  GetThreadTimes()
  GetThreadPriority()
  GetThreadPriorityBoost()
  }
//  sTempFile := Format('%s\Temp_TID%d_CompleteStackDump_%s.tmp',
//                      [Globals.Utils.GetLogDirectory, aThreadID, FormatDateTime('ddmmyyyy-hhnnsszzz',now)]);

  hThread  := OpenThread( THREAD_ALL_ACCESS, False, aThreadID);
  try
    SuspendThread(hThread);
    hProcess := GetCurrentProcess;

//    if not GSymbolsInitialized then
//      InitializeSymbols;
//    {$MESSAGE WARN 'todo: register/load all modules? See: http://www.codeproject.com/threads/StackWalker.asp' }

    {context}
    Cxt.ContextFlags := CONTEXT_ALL;
    if GetThreadContext(hThread, Cxt) = False then
    begin
      Result.Text := 'Error: could not retrieve thread context!';
      Exit;
    end;
    {fill}
    FillChar(Sf, SizeOf(TStackFrame), #0);

    Sf.AddrPC.Offset    := Cxt.Eip;
    Sf.AddrPC.Mode      := AddrModeFlat;
    Sf.AddrStack.Offset := Cxt.Esp;
    Sf.AddrStack.Mode   := AddrModeFlat;
    Sf.AddrFrame.Offset := Cxt.Ebp;      //stack frame
    Sf.AddrFrame.Mode   := AddrModeFlat;

    try
      {StackWalk}
      While StackWalk(IMAGE_FILE_MACHINE_I386, hProcess, hThread,
                      Sf, @Cxt, nil, @SymFunctionTableAccess, @SymGetModuleBase, nil) do
      begin
        if Sf.AddrPC.Offset = 0 then Break;
        if (sf.AddrPC.Offset = sf.AddrReturn.Offset) then Break;

//        s := GetModuleFromStackFrame(sf);
        Result.Add(s);
        Result.SaveToFile(sTempFile);
      end;
    except
      sleep(0);
    end;
  finally
    //if aThreadID <> GetCurrentThreadId then
    ResumeThread(hThread);
    CloseHandle(hThread);

    //if bJclExceptTrackingActive then
    //  JclStartExceptionTracking;
//    if bJclExceptTrackingActive then
//      JclHookExceptions;
//    Globals.Logger.Active := True;
    SysUtils.DeleteFile(sTempFile);
  end;
end;

type
  temp = record
    Ebp, Esp, Eip: LongWord;
  end;

function get_eip: Pointer;
asm
  mov eax, [esp]
end;
  
function GetCurrentContext: temp;
// Those three registers are enough.
asm
//@Label:
  mov [result.Ebp], ebx
  mov [result.Esp], esp
  //mov ax, @@Label
  //call get_eip
  mov [result.Eip], eax;
end;

function GetCompleteThreadStack_64(const hProcess: THandle; aThreadID: cardinal): TPointerArray;
var
//  hProcess,
  hThread: Cardinal;
  rContext : Context;
  Sf : JwaImageHlp.TStackFrame64;
//  s:string;
//  bJclExceptTrackingActive: boolean;
//  sTempFile:string;
  p: pointer;
  r: temp;
  iCount: integer;
//  pointerarray: array of Pointer;
begin
//  Result   := TStringList.Create;
  hThread  := OpenThread( THREAD_ALL_ACCESS, False, aThreadID);
  try
//    SuspendThread(hThread);
//    hProcess := GetCurrentProcess;

//    if not GSymbolsInitialized then
//      InitializeSymbols;
//    {$MESSAGE WARN 'todo: register/load all modules? See: http://www.codeproject.com/threads/StackWalker.asp' }
//    Result := TStringList.Create;

    iCount := 0;
    SetLength(Result, 1024);  //prealloc

    QueryPerformanceCounter(FMakeSnapshotStart);

    ZeroMemory( @rContext, sizeof(rContext) );
    if aThreadID = GetCurrentThreadId then
    begin
      rContext.ContextFlags := CONTEXT_CONTROL;
      // N.B. GetThreadContext cannot be used on the current thread.
      // Capture own context - on i386, there is no API for that.
      r := GetCurrentContext;
      rContext.Ebp := r.Ebp;
      rContext.Esp := r.Esp;
      rContext.Eip := r.Eip;
    end
    else
    begin
      {context}
      rContext.ContextFlags := CONTEXT_ALL;
      if GetThreadContext(hThread, rContext) = False then
      begin
        //Result.Text := 'Error: could not retrieve thread context!';
        Exit;
      end;
    end;

    {fill}
    FillChar(Sf, SizeOf(TStackFrame64), #0);
    Sf.AddrPC.Offset    := rContext.Eip;
    Sf.AddrPC.Mode      := AddrModeFlat;
    Sf.AddrStack.Offset := rContext.Esp;
    Sf.AddrStack.Mode   := AddrModeFlat;
    Sf.AddrFrame.Offset := rContext.Ebp;      //stack frame
    Sf.AddrFrame.Mode   := AddrModeFlat;

//    try
      {StackWalk}
      While StackWalk64(IMAGE_FILE_MACHINE_I386, hProcess, hThread,
                      Sf, @rContext, nil,
                      @SymFunctionTableAccess64, @SymGetModuleBase64, nil) do
      begin

//      StackTrace->Frames[ StackTrace->FrameCount++ ] =
//        StackFrame.AddrPC.Offset;

        if Sf.AddrPC.Offset = 0 then Break;
        if (sf.AddrPC.Offset = sf.AddrReturn.Offset) then Break;

        p := pointer(sf.AddrPC.Offset);
        if iCount >= Length(Result) then
          SetLength(Result, iCount + 1024);
        Result[iCount] := p;
        inc(iCount);

//        location later
//        s := GetLocationInfoStr(p, True, True, True, True);
//        if s = '' then s := '?';

//        s := GetModuleFromStackFrame(sf);
//        Result.Add(s);
//        Result.SaveToFile(sTempFile);
      end;

    QueryPerformanceCounter(FMakeSnapshotEnd);
    SetLength(Result, iCount);

//    except
//      sleep(0);
//    end;
  finally
    //if aThreadID <> GetCurrentThreadId then
//    ResumeThread(hThread);
    CloseHandle(hThread);

    //if bJclExceptTrackingActive then
    //  JclStartExceptionTracking;
//    if bJclExceptTrackingActive then
//      JclHookExceptions;
//    Globals.Logger.Active := True;
//    SysUtils.DeleteFile(sTempFile);
  end;
end;
  //
  // Set up stack frame.
  //
  (*
  ZeroMemory( &StackFrame, sizeof( STACKFRAME64 ) );
#ifdef _M_IX86
  MachineType                 = IMAGE_FILE_MACHINE_I386;
  StackFrame.AddrPC.Offset    = Context.Eip;
  StackFrame.AddrPC.Mode      = AddrModeFlat;
  StackFrame.AddrFrame.Offset = Context.Ebp;
  StackFrame.AddrFrame.Mode   = AddrModeFlat;
  StackFrame.AddrStack.Offset = Context.Esp;
  StackFrame.AddrStack.Mode   = AddrModeFlat;
#elif _M_X64
  MachineType                 = IMAGE_FILE_MACHINE_AMD64;
  StackFrame.AddrPC.Offset    = Context.Rip;
  StackFrame.AddrPC.Mode      = AddrModeFlat;
  StackFrame.AddrFrame.Offset = Context.Rsp;
  StackFrame.AddrFrame.Mode   = AddrModeFlat;
  StackFrame.AddrStack.Offset = Context.Rsp;
  StackFrame.AddrStack.Mode   = AddrModeFlat;
#elif _M_IA64
  MachineType                 = IMAGE_FILE_MACHINE_IA64;
  StackFrame.AddrPC.Offset    = Context.StIIP;
  StackFrame.AddrPC.Mode      = AddrModeFlat;
  StackFrame.AddrFrame.Offset = Context.IntSp;
  StackFrame.AddrFrame.Mode   = AddrModeFlat;
  StackFrame.AddrBStore.Offset= Context.RsBSP;
  StackFrame.AddrBStore.Mode  = AddrModeFlat;
  StackFrame.AddrStack.Offset = Context.IntSp;
  StackFrame.AddrStack.Mode   = AddrModeFlat;
#else
  #error "Unsupported platform"
#endif

  StackTrace->FrameCount = 0;

  //
  // Dbghelp is is singlethreaded, so acquire a lock.
  //
  // Note that the code assumes that
  // SymInitialize( GetCurrentProcess(), NULL, TRUE ) has
  // already been called.
  //
  EnterCriticalSection( &DbgHelpLock );

  while ( StackTrace->FrameCount < MaxFrames )
  {
    if ( ! StackWalk64(
      MachineType,
      GetCurrentProcess(),
      GetCurrentThread(),
      &StackFrame,
      MachineType == IMAGE_FILE_MACHINE_I386
        ? NULL
        : &Context,
      NULL,
      SymFunctionTableAccess64,
      SymGetModuleBase64,
      NULL ) )
    {
      //
      // Maybe it failed, maybe we have finished walking the stack.
      //
      break;
    }

    if ( StackFrame.AddrPC.Offset != 0 )
    {
      //
      // Valid frame.
      //
      StackTrace->Frames[ StackTrace->FrameCount++ ] =
        StackFrame.AddrPC.Offset;
    }
    else
    {
      //
      // Base reached.
      //
      break;
    }
  }

  LeaveCriticalSection( &DbgHelpLock );

  return S_OK;
}
#ifdef _M_IX86
  #pragma warning( pop )
  #pragma optimize( "g", on )
#endif
*)

initialization
  JclStartExceptionTracking;
  JclTrackExceptionsFromLibraries;
  JclStackTrackingOptions := [stStack, stExceptFrame, stRawMode, stAllModules, stTraceAllExceptions];

  QueryPerformanceFrequency(FHardwareCounter);

end.
