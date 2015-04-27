unit mcThreadSampler;

interface

uses
  //Windows,
  Classes,
  JwaWinNT, JwaWinBase, JwaWinType,
  JclDebug;

type
  TStackDump = record
    Nr       : Integer;
    StackBase: Pointer;
    EBP      : Pointer;
    Dump     : Pointer;
    Size     : Integer;
  end;
  PStackDump = ^TStackDump;

  TFunctionArray = array of Pointer;

  TSnapshotStack = record
    SnapshotNr: Integer;
    Functions: TFunctionArray;
  end;
  PSnapshotStack = ^TSnapshotStack;
  TSnapshotArray = array of TSnapshotStack;

  TThreadSampler = class
  private
    FThreadId: Cardinal;
    FThreadHandle: THandle;
    FProcessID: Cardinal;
    FProcessHandle: THandle;
    FThreadStackTop: Pointer;
    FThreadContext: CONTEXT;
    FProcessObject: TObject;
    FUsertime_prev,
    FKerneltime_prev,
    FUsertime_ms,
    FKerneltime_ms,
    FCreationtime,
    FExittime: Int64;
    function GetSnapshotCount: integer;
    function GetSnapshotStack(aIndex: integer): PSnapshotStack;
    function GetRawStackDump(aIndex: integer): PStackDump;
    function GetCreationtime: TDateTime;
    function GetExittime: TDateTime;
    function GetKerneltime: TDateTime;
    function GetUsertime: TDateTime;

    //snapshot:
    var
    FBuffer:    Pointer;
    FStackBase: Pointer;
    FStackSize: Integer;
    FReadSize: Cardinal;
    FStackDump: PStackDump;
    FStackDumpOK: boolean;
    FStackDumpList: TList;
    FSnapshotStackList: TList;

    FHardwareCounter: LARGE_INTEGER;
    FMakeSnapshotStart: LARGE_INTEGER;
    FMakeSnapshotEnd: LARGE_INTEGER;

    //trace vars:
    FTraceStackBase: Pointer;
    FTraceStackTop: Pointer;
//    FTraceFrameEBP: Pointer;
    FTraceStackOffset: Cardinal;
    FStackList: TList;
    procedure TraceStackRaw(aSnapshot: PStackDump);
    procedure TraceStackFrames(aSnapshot: PStackDump);
    function NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;

    procedure AddStackDump(const aDump: TStackDump);
  public
    constructor Create(aProcessID, aThreadId: Cardinal);
    destructor  Destroy;override;

    procedure MakeStackDump(const aNr: integer);
    function  GetDumpMakingDuration: TDatetime;
    function  TraceLastDump(aRaw: boolean = false): string;
    procedure ClearAllDumps;
    //
    property RawStackDump [aIndex: integer]: PStackDump read GetRawStackDump;
    //
    property SnapshotCount: integer read GetSnapshotCount;
    property SnapshotStack[aIndex: integer]: PSnapshotStack read GetSnapshotStack;
    function GetAllSnapshotStacks: TSnapshotArray;

    procedure UpdateCPUTime;

    procedure LoadDumpsFromStream(aStream: TStream);
    procedure SaveDumpsToStream(aStream: TStream);

    property ProcessID: Cardinal read FProcessID;
    property ThreadId : Cardinal read FThreadId;
    property ThreadStackTop: Pointer read FThreadStackTop; // write FThreadStackTop;

    property Creationtime: TDateTime read GetCreationtime;
    property Exittime    : TDateTime read GetExittime;
    property Kerneltime  : TDateTime read GetKerneltime;
    property Usertime    : TDateTime read GetUsertime;

    property ProcessObject: TObject read FProcessObject write FProcessObject;
  end;

implementation

uses
  JwaNative, SysUtils, mcProcessSampler, uResultTypes;

const
  C_STACK_SIZE = 2 * 1024 * 1024;   //2mb buffer = 1mb = default max threadsize?

function GetThreadTopOfStack(aThreadHandle: THandle; aProcess: THandle): Pointer;
var
  TBI: THREAD_BASIC_INFORMATION;
  ReturnedLength: ULONG;
  isize, iread: Cardinal;
  pbuffer: Pointer;
begin
  Result := nil;
  ReturnedLength := 0;

  NtQueryInformationThread(aThreadHandle, ThreadBasicInformation, @TBI, SizeOf(TBI), @ReturnedLength);
  if (ReturnedLength = SizeOf(TBI)) then
  begin
//    {$IFDEF CPU32}
//    if aProcess = 0 then
//      Result := Pointer(PNT_TIB32(TBI.TebBaseAddress)^.StackBase)
//    else
//    begin
      isize    := SizeOf(NT_TIB32);
      pbuffer  := GetMemory(isize);
      ReadProcessMemory(aProcess, TBI.TebBaseAddress, pbuffer, isize, @iread);
      Result   := Pointer(PNT_TIB32(pbuffer)^.StackBase);
      FreeMemory(pbuffer);
//    end;
//    {$ENDIF CPU32}
//    {$IFDEF CPU64}
//    Result := TJclAddr(PNT_TIB64(TBI.TebBaseAddress)^.StackBase)
//    {$ENDIF CPU64}
  end
  else
    RaiseLastOSError;
end;

{ TThreadSampler }

procedure TThreadSampler.AddStackDump(const aDump: TStackDump);
var
  snapshot: PStackDump;
begin
  new(snapshot);
  snapshot^ := aDump;
  FStackDumpList.Add(snapshot);
end;

procedure TThreadSampler.ClearAllDumps;
begin
  FStackDumpList.Clear;
  FSnapshotStackList.Clear;

  FCreationtime  := 0;
  FExittime      := 0;
  FKerneltime_ms := 0;
  FUsertime_ms   := 0;
  FUsertime_prev   := 0;
  FKerneltime_prev := 0;
end;

constructor TThreadSampler.Create(aProcessID, aThreadId: Cardinal);
begin
  FStackDumpList     := TList.Create;
  FStackList         := TList.Create;
  FSnapshotStackList := TList.Create; 

  FProcessID      := aProcessID;
  if aProcessID > 0 then
    FProcessHandle  := OpenProcess(PROCESS_ALL_ACCESS, True, aProcessID);
  FThreadId       := aThreadId;
  if aThreadId > 0 then
  begin
    FThreadHandle   := OpenThread(THREAD_ALL_ACCESS, False, FThreadId);
    FThreadStackTop := GetThreadTopOfStack(FThreadHandle, FProcessHandle);   //top, e.g. $130000

    ZeroMemory( @FThreadContext, sizeof(FThreadContext) );
    FThreadContext.ContextFlags := CONTEXT_FULL;

    FBuffer    := GetMemory(C_STACK_SIZE);  //1mb buffer = default max threadsize?

    FStackDumpList.Capacity := 5 * 1024; //pre allocate

    QueryPerformanceFrequency(FHardwareCounter);
  end;  
end;

destructor TThreadSampler.Destroy;
begin
  FreeMem(FBuffer);
  FStackDumpList.Free;
  FStackList.Free;
  FSnapshotStackList.Free;

  CloseHandle(FProcessHandle);
  CloseHandle(FThreadHandle);

  inherited;
end;

function TThreadSampler.GetAllSnapshotStacks: TSnapshotArray;
var
  i: Integer;
begin
  SetLength(Result, SnapshotCount);

  for i := 0 to SnapshotCount - 1 do
    Result[i] := SnapshotStack[i]^;
end;

function TThreadSampler.GetCreationtime: TDateTime;
begin
  Result := (Int64(FCreationtime) div 1000) / MSecsPerSec / SecsPerDay;
end;

function TThreadSampler.GetSnapshotCount: integer;
begin
  Result := FStackDumpList.Count;
end;

function TThreadSampler.GetDumpMakingDuration: TDatetime;
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

function TThreadSampler.GetExittime: TDateTime;
begin
  Result := (Int64(FExittime) div 1000) / MSecsPerSec / SecsPerDay;
end;

function TThreadSampler.GetKerneltime: TDateTime;
begin
  Result := FKerneltime_ms / MSecsPerSec / SecsPerDay;
end;

function TThreadSampler.GetRawStackDump(aIndex: integer): PStackDump;
var
  snapshot: PStackDump;
begin
  snapshot := FStackDumpList.Items[aIndex];
  Result   := snapshot;
end;

function TThreadSampler.GetSnapshotStack(aIndex: integer): PSnapshotStack;
var
  snapshot: PStackDump;
  pstack  : PStackInfo;
  i, j: Integer;
//  ps: TProcessSampler;
begin
  if FSnapshotStackList.Count <> FStackDumpList.Count then
  begin
    FSnapshotStackList.Clear;
    FSnapshotStackList.Capacity := FStackDumpList.Count;

    for i := 0 to SnapshotCount - 1 do
    begin
      snapshot := FStackDumpList.Items[i];
      FStackList.Clear;
  //    if aRaw then
  //      TraceStackRaw(snapshot)
  //    else
      TraceStackFrames(snapshot);

      new(Result);
      SetLength(Result.Functions, FStackList.Count);
      Result.SnapshotNr := snapshot.Nr;
      for j := 0 to FStackList.Count - 1 do
      begin
        pstack    := FStackList.Items[j];
        Result.Functions[j] := Pointer(pstack.CallerAddr);
      end;
      FSnapshotStackList.Add(Result);
    end;
  end;

  Result := FSnapshotStackList.Items[aIndex];

  {
  if FStackDumpList.Count > 0 then
  begin
    snapshot := FStackDumpList.Items[aIndex];
    FStackList.Clear;
//    if aRaw then
//      TraceStackRaw(snapshot)
//    else
    TraceStackFrames(snapshot);
    SetLength(Result.Functions, FStackList.Count);

//    ps := ProcessObject as TProcessSampler;
    Result.SnapshotNr := snapshot.Nr;
    for i := 0 to FStackList.Count - 1 do
    begin
      pstack    := FStackList.Items[i];
      Result.Functions[i] := Pointer(pstack.CallerAdr);
    end;
  end;
  }
end;

function TThreadSampler.GetUsertime: TDateTime;
begin
  Result := FUsertime_ms / MSecsPerSec / SecsPerDay;
end;

procedure TThreadSampler.LoadDumpsFromStream(aStream: TStream);
var
  th: TThreadHeader;
  sd: TStackDump;
  j, idumpsize: Integer;
begin
  aStream.Read(th, SizeOf(th));
  Self.FThreadId       := th.ThreadId;
  Self.FThreadStackTop := th.ThreadStackTop;

  for j := 0 to th.DumpCount - 1 do
  begin
    //read dump header
    aStream.Read(sd, SizeOf(sd));

    idumpsize := Cardinal(Self.ThreadStackTop) - Cardinal(sd.StackBase);  //$130000 - $123456
    //read dump
    sd.Dump := nil;
    GetMem(sd.Dump, idumpsize);
    assert(idumpsize = sd.Size);
    aStream.Read(sd.Dump^, idumpsize);

    Self.AddStackDump(sd);

    {
    //read snapshot header
    strm.Read(sh, SizeOf(sh));
    ss.SnapshotNr := sh.Nr;
    SetLength(ss.Functions, sh.Size);
    //read snapshot
    strm.Read(ss.Functions[0], SizeOf(TFunctionArray) * Length(ss.Functions));
    }
  end;

end;

{$STACKFRAMES OFF}

function GetEBP: Pointer;
asm
        MOV     EAX, EBP
end;

{$STACKFRAMES ON}

procedure TThreadSampler.MakeStackDump(const aNr: integer);
//no local vars: faster because no stack reservation needed
begin
  FStackDumpOK := False;
  new(FStackDump);  //prepare

  if FThreadID = GetcurrentThreadid then  //GetThreadContext does not work on current thread
  begin
    QueryPerformanceCounter(FMakeSnapshotStart);

    FStackBase := GetEBP;
    FStackSize := Cardinal(FThreadStackTop) - Cardinal(FStackBase);
    GetMem(FStackDump.Dump, FStackSize);
    System.Move(FStackBase^, FStackDump.Dump^, FStackSize);

    QueryPerformanceCounter(FMakeSnapshotEnd); //actual snapshot ends here

    FStackDump.Size      := FStackSize;
    FStackDump.StackBase := FStackBase;
    FStackDumpOK := True;
  end
  else if FProcessID = GetCurrentProcessId then  //fast copy of stack if same process
  begin
    QueryPerformanceCounter(FMakeSnapshotStart);

    if GetThreadContext(FThreadHandle, FThreadContext) then
    begin
      FStackBase := Pointer(FThreadContext.Ebp);                           //base: e.g. $123456
      FStackSize := Cardinal(FThreadStackTop) - Cardinal(FStackBase);
      //fast copy
      System.Move(FStackBase^, FBuffer^, FStackSize);

      QueryPerformanceCounter(FMakeSnapshotEnd); //actual snapshot ends here

      //copy done, alloc new mem + copy dump
      GetMem(FStackDump.Dump, FStackSize);
      Move(FBuffer^, FStackDump.Dump^, FStackSize);
      FStackDump.Size      := FStackSize;
      FStackDump.StackBase := FStackBase;
      FStackDumpOK := True;
    end;
  end
  else
  begin
    QueryPerformanceCounter(FMakeSnapshotStart);

    if GetThreadContext(FThreadHandle, FThreadContext) and
       (FThreadContext.Ebp > 0) then
    begin
      FStackBase  := Pointer(FThreadContext.Ebp);                           //base: e.g. $123456
      FStackSize  := Cardinal(FThreadStackTop) - Cardinal(FStackBase);
      //buffer is prelocated, so minimal time between GetThreadContext and ReadProcessMemory!
      //so only realloc if greater than default (safety check)
      if FStackSize > C_STACK_SIZE then
      begin
        //bigger than 2mb? then skip it...
        //ReallocMem(FBuffer, FStackSize);
        FStackSize := 0;
      end;
      //FBuffer     := GetMemory(FStackSize);
      if (FStackSize > 0) and
         ReadProcessMemory(FProcessHandle, FStackBase, FBuffer, FStackSize, @FReadSize) then
      begin
        QueryPerformanceCounter(FMakeSnapshotEnd); //actual snapshot ends here

        //copy mem from buffer
        GetMem(FStackDump.Dump, FStackSize);
        Move(FBuffer^, FStackDump.Dump^, FReadSize);
        //FSnapshot.Snapshot  := FBuffer;
        FStackDump.Size      := FStackSize;
        FStackDump.StackBase := FStackBase;
        FStackDumpOK := True;
      end;
    end;

    if not FStackDumpOK then
    begin
      { TODO : Windows stackwalk api }
      //try StackWalk64
    end;   

  end;

  if FStackDumpOK then
  begin
    FStackDump.Nr := aNr;
    FStackDumpList.Add(FStackDump);
  end
  else
    FreeMem(FStackDump);
end;

function TThreadSampler.TraceLastDump(aRaw: boolean = false): string;
var
  snapshot: PStackDump;
  pstack  : PStackInfo;
  i: Integer;
  ps: TProcessSampler;
begin
  { TODO : use process for GetLocationInfoStr }

  Result := '';
  if FStackDumpList.Count > 0 then
  begin
    snapshot := FStackDumpList.Items[FStackDumpList.Count-1];
    FStackList.Clear;
    if aRaw then
      TraceStackRaw(snapshot)
    else
      TraceStackFrames(snapshot);

    ps := ProcessObject as TProcessSampler;
    for i := 0 to FStackList.Count - 1 do
    begin
      pstack := FStackList.Items[i];
      Result := Result +
                ps.GetLocationInfo( Pointer(pstack.CallerAddr) ) +
                //GetLocationInfoStr( Pointer(pstack.CallerAdr), True, True, True, True) +
                #13;
    end;
  end;
end;

procedure TThreadSampler.TraceStackFrames(aSnapshot: PStackDump);
var
  StackFrame: PStackFrame;
  StackInfo : TStackInfo;
  pstack    : PStackInfo;
begin
  // Start at level 0
  StackInfo.Level := 0;
  StackInfo.CallerFrame := 0;

  FTraceStackOffset := Cardinal(aSnapshot.Dump) - Cardinal(aSnapshot.StackBase);  //mem - $123456
  FTraceStackTop    := Pointer(Cardinal(FThreadStackTop)     + FTraceStackOffset);    //$130000 + 100000
  FTraceStackBase   := Pointer(Cardinal(aSnapshot.StackBase) + FTraceStackOffset);    //$123456 + 100000
  StackFrame        := FTraceStackBase;  //base = EBP at moment of snapshot = stackframe
  //dec(StackFrame);

  // Loop over and report all valid stackframes
  if StackFrame <> nil then
  while NextStackFrame(StackFrame, StackInfo) do
  begin
    new(pstack);             //new mem
    pstack^ := StackInfo;    //copy contents
    FStackList.Add(pstack);  //store
  end;
end;

function TThreadSampler.NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
var
//  CallInstructionSize: Cardinal;
  StackFrameCallersEBP, NewEBP: Cardinal;
  StackFrameCallerAdr: Cardinal;

  function _ValidStackAddr(aStackAddr: DWORD): Boolean;
  begin
    Result := (aStackAddr >= Cardinal(FTraceStackBase)) and
              (aStackAddr < Cardinal(FTraceStackTop));
  end;

begin
  // Only report this stack frame into the StockInfo structure
  // if the StackFrame pointer, EBP on the stack and return
  // address on the stack are valid addresses
  StackFrameCallersEBP := StackInfo.CallerFrame;
  while _ValidStackAddr(DWORD(StackFrame)) do
  begin
    // CallersEBP above the previous CallersEBP
    NewEBP := StackFrame^.CallerFrame;
    if NewEBP <= StackFrameCallersEBP then
      Break;
    StackFrameCallersEBP := NewEBP;

    // CallerAdr within current process space, code segment etc.
    // CallersEBP within current thread stack. Added Mar 12 2002 per Hallvard's suggestion
    StackFrameCallerAdr := StackFrame^.CallerAddr;
    if //ValidCodeAddr(StackFrameCallerAdr, FModuleInfoList) and      todo
       _ValidStackAddr(StackFrameCallersEBP + FTraceStackOffset) then
    begin
      Inc(StackInfo.Level);
      StackInfo.StackFrame := StackFrame;
      StackInfo.ParamPtr   := PDWORD_PTRArray(DWORD(StackFrame) + SizeOf(TStackFrame));

      if StackFrameCallersEBP > StackInfo.CallerFrame then
        StackInfo.CallerFrame := StackFrameCallersEBP
      else
        // EBP points to an address that is below the last EBP, so it must be invalid
        Break;

      // Calculate the address of caller by subtracting the CALL instruction size (if possible)
//todo  if ValidCallSite(StackFrameCallerAdr, CallInstructionSize) then
//        StackInfo.CallerAdr := StackFrameCallerAdr - CallInstructionSize
//      else
        StackInfo.CallerAddr := StackFrameCallerAdr;
      StackInfo.DumpSize  := StackFrameCallersEBP - DWORD(StackFrame);
      StackInfo.ParamSize := (StackInfo.DumpSize - SizeOf(TStackFrame)) div 4;
      if PStackFrame(StackFrame^.CallerFrame) = StackFrame then
        Break;
      // Step to the next stack frame by following the EBP pointer
      StackFrame := PStackFrame(StackFrameCallersEBP + FTraceStackOffset);
      Result := True;
      Exit;
    end;
    // Step to the next stack frame by following the EBP pointer
    StackFrame := PStackFrame(StackFrameCallersEBP + FTraceStackOffset);
  end;
  Result := False;
end;

procedure TThreadSampler.SaveDumpsToStream(aStream: TStream);
var
  th: TThreadHeader;
  sd: TStackDump;
  j, idumpsize: integer;
//  fa: TFunctionArray;
begin
  th.ThreadId       := Self.ThreadId;
  th.ThreadStackTop := Self.ThreadStackTop;
  th.DumpCount      := Self.SnapshotCount;

  //write stack header
  aStream.Write(th, SizeOf(th));

  for j := 0 to Self.SnapshotCount - 1 do
  begin
    sd := Self.RawStackDump[j]^;
    idumpsize := Cardinal(Self.ThreadStackTop) - Cardinal(sd.StackBase);  //$130000 - $123456

    //in case no valid stack etc
    if idumpsize > C_STACK_SIZE then
    begin
      idumpsize := 0;
      sd.Size   := 0;
    end;

    //sh.Nr   := sd.Nr;
    //sh.Size := Length(sd.Functions);
    //write snapshot header
    //strm.Write(sh, SizeOf(sh));

    //write dump header
    aStream.Write(sd, SizeOf(sd));
    //write dump
    assert(sd.Size = idumpsize);
    aStream.Write(sd.Dump^, idumpsize);

    //write snapshot
    //strm.Write(ss.Functions[0], SizeOf(TFunctionArray) * Length(ss.Functions));
  end;
end;

type
  PStackContent = ^TStackContent;
  TStackContent = record
    Address: Pointer;
  end;

procedure TThreadSampler.TraceStackRaw(aSnapshot: PStackDump);
var
  pstack  : PStackInfo;
  //StackPtr: Pointer;
  PrevCaller: Cardinal;
//  CallInstructionSize: Cardinal;
  ps: TProcessSampler;

  StackContent: PStackContent;
//  StackFrame: PStackFrame;
begin
  FTraceStackOffset := Cardinal(aSnapshot.Dump) - Cardinal(aSnapshot.StackBase);  //mem - $123456
  FTraceStackTop    := Pointer(Cardinal(FThreadStackTop)     + FTraceStackOffset);    //$130000 + 100000
  FTraceStackBase   := Pointer(Cardinal(aSnapshot.StackBase) + FTraceStackOffset);    //$123456 + 100000
  StackContent        := FTraceStackBase;  //base = EBP at moment of snapshot = stackframe
  //StackPtr   := FTraceStackBase;
//  StackFrame        := FTraceStackBase;

  ps := ProcessObject as TProcessSampler;

  // Clear the previous call address
  PrevCaller := 0;
  // Loop through all of the valid stack space
  while (Cardinal(StackContent) < Cardinal(FTraceStackTop)) do //and (inherited Count <> MaxStackTraceItems) do
  begin
    if (Cardinal(StackContent.Address) <> PrevCaller) and                     //ignore dups
       not ( (Cardinal(StackContent.Address)+ FTraceStackOffset < Cardinal(FTraceStackTop)) and  //not a stack pointer
             (Cardinal(StackContent.Address)+ FTraceStackOffset > Cardinal(FTraceStackBase))
           ) and
       (ps.GetLocationInfo( Pointer(StackContent.Address) ) <> '') then       //valid function address
       //todo: via ps.moduleobject + convert: ValidCallSite(StackPtr^, CallInstructionSize) and
    begin
      //new mem
      new(pstack);
      // then pick up the callers address
      pstack.CallerAddr := Cardinal(StackContent.Address); // - CallInstructionSize;
      // remember to callers address so that we don't report it repeatedly
      PrevCaller := Cardinal(StackContent);
      //store
      FStackList.Add(pstack);

      //StackPtr := SearchForStackPtrManipulation(StackPtr, Pointer(pstack.CallerAdr));
    end;
    // Look at the next DWORD on the stack
    Inc(StackContent);
  end;
end;

procedure TThreadSampler.UpdateCPUTime;
var
  creationtime, exittime, kerneltime, usertime: JwaWinBase._FILETIME;
begin
  //Handle := OpenThread(THREAD_SET_INFORMATION or THREAD_QUERY_INFORMATION, False, windows.GetCurrentThreadId);

  //All times are expressed using FILETIME data structures. Such a structure contains two 32-bit values that combine to form a 64-bit count of 100-nanosecond time units.
  GetThreadTimes(FThreadHandle, creationtime, exittime, kerneltime, usertime);

  //alleen diff na de 1e keer
  if FUsertime_prev > 0 then
  begin
    //time duration in msec
    FUsertime_ms   := FUsertime_ms   + ((Int64(usertime)   - Int64(FUsertime_prev))   div 1000);
    FKerneltime_ms := FKerneltime_ms + ((Int64(kerneltime) - Int64(FKerneltime_prev)) div 1000);
  end;
  FUsertime_prev   := Int64(usertime);
  FKerneltime_prev := Int64(kerneltime);

  FCreationtime  := Int64(creationtime);
  FExittime      := Int64(exittime);
end;

end.
