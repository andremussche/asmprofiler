unit mcOfflineStackInfo;

interface

uses
  Windows, Classes,
  JwaImageHlp,
  mcOfflineDebugInfo,
  jclBase, jclDebug;

type
  TJclStackInfoItem = class(TObject)
  private
    FStackInfo: TStackInfo;
//    function GetCallerAddr: Pointer;
//    function GetLogicalAddress: TJclAddr;
  public
//    property CallerAddr: Pointer read Pointer(FStackInfo.CallerAddr);
//    property LogicalAddress: TJclAddr read GetLogicalAddress;
    property StackInfo: TStackInfo read FStackInfo;
  end;

  TOfflineStackInfoList = class(TJclStackBaseList)
  private
    FDumpBase: Pointer;
    FThread: PMINIDUMP_THREAD;

    //FIgnoreLevels: DWORD;
    FTopOfStack: TJclAddr;
    FBaseOfStack: TJclAddr;
    FStackData: PPointer;
    FFramePointer: Pointer;
//    FModuleInfoList: TJclModuleInfoList;
//    FCorrectOnAccess: Boolean;
//    FSkipFirstItem: Boolean;
//    FDelayedTrace: Boolean;
//    FInStackTracing: Boolean;
//    FRaw: Boolean;
    FStackOffset: TJclAddr;

    FOfflineDebugInfo: TOfflineProcessDebugInfoList;
    class var FDontCheckValidCallSite: Boolean;

    procedure StoreToList(const StackInfo: TStackInfo);
    function NextStackFrame(var StackFrame: PStackFrame;
      var StackInfo: TStackInfo): Boolean;

    function ValidStackAddr(StackAddr: TJclAddr): Boolean;
    function ValidCodeAddr(CodeAddr: DWORD): Boolean;
    function ValidCallSite(CodeAddr: TJclAddr;
      out CallInstructionSize: Cardinal): Boolean;

    function  GetItems(Index: Integer): TJclStackInfoItem;
  public
    //procedure LoadFromDump(aDumpBase: Pointer; aThread: PMINIDUMP_THREAD);
    constructor Create(aDumpBase: Pointer; aThread: PMINIDUMP_THREAD);overload;
    constructor Create(aStackData: Pointer; aTopOfStack, aBaseOfStack: NativeUInt);overload;

    procedure LoadFromDump(aDumpBase: Pointer; aThread: PMINIDUMP_THREAD);
    procedure LoadFromMemory(aStackData: Pointer; aTopOfStack, aBaseOfStack: NativeUInt);
    property  OffLineDebugInfo: TOfflineProcessDebugInfoList read FOfflineDebugInfo write FOfflineDebugInfo;

    procedure AddToStrings(Strings: TStrings; IncludeModuleName: Boolean = False;
      IncludeAddressOffset: Boolean = False; IncludeStartProcLineOffset: Boolean = False;
      IncludeVAddress: Boolean = False);

    property  Items[Index: Integer]: TJclStackInfoItem read GetItems; default;

    procedure TraceStackFrames;
    procedure TraceStackRaw;
    procedure TraceStackRaw2;
    procedure TraceStackWalk64;

    class property DontCheckValidCallSite: Boolean read FDontCheckValidCallSite write FDontCheckValidCallSite;
  end;

implementation

uses
  JclSysUtils, Math;

function TOfflineStackInfoList.ValidCallSite(CodeAddr: TJclAddr; out CallInstructionSize: Cardinal): Boolean;
var
  CodeDWORD4: DWORD;
  CodeDWORD8: DWORD;
  C4P, C8P: PDWORD;
  RM1, RM2, RM5: Byte;
begin
  // todo: 64 bit version

  CallInstructionSize := 0;

  // First check that the address is within range of our code segment!
  C8P := PDWORD(CodeAddr - 8);
  C4P := PDWORD(CodeAddr - 4);
  Result := (CodeAddr > 8) and
            //ValidCodeAddr(TJclAddr(C8P)) and
            not IsBadReadPtr(C8P, 8);
            //(CodeAddr > HInstance);

  if DontCheckValidCallSite then
    Exit;

  // Now check to see if the instruction preceding the return address
  // could be a valid CALL instruction
  if Result then
  begin
    try
      CodeDWORD8 := PDWORD(C8P)^;
      CodeDWORD4 := PDWORD(C4P)^;
      // CodeDWORD8 = (ReturnAddr-5):(ReturnAddr-6):(ReturnAddr-7):(ReturnAddr-8)
      // CodeDWORD4 = (ReturnAddr-1):(ReturnAddr-2):(ReturnAddr-3):(ReturnAddr-4)

      // ModR/M bytes contain the following bits:
      // Mod        = (76)
      // Reg/Opcode = (543)
      // R/M        = (210)
      RM1 := (CodeDWORD4 shr 24) and $7;
      RM2 := (CodeDWORD4 shr 16) and $7;
      //RM3 := (CodeDWORD4 shr 8)  and $7;
      //RM4 :=  CodeDWORD4         and $7;
      RM5 := (CodeDWORD8 shr 24) and $7;
      //RM6 := (CodeDWORD8 shr 16) and $7;
      //RM7 := (CodeDWORD8 shr 8)  and $7;

      // Check the instruction prior to the potential call site.
      // We consider it a valid call site if we find a CALL instruction there
      // Check the most common CALL variants first
      if ((CodeDWORD8 and $FF000000) = $E8000000) then
        // 5 bytes, "CALL NEAR REL32" (E8 cd)
        CallInstructionSize := 5
      else
      if ((CodeDWORD4 and $F8FF0000) = $10FF0000) and not (RM1 in [4, 5]) then
        // 2 bytes, "CALL NEAR [EAX]" (FF /2) where Reg = 010, Mod = 00, R/M <> 100 (1 extra byte)
        // and R/M <> 101 (4 extra bytes)
        CallInstructionSize := 2
      else
      if ((CodeDWORD4 and $F8FF0000) = $D0FF0000) then
        // 2 bytes, "CALL NEAR EAX" (FF /2) where Reg = 010 and Mod = 11
        CallInstructionSize := 2
      else
      if ((CodeDWORD4 and $00FFFF00) = $0014FF00) then
        // 3 bytes, "CALL NEAR [EAX+EAX*i]" (FF /2) where Reg = 010, Mod = 00 and RM = 100
        // SIB byte not validated
        CallInstructionSize := 3
      else
      if ((CodeDWORD4 and $00F8FF00) = $0050FF00) and (RM2 <> 4) then
        // 3 bytes, "CALL NEAR [EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM <> 100 (1 extra byte)
        CallInstructionSize := 3
      else
      if ((CodeDWORD4 and $0000FFFF) = $000054FF) then
        // 4 bytes, "CALL NEAR [EAX+EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM = 100
        // SIB byte not validated
        CallInstructionSize := 4
      else
      if ((CodeDWORD8 and $FFFF0000) = $15FF0000) then
        // 6 bytes, "CALL NEAR [$12345678]" (FF /2) where Reg = 010, Mod = 00 and RM = 101
        CallInstructionSize := 6
      else
      if ((CodeDWORD8 and $F8FF0000) = $90FF0000) and (RM5 <> 4) then
        // 6 bytes, "CALL NEAR [EAX+$12345678]" (FF /2) where Reg = 010, Mod = 10 and RM <> 100 (1 extra byte)
        CallInstructionSize := 6
      else
      if ((CodeDWORD8 and $00FFFF00) = $0094FF00) then
        // 7 bytes, "CALL NEAR [EAX+EAX+$1234567]" (FF /2) where Reg = 010, Mod = 10 and RM = 100
        CallInstructionSize := 7
      else
      if ((CodeDWORD8 and $0000FF00) = $00009A00) then
        // 7 bytes, "CALL FAR $1234:12345678" (9A ptr16:32)
        CallInstructionSize := 7
      else
        Result := False;
      // Because we're not doing a complete disassembly, we will potentially report
      // false positives. If there is odd code that uses the CALL 16:32 format, we
      // can also get false negatives.
    except
      Result := False;
    end;
  end;
end;

procedure TOfflineStackInfoList.TraceStackRaw;
var
  StackInfo: TStackInfo;
  StackPtr: PJclAddr;
  PrevCaller: TJclAddr;
  CallInstructionSize: Cardinal;
  StackTop: TJclAddr;
begin
  if Count < 32 then
    Capacity := 32; // reduce ReallocMem calls, must be > 1 because the caller's EIP register is already in the list

  if not Assigned(FStackData) then
    Exit;
  StackPtr := PJclAddr(FStackData);
  StackTop := FTopOfStack;

//  if Count > 0 then
//    StackPtr := SearchForStackPtrManipulation(StackPtr, Pointer(Items[0].StackInfo.CallerAddr));

  // We will not be able to fill in all the fields in the StackInfo record,
  // so just blank it all out first
  ResetMemory(StackInfo, SizeOf(StackInfo));
  // Clear the previous call address
  PrevCaller := 0;
  // Loop through all of the valid stack space
  while (TJclAddr(StackPtr) < StackTop) and (inherited Count <> MaxStackTraceItems) do
  begin
    // If the current DWORD on the stack refers to a valid call site...
    if ValidCallSite(StackPtr^, CallInstructionSize) and (StackPtr^ <> PrevCaller) then
    begin
      // then pick up the callers address
      StackInfo.CallerAddr := StackPtr^ - CallInstructionSize;
      // remember to callers address so that we don't report it repeatedly
      PrevCaller := StackPtr^;
      // increase the stack level
      Inc(StackInfo.Level);
      // then report it back to our caller
      StoreToList(StackInfo);
      //StackPtr := SearchForStackPtrManipulation(StackPtr, Pointer(StackInfo.CallerAddr));
    end;
    // Look at the next DWORD on the stack
    Inc(StackPtr);
  end;
//  if Assigned(FStackData) then
//  begin
//    FreeMem(FStackData);
//    FStackData := nil;
//  end;
end;

procedure TOfflineStackInfoList.AddToStrings(Strings: TStrings; IncludeModuleName,
  IncludeAddressOffset, IncludeStartProcLineOffset, IncludeVAddress: Boolean);
var
  I: Integer;
  s: string;
//  info: TJclLocationInfo;
begin
  Strings.BeginUpdate;
  try
    for I := 0 to Count - 1 do
    begin
//      if OffLineDebugInfo <> nil then
//        OffLineDebugInfo.GetLocationInfo(Pointer(Items[I].StackInfo.CallerAddr), info)
//      else
//        info := GetLocationInfo(Pointer(Items[I].StackInfo.CallerAddr));
      //if info.Address = nil then Continue;

      if OffLineDebugInfo <> nil then
        s := OffLineDebugInfo.GetOfflineLocationInfoStr(Pointer(Items[I].StackInfo.CallerAddr),
                                                 IncludeModuleName, IncludeAddressOffset,
                                                 IncludeStartProcLineOffset, IncludeVAddress )
      else
        s := GetLocationInfoStr( Pointer(Items[I].StackInfo.CallerAddr),
                                 IncludeModuleName, IncludeAddressOffset,
                                 IncludeStartProcLineOffset, IncludeVAddress );
      if s <> '' then
        Strings.Add(s);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

constructor TOfflineStackInfoList.Create(aDumpBase: Pointer;
  aThread: PMINIDUMP_THREAD);
begin
  inherited Create;
  LoadFromDump(aDumpBase, aThread);
end;

constructor TOfflineStackInfoList.Create(aStackData: Pointer; aTopOfStack,
  aBaseOfStack: NativeUInt);
begin
  inherited Create;
  LoadFromMemory(aStackData, aTopOfStack, aBaseOfStack);
end;

function TOfflineStackInfoList.GetItems(Index: Integer): TJclStackInfoItem;
begin
  Result := TJclStackInfoItem(Get(Index));
end;

procedure TOfflineStackInfoList.LoadFromDump(aDumpBase: Pointer; aThread: PMINIDUMP_THREAD);
var
  ctx: PCONTEXT;
  pTraceStackBase: Pointer;
  pTraceStackTop: Pointer;
  pTraceStackOffset: Integer;
begin
  ctx := PCONTEXT( NativeUInt(aDumpBase) + aThread.ThreadContext.Rva );

  FDumpBase    := aDumpBase;
  FThread      := aThread;

  FStackData   := Pointer(NativeUInt(aDumpBase) + aThread.Stack.Memory.Rva);
  FBaseOfStack := ctx.Ebp;                           //base: e.g. $123456
  FTopOfStack  := FBaseOfStack + aThread.Stack.Memory.DataSize;

  pTraceStackOffset := Cardinal(FStackData) - Cardinal(FBaseOfStack);          //mem - $123456
  pTraceStackTop    := Pointer(Cardinal(FTopOfStack)  + pTraceStackOffset);    //$130000 + 100000
  pTraceStackBase   := Pointer(Cardinal(FBaseOfStack) + pTraceStackOffset);    //$123456 + 100000
  FTopOfStack    := NativeUInt(pTraceStackTop);
  FBaseOfStack   := NativeUInt(pTraceStackBase);
//  FBaseOfStack := NativeUInt(FStackData);
//  FTopOfStack  := FBaseOfStack + aThread.Stack.Memory.DataSize;
end;

procedure TOfflineStackInfoList.LoadFromMemory(aStackData: Pointer; aTopOfStack,
  aBaseOfStack: NativeUInt);
begin
  FStackData   := aStackData;
  FTopOfStack  := aTopOfStack;
  FBaseOfStack := aBaseOfStack;
end;

procedure TOfflineStackInfoList.TraceStackFrames;
var
  StackFrame: PStackFrame;
  StackInfo: TStackInfo;
begin
  Capacity := Max(Self.Count, 32); // reduce ReallocMem calls, must be > 1 because the caller's EIP register is already in the list

  // Start at level 0
  StackInfo.Level := 0;
  StackInfo.CallerFrame := 0;

  // Get the current stack frame from the frame register
  StackFrame := FFramePointer;

  // We define the bottom of the valid stack to be the current frame Pointer
  // There is a TIB field called pvStackUserBase, but this includes more of the
  // stack than what would define valid stack frames.
  FBaseOfStack := TJclAddr(StackFrame) - 1;
  // Loop over and report all valid stackframes
  while NextStackFrame(StackFrame, StackInfo) and (inherited Count <> MaxStackTraceItems) do
    StoreToList(StackInfo);
end;

procedure TOfflineStackInfoList.StoreToList(const StackInfo: TStackInfo);
var
  Item: TJclStackInfoItem;
begin
  //if StackInfo.Level > IgnoreLevels + 1 then
  begin
    Item := TJclStackInfoItem.Create;
    Item.FStackInfo := StackInfo;
    Add(Item);
  end;
end;

function TOfflineStackInfoList.NextStackFrame(var StackFrame: PStackFrame; var StackInfo: TStackInfo): Boolean;
var
  CallInstructionSize: Cardinal;
  StackFrameCallerFrame, NewFrame: TJclAddr;
  StackFrameCallerAddr: TJclAddr;
begin
  // Only report this stack frame into the StockInfo structure
  // if the StackFrame pointer, the frame pointer and the return address on the stack
  // are valid addresses
  StackFrameCallerFrame := StackInfo.CallerFrame;
  while ValidStackAddr(TJclAddr(StackFrame)) do
  begin
    // CallersEBP above the previous CallersEBP
    NewFrame := StackFrame^.CallerFrame;
    if NewFrame <= StackFrameCallerFrame then
      Break;
    StackFrameCallerFrame := NewFrame;

    // CallerAddr within current process space, code segment etc.
    // CallerFrame within current thread stack. Added Mar 12 2002 per Hallvard's suggestion
    StackFrameCallerAddr := StackFrame^.CallerAddr;
    if ValidCodeAddr(StackFrameCallerAddr) and
       ValidStackAddr(StackFrameCallerFrame + FStackOffset) then
    begin
      Inc(StackInfo.Level);
      StackInfo.StackFrame := StackFrame;
      StackInfo.ParamPtr := PDWORD_PTRArray(TJclAddr(StackFrame) + SizeOf(TStackFrame));

      if StackFrameCallerFrame > StackInfo.CallerFrame then
        StackInfo.CallerFrame := StackFrameCallerFrame
      else
        // the frame pointer points to an address that is below
        // the last frame pointer, so it must be invalid
        Break;

      // Calculate the address of caller by subtracting the CALL instruction size (if possible)
      if ValidCallSite(StackFrameCallerAddr, CallInstructionSize) then
        StackInfo.CallerAddr := StackFrameCallerAddr - CallInstructionSize
      else
        StackInfo.CallerAddr := StackFrameCallerAddr;
      StackInfo.DumpSize := StackFrameCallerFrame - TJclAddr(StackFrame);
      StackInfo.ParamSize := (StackInfo.DumpSize - SizeOf(TStackFrame)) div 4;
      if PStackFrame(StackFrame^.CallerFrame) = StackFrame then
        Break;
      // Step to the next stack frame by following the frame pointer
      StackFrame := PStackFrame(StackFrameCallerFrame + FStackOffset);
      Result := True;
      Exit;
    end;
    // Step to the next stack frame by following the frame pointer
    StackFrame := PStackFrame(StackFrameCallerFrame + FStackOffset);
  end;
  Result := False;
end;

function TOfflineStackInfoList.ValidCodeAddr(CodeAddr: DWORD): Boolean;
begin
  Result := FOfflineDebugInfo.FindModuleFromAddr( Pointer(CodeAddr) ) <> 0;
end;

function TOfflineStackInfoList.ValidStackAddr(StackAddr: TJclAddr): Boolean;
begin
  Result := (FBaseOfStack < StackAddr) and
            (StackAddr < FTopOfStack);
end;

type
  PStackContent = ^TStackContent;
  TStackContent = record
    Address: Pointer;
  end;

procedure TOfflineStackInfoList.TraceStackRaw2;
var
  pstack  : PStackInfo;
  //StackPtr: Pointer;
  PrevCaller: Cardinal;
  iCallInstructionSize: Cardinal;
//  ps: TProcessSampler;
  StackContent: PStackContent;
//  StackFrame: PStackFrame;
  FTraceStackBase: Pointer;
  FTraceStackTop: Pointer;
//  FTraceStackOffset: Integer;
  pLocalAddr: Pointer;
begin
//  FTraceStackOffset := Cardinal(FStackData) - Cardinal(FBaseOfStack);          //mem - $123456
//  FTraceStackTop    := Pointer(Cardinal(FTopOfStack)  + FTraceStackOffset);    //$130000 + 100000
//  FTraceStackBase   := Pointer(Cardinal(FBaseOfStack) + FTraceStackOffset);    //$123456 + 100000
  FTraceStackTop    := Pointer(FTopOfStack);
  FTraceStackBase   := Pointer(FBaseOfStack);
  StackContent      := FTraceStackBase;  //base = EBP at moment of snapshot = stackframe
  //StackPtr   := FTraceStackBase;
//  StackFrame        := FTraceStackBase;
  //ps := ProcessObject as TProcessSampler;

  // Clear the previous call address
  PrevCaller := 0;
  // Loop through all of the valid stack space
  while (Cardinal(StackContent) < Cardinal(FTraceStackTop)) do //and (inherited Count <> MaxStackTraceItems) do
  begin
    if (Cardinal(StackContent.Address) <> PrevCaller)                      //ignore dups
       //not ( (Cardinal(StackContent.Address)+ FTraceStackOffset < Cardinal(FTraceStackTop)) and  //not a stack pointer
       //      (Cardinal(StackContent.Address)+ FTraceStackOffset > Cardinal(FTraceStackBase))
       //    ) //and
       //(ps.GetLocationInfo( Pointer(StackContent.Address) ) <> '') then       //valid function address
       //todo: via ps.moduleobject + convert: ValidCallSite(StackPtr^, CallInstructionSize) and
    then
    begin
      pLocalAddr := OffLineDebugInfo.GetOffline2LocalAddres(StackContent.Address);
      //if ValidCallSite(Cardinal(pLocalAddr), iCallInstructionSize) then
      if pLocalAddr <> nil then
      if ValidCallSite(Cardinal(pLocalAddr), iCallInstructionSize) then
      begin
        //new mem
        new(pstack);
        // then pick up the callers address
        pstack.CallerAddr := Cardinal(StackContent.Address) - iCallInstructionSize;
        // remember to callers address so that we don't report it repeatedly
        PrevCaller := Cardinal(StackContent.Address);

        //store
        StoreToList(pstack^);

        //StackContent := SearchForStackPtrManipulation(StackContent, StackContent.Address);
      end;
    end;
    // Look at the next DWORD on the stack
    Inc(StackContent);
  end;
end;

procedure TOfflineStackInfoList.TraceStackWalk64;
var
  ctx: PCONTEXT;
  Sf : JwaImageHlp.TStackFrame64;
begin
  ctx := PCONTEXT( NativeUInt(FDumpBase) + FThread.ThreadContext.Rva );

  {fill}
  FillChar(Sf, SizeOf(TStackFrame64), #0);
  Sf.AddrPC.Offset    := ctx.Eip;
  Sf.AddrPC.Mode      := AddrModeFlat;
  Sf.AddrStack.Offset := ctx.Esp;
  Sf.AddrStack.Mode   := AddrModeFlat;
  Sf.AddrFrame.Offset := ctx.Ebp;      //stack frame
  Sf.AddrFrame.Mode   := AddrModeFlat;

//  While StackWalk64(IMAGE_FILE_MACHINE_I386, hProcess, hThread,
//                  Sf, ctx, nil,
//                  @SymFunctionTableAccess64, @SymGetModuleBase64, nil) do
end;

end.

