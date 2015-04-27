unit mcProcessSampler;

interface

uses
  Classes, SysUtils, Windows,
  JclDebug,
  mcThreadSampler, Contnrs,
  uOfflineDebugInfo, Generics.Collections;

type
  TProcessDebugInfoList = class;

  TStackDumpInfo = class
    Time: Int64;
    Nr: Integer;
  end;
  TStackDumpInfoList = class(TObjectList)
  end;

  TProcessSampler = class
  private
    FProcessId: Cardinal;
    FProcessHandle: THandle;
    FExeName: string;

//    FExeFile: THandle;
    FDebugInfo: TProcessDebugInfoList;
    FOfflineInfo: TOfflineDebugInfo;

    FAllThreads: TObjectList;
    FActiveThreads: TList;
    FThreadArray: array of Cardinal;
    FFirstSample: TDatetime;
    FLastSample: TDatetime;
    FCounterFrequency: Int64;
    FManualStarted: boolean;
    FParams: string;
//    FStackDumpCount: Integer;
    function GetThreads(Index: integer): TThreadSampler;
    function GetThreadCount: integer;
    function GetStackDumpInfo(Index: Integer): TStackDumpInfo;
    function GetStackDumpCount: Integer;
  protected
    FStackDumpInfoList: TStackDumpInfoList;
    FLocationInfoCache: TDictionary<Pointer, TJclLocationInfo>;

    procedure InitProcess;
    procedure InitDebuginfo;
  public
    constructor Create(aProcessId: Cardinal);overload;
    constructor Create(aProcessExe: string);overload;

    procedure  AfterConstruction;override;
    destructor Destroy;override;

    procedure Clear;

    procedure RefreshThreads;

    procedure StackDumpAllThreads(aNr: Integer);
    procedure ClearAllStackdumps;
    property  StackDumpCount: Integer read GetStackDumpCount;
    property  StackDumpInfo[Index:Integer]: TStackDumpInfo read GetStackDumpInfo;

    function GetLocationInfo(aProcessAdress: Pointer): string;
    function GetLocationInfoRecord(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;

    property ProcessId: Cardinal read FProcessId;
    property ProcessHandle: THandle read FProcessHandle;

    property  ProcessExe: string read FExeName write FExeName;
    property  Params    : string read FParams write FParams;
    procedure ManualStartExe;
    property  ManualStarted: boolean read FManualStarted write FManualStarted;
    procedure ManualStopExe;

    property FirstSample: TDatetime read FFirstSample;
    property LastSample: TDatetime  read FLastSample;
    property CounterFrequency: Int64 read FCounterFrequency;

    procedure LoadDumpsFromStream(aStream: TStream);
    procedure SaveDumpsToStream(aStream: TStream);
    property  OfflineInfo: TOfflineDebugInfo read FOfflineInfo write FOfflineInfo;

    property ThreadCount: integer read GetThreadCount;
    property Threads[Index:integer]: TThreadSampler read GetThreads;
  end;

  TProcessDebugInfoList = class(TJclDebugInfoList)
  private
    FProcessHandle: THandle;

    type TInternalModule = class
      ProcessModule: HMODULE;
      OurModule    : HMODULE;
      FileName     : String;
      destructor Destroy;override;
    end;
    var FModuleList: TObjectList;

    FIntModuleCache,
    FDebugInfoCache,
    FAddressCache: TList;
  protected
    procedure LoadAllModulesOfProcess;
    function CreateDebugInfo(aModuleObject: TInternalModule): TJclDebugInfoSource;
    function GetItemFromProcessModule(aModuleObject: TInternalModule): TJclDebugInfoSource;

    function GetModuleObject(const aProcessModule: HMODULE): TInternalModule;
  public
    constructor Create(aProcessHandle: THandle);
    destructor  Destroy;override;

    function GetProcessLocationInfo(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;

    property ProcessHandle: THandle read FProcessHandle write FProcessHandle;
    property ItemFromProcessModule[aModuleObject: TInternalModule]: TJclDebugInfoSource read GetItemFromProcessModule;
  end;

  TJclDebugInfoDummy = class(TJclDebugInfoSource)
  public
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
  end;


implementation

uses
  TlHelp32, JwaWinBase, JwaWinNT, PsAPI, Dialogs, uResultTypes, ShellAPI, Forms, Messages;

const
  C_INTERNAL_FILE_TYPE = 'AsmProfiler.Sampling';
  
{ TProcessSampler }

procedure TProcessSampler.AfterConstruction;
begin
  inherited;

  FAllThreads    := TObjectList.Create(True);
  FActiveThreads := TList.Create;
  SetLength(FThreadArray, 0);

  FStackDumpInfoList := TStackDumpInfoList.Create(True);
  Windows.QueryPerformanceFrequency(FCounterFrequency);

  FLocationInfoCache := TDictionary<Pointer, TJclLocationInfo>.Create;
end;

procedure TProcessSampler.Clear;
begin
  FLocationInfoCache.Clear;

  ClearAllStackdumps;
  FAllThreads.Clear;
  FActiveThreads.Clear;
  SetLength(FThreadArray, 0);

  FFirstSample := 0;
  FLastSample := 0;

  CloseHandle(FProcessHandle);
  FProcessHandle := 0;
  FProcessId := 0;
end;

procedure TProcessSampler.ClearAllStackdumps;
var
  threadsampler: TThreadSampler;
  i: Integer;
begin
  for i := 0 to FAllThreads.Count - 1 do
  begin
    threadsampler := FAllThreads.Items[i] as TThreadSampler;
    threadsampler.ClearAllDumps;
  end;

  FStackDumpInfoList.Clear;
//  FStackDumpCount := 0;
end;

constructor TProcessSampler.Create(aProcessExe: string);
begin
  FExeName := aProcessExe;
end;

constructor TProcessSampler.Create(aProcessId: Cardinal);
begin
  FProcessId     := aProcessId;
  if aProcessId > 0 then
  begin
    FProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, True, FProcessId);
  end;

  if aProcessId > 0 then
    InitProcess;
end;

destructor TProcessSampler.Destroy;
begin
  SetLength(FThreadArray, 0);
  FAllThreads.Free;
  FActiveThreads.Free;
  FStackDumpInfoList.Free;
  FLocationInfoCache.Free;
  FDebugInfo.Free;
  FOfflineInfo.Free;

  CloseHandle(FProcessHandle);

  inherited;
end;

function TProcessSampler.GetThreads(Index: integer): TThreadSampler;
begin
  Result := FAllThreads.Items[Index] as TThreadSampler;
end;

procedure TProcessSampler.InitDebuginfo;
//var
//  FFile: THandle;
//  FBinDebug: TJclDebugInfoBinary;
begin
  {
  FreeAndNil(FDebugInfo);

  FFile := LoadLibrary( pchar(FExeName) );
  //bin := TJclDebugInfoBinary.Create( Cardinal(filestrm.Memory) );
  FBinDebug := TJclDebugInfoBinary.Create(FFile);
  if FBinDebug.InitializeSource then
    FDebugInfo := FBinDebug
  else
    FreeAndNil(FBinDebug);

  if FDebugInfo = nil then
  begin

  end;
  }

  FDebugInfo := TProcessDebugInfoList.Create(FProcessHandle);
end;

procedure TProcessSampler.InitProcess;
var
  hMod: HMODULE;
//  hProcess: THandle;
  ModuleName: array [0..300] of Char;
  cb: Cardinal;
begin
  EnumProcessModules(FProcessHandle, @hMod, SizeOf(hMod), cb);
  if GetModuleFilenameEx(FProcessHandle, hMod, ModuleName, SizeOf(ModuleName)) > 0 then
    FExeName := ModuleName;

  InitDebuginfo;
end;

function GetLocationInfoStr(const Info: PJclLocationInfo;
                            IncludeModuleName: Boolean;
                            IncludeAddressOffset: Boolean
                            //IncludeStartProcLineOffset: Boolean
                            //IncludeVAdress: Boolean
                            ): string;
var
//  StartProcInfo: TJclLocationInfo;
  OffsetStr, StartProcOffsetStr, FixedProcedureName: string;
//  Module : HMODULE;
begin
  OffsetStr := '';
//  if GetLocationInfo(Addr, Info) then 
  with Info^ do
  begin
    FixedProcedureName := ProcedureName;
    if Pos(UnitName + '.', FixedProcedureName) = 1 then
      FixedProcedureName := Copy(FixedProcedureName, Length(UnitName) + 2, Length(FixedProcedureName) - Length(UnitName) - 1);

    if LineNumber > 0 then
    begin
//      if IncludeStartProcLineOffset and GetLocationInfo(Pointer(Cardinal(Info.Address) -
//        Cardinal(Info.OffsetFromProcName)), StartProcInfo) and (StartProcInfo.LineNumber > 0) then
//          StartProcOffsetStr := Format(' + %d', [LineNumber - StartProcInfo.LineNumber])
//      else
        StartProcOffsetStr := '';
      if IncludeAddressOffset then
      begin
        if OffsetFromLineNumber >= 0 then
          OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
        else
          OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
      end;
      Result := Format('[%p] %s.%s (Line %u, "%s"%s)%s', [Info.Address, UnitName, FixedProcedureName, LineNumber,
        SourceName, StartProcOffsetStr, OffsetStr]);
    end
    else
    begin
      if IncludeAddressOffset then
        OffsetStr := Format(' + $%x', [OffsetFromProcName]);
      if UnitName <> '' then
        Result := Format('[%p] %s.%s%s', [Info.Address, UnitName, FixedProcedureName, OffsetStr])
      else
        Result := Format('[%p] %s%s', [Info.Address, FixedProcedureName, OffsetStr]);
    end;
  end;
  {
  else
  begin
    Result := Format('[%p]', [Addr]);
    IncludeVAdress := True;
  end;
  }
  (*
  if IncludeVAdress or IncludeModuleName then
  begin
    Module := ModuleFromAddr(Addr);
    if IncludeVAdress then
    begin
      OffsetStr :=  Format('(%p) ', [Pointer(DWORD(Addr) - Module - ModuleCodeOffset)]);
      Result := OffsetStr + Result;
    end;
    if IncludeModuleName then
      Insert(Format('{%-12s}', [ExtractFileName(GetModulePath(Module))]), Result, 11);
  end;
  *)
    if IncludeModuleName then
      Insert(Format('{%-12s}', [ExtractFileName(Info.BinaryFileName)]), Result, 11);
end;

function TProcessSampler.GetLocationInfo(aProcessAdress: Pointer): string;
var
  info: TJclLocationInfo;
begin
 //Result := GetLocationInfoStr(aProcessAdress, True, True, True, True);
 Result := '';
 if GetLocationInfoRecord(aProcessAdress, info) then
   Result := GetLocationInfoStr(@info, True, True);
end;

function TProcessSampler.GetLocationInfoRecord(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;
begin
  if FLocationInfoCache.ContainsKey(aProcessAddr) then
  begin
    Info   := FLocationInfoCache.Items[aProcessAddr];
    Result := True;
    Exit;
  end;

  if FOfflineInfo <> nil then
    Result := FOfflineInfo.GetProcessLocationInfo(aProcessAddr, info)
  else
    Result := FDebugInfo.GetProcessLocationInfo(aProcessAddr, Info);

  FLocationInfoCache.Add(aProcessAddr, Info);
end;

function TProcessSampler.GetStackDumpCount: Integer;
begin
  Result := FStackDumpInfoList.Count;
end;

function TProcessSampler.GetStackDumpInfo(Index: Integer): TStackDumpInfo;
begin
  Result := FStackDumpInfoList.Items[Index] as TStackDumpInfo;
end;

function TProcessSampler.GetThreadCount: integer;
begin
  Result := FAllThreads.Count;
end;

procedure TProcessSampler.RefreshThreads;
var
  hSnapProc: THandle;
  threadentry: TThreadEntry32;
  bNext: Boolean;
  i: integer;
  threadsampler: TThreadSampler;
  bFound: boolean;
begin
  hSnapProc := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, FProcessHandle);
  try

    if hSnapProc <> THandle(-1) then
    begin
      threadentry.dwSize := Sizeof(threadentry);
      bNext := Thread32First(hSnapProc, threadentry);
      while bNext do
      begin
        if ThreadEntry.th32OwnerProcessID = FProcessId then   //enums all threads!
        begin
          bFound := False;
          //previous exists?
          for i := 0 to High(FThreadArray) do
            if FThreadArray[i] = threadentry.th32ThreadID then
            begin
              bFound := True;
              Break;
            end;

          //new one, add to list
          if not bFound then
          begin
            threadsampler := TThreadSampler.Create(FProcessId, threadentry.th32ThreadID);
            threadsampler.ProcessObject := Self;
            FAllThreads.Add(threadsampler);
            FActiveThreads.Add(threadsampler);

            SetLength(FThreadArray, Length(FThreadArray) + 1);
            FThreadArray[Length(FThreadArray)-1] := threadentry.th32ThreadID;
          end;
        end;

        bNext := Thread32Next(hSnapProc, threadentry);
      end;
    end;

  finally
    CloseHandle(hSnapProc);
  end;

  for i := 0 to FAllThreads.Count - 1 do
    (FAllThreads[i] as TThreadSampler).UpdateCPUTime;
end;

procedure TProcessSampler.StackDumpAllThreads(aNr: Integer);
var
  threadsampler: TThreadSampler;
  si: TStackDumpInfo;
  i: Integer;
begin
  if FFirstSample = 0 then
  begin
    FFirstSample := Now;

    //
  end;

  si := TStackDumpInfo.Create;
  si.Nr := aNr;
  Windows.QueryPerformanceCounter(si.Time);
  FStackDumpInfoList.Add(si);

  for i := 0 to FActiveThreads.Count - 1 do
  begin
    threadsampler := FActiveThreads.Items[i];
    threadsampler.MakeStackDump(aNr);
  end;

  FLastSample := Now;  
end;

procedure TProcessSampler.SaveDumpsToStream(aStream: TStream);
var
  ph: TProcessHeader;
  st: TSampleTimes;
//  th: TThreadHeader;
//  sh: TSnapshotHeader;

  ts: TThreadSampler;
//  sd: TStackDump;
//  idumpsize: integer;
//  fa: TFunctionArray;

  i: Integer;
//  j: Integer;

  s: ansiString;
//  strm: TMemoryStream;
begin
//  strm := TMemoryStream.Create;
//  strm.Size := (Self.StackDumpCount * SizeOf(TSampleTimes)) +
//               (Self.StackDumpCount *    //1000 dumps
//                Self.ThreadCount *       //3 threads
//                SizeOf(TThreadHeader) *
//                25);                             //about 25 functions per stack?


  s := C_INTERNAL_FILE_TYPE;
  //write start
  aStream.Write(s[1], Length(C_INTERNAL_FILE_TYPE));

  ph.ProcessId   := Self.ProcessId;
  ph.FirstSample := Self.FirstSample;
  ph.LastSample  := Self.LastSample;
  ph.ThreadCount := Self.ThreadCount;
  ph.CounterFrequency := Self.CounterFrequency;
  ph.SampleCount := Self.StackDumpCount;

  //write header
  aStream.Write(ph, SizeOf(ph));

  SetLength(st, Self.StackDumpCount);
  for i := 0 to Self.StackDumpCount - 1 do
  begin
    st[i].Nr   := Self.StackDumpInfo[i].Nr;
    st[i].Time := Self.StackDumpInfo[i].Time;
  end;

  //write times
  aStream.Write(st[0], SizeOf(TSampleTime) * Length(st));

  for i := 0 to Self.ThreadCount - 1 do
  begin
    ts := Self.Threads[i];
    ts.SaveDumpsToStream(aStream);

    (*
    th.ThreadId       := ts.ThreadId;
    th.ThreadStackTop := ts.ThreadStackTop;
    th.DumpCount      := ts.SnapshotCount;

    //write stack header
    strm.Write(th, SizeOf(th));

    for j := 0 to ts.SnapshotCount - 1 do
    begin
      sd := ts.RawStackDump[j];
      idumpsize := Cardinal(ts.ThreadStackTop) - Cardinal(sd.StackBase);  //$130000 - $123456

      //sh.Nr   := sd.Nr;
      //sh.Size := Length(sd.Functions);
      //write snapshot header
      //strm.Write(sh, SizeOf(sh));

      //write dump header
      strm.Write(sd, SizeOf(sd));
      //write dump
      strm.Write(sd.Snapshot, idumpsize);

      //write snapshot
      //strm.Write(ss.Functions[0], SizeOf(TFunctionArray) * Length(ss.Functions));
    end;
    *)
  end;

//  strm.SaveToFile(aFile);
end;

procedure TProcessSampler.LoadDumpsFromStream(aStream: TStream);
var
  s: ansiString;
  ph: TProcessHeader;
  st: TSampleTimes;
//  th: TThreadHeader;
//  sh: TSnapshotHeader;

  si: TStackDumpInfo;
//  sd: TStackDump;
//  idumpsize: Integer;
  ts: TThreadSampler;
//  ss: TSnapshotStack;
//  fa: TFunctionArray;

  i: Integer;
//  j: Integer;
begin
  Clear;

  SetLength(s, Length(C_INTERNAL_FILE_TYPE));
  //read start
  aStream.Read(s[1], Length(C_INTERNAL_FILE_TYPE));
  assert(s = C_INTERNAL_FILE_TYPE, 'Not a valid ' + C_INTERNAL_FILE_TYPE + ' file!');

  //read header
  aStream.Read(ph, SizeOf(ph));

  Self.FProcessId        := ph.ProcessId;
  Self.FFirstSample      := ph.FirstSample;
  Self.FLastSample       := ph.LastSample;
  //Self.ThreadCount      := ph.ThreadCount;      FAllThreads
  Self.FCounterFrequency := ph.CounterFrequency;
  //Self.StackDumpCount   := ph.SampleCount;      FStackDumpInfoList

  SetLength(st, ph.SampleCount);
  //read times
  aStream.Read(st[0], SizeOf(TSampleTime) * Length(st));

  for i := 0 to ph.SampleCount - 1 do
  begin
    si := TStackDumpInfo.Create;
    si.Nr   := st[i].Nr;
    si.Time := st[i].Time;
    FStackDumpInfoList.Add(si);
  end;

  for i := 0 to ph.ThreadCount - 1 do
  begin
    //read stack header
    //aStream.Read(th, SizeOf(th));

    //ts := TThreadSampler.Create(Self.ProcessId, th.ThreadId);
    ts := TThreadSampler.Create(0, 0);
    ts.LoadDumpsFromStream(aStream);
    FAllThreads.Add(ts);
    //aStream.Read(th, SizeOf(th));
    //ts.ThreadStackTop := th.ThreadStackTop;
    //ts.SnapshotCount := th.DumpCount;

    (*
    for j := 0 to th.DumpCount - 1 do
    begin
      //read dump header
      aStream.Read(sd, SizeOf(sd));

      idumpsize := Cardinal(ts.ThreadStackTop) - Cardinal(sd.StackBase);  //$130000 - $123456
      //read dump
      GetMem(sd.Snapshot, idumpsize);
      aStream.Read(sd.Snapshot, idumpsize);

      ts.AddStackDump(sd);

      {
      //read snapshot header
      strm.Read(sh, SizeOf(sh));
      ss.SnapshotNr := sh.Nr;
      SetLength(ss.Functions, sh.Size);
      //read snapshot
      strm.Read(ss.Functions[0], SizeOf(TFunctionArray) * Length(ss.Functions));
      }
    end;
    *)
  end;
end;

procedure TProcessSampler.ManualStartExe;
var
  SEInfo: TShellExecuteInfo;
//  ExitCode: DWORD;
//  ParamString, StartInString: string;
begin
  FillChar(SEInfo, SizeOf(SEInfo), 0) ;
  SEInfo.cbSize := SizeOf(TShellExecuteInfo) ;
  with SEInfo do
  begin
    //fMask  := SEE_MASK_NOCLOSEPROCESS;
    fMask  := SEE_MASK_NOCLOSEPROCESS;
    Wnd    := Application.Handle;
    lpFile := PChar(FExeName) ;
    {
    ParamString can contain the
    application parameters.
    }
    lpParameters := PChar(FParams);
    {
    StartInString specifies the
    name of the working directory.
    If ommited, the current directory is used.
    }
    // lpDirectory := PChar(StartInString) ;
    nShow := SW_SHOWNORMAL;
  end;

  if ShellExecuteEx(@SEInfo) then
  begin
    FManualStarted := True;
    FProcessHandle := SEInfo.hProcess;
    FProcessId     := GetProcessId(FProcessHandle);

    InitProcess;

//    repeat
//      Application.ProcessMessages;
//      GetExitCodeProcess(SEInfo.hProcess, ExitCode) ;
//    until (ExitCode <> STILL_ACTIVE) or
//         Application.Terminated;
  end

//  FProcessHandle := ShellExecute(Application.Handle, 'open', PChar(FExeName), nil, nil, SW_SHOWNORMAL);
end;

//=======================================
//copied from JvCreateProcess.pas

var
  GWinSrvHandle: HMODULE;
  GTriedLoadWinSrvDll: Boolean;
const
  WinSrvDllName = 'WINSRV.DLL';

function WinSrvHandle: HMODULE;
begin
  if not GTriedLoadWinSrvDll then
  begin
    GTriedLoadWinSrvDll := True;

    GWinSrvHandle := SafeLoadLibrary(WinSrvDllName);
    if GWinSrvHandle <> 0 then
      FreeLibrary(GWinSrvHandle);
  end;
  Result := GWinSrvHandle;
end;

function IsConsoleWindow(AHandle: THandle): Boolean;
begin
  Result := LongWord(GetWindowLong(AHandle, GWL_HINSTANCE)) = WinSrvHandle;
end;

function InternalCloseApp(ProcessID: DWORD; UseQuit: Boolean): Boolean;
type
  PEnumWinRec = ^TEnumWinRec;
  TEnumWinRec = record
    ProcessID: DWORD;
    PostQuit: Boolean;
    FoundWin: Boolean;
  end;
var
  EnumWinRec: TEnumWinRec;

  function EnumWinProc(Wnd: HWND; Param: PEnumWinRec): BOOL; stdcall;
  var
    PID, TID: DWORD;
  begin
    TID := GetWindowThreadProcessId(Wnd, @PID);
    if PID = Param.ProcessID then
    begin
      if Param.PostQuit then
        PostThreadMessage(TID, WM_QUIT, 0, 0)
      else
      if IsWindowVisible(Wnd) or IsConsoleWindow(Wnd) then
        PostMessage(Wnd, WM_CLOSE, 0, 0);
      Param.FoundWin := True;
    end;
    Result := True;
  end;

begin
  EnumWinRec.ProcessID := ProcessID;
  EnumWinRec.PostQuit := UseQuit;
  EnumWinRec.FoundWin := False;
  EnumWindows(@EnumWinProc, Integer(@EnumWinRec));
  Result := EnumWinRec.FoundWin;
end;

//=======================================

procedure TProcessSampler.ManualStopExe;
begin
  //TerminateProcess(FProcessHandle, 0);
  InternalCloseApp(FProcessID, False);
  CloseHandle(FProcessHandle);
  FProcessHandle := 0;
  FProcessId     := 0;
end;

{ TOfflineDebugInfoList }

function ModuleFromAddrOfProcess(aProcess: THandle; const aAddr: Pointer): HMODULE;
var
  MI: TMemoryBasicInformation;
begin
  { TODO : use (offline) module list }

  VirtualQueryEx(aProcess, aAddr, MI, SizeOf(MI));
  if MI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MI.AllocationBase);
end;

constructor TProcessDebugInfoList.Create(aProcessHandle: THandle);
begin
  FModuleList    := TObjectList.Create;
  FProcessHandle := aProcessHandle;

  FAddressCache   := TList.Create;
  FDebugInfoCache := TList.Create;
  FIntModuleCache := TList.Create;

  //add dummy debug info class, in case no other one is found, so no stupid retry everytime
  RegisterDebugInfoSource(TJclDebugInfoDummy);

  inherited Create(True);
end;

function TProcessDebugInfoList.CreateDebugInfo(aModuleObject: TInternalModule): TJclDebugInfoSource;
//var
//  i: Integer;
//  im: TInternalModule;
  //sModule: string;
//  ourmodule: HMODULE;
begin
  Result := nil;
  if aModuleObject = nil then Exit;
  //load debuginfo for loaded module in OUR process space
  Result := inherited CreateDebugInfo(aModuleObject.OurModule);
end;

destructor TProcessDebugInfoList.Destroy;
begin
  FModuleList.Free;
  FAddressCache.Free;
  FDebugInfoCache.Free;
  FIntModuleCache.Free;
  inherited;
end;

function TProcessDebugInfoList.GetItemFromProcessModule(aModuleObject: TInternalModule): TJclDebugInfoSource;
var
  I: Integer;
  TempItem: TJclDebugInfoSource;
begin
  Result := nil;
  if aModuleObject = nil then Exit;
  if aModuleObject.OurModule = 0 then Exit;
  if aModuleObject.OurModule = THandle(-1) then Exit;

  for I := 0 to Count - 1 do
  begin
    TempItem := Items[I];
    if TempItem.Module = aModuleObject.OurModule then
    begin
      Result := TempItem;
      Break;
    end;
  end;

  if Result = nil then
  begin
    Result := CreateDebugInfo(aModuleObject);
    if Result <> nil then
      Add(Result);
  end;
end;

function TProcessDebugInfoList.GetModuleObject(const aProcessModule: HMODULE): TInternalModule;
var
  im: TInternalModule;
  i: Integer;
  cModuleName: array [0..300] of Char;
begin
  Result := nil;
  if aProcessModule = 0 then Exit;  

  //convert module of process into our module by loading it etc
  if FModuleList.Count = 0 then
    LoadAllModulesOfProcess;

  for i := 0 to FModuleList.Count - 1 do
  begin
    im := FModuleList.Items[i] as TInternalModule;
    if im.ProcessModule = aProcessModule then
    begin
      if im.OurModule = 0 then
      begin
        GetModuleFilenameEx(FProcessHandle, aProcessModule, cModuleName, SizeOf(cModuleName));
        im.FileName  := cModuleName;
//        im.OurModule := LoadLibrary(cModuleName); //load module in OUR process space
        im.OurModule := LoadLibraryEx(cModuleName, 0, DONT_RESOLVE_DLL_REFERENCES); //load module in OUR process space

        if im.OurModule = 0 then
        begin
          MessageDlg('Could not load module: ' + cModuleName, mtWarning, [mbOK], 0);
          im.OurModule := THandle(-1);
        end;
      end;
      Result := im;
      Exit;
    end;
  end;
end;

function TProcessDebugInfoList.GetProcessLocationInfo(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;
var
  Item: TJclDebugInfoSource;
  hProcessModule: HMODULE;
  im: TInternalModule;
  pLocalAddr: Pointer;
  iOffset: Integer;
  iIndex: integer;
begin
  Finalize(Info);
  FillChar(Info, SizeOf(Info), #0);

  iIndex := FAddressCache.IndexOf(aProcessAddr);
  if iIndex >= 0 then
  begin
    Item := FDebugInfoCache.Items[iIndex];
    im   := FIntModuleCache.Items[iIndex];
  end
  else
  begin
    //get process(!) module of process address
    hProcessModule := ModuleFromAddrOfProcess(FProcessHandle, aProcessAddr);
    //load (offline?) module object
    im   := GetModuleObject(hProcessModule);
    Item := ItemFromProcessModule[im];

    FAddressCache.Add(aProcessAddr);
    FDebugInfoCache.Add(Item);
    FIntModuleCache.Add(im);
  end;

  if Item <> nil then
  begin
    iOffset    := im.OurModule - im.ProcessModule;    //$EE123456 - $400000
    //convert remote process addr to local addr
    //(because module is loaded on different memory location in our process)
    pLocalAddr := Pointer(NativeUInt(aProcessAddr) + NativeUInt(iOffset));
    //get debug info

    //todo: own optimized version
    Result     := Item.GetLocationInfo(pLocalAddr, Info);

    //reverse offset
    Info.Address := Pointer(NativeUInt(Info.Address) - NativeUInt(iOffset));
  end
  else
    Result := False;

  //no result? than only fill module + address
  if not Result and
     (im <> nil) then
  begin
    Info.BinaryFileName := im.FileName;
    Info.Address        := aProcessAddr;
    Result := True;
  end;
end;

procedure TProcessDebugInfoList.LoadAllModulesOfProcess;
var
  hMod: HMODULE;
  modulearray: array of HMODULE;
//  hProcess: THandle;
  cb: Cardinal;
  iModuleCount: integer;
  im: TInternalModule;
  i: Integer;
begin
  //get number of modules
  EnumProcessModules(FProcessHandle, @hMod, SizeOf(hMod), cb);
  iModuleCount := cb div SizeOf(HMODULE);

  //alloc buffer
  SetLength(modulearray, iModuleCount);
  //get all modules
  EnumProcessModules(FProcessHandle, @modulearray[0], cb, cb);

  FModuleList.Clear;
  for i := 0 to High(modulearray) do
  begin
    im := TInternalModule.Create;
    im.ProcessModule := modulearray[i];
    //im.OurModule     :=
    FModuleList.Add(im);
  end;
end;

{ TJclDebugInfoDummy }

function TJclDebugInfoDummy.GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
begin
  Result := False;
end;

function TJclDebugInfoDummy.InitializeSource: Boolean;
begin
  Result := True;
end;

{ TProcessDebugInfoList.TInternalModule }

destructor TProcessDebugInfoList.TInternalModule.Destroy;
begin
  FreeLibrary(OurModule);
  inherited;
end;

end.
