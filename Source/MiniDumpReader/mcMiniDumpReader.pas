unit mcMiniDumpReader;

interface

uses
  JwaImageHlp, JclDebug,
  Classes, Windows, SysUtils, Generics.Collections,  
  mcExtraImageHlp;

//see also:
//http://www.debuginfo.com/articles/effminidumps.html  (and other articles)

//about possible minidump types:
//http://msdn.microsoft.com/en-us/library/ms680394(v=vs.85).aspx

//todo: pdb files:
//http://www.debuginfo.com/articles/debuginfomatch.html
//http://www.informit.com/articles/article.aspx?p=22429&seqNum=3
//https://sourceforge.net/projects/tds2pdb/

type
  TMiniDumpThread = class
  private
    Teb: NativeUint;
  public
    ThreadId,
    SuspendCount,
    PriorityClass,                   //ABOVE_NORMAL_PRIORITY_CLASS, BELOW_NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, PROCESS_MODE_BACKGROUND_BEGIN, PROCESS_MODE_BACKGROUND_END, REALTIME_PRIORITY_CLASS
    Priority: NativeUint;            //THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_TIME_CRITICAL
    //Stack: TOfflineStackInfoList;
    Stack: TJclStackBaseList;
//    Stack: record
//      TopOfStack:  NativeUInt;
//      BaseOfStack: NativeUInt;
//      StackData:   Pointer;
//      TracedStack: array of Pointer;
//    end;

    ThreadContext: PCONTEXT;
    //extra info
    ThreadInfo: PMINIDUMP_THREAD_INFO;
  end;

  TMiniDumpThreadList = class(TObjectList<TMiniDumpThread>)
  public
    function GetThreadByID(aThreadId: NativeUInt): TMiniDumpThread;
  end;

  TMiniDumpModule = class
  private
    TimeDateStamp: UInt32;           //The timestamp value of the module executable image, in time_t format.
//    CvRecord: MINIDUMP_LOCATION_DESCRIPTOR;         //.pdb: A MINIDUMP_LOCATION_DESCRIPTOR structure that specifies the CodeView record of the module.
//    DebugRecord: MINIDUMP_LOCATION_DESCRIPTOR;      //.dbg: A MINIDUMP_LOCATION_DESCRIPTOR structure that specifies the miscellaneous record of the module.
  public
    BaseOfImage  : UInt64;           //The base address of the module executable image in memory.
    SizeOfImage  : UInt32;           //The size of the module executable image in memory, in bytes.
    CheckSum     : UInt32;           //The checksum value of the module executable image.
    Time         : TDateTime;
    FileName     : String;

    VersionInfo  : VS_FIXEDFILEINFO; //A VS_FIXEDFILEINFO structure that specifies the version of the module.
  end;
  TMiniDumpModuleList = class(TObjectList<TMiniDumpModule>);

  TMiniDumpHandle = class
    Handle:           UInt64;
//    TypeNameRva;      RVA
//    ObjectNameRva;    RVA
    TypeName,
    ObjectName: string;
    Attributes,
    GrantedAccess,
    HandleCount,
    PointerCount:     UInt32;
  end;
  TMiniDumpHandleList = TObjectList<TMiniDumpHandle>;

  TMinidumpReader = class
  private
    FMinidumpFile, FMinidumpMap: THandle;
    FMinidumpView: pointer;

    FOfflineProcessDebugInfoList: TJclDebugInfoList;
    
    FSystemInfo: PMINIDUMP_SYSTEM_INFO;
    FProcessInfo: PMINIDUMP_MISC_INFO;
    FThreads: TMiniDumpThreadList;
    FModules: TMiniDumpModuleList;
    FHandles: TMiniDumpHandleList;
    function GetThreads: TMiniDumpThreadList;
    function GetModules: TMiniDumpModuleList;
    function GetSystemInfo: PMINIDUMP_SYSTEM_INFO;
    function GetProcessInfo: PMINIDUMP_MISC_INFO;
    function GetServicepack: string;
    function GetHandles: TMiniDumpHandleList;

    function RVAToString(aRVA: RVA): string;
    function GetDontCheckValidCallSite: Boolean;
    procedure SetDontCheckValidCallSite(const Value: Boolean);
  public
    constructor Create(const aMinidump: TFileName);
    destructor  Destroy;override;

    procedure AddThreadStackToStrings(const aOriginalExe: TFileName;
                                      aThread: TMiniDumpThread; aStrings: TStrings;
                                      aIncludeModuleName: Boolean = False; aIncludeAddressOffset: Boolean = False;
                                      aIncludeStartProcLineOffset: Boolean = False; aIncludeVAddress: Boolean = False);

    property SystemInfo:  PMINIDUMP_SYSTEM_INFO read GetSystemInfo;
    property Servicepack: string read GetServicepack;
    property ProcessInfo: PMINIDUMP_MISC_INFO read GetProcessInfo;

    property Threads: TMiniDumpThreadList read GetThreads;
    property Modules: TMiniDumpModuleList read GetModules;
    property Handles: TMiniDumpHandleList read GetHandles;

    property DontCheckValidCallSite: Boolean read GetDontCheckValidCallSite write SetDontCheckValidCallSite;
  end;

implementation

uses
  JwaWinType, DateUtils,
  mcOfflineDebugInfo, mcOfflineStackInfo;

{ TMinidumpReader }

constructor TMinidumpReader.Create(const aMinidump: TFileName);
begin
  if aMinidump <> '' then
  begin
    // Map the dump file into memory and let the operating system decide how to cache
    FMinidumpFile := CreateFile( PChar(aMinidump), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if FMinidumpFile = INVALID_HANDLE_VALUE then
      RaiseLastOSError;

    FMinidumpMap := CreateFileMapping(FMinidumpFile, nil, PAGE_READONLY, 0, 0, nil);
    if FMinidumpMap = 0 then
      RaiseLastOSError;

    FMinidumpView := MapViewOfFile(FMinidumpMap, FILE_MAP_READ, 0, 0, 0);
    if FMinidumpView = nil then
      RaiseLastOSError;
  end;

  DontCheckValidCallSite := False;
end;

destructor TMinidumpReader.Destroy;
begin
  FThreads.Free;
  FModules.Free;
  FHandles.Free;
  FSystemInfo  := nil;
  FProcessInfo := nil;

  FOfflineProcessDebugInfoList.Free;

  UnmapViewOfFile(FMinidumpView);
  CloseHandle(FMinidumpMap);
  CloseHandle(FMinidumpFile);
  inherited;
end;

function TMinidumpReader.GetDontCheckValidCallSite: Boolean;
begin
  Result := TOfflineStackInfoList.DontCheckValidCallSite;
end;

function TMinidumpReader.GetHandles: TMiniDumpHandleList;
var
  i: integer;
  handle: TMiniDumpHandle;

  pdir: PMINIDUMP_DIRECTORY;
  phandles: PMINIDUMP_HANDLE_DATA_STREAM;
  phandle: PMINIDUMP_HANDLE_DESCRIPTOR;
  pStream: pointer;
  istreamsize: Cardinal;
begin
  Result := FHandles;
  if FHandles <> nil then Exit;
  if FMinidumpView = nil then Exit;
  FHandles := TMiniDumpHandleList.Create(True{owns objects});
  Result   := FHandles;

  if MiniDumpReadDumpStream(FMinidumpView, HandleDataStream, pdir, pStream, istreamsize) then
  begin
    //threads
    phandles := PMINIDUMP_HANDLE_DATA_STREAM(pStream);
    for i := 0 to phandles.NumberOfDescriptors - 1 do
    begin
      phandle  := PMINIDUMP_HANDLE_DESCRIPTOR( NativeUInt(phandles) + 
                                               SizeOf(_MINIDUMP_HANDLE_DATA_STREAM) + 
                                               (i * SizeOf(PMINIDUMP_HANDLE_DESCRIPTOR)) );
      //Assert( SizeOf(_MINIDUMP_HANDLE_DATA_STREAM) = 32 );

      handle   := TMiniDumpHandle.Create;
      FHandles.Add(handle);

      handle.Handle           := phandle.Handle;
      handle.TypeName         := RVAToString(phandle.TypeNameRva);
      handle.ObjectName       := RVAToString(phandle.ObjectNameRva);
      handle.Attributes       := phandle.Attributes;
      handle.GrantedAccess    := phandle.GrantedAccess;
      handle.HandleCount      := phandle.HandleCount;
      handle.PointerCount     := phandle.PointerCount;
    end;
  end;
end;

function TMinidumpReader.GetModules: TMiniDumpModuleList;
var
  i: integer;
  module: TMiniDumpModule;

  pdir: PMINIDUMP_DIRECTORY;
  pmodules: PMINIDUMP_MODULE_LIST;
  pmodule: PMINIDUMP_MODULE;
  pStream: pointer;
  istreamsize: Cardinal;
begin
  Result := FModules;
  if FModules <> nil then Exit;
  if FMinidumpView = nil then Exit;
  FModules := TMiniDumpModuleList.Create(True{owns objects});
  Result   := FModules;

  if MiniDumpReadDumpStream(FMinidumpView, ModuleListStream, pdir, pStream, istreamsize) then
  begin
    //threads
    pmodules := PMINIDUMP_MODULE_LIST(pStream);
    for i := 0 to pmodules.NumberOfModules - 1 do
    begin
      //does not work, uses 8byte offset, must be 4byte, so we do it ourselves:
      //pmodule  := @pmodules.Modules[i];
      pmodule  := PMINIDUMP_MODULE( NativeUInt(pmodules) + SizeOf(ULONG32) + (i * SizeOf(MINIDUMP_MODULE)) );
      Assert( SizeOf(MINIDUMP_MODULE) = 108 ); //must be packed record, or other compiler record alignment setting?

      module   := TMiniDumpModule.Create;
      FModules.Add(module);

      module.BaseOfImage      := pmodule.BaseOfImage;
      module.SizeOfImage      := pmodule.SizeOfImage;
      module.CheckSum         := pmodule.CheckSum;
      module.TimeDateStamp    := pmodule.TimeDateStamp;
      module.Time             := IncSecond(EncodeDateTime(1970, 1, 1, 0, 0, 0, 0), pmodule.TimeDateStamp);
      module.VersionInfo      := pmodule.VersionInfo;
      module.FileName         := RVAToString(pmodule.ModuleNameRva);
    end;
  end;
end;

function TMinidumpReader.GetProcessInfo: PMINIDUMP_MISC_INFO;
var
  pdir: PMINIDUMP_DIRECTORY;
  pStream: pointer;
  istreamsize: Cardinal;
begin
  Result := FProcessInfo;
  if FProcessInfo <> nil then Exit;
  if FMinidumpView = nil then Exit;

  if MiniDumpReadDumpStream(FMinidumpView, MiscInfoStream, pdir, pStream, istreamsize) then
  begin
    FProcessInfo := PMINIDUMP_MISC_INFO(pStream);
    Result       := FProcessInfo;
  end;
end;

function TMinidumpReader.GetServicepack: string;
begin
  if SystemInfo = nil then Exit;
  Result := '<none>';

  if SystemInfo.CSDVersionRva > 0 then
    Result := RVAToString(SystemInfo.CSDVersionRva);
end;

function TMinidumpReader.GetSystemInfo: PMINIDUMP_SYSTEM_INFO;
var
  pdir: PMINIDUMP_DIRECTORY;
  pStream: pointer;
  istreamsize: Cardinal;
begin
  Result := FSystemInfo;
  if FSystemInfo <> nil then Exit;
  if FMinidumpView = nil then Exit;

  if MiniDumpReadDumpStream(FMinidumpView, SystemInfoStream, pdir, pStream, istreamsize) then
  begin
    FSystemInfo := PMINIDUMP_SYSTEM_INFO(pStream);
    Result      := FSystemInfo;
  end;
end;

function TMinidumpReader.GetThreads: TMiniDumpThreadList;
var
  i: integer;
  mdthread: TMiniDumpThread;

  pdir: PMINIDUMP_DIRECTORY;
  pthreads: PMINIDUMP_THREAD_LIST;
  pthread : PMINIDUMP_THREAD;
  pthreadsinfo: PMINIDUMP_THREAD_INFO_LIST;
  pthreadinfo: PMINIDUMP_THREAD_INFO;
  pStream: pointer;
  istreamsize: Cardinal;
begin
  Result := FThreads;
  if FThreads <> nil then Exit;
  if FMinidumpView = nil then Exit;
  FThreads := TMiniDumpThreadList.Create(True{owns objects});
  Result   := FThreads;

  if MiniDumpReadDumpStream(FMinidumpView, ThreadListStream, pdir, pStream, istreamsize) then
  begin

    //threads
    pthreads := PMINIDUMP_THREAD_LIST(pStream);
    for i := 0 to pthreads.NumberOfThreads - 1 do
    begin
      //thread := Threads.Threads[i]  does not work, uses 8byte offset, must be 4byte, so we do it ourselves:
      pthread  := PMINIDUMP_THREAD( NativeUInt(pthreads) + SizeOf(ULONG32) + (i * SizeOf(MINIDUMP_THREAD)) );

      mdthread := TMiniDumpThread.Create;
      FThreads.Add(mdthread);

      mdthread.ThreadId       := pthread.ThreadId;
      mdthread.SuspendCount   := pthread.SuspendCount;
      mdthread.PriorityClass  := pthread.PriorityClass;
      mdthread.Priority       := pthread.Priority;
      mdthread.Teb            := pthread.Teb;

      mdthread.Stack := TOfflineStackInfoList.Create(FMinidumpView, pthread);
      //stack.TraceStackRaw2;
      //stack.AddToStrings(memo1.Lines, True, True, True, True);

      mdthread.ThreadContext     := PCONTEXT( NativeUInt(FMinidumpView) + pthread.ThreadContext.Rva );

//      mdthread.Stack.StackData   := Pointer(NativeUInt(FMinidumpView) + pthread.Stack.Memory.Rva);
//      mdthread.Stack.BaseOfStack := mdthread.ThreadContext.Ebp;                           //base: e.g. $123456
//      mdthread.Stack.TopOfStack  := mdthread.Stack.BaseOfStack + pthread.Stack.Memory.DataSize;
    end;

    //extra thread info
    if MiniDumpReadDumpStream(FMinidumpView, ThreadInfoListStream, pdir, pStream, istreamsize) then
    begin
      pthreadsinfo := PMINIDUMP_THREAD_INFO_LIST(pStream);

      for i := 0 to pthreadsinfo.NumberOfEntries - 1 do
      begin
        pthreadinfo := @pthreadsinfo.Threads[i];
        mdthread    := FThreads.GetThreadByID(pthreadinfo.ThreadId);
        if mdthread <> nil then
          mdthread.ThreadInfo := pthreadinfo;
      end;
    end;
  end;
end;

function TMinidumpReader.RVAToString(aRVA: RVA): string;
var
  pstring: PMINIDUMP_STRING;
begin
  if FMinidumpView = nil then Exit;

  if aRVA > 0 then
  begin
    pstring  := PMINIDUMP_STRING( NativeUInt(FMinidumpView) + aRVA );
    if pstring.Length > 0 then
      Result := PWCHAR( NativeUInt(pstring) + SizeOf(ULONG32) );
  end;
end;

procedure TMinidumpReader.SetDontCheckValidCallSite(const Value: Boolean);
begin
  TOfflineStackInfoList.DontCheckValidCallSite := Value;
end;

procedure TMinidumpReader.AddThreadStackToStrings(
                                  const aOriginalExe: TFileName;
                                  aThread: TMiniDumpThread; aStrings: TStrings;
                                  aIncludeModuleName: Boolean = False; aIncludeAddressOffset: Boolean = False;
                                  aIncludeStartProcLineOffset: Boolean = False; aIncludeVAddress: Boolean = False);
begin
  if FOfflineProcessDebugInfoList = nil then
    FOfflineProcessDebugInfoList := TOfflineProcessDebugInfoList.Create(Self, aOriginalExe);
  
  Assert(aThread.Stack is TOfflineStackInfoList);
  (aThread.Stack as TOfflineStackInfoList).OffLineDebugInfo := FOfflineProcessDebugInfoList as TOfflineProcessDebugInfoList; 

  //do raw trace
  if aThread.Stack.Count = 0 then
    (aThread.Stack as TOfflineStackInfoList).TraceStackRaw2;  

  (aThread.Stack as TOfflineStackInfoList).AddToStrings(aStrings, aIncludeModuleName, aIncludeAddressOffset, aIncludeStartProcLineOffset, aIncludeVAddress);
end;

{ TMiniDumpThreadList }

function TMiniDumpThreadList.GetThreadByID(
  aThreadId: NativeUInt): TMiniDumpThread;
var
  t: TMiniDumpThread;
begin
  Result := nil;
  for t in Self do
  begin
    if t.ThreadId = aThreadId then  //found!
      Exit(t)
  end;
end;

end.
