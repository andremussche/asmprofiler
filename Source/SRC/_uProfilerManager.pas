unit _uProfilerManager;

interface

uses
  Classes, Windows, SysUtils, Forms,
  Dialogs, Controls,
  Inifiles, Contnrs,
  GpSync,
  _uProfileClasses, _uProfileTypes;

type
  TProfileThreadSlot = record
    Nr: Integer;
    Time: TDatetime;
    ThreadID: Cardinal;
    ProfileTime: TProfileTimeArray;
    ProfileCount: integer;
    Filename: string;
    PendingSave: boolean;
  end;

  TProfileThreadSlotArray = array of TProfileThreadSlot;

  TProfileRun = class(TCustomSerializable)
  private
    FCPUSpeed: Extended;

    FProfileStartTime,
    FProfileStopTime: TDatetime;
    FSaveTime: TDatetime;

    FThreadNameCount: integer;
    FThreadNames: TThreadNameArray;

    FProfileTimesCount: integer;
    FProfileTimes: TProfileThreadSlotArray;
    FMapFilename: string;
    FCustomDebugInfoFile: string;
    FInternalDebugInfoFile: string;
    FMapInfoFile: string;
    FProfileStartCycle: int64;
    FProfileStopCycle: int64;
    FMapMemOffset: integer;
    FProfileClockType: string;
    FProfileOverhead: integer;
    FFilename: string;
    FFileVersion: Single;
    FLoadedDllsInfoFile: string;
    FLoadedDllsDbgFile: string;
    procedure SetMapFilename(const Value: string);
    procedure SetCustomDebugInfoFile(const Value: string);
    procedure SetInternalDebugInfoFile(const Value: string);
    procedure SetMapInfoFile(const Value: string);
    procedure SetProfileStartCycle(const Value: int64);
    procedure SetProfileStopCycle(const Value: int64);
    procedure SetMapMemOffset(const Value: integer);
    procedure SetProfileClockType(const Value: string);
    procedure SetProfileOverhead(const Value: integer);
    procedure SetFilename(const Value: string);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property Filename: string read FFilename write SetFilename;
    function SaveToFile(const aDirectory: string):string;
    procedure LoadFromFile(const aFile:string);

    procedure SaveProfileTimesToDir(const aDir:string);
    procedure LoadProfileTimesFromDir(const aDir:string);

    procedure ReadThreadNames(Reader: TReader);
    procedure WriteThreadNames(Writer: TWriter);
    procedure ReadSaveRecords(Reader: TReader);
    procedure WriteSaveRecords(Writer: TWriter);
  published
    property FileVersion: Single read FFileVersion;

    property CPUSpeed: Extended read FCPUSpeed write FCPUSpeed;

    property ProfileClockType: string read FProfileClockType write SetProfileClockType;
    property ProfileStartCycle: int64 read FProfileStartCycle write SetProfileStartCycle;
    property ProfileStopCycle: int64 read FProfileStopCycle write SetProfileStopCycle;
    property ProfileStartTime: TDatetime read FProfileStartTime write FProfileStartTime;
    property ProfileStopTime: TDatetime read FProfileStopTime write FProfileStopTime;
    property SaveTime: TDatetime read FSaveTime write FSaveTime;
    property ProfileOverhead: integer read FProfileOverhead write SetProfileOverhead;

    property ThreadNameCount: integer read FThreadNameCount write FThreadNameCount;
    property ProfileTimesCount: integer read FProfileTimesCount write FProfileTimesCount;

    property MapFilename: string read FMapFilename write SetMapFilename;
    property MapInfoFile: string read FMapInfoFile write SetMapInfoFile;
    property MapMemOffset: integer read FMapMemOffset write SetMapMemOffset;

    property InternalDebugInfoFile: string read FInternalDebugInfoFile write SetInternalDebugInfoFile;
    property CustomDebugInfoFile: string read FCustomDebugInfoFile write SetCustomDebugInfoFile;

    property LoadedDllsInfoFile: string read FLoadedDllsInfoFile write FLoadedDllsInfoFile;
    property LoadedDllsDbgFile: string read FLoadedDllsDbgFile write FLoadedDllsDbgFile;
  public
    property ThreadNames: TThreadNameArray read FThreadNames write FThreadNames;
    property ProfileTimes: TProfileThreadSlotArray read FProfileTimes write FProfileTimes;
  end;

  TProfilerManager = class(TObject)
  private
    FCustomDebugInfo: TDebugInfoStorage;
    FInternalDebugInfo: TInternalItemsStorage;
    FMainMapFile: TMapFileLoader;
    //FCustomSelectedItemsCount: Integer;
    FIniFile: TMemInifile;
    //FInternalSelectedItemsCount: Integer;
    FSelectedItemsCount: Integer;
    FSelectItemsChanged: Boolean;
    FSelectItemsRefreshed: Boolean;
    FTotalSavedResults: Integer;
    FBaseDirectory,
    FDirectory: string;
    //FStarted: boolean;
    FCurrentRun: TProfileRun;
    FStarted: boolean;
//    FProfileOverhead: integer;
    FLoadedDllsInfo: TProgramLoadedDllsStorage;
    function GetStarted: boolean;
    function GetStartTime: TDatetime;
    //procedure SetProfileOverhead(const Value: integer);
  protected
    FFlagStarted : TGpFlag;

    FIgnoredProcedures: TStrings;
    FInterceptedProcedures: TStrings;
    function BuildThreadsList(ProcessID: DWORD): TObjectList;
    procedure ResumeAllThreads(const aPreviousThreads: TCardinalArray);
    function SuspendAllThreads: TCardinalArray;

    procedure SaveProfileTimesToFile;
    procedure LoadProfileTimesFromFile(const aProfRunFile:string);
    //
    procedure LoadSelectedProcsFromFile;
    procedure SaveSelectedProcsToFile;
    procedure RefreshSelectedProcs;

    procedure SetDirectory;

    procedure CreateProfileIntercepts(const aArray:PSelectedUnitArray; const aOffSet:Cardinal);
    procedure RemoveProfileIntercepts(var aArray:TSelectedUnitArray; const aOffSet:Cardinal);
  public
    constructor Create;virtual;
    destructor Destroy;override;

    procedure StartProfiling(aAskForConfirmation: boolean = true);
    procedure StopProfiling;

    procedure CreateProfileForm;
    procedure ShowProfileForm;
    procedure DestroyProfileForm;

    procedure SaveAndClear;
    procedure Clear;
    procedure ShowSelectItemsForm;
    procedure ShowResultsForm;

    property CurrentRun: TProfileRun read FCurrentRun;
    property StartTime:TDatetime read GetStartTime;
    property Started: boolean read GetStarted;

    function SelectedItemsCount: integer;
    property CustomDebugInfo: TDebugInfoStorage read FCustomDebugInfo;
    property InternalDebugInfo: TInternalItemsStorage read FInternalDebugInfo;
    property LoadedDllsInfo: TProgramLoadedDllsStorage read FLoadedDllsInfo;
    property MainMapFile: TMapFileLoader read FMainMapFile;
  end;

var
  ProfilerManager: TProfilerManager;

implementation

uses
  {$if CompilerVersion >= 24}  //Delphi XE3
  UITypes,
  {$ifend}
  TlHelp32, JclDebug, JclFileUtils,
  _uAsmProfiler, _frmResults, _frmSelectItems, _frmProfileMain,
  _uProfileTools, JclPeImage, ImageHlp, JwaWinBase, IOUtils;

function TProfilerManager.BuildThreadsList(ProcessID: DWORD): TObjectList;
var
  SnapProcHandle: THandle;
  ThreadEntry: TThreadEntry32;
  Next: Boolean;
  ti: TThreadInfo;
begin
  Result := TObjectList.Create;
  with Result do
  try
      //BeginUpdate;
    Clear;
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if SnapProcHandle <> THandle(-1) then
    begin
      ThreadEntry.dwSize := Sizeof(ThreadEntry);
      Next := Thread32First(SnapProcHandle, ThreadEntry);
      while Next do
      begin
        if ThreadEntry.th32OwnerProcessID = ProcessID then
          with ThreadEntry do
          begin
            ti := TThreadInfo.Create;
            ti.FThreadID := th32ThreadID;
            ti.FName     := '';
            ti.FHandle   := 0;
            ti.FIsMainThread := (Count = 0);
            if ti.FIsMainThread then
  //              ti.FProcessHandle  := openprocess(PROCESS_ALL_ACCESS, false, ProcessID);
              ti.FProcessHandle  := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                                                false, ProcessID);
            Add(ti);

              //mmInfo.Lines.Add( Format('TID: %d', [th32ThreadID]) );
          end;
        Next := Thread32Next(SnapProcHandle, ThreadEntry);
      end;
      CloseHandle(SnapProcHandle);
    end;
  finally
  //    EndUpdate;
  end;
end;

procedure TProfilerManager.Clear;
begin
  ClearAllProfileTimes;
end;

constructor TProfilerManager.Create;
var
  sMap, sDll: string;
  iMappedAddress, iLength: Cardinal;
begin
  sMap   := ChangeFileExt(Application.ExeName,'.map');
  sDll   := '';

  if not FileExists(sMap) then
  if ModuleIsLib or ModuleIsPackage or IsLibrary then
  begin
    //MessageDlg(sMap, mtWarning, [mbOK], 0);
    SetLength(sDll, 1024);
    iLength := GetModuleFileName(HInstance,pchar(sDll),1024);
    if iLength > 0 then
      //sMap := Copy(sMap,0,iLength);
      SetLength(sDll, iLength);
    sMap := ChangeFileExt(sDll, '.map');
  end;

  if FileExists(sMap) then
  begin
    FMainMapFile        := TMapFileLoader.Create(sMap);

    if (sDll <> '') and (ModuleIsLib or ModuleIsPackage or IsLibrary) then
    begin
      //iMappedAddress := DWORD(ImageLoad(pchar(sdll),nil).MappedAddress);
      iMappedAddress := HInstance + $1000;   //Module loaded base address + code segment offset
      
      {$ifndef NOT_VISIBLE}
      if GetCurrentThreadId = MainThreadID then
        MessageDlg(format('Map file not found of: %s' + #10#13 +
                   'Loaded map file of: %s' + #10#13 +
                   'Mapped address old: %p - New: %p',
                   [Application.ExeName, sDll, pointer(FMainMapFile.Offset), pointer(iMappedAddress)]), mtInformation, [mbOK], 0);
      {$endif}

      FMainMapFile.Offset := iMappedAddress;
    end;

    {
    with TJclPeImage.Create(True) do
    begin
      FileName := sDll;
      MessageDlg(format('Mapped address old: %p - New: %p',
                 [pointer(FMainMapFile.Offset), pointer(MappedAddress)]), mtInformation, [mbOK], 0);
      FMainMapFile.Offset := MappedAddress;
      Free;
    end;
    }
  end
  else
  begin
    FMainMapFile        := TMapFileLoader.Create('');
  end;

  SetDirectory;
  FIniFile := TMemIniFile.Create( FBaseDirectory + 'Profile.profini' );

  { TODO 1 -oAM -cTodo : Check for filedate, no greated dff than 1 minute? }
  { TODO 1 -oAM -cTodo : Load other debug files (JDBG, TD32, etc) }

  FInternalDebugInfo := TInternalItemsStorage.Create;
  FInternalDebugInfo.HasRelativeAddresses := False;
  FInternalDebugInfo.Offset               := 0;

  FCustomDebugInfo := TDebugInfoStorage.Create;
  FCustomDebugInfo.HasRelativeAddresses := False;
  FCustomDebugInfo.Offset               := 0;

  FLoadedDllsInfo := TProgramLoadedDllsStorage.Create;
  FLoadedDllsInfo.HasRelativeAddresses := False;
  FLoadedDllsInfo.Offset               := 0;

  FIgnoredProcedures     := TStringList.Create;
  FInterceptedProcedures := TStringList.Create;

  //load previous selected items from file
  LoadSelectedProcsFromFile;

  FFlagStarted := TGpFlag.Create('Global/AsmProfiler/FlagIsStarted');
  FFlagStarted.ClearFlag;
end;

procedure TProfilerManager.CreateProfileForm;
begin
  {$ifndef NOT_VISIBLE}
  if frmProfileMain = nil then
    frmProfileMain := TfrmProfileMain.Create(Application);
  {$endif}
end;

procedure TProfilerManager.CreateProfileIntercepts(
  const aArray: PSelectedUnitArray; const aOffSet: Cardinal);
var
  i, j: Integer;
  c: Cardinal;
  p: Pointer;
  // TCardinalArray;
  s, sProc, sError:string;
begin
  if aArray = nil then exit;
  //:= SuspendAllThreads;

  for i := low(aArray^) to high(aArray^) do
  begin
    if aArray^[i].SelectedProcsCount > 0 then
    for j := low(aArray^[i].SelectedProcs) to high(aArray^[i].SelectedProcs) do
    begin
      s := '';
      sProc := '';
      if (aArray^[i].SelectedProcs[j].ProcNameRecord <> nil) then
      begin
        c := aArray^[i].SelectedProcs[j].ProcNameRecord.Addr + aOffSet;
        p := Pointer(c);
        sProc := String(aArray^[i].SelectedProcs[j].ProcNameRecord.ProcName);
        s := aArray^[i].UnitName + '.' + sProc;
      end
      else
        p := nil;
      //else if (aArray^[i].SelectedProcs[j].Addr > 0) then
      //  p := Pointer(aArray^[i].SelectedProcs[j].Addr + C_MEMORY_OFFSET);

      //MessageDlg(format('%s = %p',['Windows.AnsiLower',@Windows.AnsiLower]), mtWarning, [mbOK], 0);
                                                
      try
        if not ( //(p = @Windows.GetCurrentThreadId) or
                 //(p = @RDTSC) or
                 //(p = @Windows.InterlockedIncrement) or
                 //(p = @Windows.QueryPerformanceCounter) or
                 (p = nil)
                 //( (length(sProc) > 0) and
                 //  (sProc[1] = '@') )
               )
        then
        begin
          //OutputDebugString(pchar(s));
          if CreateProfileIntercept( p, s, sError) = nil then
          begin
            FIgnoredProcedures.Add( Format('%p - %s (%s)',[p, s,sError]));
            //FIgnoredProcedures.SaveToFile( ExtractFilePath(Application.ExeName) + 'IgnoredProcedures.txt' );
          end
          else
          begin
            FInterceptedProcedures.Add(format('%p - %s',[p, s]));
            //FInterceptedProcedures.SaveToFile( ExtractFilePath(Application.ExeName) + 'InterceptingProcedures.txt' );
          end;
        end
        else
        begin
          FIgnoredProcedures.Add(s);
          //FIgnoredProcedures.SaveToFile( ExtractFilePath(Application.ExeName) + 'IgnoredProcedures.txt' );
        end;

        { TODO 1 -oAM -cTodo :if not succeeded, then show these procs in a list?' }
      except
        on e:exception do
        {$ifndef NOT_VISIBLE}
          MessageDlg(e.Message, mtError, [mbOK], 0);
        {$endif}
      end;
    end;
  end;

  //sumeAllThreads(a);
end;

destructor TProfilerManager.Destroy;
begin
  if Started or GProfileStarted^ then
    StopProfiling;
  RemoveAllProfileIntercepts;

  //remove old dir if nothing saved
  //if FTotalSavedResults = 0 then
  //  DelTreeEx(FMainMapFile.Directory,True,nil);

  FMainMapFile.Free;
  FInternalDebugInfo.Free;
  FCustomDebugInfo.Free;
  FLoadedDllsInfo.Free;

  FIniFile.Free;
  FCurrentRun.Free;
  FIgnoredProcedures.Free;
  FInterceptedProcedures.Free;

  FFlagStarted.Free;
  inherited;
end;

procedure TProfilerManager.DestroyProfileForm;
begin
  if frmProfileMain <> nil then
    frmProfileMain.Release;
end;

function TProfilerManager.GetStarted: boolean;
begin
  //Result := GProfileStarted;
  Result := FStarted;
end;

function TProfilerManager.GetStartTime: TDatetime;
begin
  Result := CurrentRun.FProfileStartTime;
end;

(*
function TProfilerManager.LoadProfileTimesFromFile(
  const aProfRunFile: string): TProfileRunRecordArray;
var
  iniProfRun: TMemInifile;
  sDir, sFile: string;
  strm: TMemoryStream;
  fs: TFileStream;
  i: Integer;
  iProfileFreq: Int64;
  iNrOfResults, iNr: Integer;
  dTime: TDatetime;
  sDescription: string;
  
  function __ReadString:string;
  var iStringLength: integer;
  begin
    strm.Read(iStringLength,Sizeof(iStringLength));
    SetLength(Result, iStringLength);
    if iStringLength > 0 then
      strm.Read(Result[1], iStringLength)
    else
      Result := '';
  end;
  
begin
  sDir := ExtractFilePath(aProfRunFile);
  iniProfRun   := TMemInifile.Create(aProfRunFile);
  sDescription := iniProfRun.ReadString('Run','Description','');
  iNrOfResults := iniProfRun.ReadInteger('Run','NrOfResults',0);
  
  SetLength(Result,iNrOfResults);
  strm  := TMemoryStream.Create;
  
  for i := 0 to iNrOfResults-1 do
  begin
    sFile := iniProfRun.ReadString('Result_' + IntToStr(i),'File','');
    if sFile <> '' then
      sFile := sDir + sFile
    else
      Continue;
  
    //iThreadID := iniProfRun.ReadInteger('Result_' + IntToStr(i), 'ThreadID', 0);
  
    fs := TFileStream.Create(sFile,fmOpenRead or fmShareDenyNone);
    fs.Position   := 0;
    strm.Clear;
    strm.CopyFrom(fs, fs.Size);
    strm.Position := 0;
    fs.Free;
  
    //nr
    strm.Read(iNr, Sizeof(iNr) );
    //save time
    strm.Read(dTime, Sizeof(dTime) );
    //profile start time
    strm.Read(Result[i].Time, Sizeof(Result[i].Time) );
    //thread id
    strm.Read(Result[i].ThreadID, Sizeof(Result[i].ThreadID) );
    //cpu speed
    strm.Read(iProfileFreq, Sizeof(iProfileFreq));
  
    //custom text
    __ReadString;
  
    //iResultCount :=
    Load_TProfileTimeArray_FromStream(GProfileTimes[i].ProfileTimeArray, strm);
  end;
  
  strm.Free;
end;
*)

procedure TProfilerManager.LoadProfileTimesFromFile(
  const aProfRunFile: string);
begin
  if CurrentRun <> nil then
    FreeAndNil(FCurrentRun);

  FCurrentRun := TProfileRun.Create;
  CurrentRun.LoadFromFile(aProfRunFile);
end;

procedure TProfilerManager.LoadSelectedProcsFromFile;
var
  sFile: string;
begin
  RemoveAllProfileIntercepts;
  FSelectedItemsCount  := 0;
  //lblItemCount.Caption := IntToStr(FSelectedItemsCount);

  sFile := FBaseDirectory+ 'SelectedProcs.ini';
  FMainMapFile.LoadSelectionFromFile(sFile);
  sFile := FBaseDirectory + 'InternalSelectedProcs.ini';
  FInternalDebugInfo.LoadSelectionFromFile(sFile);
  sFile := FBaseDirectory + 'CustomSelectedProcs.ini';
  FCustomDebugInfo.LoadSelectionFromFile(sFile);
  sFile := FBaseDirectory + 'LoadedDllsSelectedProcs.ini';
  FLoadedDllsInfo.LoadSelectionFromFile(sFile);

  //FSelectedItemsCount          := FMainMapFile.SelectedUnitProceduresCount;
  //FInternalSelectedItemsCount  := FInternalDebugInfo.SelectedUnitProceduresCount;
  //FCustomSelectedItemsCount    := FCustomDebugInfo.SelectedUnitProceduresCount;
  //FLoadedDllsSelectedItemsCount:= FLoadedDllsInfo.SelectedUnitProceduresCount;
  //lblItemCount.Caption := IntToStr(FSelectedItemsCount +
  //                                 FInternalSelectedItemsCount +
  //                                 FCustomSelectedItemsCount);
  //lblItemCount.Caption := IntToStr(FSelectedItemsCount);

  FSelectItemsChanged   := True;
  FSelectItemsRefreshed := False;
end;

procedure TProfilerManager.RefreshSelectedProcs;
begin
  frmSelectItems := TfrmSelectItems.Create(nil);
  try
    frmSelectItems.MapFileLoader      := Self.FMainMapFile;
    frmSelectItems.InternalDebugInfo  := Self.FInternalDebugInfo;
    frmSelectItems.CustomDebugInfo    := Self.CustomDebugInfo;
    frmSelectItems.LoadedDllsInfo     := Self.LoadedDllsInfo;

    frmSelectItems.SaveSelectedItemLists;
    FSelectItemsChanged := True;
    //save selected items
    //FSelectedItemsCount          := FMainMapFile.SelectedUnitProceduresCount;
    //FInternalSelectedItemsCount  := FInternalDebugInfo.SelectedUnitProceduresCount;
    //FCustomSelectedItemsCount    := FCustomDebugInfo.SelectedUnitProceduresCount;
    //FLoadedDllsSelectedItemsCount:= FLoadedDllsInfo.SelectedUnitProceduresCount;

    //save selected items to file
    SaveSelectedProcsToFile;
  finally
    frmSelectItems.Free;
  end;

  FSelectItemsRefreshed := True;
end;

procedure TProfilerManager.RemoveProfileIntercepts(
  var aArray: TSelectedUnitArray; const aOffSet: Cardinal);
var
  i, j: Integer;
  p: Pointer;
begin
  if aArray = nil then exit;

  for i := low(aArray) to high(aArray) do
  begin
    if aArray[i].SelectedProcsCount > 0 then
    for j := low(aArray[i].SelectedProcs) to high(aArray[i].SelectedProcs) do
    begin
      if (aArray[i].SelectedProcs[j].ProcNameRecord <> nil) then
        p := Pointer(aArray[i].SelectedProcs[j].ProcNameRecord.Addr + aOffSet)
      else
        p := nil;
      //else if (aArray[i].SelectedProcs[j].Addr > 0) then
      //  p := Pointer(aArray[i].SelectedProcs[j].Addr + C_MEMORY_OFFSET);

      RemoveProfileIntercept( p );
    end;
    SetLength(aArray[i].SelectedProcs,0);
  end;
  SetLength(aArray,0);
end;

procedure TProfilerManager.ResumeAllThreads(
  const aPreviousThreads: TCardinalArray);
var
//  ol: TObjectList;
  i: Integer;
//  ti: TThreadInfo;
begin
  {
  ol := BuildThreadsList(GetCurrentProcessId);
  for i := 0 to ol.Count-1 do
  begin
    ti := ol.Items[i] as TThreadInfo;
    ResumeThread(ti.FThreadID);
  end;
  ol.Free;
  }

  for i := low(aPreviousThreads) to high(aPreviousThreads) do
  begin
    //ti := ol.Items[i] as TThreadInfo;
    ResumeThread(aPreviousThreads[i]);
    CloseHandle(aPreviousThreads[i]);
  end;
end;

procedure TProfilerManager.SaveAndClear;
begin
  SaveProfileTimesToFile;
  ClearAllProfileTimes;
end;

procedure TProfilerManager.SaveProfileTimesToFile;
var
  i:integer;
  sr: TProfileThreadSlotArray;
  sDir, sFile{, sRename} : string;
begin
  FTotalSavedResults := 0;
  //FInternalDebugInfo.SaveToFile;
  //FCustomDebugInfo.SaveToFile;

  CurrentRun.ProfileOverhead   := GProfileOverhead;
  CurrentRun.ThreadNameCount   := GThreadNameCount;
  CurrentRun.ThreadNames       := GThreadNames;
  CurrentRun.ProfileTimesCount := GProfileTimeArrayCount;
  SetLength( sr, GProfileTimeArrayCount);
  CurrentRun.ProfileTimes := sr;
  with CurrentRun do
  for i := low(ProfileTimes) to GProfileTimeArrayCount - 1 do
  begin
    ProfileTimes[i].Nr          := i + 1;
    ProfileTimes[i].ThreadID    := GProfileTimes[i].ThreadID;
    ProfileTimes[i].Time        := GProfileTimes[i].Time;
    ProfileTimes[i].ProfileTime := GProfileTimes[i].ProfileTimeArray;
    FTotalSavedResults := FTotalSavedResults + Length(GProfileTimes[i].ProfileTimeArray);
  end;

  sDir := Self.FDirectory;
  //FMainMapFile.SaveToFile;
  sFile := FMainMapFile.ExportToDir(sDir);
  CurrentRun.MapFilename  := ExtractFileName(sFile);

  sFile := FMainMapFile.ExportSelectionToDir(sDir);
  CurrentRun.MapInfoFile           := ExtractFileName(sFile);
  CurrentRun.MapMemOffset          := FMainMapFile.Offset;

  sFile := FInternalDebugInfo.ExportSelectionToDir(sDir);
  CurrentRun.InternalDebugInfoFile := ExtractFileName(sFile);

  sFile := FLoadedDllsInfo.ExportSelectionToDir(sDir);
  CurrentRun.LoadedDllsInfoFile := ExtractFileName(sFile);
  sFile := ChangeFileExt(sFile,'.pdbg');
  FLoadedDllsInfo.SaveToFile_Pdbg(sFile);
  CurrentRun.LoadedDllsDbgFile := sFile;

  sFile := FCustomDebugInfo.ExportSelectionToDir(sDir);
  CurrentRun.CustomDebugInfoFile   := ExtractFileName(sFile);

  CurrentRun.SaveProfileTimesToDir( sDir );
  CurrentRun.SaveToFile( sDir );
  //CurrentRun.LoadProfileTimesFromDir( sDir );
end;

(*
procedure TProfilerManager.SaveProfileTimesToFile;
var
  iniProfRun: TMemInifile;
  sDir, sFile: string;
  i: Integer;
  strm: TMemoryStream;
  fs: TFileStream;
  iResultCount: Integer;
//  iText: Integer;
begin

  sDir  := FormatDateTime('hhnnsszzz',CurrentRun.FProfileStartTime) + '\';
  strm  := TMemoryStream.Create;
  iniProfRun := TMemInifile.Create(FMainMapFile.Directory + '\' +
                                   FormatDateTime('hhnnsszzz',
                                                  CurrentRun.ProfileStartTime) + '.profrun');

  {
  if (edtSessionName.Text = '<enter name>') then
    edtSessionName.Text := '';
  if (edtSessionName.Text = '') then
    edtSessionName.Text := Format('Count: %d, Startime: %s, Savetime: %s', [GProfileTimeArrayCount,
      DateTimeToStr(FStartTime),DateTimeToStr(now)]);
  }
  //iniProfRun.WriteString('Run','Description',edtSessionName.Text);
  //iniProfRun.WriteInteger('Run','NrOfResults',GProfileTimeArrayCount);

  {save thread names}
  for i := 0 to GThreadNameCount-1 do
  begin
    if GThreadNames[i].Name <> '' then
      iniProfRun.WriteString('ThreadNames',IntToStr(GThreadNames[i].ThreadID),GThreadNames[i].Name);
  end;
  //save default main name if not specified already
  iniProfRun.WriteString('ThreadNames',IntToStr(MainThreadID),'Main thread (application)');

  for i := 0 to GProfileTimeArrayCount-1 do
  begin
    strm.Clear;
    strm.SetSize(10 * 1024 * 1024);      //default 10mb size
    sFile := sDir + Format('Nr%d_TID%d_%s_%s', [i+1, GProfileTimes[i].ThreadID,
                    FormatDateTime('hhnnsszzz',now), '.bin']);

    iniProfRun.WriteString('Result_' + IntToStr(i),'File', sFile);
    iniProfRun.WriteInteger('Result_' + IntToStr(i), 'ThreadID', GProfileTimes[i].ThreadID);

    //nr
    strm.Write(i, Sizeof(i) );
    //current time
    strm.Write(now, Sizeof(now) );
    //profile start time
    strm.Write(GProfileTimes[i].Time, Sizeof(GProfileTimes[i].Time) );
    //thread id
    strm.Write(GProfileTimes[i].ThreadID, Sizeof(GProfileTimes[i].ThreadID) );
    //cpu speed
    strm.Write(GCPUSpeed, Sizeof(GCPUSpeed));

    //custom text
    {
    iText := Length(edtSessionName.Text);
    strm.Write(iText, Sizeof(iText) );
    if iText > 0 then
      strm.Write(edtSessionName.Text[1], iText);
    }
    iResultCount := Save_TProfileTimeArray_ToStream(GProfileTimes[i].ProfileTimeArray, strm);
    strm.SetSize(strm.position);      //resize back to actual size

    {only save if we have results}
    if iResultCount > 0 then
    begin
      ForceDirectories(FMainMapFile.Directory + '\' + sDir);
      fs := TFileStream.Create(FMainMapFile.Directory + '\' + sFile, fmCreate or fmShareExclusive);
      strm.Position := 0;
      fs.CopyFrom(strm, strm.Size);
      fs.Free;

      inc(FTotalSavedResults);
    end;
  end;

  //if iTotalResultCount = 0 then
  //  DeleteDirectory(sDir,False);

  iniProfRun.Free;
  strm.Free;
end;
  *)

procedure TProfilerManager.SaveSelectedProcsToFile;
var
  sFile: string;
begin
  sFile := FBaseDirectory + 'SelectedProcs.ini';
  FMainMapFile.SaveSelectionToFile(sFile);
  sFile := FBaseDirectory + 'InternalSelectedProcs.ini';
  FInternalDebugInfo.SaveSelectionToFile(sFile);
  sFile := FBaseDirectory + 'CustomSelectedProcs.ini';
  FCustomDebugInfo.SaveSelectionToFile(sFile);
  sFile := FBaseDirectory + 'LoadedDllsSelectedProcs.ini';
  FLoadedDllsInfo.SaveSelectionToFile(sFile);
end;

function TProfilerManager.SelectedItemsCount: integer;
begin
  Result := (FMainMapFile.SelectedUnitProceduresCount) +
            (FInternalDebugInfo.SelectedUnitProceduresCount) +
            (FLoadedDllsInfo.SelectedUnitProceduresCount) +
            (FCustomDebugInfo.SelectedUnitProceduresCount);
end;

procedure TProfilerManager.SetDirectory;
var
  sFile: string;
  //tMapTime: TFileTime;
  //dMapTime: TDatetime;

  function __FiletimeToDatetime(const aFiletime:TFileTime):TDatetime;
  var st: TSystemTime;
  begin
    FileTimeToSystemTime(aFiletime,st);
    Result := SystemTimeToDateTime(st);
  end;

begin
//  if (FMainMapFile = nil) or (FMainMapFile.MapFilename = '') or
//     not FileExists(FMainMapFile.MapFilename)
//  then
    sfile := Application.ExeName;
//  else
//    sFile := FMainMapFile.MapFilename;

  //tMapTime   := GetFileLastWrite(Application.ExeName);
  //tMapTime   := GetFileLastWrite(sFile);
  ///dMapTime   := __FiletimeToDatetime(tMapTime);

  FBaseDirectory := ExtractFilePath(sFile);
  if TFileAttribute.faReadOnly in TPath.GetAttributes(FBaseDirectory) then
    FBaseDirectory := ExtractFilePath( GetModulePath(HInstance) );
  if not TDirectory.Exists(FBaseDirectory) or
     (TFileAttribute.faReadOnly in TPath.GetAttributes(FBaseDirectory))
  then
   FBaseDirectory := TPath.GetTempPath;

  FBaseDirectory := FBaseDirectory + 'AsmProfiler\';
  FDirectory     := FBaseDirectory;
  if not ForceDirectories(FBaseDirectory) then
  begin
    FBaseDirectory := ExtractFilePath( GetModulePath(HInstance) );
    FBaseDirectory := FBaseDirectory + 'AsmProfiler\';
    if not ForceDirectories(FBaseDirectory) then
    begin
      FBaseDirectory := TPath.GetTempPath;
      FBaseDirectory := FBaseDirectory + 'AsmProfiler\';
      if not ForceDirectories(FBaseDirectory) then
        MessageDlg('Could not create a directory!', mtError, [mbOK], 0)
      else
        MessageDlg('Temp direcotory created: '#13 + FBaseDirectory, mtWarning, [mbOK], 0);
    end;
  end;

  if ModuleIsLib or ModuleIsPackage or IsLibrary then
    FDirectory := FDirectory +
                  ExtractFileName(Application.ExeName) + '.';

  FDirectory   := FDirectory +
                  FormatDateTime('dd-mm-yyyy_hh-nn-ss', now) + '\';
end;

//procedure TProfilerManager.SetProfileOverhead(const Value: integer);
//begin
//  FProfileOverhead := Value;
//end;

procedure TProfilerManager.ShowProfileForm;
begin
{$ifndef NOT_VISIBLE}
  CreateProfileForm;
  frmProfileMain.Show;
{$endif}
end;

procedure TProfilerManager.ShowResultsForm;
begin
  frmResults := TfrmResults.Create(nil);
   try
     frmResults.Profilerun := Self.CurrentRun;

     {
    property ProfileStartTime: TDatetime read FProfileStartTime write FProfileStartTime;
    property ProfileStopTime: TDatetime read FProfileStopTime write FProfileStopTime;
    property SaveTime: TDatetime read FSaveTime write FSaveTime;

     frmResults.MapFileLoader     := Self.FMainMapFile;
     frmResults.InternalDebugInfo := Self.FInternalDebugInfo;
     frmResults.CustomDebugInfo   := Self.FCustomDebugInfo;
     frmResults.DumpProfileResults;
    }

     if frmResults.ShowModal = mrOk then
     begin
       //save selected items
       //FSelectedUnitArray := frmSelectItems.SelectedItemsList;
       //save selected items to file
     end;
   finally
     frmResults.Free;
   end;
end;

procedure TProfilerManager.ShowSelectItemsForm;
begin
  frmSelectItems := TfrmSelectItems.Create(nil);
  try
    frmSelectItems.MapFileLoader      := Self.FMainMapFile;
    frmSelectItems.InternalDebugInfo  := Self.FInternalDebugInfo;
    frmSelectItems.CustomDebugInfo    := Self.CustomDebugInfo;
    frmSelectItems.LoadedDllsInfo     := Self.LoadedDllsInfo;

    if frmSelectItems.ShowModal = mrOk then
    begin
      FSelectItemsChanged := True;

      //save selected items
      //FSelectedItemsCount          := FMainMapFile.SelectedUnitProceduresCount;
      //FInternalSelectedItemsCount  := FInternalDebugInfo.SelectedUnitProceduresCount;
      //FCustomSelectedItemsCount    := FCustomDebugInfo.SelectedUnitProceduresCount;
      //FLoadedDllsSelectedItemsCount:= FLoadedDllsInfo.SelectedUnitProceduresCount;

      //lblItemCount.Caption := IntToStr(FSelectedItemsCount +
      //                                 FInternalSelectedItemsCount +
      //                                 FCustomSelectedItemsCount);

      //save selected items to file
      SaveSelectedProcsToFile;
    end;
  finally
    frmSelectItems.Free;
  end;
end;

procedure TProfilerManager.StartProfiling(aAskForConfirmation: boolean = true);
var a: TCardinalArray;
    //h: thandle;
begin
  FFlagStarted.SetFlag;
  
  FreeAndNil(FCurrentRun);
  if FCurrentRun = nil then
    FCurrentRun := TProfileRun.Create;

  ClearAllProfileTimes;

{$ifndef NOT_VISIBLE}
  if not FSelectItemsRefreshed then
    RefreshSelectedProcs;
{$endif}

  if FSelectItemsChanged then
  begin
    a := SuspendAllThreads;

    try
      RemoveAllProfileIntercepts;
    except
      on e:exception do
      {$ifndef NOT_VISIBLE}
        MessageDlg(e.Message, mtError, [mbOK], 0);
      {$endif}
    end;

    FIgnoredProcedures.Clear;
    FIgnoredProcedures.BeginUpdate;
    FIgnoredProcedures.Capacity := 1024;
    FInterceptedProcedures.Clear;
    FInterceptedProcedures.BeginUpdate;
    FInterceptedProcedures.Capacity := 1024;

    try
      CreateProfileIntercepts(FMainMapFile.SelectedUnitProcedures, FMainMapFile.Offset);
      CreateProfileIntercepts(FInternalDebugInfo.SelectedUnitProcedures, FInternalDebugInfo.Offset);
      CreateProfileIntercepts(FCustomDebugInfo.SelectedUnitProcedures, FCustomDebugInfo.Offset);
      CreateProfileIntercepts(FLoadedDllsInfo.SelectedUnitProcedures, FLoadedDllsInfo.Offset);
      FSelectItemsChanged := False;
    except
      on e:exception do
      {$ifndef NOT_VISIBLE}
        MessageDlg(e.Message, mtError, [mbOK], 0);
      {$endif}
    end;

    FInterceptedProcedures.EndUpdate;
    FInterceptedProcedures.SaveToFile( FBaseDirectory + 'InterceptingProcedures.txt' );
    FIgnoredProcedures.EndUpdate;
    FIgnoredProcedures.SaveToFile( FBaseDirectory + 'IgnoredProcedures.txt' );
    ResumeAllThreads(a);
  end;
  {$ifndef NOT_VISIBLE}
  if aAskForConfirmation then
    if MessageDlg('Start now?', mtConfirmation, [mbOK, mbAbort], 0) <> mrOk then exit;
  {$endif}

  FStarted         := True;
  GProfileStarted^ := True;

  if frmProfileMain <> nil then
    frmProfileMain.UpdateEnabledState;

  CurrentRun.ProfileStartTime  := now;
  CurrentRun.ProfileStartCycle := ClockTicks;
  CurrentRun.ProfileClockType  := ClockType;
end;

procedure TProfilerManager.StopProfiling;
begin
  FStarted         := False;
  GProfileStarted^ := False;
  CurrentRun.FProfileStopCycle  := ClockTicks;
  CurrentRun.FProfileStopTime   := now;

  FFlagStarted.ClearFlag;
  SaveProfileTimesToFile;

  //RemoveAllProfileIntercepts;

  if frmProfileMain <> nil then
    frmProfileMain.UpdateEnabledState;
end;

function TProfilerManager.SuspendAllThreads: TCardinalArray;
var
  ol: TObjectList;
  i: Integer;
  ti: TThreadInfo;
  h: thandle;

const THREAD_SUSPEND_RESUME    = $0002;
begin
  ol := BuildThreadsList(GetCurrentProcessId);
  SetLength(Result,ol.Count);
  for i := 0 to ol.Count-1 do
  begin
    ti := ol.Items[i] as TThreadInfo;
    if ti.FThreadID <> GetCurrentThreadId then
    begin
      h := OpenThread(THREAD_SUSPEND_RESUME, False, ti.FThreadID);
      //SuspendThread(ti.FThreadID);
      //Result[i] := ti.FThreadID;
      SuspendThread(h);
      Result[i] := h;
    end;
  end;
  ol.Free;
end;

{ TProfileRun }

constructor TProfileRun.Create;
begin
  //Create(nil);
  CPUSpeed := GCPUSpeed;
  FFileVersion := 1.0;
end;

procedure TProfileRun.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ThreadNames',ReadThreadNames,WriteThreadNames,True);
  Filer.DefineProperty('SaveRecords',ReadSaveRecords,WriteSaveRecords,True);
end;


destructor TProfileRun.Destroy;
var i:integer;
begin
  ProfileOverhead   := 0;
  ThreadNameCount   := 0;
  //ThreadNames       := nil;

  for i := low(ProfileTimes) to ProfileTimesCount - 1 do
  begin
    ProfileTimes[i].ProfileTime := nil;
  end;
  SetLength(FProfileTimes, 0);
  ProfileTimesCount := 0;
end;

procedure TProfileRun.LoadFromFile(const aFile:string);
var
  fs: TFileStream;
  fversion: Single;
begin
  if not FileExists(aFile) then exit;

  fs := TFileStream.Create(aFile, fmOpenRead);
  try
    fs.position := 0;
    fs.Read(fversion, sizeof(fversion));
    if fversion <> Self.FileVersion then
    begin
      MessageDlg(Format('Incompatible fileversion!'+#13#10+
                        'File has version %1.2f, supported version is %1.2f',
                 [fversion, Self.FileVersion]),
                 mtError, [mbOK], 0);
      Abort;
    end;
    if fs.position = 0 then
    begin
      {$ifndef NOT_VISIBLE}
      Showmessage('empty file!');
      {$endif}
      Exit;
    end;

    LoadObjectFromStream(fs, self);
    Filename := aFile;
  finally
    fs.free;
    if self.CPUSpeed = 0 then
      self.CPUSpeed := 1;
  end;
end;

//    property SaveRecords: TProfileThreadSlotArray read FSaveRecords write FSaveRecords;
{
  TProfileThreadSlotArray = array of TProfileThreadSlot;

  TProfileThreadSlot = record
    Nr: Integer;
    Time: TDatetime;
    ThreadID: Cardinal;
    ProfileTime: TProfileTimeArray;
  end;

  TProfileTimeArray = array of TProfileTimeRecord;

  TProfileTimeRecord = record
    ProfileType: TProfileType;
    //ThreadID: Cardinal;
    Address: Pointer;
    Time: Int64;
  end;

  TProfileType = (ptNone, ptEnter, ptLeave);
}

procedure TProfileRun.LoadProfileTimesFromDir(const aDir: string);
var
  i, iRead:integer;
  sFile:string;
  hFile:THandle;
begin
  for i := low(FProfileTimes) to high(FProfileTimes) do
  begin
    //find last real result
    with FProfileTimes[i] do
    begin
      SetLength(ProfileTime, ProfileCount);

      sFile := FProfileTimes[i].Filename;
      sFile := aDir + '\' + sFile;

      hFile := FileOpen(sFile, fmOpenRead or fmShareDenyNone);
      if hFile > 0 then
      begin
        //fast save: dump whole array at once (no cpu intensive loops)
        iRead := FileRead(hFile, FProfileTimes[i].ProfileTime[0],
                          sizeof(TProfileTimeRecord)*ProfileCount );
        if iRead < ProfileCount then
          MessageDlg('not enough read?', mtError, [mbOK], 0);
      end;
      FileClose(hFile);
    end;
  end;
end;

procedure TProfileRun.ReadSaveRecords(Reader: TReader);
var
  iCount, //iProfileTimeCount,
  i:integer;
begin
  with Reader do
  begin
    ReadListBegin;

    iCount := ReadInteger;
    SetLength(FProfileTimes,iCount);

    for i := low(FProfileTimes) to iCount-1 do
    begin
      FProfileTimes[i].Nr       := ReadInteger;
      FProfileTimes[i].Time     := ReadDate;
      FProfileTimes[i].ThreadID := ReadInteger;
      FProfileTimes[i].Filename := ReadString;
      FProfileTimes[i].ProfileCount := ReadInteger;

      SetLength(FProfileTimes[i].ProfileTime,FProfileTimes[i].ProfileCount);

      {
      with FProfileTimes[i] do
      for j := 0 to iProfileTimeCount - 1 do
      begin
        ProfileTime[j].ProfileType := TProfileType(ReadInteger);
        ProfileTime[j].Address     := pointer(ReadInteger);
        ProfileTime[j].Time        := ReadInt64;
      end;
      }

      {
      //read whole array at once, much faster then read one by one
      with FProfileTimes[i] do
      begin
        Read( FProfileTimes[i].ProfileTime[0], sizeof(TProfileTimeRecord)*iProfileTimeCount);
      end;
      }
      
      {
      TProfileTimeRecord = record
        ProfileType: TProfileType;
        //ThreadID: Cardinal;
        Address: Pointer;
        Time: Int64;
      end;
      }
    end;

    ReadListEnd;
  end;
end;

//    property ThreadNames: TThreadNameArray read FThreadNames write FThreadNames;
{
  TThreadNameArray = array[0..C_MAX_THREADS] of TThreadName;

  TThreadName = record
    ThreadID: Cardinal;
    Name: string;
  end;
  }

procedure TProfileRun.ReadThreadNames(Reader: TReader);
var
  i:integer;
begin
  with Reader do
  begin
    ReadListBegin;

    for i := low(FThreadNames) to high(FThreadNames) do
    begin
      FThreadNames[i].ThreadID := ReadInteger;
      FThreadNames[i].Name     := AnsiString(ReadString);
    end;

    ReadListEnd;
  end;
end;

procedure TProfileRun.SaveProfileTimesToDir(const aDir: string);
var
  iProfileTimeCount,
  i,j, iWritten:integer;
  sFile:string;
  hFile:THandle;
begin
  for i := low(FProfileTimes) to high(FProfileTimes) do
  begin
    iProfileTimeCount := Length(FProfileTimes[i].ProfileTime);
    //find last real result
    with FProfileTimes[i] do
    for j := 0 to iProfileTimeCount - 1 do
      if ProfileTime[j].Address = nil then
      begin
        iProfileTimeCount := j+1;
        Break;
      end;
    //skip if no results
    if iProfileTimeCount <= 0 then Continue;

    sFile := Format('Tid%d_Nr%d_T%s_C%d.profiletime',
                    [FProfileTimes[i].ThreadID,
                     FProfileTimes[i].Nr,
                     FormatDateTime('hhnnsszzz',FProfileTimes[i].Time),
                     iProfileTimeCount]);
    FProfileTimes[i].Filename := sFile;
    sFile := aDir + '\' + sFile;

    {
    TProfileTimeRecord = record
      ProfileType: TProfileType;
      //ThreadID: Cardinal;
      Address: Pointer;
      Time: Int64;
    end;
    TProfileTimeArray = array of TProfileTimeRecord;
    }

    hFile := FileCreate(sFile);
    if hFile > 0 then
    begin
      //fast save: dump whole array at once (no cpu intensive loops)
      iWritten := FileWrite(hFile, FProfileTimes[i].ProfileTime[0],
                            sizeof(TProfileTimeRecord)*iProfileTimeCount );
      if iWritten < iProfileTimeCount then
        MessageDlg('not enough written?', mtError, [mbOK], 0);
    end;
    FileClose(hFile);
  end;
end;

function TProfileRun.SaveToFile(const aDirectory: string):string;
var
  fs: TFileStream;
  strm: TStream;
begin
  Result := aDirectory + '\' +
            FormatDateTime('hhnnsszzz', ProfileStartTime) + '.profilerun';
  fs   := TFileStream.Create(Result, fmCreate);
  strm := nil;
  try
    Self.SaveTime := now;
    strm := Self.SaveObjectToStream(Self);
    fs.Write(Self.FileVersion, sizeof(FileVersion));
    fs.CopyFrom( strm, strm.Size );
    Filename := Result;
  finally
    fs.free;
    strm.free;
  end;
end;

procedure TProfileRun.SetCustomDebugInfoFile(const Value: string);
begin
  FCustomDebugInfoFile := Value;
end;

procedure TProfileRun.SetFilename(const Value: string);
begin
  FFilename := Value;
end;

procedure TProfileRun.SetInternalDebugInfoFile(const Value: string);
begin
  FInternalDebugInfoFile := Value;
end;

procedure TProfileRun.SetMapFilename(const Value: string);
begin
  FMapFilename := Value;
end;

procedure TProfileRun.SetMapInfoFile(const Value: string);
begin
  FMapInfoFile := Value;
end;

procedure TProfileRun.SetMapMemOffset(const Value: integer);
begin
  FMapMemOffset := Value;
end;

procedure TProfileRun.SetProfileClockType(const Value: string);
begin
  FProfileClockType := Value;
end;

procedure TProfileRun.SetProfileOverhead(const Value: integer);
begin
  FProfileOverhead := Value;
end;

procedure TProfileRun.SetProfileStartCycle(const Value: int64);
begin
  FProfileStartCycle := Value;
end;

procedure TProfileRun.SetProfileStopCycle(const Value: int64);
begin
  FProfileStopCycle := Value;
end;

procedure TProfileRun.WriteSaveRecords(Writer: TWriter);
var
  iCount, iProfileTimeCount,
  i,j:integer;
begin
  with Writer do
  begin
    WriteListBegin;

    iCount := Length(FProfileTimes);
    WriteInteger( iCount );

    for i := low(FProfileTimes) to iCount-1 do
    begin
      WriteInteger(FProfileTimes[i].Nr);
      WriteDate(FProfileTimes[i].Time);
      WriteInteger(FProfileTimes[i].ThreadID);
      WriteString(FProfileTimes[i].Filename);

      iProfileTimeCount := Length(FProfileTimes[i].ProfileTime);
      //find last real result
      with FProfileTimes[i] do
      for j := 0 to iProfileTimeCount - 1 do
        if ProfileTime[j].Address = nil then
        begin
          iProfileTimeCount := j+1;
          Break;
        end;
      FProfileTimes[i].ProfileCount := iProfileTimeCount;
      WriteInteger( iProfileTimeCount );

      {
      TProfileTimeRecord = record
        ProfileType: TProfileType;
        //ThreadID: Cardinal;
        Address: Pointer;
        Time: Int64;
      end;
      TProfileTimeArray = array of TProfileTimeRecord;
      }

      {
      with FProfileTimes[i] do
      for j := 0 to iProfileTimeCount - 1 do
      begin
        WriteInteger( ord(ProfileTime[j].ProfileType) );
        WriteInteger( integer(ProfileTime[j].Address) );
        WriteInteger( ProfileTime[j].Time );
      end;
      }

      //fast save: dump whole array at once (no cpu intensive loops)
      {
      with FProfileTimes[i] do
      begin
        //todo: write to file
        Write( FProfileTimes[i].ProfileTime[0], sizeof(TProfileTimeRecord)*iProfileTimeCount );
      end;
      }

    end;

    WriteListEnd;
  end;
end;

procedure TProfileRun.WriteThreadNames(Writer: TWriter);
var i:integer;
begin
  with Writer do
  begin
    WriteListBegin;

    for i := low(FThreadNames) to high(FThreadNames) do
    begin
      WriteInteger(FThreadNames[i].ThreadID);
      WriteString(String(FThreadNames[i].Name));
    end;

    WriteListEnd;
  end;
end;

//initialization

end.
