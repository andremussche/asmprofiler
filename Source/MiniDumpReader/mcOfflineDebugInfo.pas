unit mcOfflineDebugInfo;

interface

uses
  Classes, SysUtils, Windows,
  mcMiniDumpReader,
  JclDebug, Contnrs;

type
  TOfflineProcessDebugInfoList = class(TJclDebugInfoList)
  private
    FMiniDump: TMinidumpReader;
    FExeName: TFileName;

    type TInternalModule = class
      ProcessModule: HMODULE;
      OurModule    : HMODULE;
      FileName     : String;
    end;
    var FModuleList: TObjectList;

    FIntModuleCache,
    FDebugInfoCache,
    FAddressCache: TList;
  protected
    procedure LoadAllModulesOfProcess;
    function  CreateDebugInfo(aModuleObject: TInternalModule): TJclDebugInfoSource;
    function  GetItemFromProcessModule(aModuleObject: TInternalModule): TJclDebugInfoSource;

    function GetModuleObject(const aProcessModule: HMODULE): TInternalModule;

    property ItemFromProcessModule[aModuleObject: TInternalModule]: TJclDebugInfoSource read GetItemFromProcessModule;
  public
    constructor Create(aMiniDump: TMinidumpReader; const aDebugFile: TFileName);
    procedure   AfterConstruction;override;
    destructor  Destroy;override;

    function FindModuleFromAddr(const aProcessAddr: Pointer): HMODULE;

    function GetOffline2LocalAddres(const aProcessAddr: Pointer): Pointer;
    function GetOfflineLocationInfo(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;
    function GetOfflineLocationInfoStr(const Addr: Pointer; IncludeModuleName,
      IncludeAddressOffset, IncludeStartProcLineOffset,
      IncludeVAddress: Boolean; EmptyIfNotFound: Boolean = False): string;

    property ExeName: TFileName read FExeName write FExeName;
  end;

  TJclDebugInfoDummy = class(TJclDebugInfoSource)
  public
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
  end;

implementation

uses
  JclBase;

procedure TOfflineProcessDebugInfoList.AfterConstruction;
begin
  inherited;
  FModuleList     := TObjectList.Create;
  FAddressCache   := TList.Create;
  FDebugInfoCache := TList.Create;
  FIntModuleCache := TList.Create;
end;

constructor TOfflineProcessDebugInfoList.Create(aMiniDump: TMinidumpReader; const aDebugFile: TFileName);
begin
  FMiniDump      := aMiniDump;

  if FileExists(aDebugFile) then
    FExeName     := aDebugFile
  else if (FMiniDump <> nil) and (FMiniDump.Modules.Count > 0) then
  begin
    FExeName     := FMiniDump.Modules[0].FileName;
  end;

  //add dummy debug info class, in case no other one is found, so no stupid retry everytime
  RegisterDebugInfoSource(TJclDebugInfoDummy);
end;

function TOfflineProcessDebugInfoList.CreateDebugInfo(aModuleObject: TInternalModule): TJclDebugInfoSource;
begin
  Result := nil;
  if aModuleObject = nil then Exit;
  //load debuginfo for loaded module in OUR process space
  Result := inherited CreateDebugInfo(aModuleObject.OurModule);
end;

destructor TOfflineProcessDebugInfoList.Destroy;
begin
  FModuleList.Free;
  FAddressCache.Free;
  FDebugInfoCache.Free;
  FIntModuleCache.Free;
  inherited;
end;

function TOfflineProcessDebugInfoList.FindModuleFromAddr(
  const aProcessAddr: Pointer): HMODULE;
var
  m: TMiniDumpModule;
begin
  Result := 0;
  if (FMiniDump <> nil) and (FMiniDump.Modules <> nil) then
    for m in FMiniDump.Modules do
    begin
      //address within memory range of a module?
      if (NativeUInt(aProcessAddr) >= m.BaseOfImage) and
         (NativeUInt(aProcessAddr) <= (m.BaseOfImage + m.SizeOfImage))
      then
        Exit(m.BaseOfImage);
    end;
end;

function TOfflineProcessDebugInfoList.GetItemFromProcessModule(aModuleObject: TInternalModule): TJclDebugInfoSource;
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

function TOfflineProcessDebugInfoList.GetModuleObject(const aProcessModule: HMODULE): TInternalModule;
var
  im: TInternalModule;
  i: Integer;
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
        im.OurModule := LoadLibraryEx( PChar(im.FileName), 0, DONT_RESOLVE_DLL_REFERENCES); //load module in OUR process space

        //retry without full path
        if im.OurModule = 0 then
          im.OurModule := LoadLibraryEx( PChar(ExtractFileName(im.FileName)), 0, DONT_RESOLVE_DLL_REFERENCES); //load module in OUR process space

        if im.OurModule = 0 then
        begin
          //MessageDlg('Could not load module: ' + cModuleName, mtWarning, [mbOK], 0);
          im.OurModule := THandle(-1);
        end;
      end;
      Result := im;
      Exit;
    end;
  end;
end;

function TOfflineProcessDebugInfoList.GetOffline2LocalAddres(
  const aProcessAddr: Pointer): Pointer;
var
  Item: TJclDebugInfoSource;
  im: TInternalModule;
  iIndex: integer;
  hProcessModule: HMODULE;
  pLocalAddr: Pointer;
  iOffset: Integer;
begin
  iIndex := FAddressCache.IndexOf(aProcessAddr);
  if iIndex >= 0 then
  begin
    Item := FDebugInfoCache.Items[iIndex];
    im   := FIntModuleCache.Items[iIndex];
  end
  else
  begin
    //get process(!) module of process address
    hProcessModule := FindModuleFromAddr(aProcessAddr);
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
    pLocalAddr := Pointer(Cardinal(aProcessAddr) + iOffset);
  end
  else
    pLocalAddr := nil;

  Result := pLocalAddr;
end;

function TOfflineProcessDebugInfoList.GetOfflineLocationInfo(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;
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
    hProcessModule := FindModuleFromAddr(aProcessAddr);
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
    pLocalAddr := Pointer(Cardinal(aProcessAddr) + iOffset);

    //get debug info
    //todo: own optimized version
    Result     := Item.GetLocationInfo(pLocalAddr, Info);

    //reverse offset
    Info.Address := Pointer(Cardinal(Info.Address) - iOffset);
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

function TOfflineProcessDebugInfoList.GetOfflineLocationInfoStr(const Addr: Pointer; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset: Boolean; IncludeVAddress: Boolean; EmptyIfNotFound: Boolean = False): string;
var
  Info, StartProcInfo: TJclLocationInfo;
  OffsetStr, StartProcOffsetStr, FixedProcedureName: string;
  Module : HMODULE;
const
  ModuleCodeOffset = $1000;
begin
  OffsetStr := '';
  if GetOfflineLocationInfo(Addr, Info) then
    with Info do
    begin
      FixedProcedureName := ProcedureName;
      if Pos(UnitName + '.', FixedProcedureName) = 1 then
        FixedProcedureName := Copy(FixedProcedureName, Length(UnitName) + 2, Length(FixedProcedureName) - Length(UnitName) - 1);

      if LineNumber > 0 then
      begin
        if IncludeStartProcLineOffset and
           GetLocationInfo(Pointer(TJclAddr(Info.Address) - Cardinal(Info.OffsetFromProcName)), StartProcInfo) and
           (StartProcInfo.LineNumber > 0)
        then
          StartProcOffsetStr := Format(' + %d', [LineNumber - StartProcInfo.LineNumber])
        else
          StartProcOffsetStr := '';
        if IncludeAddressOffset then
        begin
          if OffsetFromLineNumber >= 0 then
            OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
          else
            OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
        end;
        Result := Format('[%p] %s.%s (Line %u, "%s"%s)%s', [Addr, UnitName, FixedProcedureName, LineNumber,
          SourceName, StartProcOffsetStr, OffsetStr]);
      end
      else
      begin
        if IncludeAddressOffset then
          OffsetStr := Format(' + $%x', [OffsetFromProcName]);
        if UnitName <> '' then
          Result := Format('[%p] %s.%s%s', [Addr, UnitName, FixedProcedureName, OffsetStr])
        else
          Result := Format('[%p] %s%s', [Addr, FixedProcedureName, OffsetStr]);
      end;
    end
  else
  begin
    if EmptyIfNotFound then Exit('');
    Result := Format('[%p]', [Addr]);
    IncludeVAddress := True;
  end;
  if IncludeVAddress or IncludeModuleName then
  begin
    Module := FindModuleFromAddr(Addr);
    if IncludeVAddress then
    begin
      OffsetStr :=  Format('(%p) ', [Pointer(TJclAddr(Addr) - Module - ModuleCodeOffset)]);
      Result := OffsetStr + Result;
    end;
    if IncludeModuleName
       and (Module <> 0)
    then
      System.Insert(Format('{%-12s}', [ExtractFileName(GetModuleObject(Module).FileName)]), Result, 11);
//      System.Insert(Format('{%-12s}', [GetModuleObject(Module).FileName]), Result, 11);
  end;
end;


procedure TOfflineProcessDebugInfoList.LoadAllModulesOfProcess;
var
  im: TInternalModule;
  i: Integer;
begin
//  todo: load from exe instead of minidump
  FModuleList.Clear;

  //load all modules
  for i := 0 to FMiniDump.Modules.Count-1 do
  begin
    im := TInternalModule.Create;
    im.ProcessModule := FMiniDump.Modules[i].BaseOfImage;
    im.FileName      := FMiniDump.Modules[i].FileName;
    im.OurModule     := 0;  //not loaded (yet)

    //try load original exe (first module is application) from specified location
    if i = 0 then
    begin
      if not FileExists(FExeName) then            //original location
        FExeName   := ExtractFilePath(FExeName);  //local location (current dir)
      //im.FileName  := FExeName;  do not overwrite the location of moment of the dump
      im.OurModule := LoadLibraryEx( PChar(FExeName), 0, DONT_RESOLVE_DLL_REFERENCES); //load module in OUR process space
    end;

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


end.
