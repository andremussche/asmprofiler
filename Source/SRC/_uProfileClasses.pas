unit _uProfileClasses;

interface

uses
  Classes, SysUtils, Windows, Forms,
  JclDebug, JclBase,
  _uProfileTypes, jclSysInfo, JclPeImage;

type
  TMapFileLoader = class;
  TDebugInfoStorage = class;
  TInternalItemsStorage = class;

  TThreadInfo = class (TObject)
  public
    FHandle: THandle;
    FIsMainThread: Boolean;
    FName: string;
    FProcessHandle: THandle;
    FThreadID: Cardinal;
  end;
  
  TDebugInfoStorage = class(TObject)
  private
    FHasRelativeAddresses: Boolean;
    FOffset: Cardinal;
    FFilename: string;
    function GetSelectedUnitProcedures: PSelectedUnitArray;
    function GetSelectedUnitProceduresCount: Integer;
    function GetSelectedUnitProceduresText: Tstrings;
    procedure SetHasRelativeAddresses(const Value: Boolean);
    procedure SetOffset(const Value: Cardinal);
    procedure SetSelectedUnitProceduresText(const Value: Tstrings);
    procedure SetFilename(const Value: string);
  protected
    FProcNames: TJclMapProcNameExtArray;
    FProcNamesCnt: Integer;
    FSegments: TJclMapSegmentExtArray;
    FSelectedUnitCount: Integer;
    FSelectedUnitProcedures: TSelectedUnitArray;
    FSelectedUnitProceduresText: Tstringlist;

    procedure UpdateInternalSelection;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearSelection;

    function AddProcedure(const aUnitIndex:integer; const aProcName: string;
            const aProcAddr:cardinal): Integer;
    function AddSegment(const aUnit:string): Integer;
    procedure AddSelectedProcedure(const aUnitIndex, aProcIndex:integer);
    //procedure ClearSelectedProcedures;
    function GetProcByFullAddress(const aProc:pointer): Integer;
    function GetProcByName(const aProc:string; const aSegmentIndex: integer):
            Integer;
    function GetSegmentByFullAddress(const aAddr:Cardinal): Integer;
    function GetSegmentByName(const aUnit:string): Integer;

    procedure LoadFromFile_Pdbg(const aPdbgfile: string);
    procedure SaveToFile_Pdbg(const aDbgFilename: string);

    procedure LoadSelectionFromFile(const aFile:string);
    procedure SaveSelectionToFile(const aFile:string);
    procedure ExportSelectionToFile(const aFile:string);
    function  ExportSelectionToDir(const aDir:string):string;

    property HasRelativeAddresses: Boolean read FHasRelativeAddresses write
            SetHasRelativeAddresses;
    property Offset: Cardinal read FOffset write SetOffset;
    property ProcedureNames: TJclMapProcNameExtArray read FProcNames;
    property SelectedUnitProcedures: PSelectedUnitArray read
            GetSelectedUnitProcedures;
    property SelectedUnitProceduresCount: Integer read
            GetSelectedUnitProceduresCount;
    property SelectedUnitProceduresText: Tstrings read
            GetSelectedUnitProceduresText write SetSelectedUnitProceduresText;
    property UnitNames: TJclMapSegmentExtArray read FSegments;

    property Filename: string read FFilename write SetFilename;
  end;
  
  TInternalItemsStorage = class(TDebugInfoStorage)
  protected
    procedure LoadInternalItems;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TMapFileLoader = class(TDebugInfoStorage)
  private
    FDirectory: string;
    FMapFile: TJclMapParser;
    FTopValidAddr: Integer;
    FMapFilename: string;
    FDbgFilename: string;
  protected
    procedure ClearUnusedUnits;
    procedure JclMapLineNumberUnitItem(Sender: TObject; const UnitName,
            UnitFileName: string);
    procedure JclMapPublicsEvent(Sender: TObject; const Address: TJclMapAddress;
            const Name: string);
    procedure JclMapSegmentEvent(Sender: TObject; const Address: TJclMapAddress;
            Len: Integer; const GroupName, UnitName: string);
  public
    constructor Create(const aMapFile: TFilename);reintroduce;
    destructor Destroy; override;
    function GetProcByAddr(const aProc:pointer): string;
    function GetSegment(const aAddr:Cardinal): Integer;

    //procedure LoadFromFile_Pdbg(const aPdbgfile:string);
    procedure LoadFromFile;
    procedure SaveToFile;
    function ExportToDir(const aDir:string):string;

    property Directory: string read FDirectory;
    property MapFilename: string read FMapFilename;
    property DbgFilename: string read FDbgFilename;
  end;

  TProgramImportsExportsStorage = class (TDebugInfoStorage)
  protected
    procedure LoadProgramImportsExports;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TProgramLoadedDllsStorage = class (TDebugInfoStorage)
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadProgramLoadedDlls;
  end;

  ISerializableObject = interface(IInterface)
    ['{63765481-17AB-46C3-9B26-F29C9BB77CB4}']
    function SaveToString(const aAsIniString: boolean = false): string;
    function LoadFromString(const AObjectString:string; const aIsIniString: boolean = false): TComponent;
  end;

  TCustomSerializable = class(TComponent, ISerializableObject)
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadCustom(Reader:Treader);
    procedure WriteCustom(Writer:Twriter);
  public
    function LoadFromString(const AObjectString:string; const aIsIniString: boolean = false): TComponent;virtual;
    class function LoadObjectFromString(const AObjectString:string; const aInstance: TComponent = nil): TComponent;virtual;
    class function LoadObjectFromStream(const AObjectStream:TStream; const aInstance: TComponent = nil): TComponent;virtual;

    function SaveToString(const aAsIniString: boolean = false): string;virtual;
    class function SaveObjectToString(const aObject: TComponent): string;
    class function SaveObjectToStream(const aObject: TComponent): TMemoryStream;
  end;


implementation

uses
  Math, _uAsmProfiler, JclFileUtils;

{
****************************** TDebugInfoStorage *******************************
}
constructor TDebugInfoStorage.Create;
begin
  SetLength(FSegments,0);
  SetLength(FProcNames,0);
  SetLength(FSelectedUnitProcedures,0);
  
  FSelectedUnitProceduresText := Tstringlist.Create;
  FSelectedUnitProceduresText.Sorted := True;
end;

destructor TDebugInfoStorage.Destroy;
begin
  Clear;
  ClearSelection;
  inherited;
end;

function TDebugInfoStorage.AddProcedure(const aUnitIndex:integer; const 
        aProcName: string; const aProcAddr:cardinal): Integer;
var
  iSelIndex: Integer;
  sNameValue: string;
begin
  if FProcNamesCnt mod 256 = 0 then
    SetLength(FProcNames, FProcNamesCnt + 256);
  FProcNames[FProcNamesCnt].Addr     := aProcAddr;
  FProcNames[FProcNamesCnt].ProcName := Ansistring(aProcName);
  
  Result := -1;
  if (aUnitIndex >= 0) then
  begin
    Result := Length(FSegments[aUnitIndex].Procs);
    SetLength(FSegments[aUnitIndex].Procs, Result + 1);
    FSegments[aUnitIndex].Procs[Result] := FProcNames[FProcNamesCnt];
    Inc(FProcNamesCnt);
  
    with FSegments[aUnitIndex] do
    begin
      if StartAddr = 0 then
        StartAddr := aProcAddr
      else
        StartAddr := min(StartAddr, aProcAddr);
      EndAddr := max(EndAddr,aProcAddr);

      sNameValue := String(UnitName) + '=' + String(aProcName);
      //if previous selected, and later added, then add to selected array
      iSelIndex := FSelectedUnitProceduresText.IndexOf(sNameValue);
      if iSelIndex >= 0 then
        Self.AddSelectedProcedure(aUnitIndex,FProcNamesCnt - 1);

      (*
      iSelIndex := FSelectedUnitProceduresText.IndexOfName(UnitName);
      if iSelIndex >= 0 then
      begin
        for i := iSelIndex to FSelectedUnitProceduresText.Count-1 do
        begin
          if (FSelectedUnitProceduresText.Names[i] = UnitName) then
          begin
            if FSelectedUnitProceduresText.ValueFromIndex[i] = aProcName then
            begin
              //Self.AddSelectedProcedure(aUnitIndex,FProcNamesCnt-1);
              Self.AddSelectedProcedure(aUnitIndex,FProcNamesCnt - 1);
              Break;
            end;
          end
          else
            Break;
        end;
      end;
      *)
    end;
  
  end;
end;

function TDebugInfoStorage.AddSegment(const aUnit:string): Integer;
begin
  Result := Length(FSegments);
  SetLength(FSegments, Result + 1);
  FSegments[Result].UnitName  := Ansistring(aUnit);
  
  //FSegments[Result].StartAddr := Address.Offset;
  //FSegments[Result].EndAddr   := Address.Offset + Len;
  //FTopValidAddr := Max(FTopValidAddr, Address.Offset + Len);
end;

procedure TDebugInfoStorage.AddSelectedProcedure(const aUnitIndex, 
        aProcIndex:integer);
var
  iSelUnit, iSelProc, i, j: Integer;
begin
  iSelUnit := -1;
  if aUnitIndex < 0 then exit;
  if aProcIndex < 0 then exit;
  
  for i := 0 to FSelectedUnitCount - 1 do
  begin
    if FSelectedUnitProcedures[i].UnitSegmentRecord = @Self.UnitNames[aUnitIndex] then
    with FSelectedUnitProcedures[i] do
    begin
      assert( length(ProcedureNames) > aProcIndex);
      assert( length(SelectedProcs) = SelectedProcsCount);

      for j := 0 to SelectedProcsCount-1 do
      begin
        if SelectedProcs[j].ProcNameRecord = @Self.ProcedureNames[aProcIndex] then
        begin
          Exit;
        end;
      end;
      iSelUnit := i;
      Break;
    end;
  end;
  
  if iSelUnit < 0 then
  begin
    SetLength(FSelectedUnitProcedures,FSelectedUnitCount+1);
    iSelUnit := FSelectedUnitCount;
    inc(FSelectedUnitCount);

    FSelectedUnitProcedures[iSelUnit].UnitName          := String(UnitNames[aUnitIndex].UnitName);
    FSelectedUnitProcedures[iSelUnit].UnitSegmentRecord := @UnitNames[aUnitIndex];
    FSelectedUnitProcedures[iSelUnit].TotalProcsCount   := Length(UnitNames[aUnitIndex].Procs);
    with UnitNames[aUnitIndex] do
    for i := high(Procs) to low(Procs) do
    begin
      if Procs[i].ProcName = '' then
        dec(FSelectedUnitProcedures[iSelUnit].TotalProcsCount)
      else
        Break;
    end;
  end;
  
  with FSelectedUnitProcedures[iSelUnit] do
  begin
    SetLength(SelectedProcs,SelectedProcsCount+1);
    iSelProc := SelectedProcsCount;
    inc(SelectedProcsCount);
  
    SelectedProcs[iSelProc].ProcNameRecord := @Self.ProcedureNames[aProcIndex];
    SelectedProcs[iSelProc].ProcName       := Self.ProcedureNames[aProcIndex].ProcName;
  end;
end;

procedure TDebugInfoStorage.Clear;
var
  i: Integer;
begin
  for i := low(FSegments) to high(FSegments) do
  begin
    SetLength( FSegments[i].Procs, 0);
  end;
  SetLength(FSegments,0);
  SetLength(FProcNames,0);
  FProcNamesCnt := 0;
end;

{
procedure TDebugInfoStorage.ClearSelectedProcedures;
begin
  SetLength(FSelectedUnitProcedures,0);
  FSelectedUnitCount := 0;
end;
}

procedure TDebugInfoStorage.ClearSelection;
var
  i: Integer;
begin
  FSelectedUnitProceduresText.Clear;
  for i := low(FSelectedUnitProcedures) to high(FSelectedUnitProcedures) do
  begin
    SetLength(FSelectedUnitProcedures[i].SelectedProcs,0);
  end;
  SetLength(FSelectedUnitProcedures,0);
  FSelectedUnitCount := 0;
end;

function TDebugInfoStorage.GetProcByFullAddress(const aProc:pointer): Integer;
var
  iaddr, i: Cardinal;
begin
  if HasRelativeAddresses then
    iaddr  := Cardinal(aProc) - Offset
  else
    iaddr  := Cardinal(aProc);
  
  Result := -1;
  
  for i := low(FProcNames) to high(FProcNames) do
  begin
    if iaddr = FProcNames[i].Addr then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TDebugInfoStorage.GetProcByName(const aProc:string; const 
        aSegmentIndex: integer): Integer;
var
  i: Integer;
  cAddr: Cardinal;
begin
  Result := -1;
  
  cAddr := 0;
  with FSegments[aSegmentIndex] do
  for i := low(Procs) to high(Procs) do
  begin
    if SameText(String(Procs[i].ProcName), aProc) then
    begin
      cAddr := Procs[i].Addr;
      Break;
    end;
  end;
  
  if cAddr > 0 then
  begin
    for i := low(FProcNames) to high(FProcNames) do
    begin
      if (FProcNames[i].Addr = cAddr) then
      begin
        Result := i;
        Exit;
      end;
    end;
  end
  else
  for i := low(FProcNames) to high(FProcNames) do
  begin
    if SameText(String(FProcNames[i].ProcName), aProc) and
       (FProcNames[i].Addr >= FSegments[aSegmentIndex].StartAddr) and
       (FProcNames[i].Addr <= FSegments[aSegmentIndex].EndAddr)
    then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TDebugInfoStorage.GetSegmentByFullAddress(const aAddr:Cardinal): 
        Integer;
var
  i: Integer;
  iaddr: Cardinal;
begin
  Result := -1;
  if HasRelativeAddresses then
    iaddr  := Cardinal(aAddr) - Offset
  else
    iaddr  := Cardinal(aAddr);
  
  for i := low(FSegments) to high(FSegments) do
  begin
    if (iAddr >= FSegments[i].StartAddr) and
       (iAddr <= FSegments[i].EndAddr) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TDebugInfoStorage.GetSegmentByName(const aUnit:string): Integer;
var
  i: Integer;
begin
  Result := -1;
  
  for i := low(FSegments) to high(FSegments) do
  begin
    if SameText(String(FSegments[i].UnitName), aUnit) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TDebugInfoStorage.GetSelectedUnitProcedures: PSelectedUnitArray;
begin
  Result := @FSelectedUnitProcedures;
end;

function TDebugInfoStorage.GetSelectedUnitProceduresCount: Integer;
var
  i: Integer;
begin
  Result := 0;

  FSelectedUnitCount := length(FSelectedUnitProcedures);
  for i := 0 to self.FSelectedUnitCount-1 do
  begin
    Result := Result + FSelectedUnitProcedures[i].SelectedProcsCount;
  end;
end;

function TDebugInfoStorage.GetSelectedUnitProceduresText: Tstrings;
var
  str: Tstrings;
  i, j: Integer;
begin
  str := TStringList.Create;
  
  for i := 0 to FSelectedUnitCount-1 do
  with FSelectedUnitProcedures[i] do
  begin
    if SelectedProcsCount = TotalProcsCount then
      str.Add(UnitName + '=*')
    else
    for j := 0 to SelectedProcsCount-1 do
    begin
      str.Add(UnitName + '=' + String(SelectedProcs[j].ProcName));
    end;
  end;
  
  Result := Str;
end;

procedure TDebugInfoStorage.LoadSelectionFromFile(const aFile:string);
var
  str: Tstrings;
  sFile:string;
  //k,l:integer;
begin
  str   := TStringList.Create;
  try
    if FileExists(aFile) then
      str.LoadFromFile(aFile);
    Filename := aFile;

    SelectedUnitProceduresText := str;
  finally
    str.Free;
  end;

  sFile := Changefileext(aFile,'.pselected');
  FSelectedUnitProcedures := Load_TSelectedUnitArray_FromFile(sFile);

  UpdateInternalSelection;
end;

procedure TDebugInfoStorage.SaveSelectionToFile(const aFile:string);
begin
  ExportSelectionToFile(aFile);
  Filename := aFile;
end;

procedure TDebugInfoStorage.SetFilename(const Value: string);
begin
  FFilename := Value;
end;

procedure TDebugInfoStorage.SetHasRelativeAddresses(const Value: Boolean);
begin
  FHasRelativeAddresses := Value;
end;

procedure TDebugInfoStorage.SetOffset(const Value: Cardinal);
begin
  FOffset := Value;
end;

procedure TDebugInfoStorage.SetSelectedUnitProceduresText(const Value: 
        Tstrings);
var
  iSegment, iProc, i, j: Integer;
  sUnit, sProc: string;

  function __GetValueFromIndex(aStrings: Tstrings; aIndex: Integer): string;
  begin
    if aIndex >= 0 then
      Result := Copy(aStrings.Strings[aIndex], Length(aStrings.Names[aIndex]) + 2, MaxInt) else
      Result := '';
  end;

begin
  //iUnitCount := 0;
  //iSegment   := 0;
  FSelectedUnitProceduresText.Text := Value.Text;
  
  for i := 0 to Value.Count-1 do
  begin
    sUnit := Value.Names[i];
    {$ifndef COMPILER_7_UP}
      sProc := __GetValueFromIndex(Value,i);
    {$else}
      sProc := Value.ValueFromIndex[i];
    {$endif}  

    iSegment := GetSegmentByName(sUnit);
    if iSegment < 0 then Continue;
  
    {add all procedure of unit}
    if sProc = '*' then
    begin
      for j := low(UnitNames[iSegment].Procs) to
               high(UnitNames[iSegment].Procs)
      do
      begin
        iProc := GetProcByName(String(UnitNames[iSegment].Procs[j].ProcName), iSegment);
        AddSelectedProcedure(iSegment,iProc);
      end;
    end
    else
    begin
      iProc := GetProcByName(sProc, iSegment);
      AddSelectedProcedure(iSegment,iProc);
    end;
  
    //inc(FSelectedUnitCount);
  end;
end;

procedure TDebugInfoStorage.UpdateInternalSelection;
var
  i,j,k,l: integer;
begin
  //ChoiceMainForm=ChoiceMainForm.TfrmChoiceMainForm.btnStartProfilingClick

  for i := low(FSelectedUnitProcedures) to high(FSelectedUnitProcedures) do
  begin
    for j := low(UnitNames) to high(UnitNames) do
    begin
      //search unit name record
      if String(UnitNames[j].UnitName) = FSelectedUnitProcedures[i].UnitName then
      begin
        FSelectedUnitProcedures[i].UnitSegmentRecord := @UnitNames[j];

        //search proc name records
        with FSelectedUnitProcedures[i], UnitNames[j] do
        for k := low(SelectedProcs) to high(SelectedProcs) do
        begin
          for l := low(Procs) to high(Procs) do
          begin
            if Procs[l].ProcName = SelectedProcs[k].ProcName then
              SelectedProcs[k].ProcNameRecord := @Procs[l];
          end;
        end;
      end;
    end;
  end;
end;

procedure TDebugInfoStorage.LoadFromFile_Pdbg(const aPdbgfile: string);
var
  fs: TFileStream;
  strm: TMemoryStream;
  sString: ansistring;
  i, j: Integer;
  iStringLength, iTotalProcCount, iProcCount, iProcs, iUnitCount: Integer;
begin
  fs    := TFileStream.Create(aPdbgfile,fmOpenRead or fmShareDenyNone	);
  fs.Position   := 0;

  strm := TMemoryStream.Create;
  strm.CopyFrom(fs, fs.Size);
  strm.Position := 0;
  fs.Free;

  strm.Read(iUnitCount,Sizeof(iUnitCount));
  SetLength(FSegments,iUnitCount);
  iTotalProcCount := 0;

  for i := 0 to iUnitCount-1 do
  begin
    strm.Read(FSegments[i], sizeof(TJclMapSegmentExt));

    {
    TJclMapSegmentExt = record
      StartAddr: DWORD;
      EndAddr: DWORD;
      UnitName: String;
      UnitFile: String;
      Procs: array of TJclMapProcNameExt;
    end;
    }

    strm.Read(iStringLength,Sizeof(iStringLength));
    SetLength(sString, iStringLength);
    if iStringLength > 0 then
      strm.Read(sString[1], iStringLength)
    else
      sString := '';
    FSegments[i].UnitName := sString;

    strm.Read(iStringLength,Sizeof(iStringLength));
    SetLength(sString, iStringLength);
    if iStringLength > 0 then
      strm.Read(sString[1], iStringLength)
    else
      sString := '';
    FSegments[i].UnitFile := sString;

    strm.Read(iProcCount,Sizeof(iProcCount));
    SetLength(FSegments[i].Procs,iProcCount);
    iTotalProcCount := iTotalProcCount + iProcCount;

    for j := 0 to iProcCount-1 do
    begin
      strm.Read(FSegments[i].Procs[j], sizeof(TJclMapProcNameExt));
      {
      TJclMapProcNameExt = record
        Addr: DWORD;
        ProcName: String;
      end;
      }

      strm.Read(iStringLength,Sizeof(iStringLength));
      SetLength(sString, iStringLength);
      if iStringLength > 0 then
        strm.Read(sString[1], iStringLength)
      else
        sString := '';
      FSegments[i].Procs[j].ProcName := sString;
    end;
  end;

  iProcCount := 0;
  SetLength(FProcNames, iTotalProcCount );
  for i := 0 to iUnitCount-1 do
  begin
    iProcs := Length(FSegments[i].Procs);
    for j := 0 to iProcs-1 do
    begin
      FProcNames[iProcCount].ProcName :=  FSegments[i].Procs[j].ProcName;
      FProcNames[iProcCount].Addr     :=  FSegments[i].Procs[j].Addr;
      inc(iProcCount);
    end;
  end;

  strm.Free;
end;

procedure TDebugInfoStorage.SaveToFile_Pdbg(const aDbgFilename: string);
var
  strm: TMemoryStream;
  fs: TFileStream;
  i, j: Integer;
  iStringLength, iProcCount, iSegmentCount: Integer;
  sUnitName, sUnitFile, sProcName{, sFile}: ansistring;
  aProcs: TJclMapProcNameExtArray;
  //dMapTime: TDatetime;
  //tMapTime: TFileTime;
  
  function __FiletimeToDatetime(const aFiletime:TFileTime):TDatetime;
  var st: TSystemTime;
  begin
    FileTimeToSystemTime(aFiletime,st);
    Result := SystemTimeToDateTime(st);
  end;
  
begin
  strm := TMemoryStream.Create;
  strm.SetSize(1024 * 1024);      //default 1mb size
  try
    iSegmentCount := high(FSegments);
    strm.Write(iSegmentCount, sizeof(iSegmentCount));
  
    for i := 0 to iSegmentCount-1 do
    begin
      aProcs      := FSegments[i].Procs;
      sUnitName   := FSegments[i].UnitName;
      sUnitFile   := FSegments[i].UnitFile;
      FSegments[i].UnitName := '';
      FSegments[i].UnitFile := '';
      FSegments[i].Procs := nil;
      //
      strm.Write(FSegments[i], sizeof(TJclMapSegmentExt));
      //
      FSegments[i].UnitName := sUnitName;
      FSegments[i].UnitFile := sUnitFile;
      FSegments[i].Procs    := aProcs;
  
      {
      TJclMapSegmentExt = record
        StartAddr: DWORD;
        EndAddr: DWORD;
        UnitName: String;
        UnitFile: String;
        Procs: array of TJclMapProcNameExt;
      end;
      }
  
      iStringLength := Length(sUnitName);
      strm.Write(iStringLength, sizeof(iStringLength));
      if iStringLength > 0 then
        strm.Write(sUnitName[1], iStringLength);
  
      iStringLength := Length(sUnitFile);
      strm.Write(iStringLength, sizeof(iStringLength));
      if iStringLength > 0 then
        strm.Write(sUnitFile[1], iStringLength);
  
      iProcCount := Length(aProcs);
      strm.Write(iProcCount, sizeof(iProcCount));
  
      for j := 0 to iProcCount-1 do
      begin
        sProcName          := aProcs[j].ProcName;
        aProcs[j].ProcName := '';
        strm.Write(aProcs[j], sizeof(TJclMapProcNameExt));
        aProcs[j].ProcName := sProcName;
  
        {
        TJclMapProcNameExt = record
          Addr: DWORD;
          ProcName: String;
        end;
        }

        iStringLength := Length(sProcName);
        strm.Write(iStringLength, sizeof(iStringLength));
        if iStringLength > 0 then
          strm.Write(sProcName[1], iStringLength);
      end;

      FSegments[i].Procs := aProcs;
    end;

    fs := TFileStream.Create(aDbgFilename, fmCreate	or fmShareExclusive);
    strm.SetSize(strm.position);      //resize back to actual size
    strm.Position := 0;
    fs.CopyFrom(strm, strm.Size);
    fs.Free;
  finally
    strm.Free;
  end;
end;

{
**************************** TInternalItemsStorage *****************************
}
constructor TInternalItemsStorage.Create;
begin
  inherited;
  LoadInternalItems;
end;

destructor TInternalItemsStorage.Destroy;
begin
  
  inherited;
end;

procedure TInternalItemsStorage.LoadInternalItems;
var
  mm: TMemoryManagerEx;
  sUnit: string;
  
  procedure __AddCustomProc(const aUnitName: String; const aProcName: string;
                            const aProcAddr: Pointer);
  var
  //    p: PJclMapProcNameExt;
    iUnit, iproc: integer;
  //    iSelProc:integer;
    iProcAddr: Cardinal;
  begin
    iProcAddr := Cardinal(aProcAddr);

    iUnit  := GetSegmentByName(aUnitName);
    if iUnit < 0 then
      iUnit := AddSegment(aUnitName);

    iproc := GetProcByName(aProcName,iUnit);
    if iproc < 0 then
      AddProcedure(iUnit, aProcName, iProcAddr );

    //FInternalDebugInfo.AddSelectedProcedure(iUnit,iProc);
    //inc(FCustomItemsCount);
  end;
  
begin
  GetMemoryManager(mm);
  sUnit := 'DelphiMemory';
  __AddCustomProc(sUnit, 'GetMem', @mm.GetMem);
  __AddCustomProc(sUnit, 'ReallocMem', @mm.ReallocMem);
  __AddCustomProc(sUnit, 'FreeMem', @mm.FreeMem);
  
  sUnit := 'WindowsMemory';
  
  __AddCustomProc(sUnit, 'LocalAlloc', @LocalAlloc);
  __AddCustomProc(sUnit, 'LocalReAlloc', @LocalReAlloc);
  __AddCustomProc(sUnit, 'LocalFree', @LocalFree);
  __AddCustomProc(sUnit, 'GlobalAlloc', @GlobalAlloc);
  __AddCustomProc(sUnit, 'GlobalReAlloc', @GlobalReAlloc);
  __AddCustomProc(sUnit, 'GlobalFree', @GlobalFree);
  __AddCustomProc(sUnit, 'VirtualAlloc', @VirtualAlloc);
  __AddCustomProc(sUnit, 'VirtualFree', @VirtualFree);
  __AddCustomProc(sUnit, 'HeapAlloc', @HeapAlloc);
  __AddCustomProc(sUnit, 'HeapReAlloc', @HeapReAlloc);
  __AddCustomProc(sUnit, 'HeapFree', @HeapFree);
  __AddCustomProc(sUnit, 'HeapCreate', @HeapCreate);
  __AddCustomProc(sUnit, 'HeapDestroy', @HeapDestroy);
  __AddCustomProc(sUnit, 'malloc', @malloc);
  __AddCustomProc(sUnit, 'calloc', @calloc);
  __AddCustomProc(sUnit, 'realloc', @realloc);
  __AddCustomProc(sUnit, 'free', @c_free);
  
  sUnit := 'DelphiWaits';
  __AddCustomProc(sUnit, 'TApplication.ProcessMessages', @TApplication.ProcessMessages);
  __AddCustomProc(sUnit, 'TApplication.HandleMessage', @TApplication.HandleMessage);
  __AddCustomProc(sUnit, 'TThread.WaitFor', @TThread.WaitFor);
  __AddCustomProc(sUnit, 'Sleep', @Sleep);
  __AddCustomProc(sUnit, 'Beep', @Beep);
  
  sUnit := 'WindowsWaits';
  __AddCustomProc(sUnit, 'GetMessage', @GetMessage);
  __AddCustomProc(sUnit, 'WaitMessage', @WaitMessage);
  __AddCustomProc(sUnit, 'SendMessage', @SendMessage);
  
  __AddCustomProc(sUnit, 'WaitForMultipleObjects', @WaitForMultipleObjects);
  __AddCustomProc(sUnit, 'WaitForMultipleObjectsEx', @WaitForMultipleObjectsEx);
  __AddCustomProc(sUnit, 'WaitForSingleObject', @WaitForSingleObject);
  __AddCustomProc(sUnit, 'WaitForSingleObjectEx', @WaitForSingleObjectEx);
  __AddCustomProc(sUnit, 'MsgWaitForMultipleObjects', @MsgWaitForMultipleObjects);
  __AddCustomProc(sUnit, 'MsgWaitForMultipleObjectsEx', @MsgWaitForMultipleObjectsEx);
  __AddCustomProc(sUnit, 'SignalObjectAndWait', @SignalObjectAndWait);
  
  __AddCustomProc(sUnit, 'EnterCriticalSection', @EnterCriticalSection);
  __AddCustomProc(sUnit, 'LeaveCriticalSection', @LeaveCriticalSection);
  
  sUnit := 'FileOperations';
  __AddCustomProc(sUnit, 'CreateFile', @CreateFile);
  __AddCustomProc(sUnit, 'ReadFile', @ReadFile);
  __AddCustomProc(sUnit, 'WriteFile', @WriteFile);
  //
  __AddCustomProc(sUnit, 'FindFirstFile', @FindFirstFile);
  __AddCustomProc(sUnit, 'FindNextFile', @FindNextFile);
  __AddCustomProc(sUnit, 'FindClose', @FindClose);
  //
  __AddCustomProc(sUnit, 'CopyFile', @CopyFile);
  __AddCustomProc(sUnit, 'DeleteFile', @DeleteFile);
  __AddCustomProc(sUnit, 'MoveFile', @MoveFile);
  __AddCustomProc(sUnit, 'CreateDirectory', @CreateDirectory);
  __AddCustomProc(sUnit, 'RemoveDirectory', @RemoveDirectory);
  __AddCustomProc(sUnit, 'GetDiskFreeSpace', @GetDiskFreeSpace);
  //
  __AddCustomProc(sUnit, 'LoadLibrary', @LoadLibrary);
  __AddCustomProc(sUnit, 'LoadModule', @LoadModule);
  __AddCustomProc(sUnit, 'LoadResource', @LoadResource);


  { TODO : custom ado.execute logging? }
end;

{
******************************** TMapFileLoader ********************************
}
constructor TMapFileLoader.Create(const aMapFile: TFilename);
begin
  inherited Create;

  FMapFilename := aMapFile;
  OffSet       := C_MEMORY_OFFSET;
  HasRelativeAddresses := True;
  FMapFile := TJclMapParser.Create(aMapFile);

  FMapFile.OnSegment        := Self.JclMapSegmentEvent;
  //  FMapFile.OnClassTable     := Self.JclMapClassTableEvent;
  //  FMapFile.OnPublicsByValue := Self.JclMapPublicsEvent;
  FMapFile.OnPublicsByName  := Self.JclMapPublicsEvent;
  FMapFile.OnLineNumberUnit := Self.JclMapLineNumberUnitItem;

  if not fileexists(aMapFile) then exit;

  LoadFromFile;
  if Length(FSegments) = 0 then
  begin
    FmapFile.Parse;
    ClearUnusedUnits;
      //SaveToFile;
  end;
end;

destructor TMapFileLoader.Destroy;
begin
  FMapFile.Free;
  inherited Destroy;
end;

procedure TMapFileLoader.ClearUnusedUnits;
var
  sa: TJclMapSegmentExtArray;
  i, iCnt: Integer;
begin
  iCnt := 0;
  SetLength(sa, Length(FSegments) );
  
  for i := low(FSegments) to high(FSegments) do
  begin
    if Length(FSegments[i].Procs) > 0 then
    begin
      sa[iCnt] := FSegments[i];
      inc(iCnt);
    end;
  end;
  SetLength(FSegments,0);
  SetLength(sa,iCnt+1);
  FSegments := sa;
end;

function TMapFileLoader.GetProcByAddr(const aProc:pointer): string;
var
  iaddr, i, j: NativeUInt;
begin
  iaddr  := NativeUInt(aProc) - Offset;
  Result := IntToStr(iaddr);
  
  for i := low(FSegments) to high(FSegments) do
  begin
    if (iaddr > FSegments[i].StartAddr) and
       (iaddr < FSegments[i].EndAddr) then
    begin
      Result := String(FSegments[i].UnitName);
  
      for j := low(FSegments[i].Procs) to high(FSegments[i].Procs) do
      begin
        if (FSegments[i].Procs[j].Addr) = iAddr then
        begin
          Result := Result + '.' + String(FSegments[i].Procs[j].ProcName);
          Exit;
        end
        else if ((FSegments[i].Procs[j].Addr + 10) >= iAddr) and
                ((FSegments[i].Procs[j].Addr - 10) <= iAddr) then
        begin
          Result := Result + '.' + String(FSegments[i].Procs[j].ProcName) + Format(' (+-10,%d, %d)',[FSegments[i].Procs[j].Addr, iAddr]);
          Exit;
        end;
      end;
    end;
  end;
end;

function TMapFileLoader.GetSegment(const aAddr:Cardinal): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := low(FSegments) to high(FSegments) do
  begin
    if (aAddr >= FSegments[i].StartAddr) and
       (aAddr <= FSegments[i].EndAddr) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TMapFileLoader.JclMapLineNumberUnitItem(Sender: TObject; const 
        UnitName, UnitFileName: string);
var
  iSegment: Integer;
begin
  iSegment := GetSegmentByName(UnitName);
  if iSegment >= 0 then
  begin
    FSegments[iSegment].UnitFile := Ansistring(UnitFileName);
  end;
end;

procedure TMapFileLoader.JclMapPublicsEvent(Sender: TObject; const Address: 
        TJclMapAddress; const Name: string);
var
  iSegment, P: Integer;
  
  function __HandleDuplicateNames(const aName: string):string;
  var
    i,iNr:integer;
  begin
    i      := FProcNamesCnt;
    iNr    := 0;
    Result := aName;
    while i > 0 do
    begin
      dec(i);
      if (String(FProcNames[i].ProcName) = Result) or
         (String(FProcNames[i].ProcName) = aName)
      then
        inc(iNr)
      else
        Break;
  
      if iNr > 0 then
        Result := aName + '#' + IntToStr(iNr);
    end;
  end;
  
begin
  if (Address.Segment = 1) and (Name <> 'Finalization') then
  if Pos('..', Name) = 0 then                                     //sometime weird classnames as functions...
  begin
    if FProcNamesCnt mod 256 = 0 then
      SetLength(FProcNames, FProcNamesCnt + 256);
    FProcNames[FProcNamesCnt].Addr := Address.Offset;
    FProcNames[FProcNamesCnt].ProcName := Ansistring(__HandleDuplicateNames(Name));
  
    iSegment := GetSegment(Address.Offset);
    if (iSegment >= 0) and (String(FSegments[iSegment].UnitName) <> Name) then
    begin
      P := Length(FSegments[iSegment].Procs);
      SetLength(FSegments[iSegment].Procs, P + 1);
      FSegments[iSegment].Procs[P] := FProcNames[FProcNamesCnt];
      Inc(FProcNamesCnt);
    end;
  end;
end;

procedure TMapFileLoader.JclMapSegmentEvent(Sender: TObject; const Address: 
        TJclMapAddress; Len: Integer; const GroupName, UnitName: string);
var
  C: Integer;
  iProfilerUnitMarker: NativeInt;
begin
  if Address.Segment = 1 then
  begin
    {filter/remove our own profiler unit!}
    iProfilerUnitMarker := NativeInt(@DummyProfileMarker) - NativeInt(Offset);
    if (iProfilerUnitMarker > NativeInt(Address.Offset)) and
       (iProfilerUnitMarker < NativeInt(Address.Offset) + Len) then
      exit;

    C := Length(FSegments);
    SetLength(FSegments, C + 1);
    FSegments[C].StartAddr := Address.Offset;
    FSegments[C].EndAddr   := NativeInt(Address.Offset) + Len;
    FSegments[C].UnitName  := Ansistring(UnitName);
    FTopValidAddr := Max(FTopValidAddr, NativeInt(Address.Offset) + Len);
  end;
end;

  function __FiletimeToDatetime(const aFiletime:TFileTime):TDatetime;
  var st: TSystemTime;
  begin
    FileTimeToSystemTime(aFiletime,st);
    Result := SystemTimeToDateTime(st);
  end;

procedure TMapFileLoader.LoadFromFile;
var
//  strm: TMemoryStream;
//  fs: TFileStream;
//  i, j: Integer;
//  iStringLength, iTotalProcCount, iProcCount, iProcs, iSegmentCount: Integer;
  sFile: string;
//  sString: string;
  tMapTime, tPdbgTime: TFileTime;
  dMapTime, dPdbgTime: TDatetime;

begin
  if (MapFilename = '') then
    FMapFilename := ChangeFileExt(Application.ExeName, '.map');
  if not FileExists(FMapFilename) then exit;

  tMapTime   := GetFileLastWrite( FMapFilename );
  dMapTime   := __FiletimeToDatetime(tMapTime);
  FDirectory := ExtractFilePath(FMapFilename) + FormatDateTime('ddmmyyyy_hhnnsszzz', dMapTime) + '\';
  sFile      := FDirectory + ChangeFileExt(ExtractFileName(FMapFilename), '.pdbg');
  if not FileExists(sFile) then exit;
  FMapFilename := sFile;

  tPdbgTime := GetFileCreation(FMapFilename);
  dPdbgTime := __FiletimeToDatetime(tPdbgTime);
  {file times must be the same, in case of rebuild/recompile}
  if dPdbgTime <> dMapTime then exit;

  LoadFromFile_Pdbg(FMapFilename);
end;

(*
procedure TMapFileLoader.LoadFromFile_Pdbg(const aPdbgfile: string);
var
  //tMapTime, tPdbgTime: TFileTime;
  //dMapTime, dPdbgTime: TDatetime;
  fs: TFileStream;
  strm: TMemoryStream;
  sString: string;
  i, j: Integer;
  iStringLength, iTotalProcCount, iProcCount, iProcs, iSegmentCount: Integer;
begin
  //tPdbgTime := GetFileCreation(aPdbgfile);
  //dPdbgTime := __FiletimeToDatetime(tPdbgTime);
  {file times must be the same, in case of rebuild/recompile}
  //if dPdbgTime <> dMapTime then exit;

  fs    := TFileStream.Create(aPdbgfile,fmOpenRead or fmShareDenyNone	);
  fs.Position   := 0;

  strm := TMemoryStream.Create;
  strm.CopyFrom(fs, fs.Size);
  strm.Position := 0;
  fs.Free;

  strm.Read(iSegmentCount,Sizeof(iSegmentCount));
  SetLength(FSegments,iSegmentCount);
  iTotalProcCount := 0;

  for i := 0 to iSegmentCount-1 do
  begin
    strm.Read(FSegments[i], sizeof(TJclMapSegmentExt));

    {
    TJclMapSegmentExt = record
      StartAddr: DWORD;
      EndAddr: DWORD;
      UnitName: String;
      UnitFile: String;
      Procs: array of TJclMapProcNameExt;
    end;
    }

    strm.Read(iStringLength,Sizeof(iStringLength));
    SetLength(sString, iStringLength);
    if iStringLength > 0 then
      strm.Read(sString[1], iStringLength)
    else
      sString := '';
    FSegments[i].UnitName := sString;

    strm.Read(iStringLength,Sizeof(iStringLength));
    SetLength(sString, iStringLength);
    if iStringLength > 0 then
      strm.Read(sString[1], iStringLength)
    else
      sString := '';
    FSegments[i].UnitFile := sString;

    strm.Read(iProcCount,Sizeof(iProcCount));
    SetLength(FSegments[i].Procs,iProcCount);
    iTotalProcCount := iTotalProcCount + iProcCount;

    for j := 0 to iProcCount-1 do
    begin
      strm.Read(FSegments[i].Procs[j], sizeof(TJclMapProcNameExt));
      {
      TJclMapProcNameExt = record
        Addr: DWORD;
        ProcName: String;
      end;
      }

      strm.Read(iStringLength,Sizeof(iStringLength));
      SetLength(sString, iStringLength);
      if iStringLength > 0 then
        strm.Read(sString[1], iStringLength)
      else
        sString := '';
      FSegments[i].Procs[j].ProcName := sString;
    end;
  end;

  iProcCount := 0;
  SetLength(FProcNames, iTotalProcCount );
  for i := 0 to iSegmentCount-1 do
  begin
    iProcs := Length(FSegments[i].Procs);
    for j := 0 to iProcs-1 do
    begin
      FProcNames[iProcCount].ProcName :=  FSegments[i].Procs[j].ProcName;
      FProcNames[iProcCount].Addr     :=  FSegments[i].Procs[j].Addr;
      inc(iProcCount);
    end;
  end;

  strm.Free;
//  LoadSelectionFromFile();
end;
*)

procedure TMapFileLoader.SaveToFile;
var
  strm: TMemoryStream;
  fs: TFileStream;
  i, j: Integer;
  iStringLength, iProcCount, iSegmentCount: Integer;
  sUnitName, sUnitFile, sProcName{, sFile}: ansistring;
  aProcs: TJclMapProcNameExtArray;
  dMapTime: TDatetime;
  tMapTime: TFileTime;
  
  function __FiletimeToDatetime(const aFiletime:TFileTime):TDatetime;
  var st: TSystemTime;
  begin
    FileTimeToSystemTime(aFiletime,st);
    Result := SystemTimeToDateTime(st);
  end;
  
begin
  strm := TMemoryStream.Create;
  strm.SetSize(1024 * 1024);      //default 1mb size
  try
    iSegmentCount := high(FSegments);
    strm.Write(iSegmentCount, sizeof(iSegmentCount));
  
    for i := 0 to iSegmentCount-1 do
    begin
      aProcs      := FSegments[i].Procs;
      sUnitName   := FSegments[i].UnitName;
      sUnitFile   := FSegments[i].UnitFile;
      FSegments[i].UnitName := '';
      FSegments[i].UnitFile := '';
      FSegments[i].Procs := nil;
      //
      strm.Write(FSegments[i], sizeof(TJclMapSegmentExt));
      //
      FSegments[i].UnitName := sUnitName;
      FSegments[i].UnitFile := sUnitFile;
      FSegments[i].Procs    := aProcs;
  
      {
      TJclMapSegmentExt = record
        StartAddr: DWORD;
        EndAddr: DWORD;
        UnitName: String;
        UnitFile: String;
        Procs: array of TJclMapProcNameExt;
      end;
      }
  
      iStringLength := Length(sUnitName);
      strm.Write(iStringLength, sizeof(iStringLength));
      if iStringLength > 0 then
        strm.Write(sUnitName[1], iStringLength);
  
      iStringLength := Length(sUnitFile);
      strm.Write(iStringLength, sizeof(iStringLength));
      if iStringLength > 0 then
        strm.Write(sUnitFile[1], iStringLength);
  
      iProcCount := Length(aProcs);
      strm.Write(iProcCount, sizeof(iProcCount));
  
      for j := 0 to iProcCount-1 do
      begin
        sProcName          := aProcs[j].ProcName;
        aProcs[j].ProcName := '';
        strm.Write(aProcs[j], sizeof(TJclMapProcNameExt));
        aProcs[j].ProcName := sProcName;
  
        {
        TJclMapProcNameExt = record
          Addr: DWORD;
          ProcName: String;
        end;
        }

        iStringLength := Length(sProcName);
        strm.Write(iStringLength, sizeof(iStringLength));
        if iStringLength > 0 then
          strm.Write(sProcName[1], iStringLength);
      end;

      FSegments[i].Procs := aProcs;
    end;

    if (MapFilename = '') then
      FMapFilename := ChangeFileExt(Application.ExeName, '.map');
    if not FileExists(FMapFilename) then exit;
    tMapTime := GetFileLastWrite( FMapFilename );
    dMapTime := __FiletimeToDatetime(tMapTime);
                                                                             
    FDirectory := ExtractFilePath(FMapfilename) + FormatDateTime('ddmmyyyy_hhnnsszzz', dMapTime) + '\';
    ForceDirectories(FDirectory);

    FDbgFilename := FDirectory + ChangeFileExt(ExtractFileName(FMapFilename), '.pdbg');

    fs := TFileStream.Create(FDbgFilename,fmCreate	or fmShareExclusive);
    strm.SetSize(strm.position);      //resize back to actual size
    strm.Position := 0;
    fs.CopyFrom(strm, strm.Size);
    fs.Free;
  finally
    strm.Free;
  end;

  SetFileCreation( FMapFilename, dMapTime );
  SetFileCreation( FDbgFilename, dMapTime );
end;


{ TCustomSerializable }

procedure TCustomSerializable.DefineProperties(Filer: TFiler);
begin
  inherited;
  //Filer.DefineProperty('Custom', ReadCustom, WriteCustom, True);
end;

function TCustomSerializable.LoadFromString(const AObjectString: string;
  const aIsIniString: boolean): TComponent;
var
  s:string;
begin
  if aIsIniString then
    s := StringReplace(AObjectString,'|',#13#10,[rfReplaceAll])
  else
    s := AObjectString;

  Result := LoadObjectFromString(s, Self);
end;

class function TCustomSerializable.LoadObjectFromStream(
  const AObjectStream: TStream; const aInstance: TComponent): TComponent;
//var
//  BinStream: TMemoryStream;
begin
  Result := aInstance;
  if AObjectStream = nil then exit;

  //AObjectStream.Position := 0;
  Result := AObjectStream.ReadComponent(aInstance);
end;

class function TCustomSerializable.LoadObjectFromString(
  const AObjectString: string; const aInstance: TComponent): TComponent;
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
begin
  Result := aInstance;
  if AObjectString = '' then exit;

  StrStream := TStringStream.Create(AObjectString);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Position := 0;
      Result := BinStream.ReadComponent(aInstance);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure TCustomSerializable.ReadCustom(Reader: Treader);
begin
  Reader.ReadListBegin;
  //custom := Reader.ReadInteger;
  Reader.ReadListEnd;
end;

class function TCustomSerializable.SaveObjectToStream(
  const aObject: TComponent): TMemoryStream;
var
  Writer: TWriter;
begin
  Result := TMemoryStream.Create;

  Writer := TWriter.Create(Result, 1 * 1024 * 1024);  //1mb buffer blocks;
  try
    Writer.WriteDescendent(aObject, nil);
  finally
    Writer.Free;
  end;
  //Result.WriteComponent(aObject);
  Result.Position := 0;
end;

class function TCustomSerializable.SaveObjectToString(
  const aObject: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(aObject);
      BinStream.Position := 0;
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Position := 0;
      Result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free;
  end;
end;

function TCustomSerializable.SaveToString(const aAsIniString: boolean): string;
begin
  Result := TCustomSerializable.SaveObjectToString(Self);
  if aAsIniString then
    Result := StringReplace(Result,#13#10,'|',[rfReplaceAll])
end;

procedure TCustomSerializable.WriteCustom(Writer: Twriter);
begin
  Writer.WriteListBegin;
  //Writer.WriteInteger(custom.integer);
  Writer.WriteListEnd;
end;

function TDebugInfoStorage.ExportSelectionToDir(const aDir: string): string;
//var
//  sRename: string;
begin
  Result := aDir + '\' +
            FormatDateTime('hhnnsszzz', now) + '.' +
            ExtractFileName(Filename);

  ExportSelectionToFile(Result);
end;

procedure TDebugInfoStorage.ExportSelectionToFile(
  const aFile: string);
var
  str: Tstrings;
  sFile:string;
begin
  str := SelectedUnitProceduresText;
  try
    str.SaveToFile(aFile);
  finally
    str.Free;
  end;

  sFile := Changefileext(aFile,'.pselected');
  Save_TSelectedUnitArray_ToFile(FSelectedUnitProcedures, sFile);
end;

function TMapFileLoader.ExportToDir(const aDir: string): string;
begin
  if (FDbgFilename = '') or
     not FileExists(FDbgFilename)
  then
    SaveToFile;

  ForceDirectories(aDir);
  Result := aDir + '\' +
            FormatDateTime('hhnnsszzz', now) + '.' +
            ExtractFileName(FDbgFilename);
  CopyFile( pchar(FDbgFilename), pchar(Result), False);
end;

{ TProgramImportsExportsStorage }

constructor TProgramImportsExportsStorage.Create;
begin
  inherited;
  LoadProgramImportsExports;
end;

destructor TProgramImportsExportsStorage.Destroy;
begin

  inherited;
end;

procedure TProgramImportsExportsStorage.LoadProgramImportsExports;
begin
  Clear;
end;

{ TProgramLoadedDllsStorage }

constructor TProgramLoadedDllsStorage.Create;
begin
  inherited;
  LoadProgramLoadedDlls;
end;

destructor TProgramLoadedDllsStorage.Destroy;
begin

  inherited;
end;

procedure TProgramLoadedDllsStorage.LoadProgramLoadedDlls;
var
  strM, strE:Tstrings;
  I: Integer;
  hModule, hProc: THandle;
  sModule, sProc: string;
  iUnit: Integer;
  pe: TJclPeImage;
  j: Integer;

  function __PeImportedFunctions(const FileName: TFileName; const FunctionsList: TStrings;
    const LibraryName: string; IncludeLibNames: Boolean): Boolean;
  var
    I: Integer;
  begin
    assert(pe <> nil);

    Result := pe.StatusOK;
    if Result then
    with pe.ImportList do
    begin
      TryGetNamesForOrdinalImports;
      FunctionsList.BeginUpdate;
      try
        for I := 0 to AllItemCount - 1 do
          with AllItems[I] do
            if ((Length(LibraryName) = 0) or (ImportLib.Name = LibraryName)) and
              (Name <> '') then
            begin
              if IncludeLibNames then
                FunctionsList.AddObject(ImportLib.Name + '=' + Name, pointer(Ordinal))
              else
                FunctionsList.AddObject(Name, pointer(Ordinal));
            end;
      finally
        FunctionsList.EndUpdate;
      end;
    end;
  end;

  function __PeExportedFunctions(const FileName: TFileName; const FunctionsList: TStrings): Boolean;
  var
    I: Integer;
  begin
    Result := pe.StatusOK;
    if Result then
    begin
      FunctionsList.BeginUpdate;
      try
        with pe.ExportList do
          for I := 0 to Count - 1 do
            with Items[I] do
              if not IsExportedVariable then
                FunctionsList.AddObject(Name,
                    pointer(Items[I].Address ) );
      finally
        FunctionsList.EndUpdate;
      end;
    end;
  end;

begin
  Self.Clear;

  strM := TStringList.create;
  strE := TStringList.create;
  strM.Capacity := 1024;
  strE.Capacity := 1024;
  pe   := TJclPeImage.Create(True);
  try
    jclSysInfo.LoadedModulesList(strM, GetCurrentProcessId, False);
    for I := 0 to strM.Count - 1 do
    begin
      hModule := THandle(strM.Objects[i]);
      sModule := strM.Strings[i];
      //iUnit  := GetSegmentByName(s);
      //if iUnit < 0 then
      iUnit := AddSegment( ExtractFileName(sModule) );

      //load PE info of module
      pe.FileName := sModule;

      (*
      strI := TStringList.create;
      __PeImportedFunctions(sModule, strI, '', False);
      for j := 0 to strI.Count - 1 do
      begin
        sProc := strI.Strings[j];
        hProc := THandle(strI.Objects[j]);
        AddProcedure(iUnit, sProc, hProc);
      end;
      *)

      strE.Clear;
      __PeExportedFunctions(sModule, strE);
      for j := 0 to strE.Count - 1 do
      begin
        sProc := strE.Strings[j];
        hProc := THandle(strE.Objects[j]);
        AddProcedure(iUnit, sProc, hModule + hProc);
      end;
    end;

  finally
    pe.Free;
    strM.Free;
    strE.Free;
  end;

  UpdateInternalSelection;
end;

end.
