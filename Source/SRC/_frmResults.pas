unit _frmResults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, _uProfileTypes, _uProfileClasses,
  //_frmProfileMain,
  _uProfilerManager, GpLists,
  Buttons, ComCtrls, VirtualTrees, _framProfileResultTree,
  ExtCtrls, contnrs, _framProfileTraceTree, TeEngine, Series, TeeProcs, Chart, Menus, ImgList;

  { TODO : 'TODO: Last 1024 value graph'}
  { TODO : 'Total thread cpu usage (kernel, user), incl total cpu time duration'}
  { TODO : 'Thread name import function (dll)'}
  { TODO : 'Show other "plugin" data? Memory, Thread CPU, Core CPU, Context switches, Disk, etc' }

type
  TfrmResults = class(TForm)
    BitBtn1: TBitBtn;
    PopupMenu1: TPopupMenu;
    SavetoCSV1: TMenuItem;
    SavetoRTF1: TMenuItem;
    SavetoText1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    mmoInfo: TMemo;
    Panel1: TPanel;
    pgProfileTimes: TPageControl;
    tsFunctionOverview: TTabSheet;
    vtreeItems: TVirtualStringTree;
    tsUnitOverview: TTabSheet;
    Splitter2: TSplitter;
    Label12: TLabel;
    framProfileResultTree1: TframProfileResultTree;
    Panel2: TPanel;
    Splitter1: TSplitter;
    gbxParents: TGroupBox;
    framProfileResultParents: TframProfileResultTree;
    Panel3: TPanel;
    gbxChilds: TGroupBox;
    framProfileResultChilds: TframProfileResultTree;
    gbxDetails: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtMinOwnTime: TEdit;
    edtMaxOwnTime: TEdit;
    edtAvgOwnTime: TEdit;
    edtCalls: TEdit;
    edtParentCalls: TEdit;
    edtChildCalls: TEdit;
    edtMinChildTime: TEdit;
    edtMaxChildTime: TEdit;
    edtAvgChildTime: TEdit;
    edtMaxTotalTime: TEdit;
    edtAvgTotalTime: TEdit;
    edtMinTotalTime: TEdit;
    edtTotalOwnTime: TEdit;
    edtTotalChildTime: TEdit;
    edtTotalTotalTime: TEdit;
    tsTraceTree: TTabSheet;
    framProfileTraceTree1: TframProfileTraceTree;
    tsText: TTabSheet;
    mmoProfileTimes: TMemo;
    tsCompleteTraceText: TTabSheet;
    mmoResults: TMemo;
    btnCompleteTraceExecute: TButton;
    tsInfo: TTabSheet;
    mmoInfoThread: TMemo;
    TabSheet1: TTabSheet;
    mmoErrorsAndWarnings: TMemo;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    cmbxThread: TComboBox;
    btnRefresh: TBitBtn;
    chkOverheadCompensation: TCheckBox;
    PopupMenu2: TPopupMenu;
    Showlargechartinseperatepopup1: TMenuItem;
    Loadalldata1: TMenuItem;
    N1: TMenuItem;
    ImageList1: TImageList;
    Chart1: TChart;
    OwnTimeSeries: TBarSeries;
    ChildTimeSeries: TBarSeries;
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure vtreeItemsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
    procedure vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtreeItemsHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormCreate(Sender: TObject);
    procedure framProfileResultTree1vtreeUnitChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtreeItemsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure btnCompleteTraceExecuteClick(Sender: TObject);
    procedure framProfileResultTree1vtreeUnitDblClick(Sender: TObject);
    procedure vtreeItemsDblClick(Sender: TObject);
    procedure framProfileResultParentsvtreeUnitDblClick(Sender: TObject);
    procedure framProfileResultChildsvtreeUnitDblClick(Sender: TObject);
    procedure pgProfileTimesChange(Sender: TObject);
    procedure edtTotalChildTimeChange(Sender: TObject);
    procedure Chart1DblClick(Sender: TObject);
    procedure SavetoCSV1Click(Sender: TObject);
    procedure SavetoRTF1Click(Sender: TObject);
    procedure SavetoText1Click(Sender: TObject);
    procedure Showlargechartinseperatepopup1Click(Sender: TObject);
    procedure Loadalldata1Click(Sender: TObject);
    procedure cmbxThreadChange(Sender: TObject);
  private
    FProfileRun: TProfilerun;
    FCurrentUnitProcedure: PUnitRec;
    procedure SetProfileRun(const Value: TProfilerun);
    procedure FullTraceDump;
    function CalcTimeDiff(const aDiffCycles: int64): string;
  protected
    FProfiledUnitArray: TProfiledUnitArray;
    FProfiledProcArray: TProfiledProcArray;
    FTotalCycles: int64;
    FUnknownUnitRecord: TJclMapSegmentExt;

    FMainMapFile: TMapFileLoader;
    FInternalDebugInfo: TInternalItemsStorage;
    FCustomDebugInfo: TDebugInfoStorage;
    FLoadedDllsDebugInfo: TDebugInfoStorage;

    FProfiledUnitProcList: TGpIntegerObjectList;
    procedure _ClearProfiledUnitProcList;

    function _NewProfiledProcRecord: PProfiledProc;
    function _CreateProfiledUnitArrayOfProcArray(const aParentProcRecord: PProfiledProc; const aProcArray: TProfiledProcArray): TProfiledUnitArray;
    procedure _ClearProfiledUnitArrayOfProcArray(const aUnitArray: TProfiledUnitArray);

    function _SearchAndAddCustomUnit(const aDebugInfo:TDebugInfoStorage; const aAddr: Cardinal): integer;
    function _SearchAndAddCustomProc(const aDebugInfo:TDebugInfoStorage; const aUnitIndex: integer; const aAddr: Cardinal): integer;
    function _SearchAndAddUnknownUnit(const aAddr: Cardinal): integer;
    function _SearchAndAddUnknownProc(const aUnitIndex: integer; const aAddr: Cardinal): integer;

    procedure _SearchAndAddUnitProc(const aAddr: Cardinal; out aUnitIndex, aProcIndex: integer);
    procedure _FullSearchAndAddUnitProc(const aAddr: Cardinal; out aUnitIndex, aProcIndex: integer);
    procedure _AddChildProc(const aProcIndex, aChildProcIndex: integer; const aChildTotalTime, aChildChildTime: int64);
    procedure _AddParentProc(const aParentProcIndex, aChildProcIndex: integer; const aParentOwnTime, aParentChildTime: int64);

    function _SearchUnitResultRecord(const aUnitMapIndex:integer):integer;
    function _SearchProcResultRecord(const aUnitIndex, aProcMapIndex:integer):integer;

    function ProcessProfileTimes(const aProfileRunArray: TProfileThreadSlotArray; const aThreadID:Cardinal;
        out aProfileRecords: integer; out aTotalCallCount: integer):int64;

    //procedure FullTraceDump;
    procedure CalcProfileTimes;
    procedure ClearProfileArrays;
  public
    { Public declarations }
    //procedure DumpProfileResults;

    destructor Destroy;override;
    property ProfileRun: TProfilerun read FProfileRun write SetProfileRun;
  end;

  TProfiledUnitProcListRecord = record
    UnitIndex: integer;
    ProcIndex: integer;
  end;
  PProfiledUnitProcListRecord = ^TProfiledUnitProcListRecord;

var
  frmResults: TfrmResults;

implementation

uses
  {$if CompilerVersion >= 24}  //Delphi XE3
  UITypes,
  {$ifend}
  //_uAsmProfiler,
  Math, _frmCallChart, StrUtils;

{$R *.dfm}

type
  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    UnitName,
    ClassName,
    FunctionName: String;
    //
    Calls: integer;
    UnitProcent, TotalProcent, TotalChildProcent: Single;
    TotalTime, TotalChildTime, TotalWithChildTime: Real;
    AvgTime, {AvgChildTime,} AvgWithChildTime: Real;
    //
    ProfiledProc: PProfiledProc;
  end;

function TfrmResults.CalcTimeDiff(const aDiffCycles: int64): string;
var
  tDiff: Extended;
begin
  tDiff := aDiffCycles / ProfileRun.CPUSpeed;
  Result := CalcTimeText(tDiff);

  (*
  if tDiff >= 1 then
    Result := Format('%8.2fs',[tDiff])
  else if tDiff >= 0.0001 then
    Result := Format('%8.2fms',[tDiff * 1000])
  else if tDiff >= 0.0000001 then
    Result := Format('%8.2fµs',[tDiff * 1000 * 1000])
  else //if tDiff >= 0.0000001 then
    Result := Format('%8.2fns',[tDiff * 1000 * 1000 * 1000])
  *)
end;

procedure TfrmResults.Chart1DblClick(Sender: TObject);
begin
  if frmCallChart = nil then
    frmCallChart := TfrmCallChart.Create(Self);

  if FCurrentUnitProcedure <> nil then  
    frmCallChart.Caption := 'Call duration history ('+ FCurrentUnitProcedure.FunctionName + ')'
  else
    frmCallChart.Caption := 'Call duration history ( )';

  frmCallChart.OwnTimeSeries.AssignValues( Self.OwnTimeSeries );
  frmCallChart.ChildTimeSeries.AssignValues( Self.ChildTimeSeries );

  if Sender <> nil then  
    frmCallChart.Show;
end;

procedure TfrmResults.CalcProfileTimes;
var
  i,j:integer;
  //iTotalCycles: int64;
  pta: TProfileTimeArray;
  iThreadID:integer;
  sThread:string;
begin
  cmbxThread.Items.Clear;
  iThreadID := 0;

  with ProfileRun do
  if length(ProfileTimes) > 0 then
  for i := low(ProfileTimes) to ProfileTimesCount-1 do
  begin
    pta := ProfileTimes[i].ProfileTime;
    if pta = nil then break;

    sThread := IntToStr(ProfileTimes[i].ThreadID);
    for j := 0 to ThreadNameCount-1 do
    begin
      if ThreadNames[j].ThreadID = ProfileTimes[i].ThreadID then
      begin
        sThread := Format('%d - %s',[ProfileTimes[i].ThreadID, ThreadNames[j].Name]);
        Break;
      end;
    end;

    if cmbxThread.Items.IndexOf(sThread) < 0 then
      cmbxThread.Items.AddObject(sThread,TObject(ProfileTimes[i].ThreadID));
    if iThreadID = 0 then
    begin
      iThreadID := ProfileTimes[i].ThreadID;
      cmbxThread.ItemIndex := 0;
    end;
  end;

  with ProfileRun do
  begin
    mmoInfo.Clear;
    mmoInfo.Lines.Add( 'Profile start: ' + DatetimeToStr(ProfileStartTime) );
    mmoInfo.Lines.Add( 'Profile stop:  ' + DatetimeToStr(ProfileStopTime) );
//    mmoInfo.Lines.Add( '-----------------------------------' );
    mmoInfo.Lines.Add( 'Profile duration:  ' + TimeToStr(ProfileStopTime - ProfileStartTime) );
    mmoInfo.Lines.Add( '' );
    mmoInfo.Lines.Add( 'Result saved:  ' + DatetimeToStr(SaveTime) );
    mmoInfo.Lines.Add( '' );
    mmoInfo.Lines.Add( format('Clock type: %s',[ProfileClockType]) );
    mmoInfo.Lines.Add( format('Clock speed: %4.3f Mhz',[CPUSpeed / (1000*1000)]) );
    mmoInfo.Lines.Add( format('Overhead cycles: %d',[ ProfileRun.ProfileOverhead]) );
    mmoInfo.Lines.Add( format('Overhead time: %s',[ CalcTimeDiff(ProfileRun.ProfileOverhead)]) );
    mmoInfo.Lines.Add( format('Thread count: %d',[cmbxThread.Items.Count]) );
    mmoInfo.Lines.Add( format('Profile chunks count: %d',[ProfileTimesCount]) );
    mmoInfo.Lines.Add( '' );
    mmoInfo.Lines.Add( 'Map file:' );
    mmoInfo.Lines.Add( MapFilename );
    mmoInfo.Lines.Add( 'Map selections:' );
    mmoInfo.Lines.Add( MapInfoFile );
    mmoInfo.Lines.Add( 'Internal selections:' );
    mmoInfo.Lines.Add( InternalDebugInfoFile );
    mmoInfo.Lines.Add( 'Custom selections:' );
    mmoInfo.Lines.Add( CustomDebugInfoFile );
  end;

  btnRefresh.Click;
end;

destructor TfrmResults.Destroy;
begin
  _ClearProfiledUnitProcList;
  FProfiledUnitProcList.Free;
  FMainMapFile.Free;
  FInternalDebugInfo.Free;
  FCustomDebugInfo.Free;
  FLoadedDllsDebugInfo.Free;
  inherited;
end;

procedure TfrmResults.edtTotalChildTimeChange(Sender: TObject);
begin

end;

{
procedure TfrmResults.DumpProfileResults;
begin
  //FullTraceDump;
  CalcProfileTimes;
end;
}

procedure TfrmResults.FullTraceDump;
var
//  iEnterCount,
  j:integer;
//  iPrev: int64;
//  iDiff: int64;
  pta: TProfileTimeArray;
  //p:pointer;
  //sFill:string;

  function __FillString(const aCount:integer):string;
  var
    i:integer;
  begin
    SetLength(Result,aCount);
    for i := 1 to aCount do
    begin
      Result[i] := ' ';
    end;
  end;

  function __ProcessResult(const aLevel:integer): int64;
  var
    iStartTime, iEndTime,
    iTotalTime,
    iChildTime: Int64;
    sFill:string;
    //iaUnit, iaProc: integer;
  begin
    Result := 0;
    if (pta[j].Address <> nil) then
    begin
      iTotalTime := 0;
      if pta[j].ProfileType = ptEnter then
      begin
        iStartTime := pta[j].Time;
        sFill := __FillString(aLevel*2);

       // _SearchAndAddUnitProc(pta[j].Address, iaUnit, iaProc);
       // if iaProc >= 0 then
       // with FProfiledProcArray[iaProc]^ do
        mmoResults.Lines.Add( Format('%s-> %s - Adress: %p',
            //[sFill, FMainMapFile.GetProcByAddr(pta[j].Address), pta[j].Address]) );
            [sFill, FMainMapFile.GetProcByAddr(pta[j].Address), pta[j].Address]) );

        inc(j);
        if j >= high(pta) then exit;

        if pta[j].ProfileType = ptEnter then
          iChildTime := __ProcessResult(aLevel+1)
        else
          iChildTime := 0;
        iEndTime   := pta[j].Time;
        iTotalTime := iEndTime - iStartTime;

        sFill := __FillString(aLevel*2);
        mmoResults.Lines.Add( Format('%s<- %s - Adress: %p - Diff: %s - Child: %s',
            [sFill, FMainMapFile.GetProcByAddr(pta[j].Address), pta[j].Address, CalcTimeDiff(iTotalTime), CalcTimeDiff(iChildTime)]) );
      end;

      Result := iTotalTime;
    end
    else
      exit;
  end;

begin
  mmoResults.Clear;
  (*
  mmoResults.Lines.BeginUpdate;

  with ProfileRun do
  if length(ProfileTimes) > 0 then
  for i := low(ProfileTimes) to ProfileTimesCount-1 do
  begin
    pta := ProfileTimes[i].ProfileTime;
    if pta = nil then break;

    j := 0;
    while j <= high(pta) do
    begin
      __ProcessResult(1);
      inc(j);
    end;
  end;

  mmoResults.Lines.EndUpdate;
  *)
end;

procedure TfrmResults.Loadalldata1Click(Sender: TObject);

  procedure __FillChart;
  var
    i: Integer;
    fTime: single;
    //c: TColor;
  begin
    OwnTimeSeries.Clear;
    ChildTimeSeries.Clear;
    Loadalldata1.Visible := False;

    if FCurrentUnitProcedure.ProfiledProc <> nil then
    with FCurrentUnitProcedure.ProfiledProc^ do
    begin
      for I := low(AllOwnProfileTimes) to high(AllOwnProfileTimes) do
      begin
        if (AllOwnProfileTimes[i] > 0) or (AllChildProfileTimes[i] > 0) then
        begin
          fTime := AllOwnProfileTimes[i] / ProfileRun.CPUSpeed;
          OwnTimeSeries.Add(fTime);
          fTime := AllChildProfileTimes[i] / ProfileRun.CPUSpeed;
          ChildTimeSeries.Add(fTime);
        end;
      end;
    end;

    //refresh popup detail
    if frmCallChart <> nil then
      Chart1DblClick(nil);
  end;

begin
  if FCurrentUnitProcedure <> nil then
  begin
    Screen.Cursor := crHourGlass;
    try
      __FillChart;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

(*
procedure TfrmResults.SetMapFileLoader(const Value: TMapFileLoader);
begin
  FMapFileLoader := Value;
end;

procedure TfrmResults.SetProfileTimeArrayCount(const Value: integer);
begin
  FProfileTimeArrayCount := Value;
end;

procedure TfrmResults.SetProfileTimes(const Value: TProfileRunRecordArray);
begin
  FProfileTimes := Value;
end;

procedure TfrmResults.SetThreadNameCount(const Value: integer);
begin
  FThreadNameCount := Value;
end;

procedure TfrmResults.SetThreadNames(const Value: TThreadNameArray);
begin
  FThreadNames := Value;
end;
*)

procedure SaveStringToFile_Ask(const aData: string);
var
  dlg: TOpenDialog;
  str: TStrings;
begin
  dlg := TOpenDialog.Create(nil);
  str := nil;
  try
    if dlg.Execute then
    try
      str := TStringList.Create;
      str.Text := aData;
      str.SaveToFile( dlg.FileName );
    finally
      str.Free;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrmResults.SavetoCSV1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeItems.ContentToHTML(tstAll, 'Profiler Function Result HTML Export')) );
end;

procedure TfrmResults.SavetoRTF1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeItems.ContentToRTF(tstAll)) );
end;

procedure TfrmResults.SavetoText1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeItems.ContentToText(tstAll, ',')) );
end;

procedure TfrmResults.SetProfileRun(const Value: TProfilerun);
var
  sDir,
  sfile:string;
begin
  FProfileRun := Value;
  framProfileResultChilds.ProfileRun  := Value;
  framProfileResultParents.ProfileRun := Value;
  framProfileResultTree1.ProfileRun   := Value;

  if FProfileRun <> nil then
  begin
    if FProfileRun.Filename <> '' then
      sDir := ExtractFilePath(FProfileRun.Filename)
    else
      sDir := ExtractFilePath(FProfileRun.MapFilename);

    if FMainMapFile <> nil then
    begin
      if ExtractFileName(FMainMapFile.Filename) <>
         ExtractFileName(FProfileRun.MapFilename)
      then
        FreeAndNil(FMainMapFile);
    end;
    if FMainMapFile = nil then
    begin
      FMainMapFile := TMapFileLoader.Create('');

      //if FileExists(FProfileRun.MapFilename) then
      //  sFile := FProfileRun.MapFilename
      //else
      sFile := sDir + ExtractFileName(FProfileRun.MapFilename);

      if FileExists(sFile) then
        FMainMapFile.LoadFromFile_Pdbg(sFile)
      else
        showmessage('no profiled map file (*.pdbg) found!');

      if FProfileRun.MapMemOffset > 0 then
        FMainMapFile.Offset := FProfileRun.MapMemOffset;

      //if FileExists(FProfileRun.MapInfoFile) then
      //  sFile := FProfileRun.MapInfoFile
      //else
      sFile := sDir + ExtractFileName(FProfileRun.MapInfoFile);

      if FileExists(sFile) then
        FMainMapFile.LoadSelectionFromFile(sFile);
    end;

    if FInternalDebugInfo <> nil then
    begin
      if ExtractFileName(FInternalDebugInfo.Filename) <>
         ExtractFileName(FProfileRun.InternalDebugInfoFile)
      then
        FreeAndNil(FInternalDebugInfo);
    end;
    if FInternalDebugInfo = nil then
    begin
      FInternalDebugInfo := TInternalItemsStorage.Create;

      //if FileExists(FProfileRun.InternalDebugInfoFile) then
      //  sFile := FProfileRun.InternalDebugInfoFile
      //else
      sFile := sDir + ExtractFileName(FProfileRun.InternalDebugInfoFile);

      if FileExists(sFile) then
        FInternalDebugInfo.LoadSelectionFromFile(sFile);
    end;

    if FCustomDebugInfo <> nil then
    begin
      if ExtractFileName(FCustomDebugInfo.Filename) <>
         ExtractFileName(FProfileRun.CustomDebugInfoFile)
      then
        FreeAndNil(FCustomDebugInfo);
    end;
    if FCustomDebugInfo = nil then
    begin
      FCustomDebugInfo := TDebugInfoStorage.Create;

      //if FileExists(FProfileRun.CustomDebugInfoFile) then
      //  sFile := FProfileRun.CustomDebugInfoFile
      //else
      sFile := sDir + ExtractFileName(FProfileRun.CustomDebugInfoFile);

      if FileExists(sFile) then
        FCustomDebugInfo.LoadSelectionFromFile(sFile);
    end;

    if FLoadedDllsDebugInfo <> nil then
    begin
      if ExtractFileName(FLoadedDllsDebugInfo.Filename) <>
         ExtractFileName(FProfileRun.LoadedDllsInfoFile)
      then
        FreeAndNil(FLoadedDllsDebugInfo);
    end;
    if FLoadedDllsDebugInfo = nil then
    begin
      FLoadedDllsDebugInfo := TDebugInfoStorage.Create;

      sFile := sDir + ExtractFileName(FProfileRun.LoadedDllsDbgFile);
      if FileExists(sFile) then
        FLoadedDllsDebugInfo.LoadFromFile_Pdbg(sFile);

      sFile := sDir + ExtractFileName(FProfileRun.LoadedDllsInfoFile);
      if FileExists(sFile) then
        FLoadedDllsDebugInfo.LoadSelectionFromFile(sFile);
    end;

    CalcProfileTimes;
  end;
end;

procedure TfrmResults.Showlargechartinseperatepopup1Click(Sender: TObject);
begin
  Chart1DblClick(Sender);
end;

procedure TfrmResults.FormDestroy(Sender: TObject);
begin
  ClearProfileArrays;

  _ClearProfiledUnitArrayOfProcArray(framProfileResultParents.ProfiledUnitArray);
  _ClearProfiledUnitArrayOfProcArray(framProfileResultChilds.ProfiledUnitArray);
end;

procedure TfrmResults.framProfileResultTree1vtreeUnitChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PUnitRec;

  function MinInt64Value(aValues: array of int64): int64;
  var i:integer;
  begin
    Result := 0;
    if Length(aValues) = 0 then exit;

    Result := aValues[0];
    for I := low(aValues) to high(aValues) do
      if aValues[i] > 0 then
        Result := min(aValues[i],Result);
  end;

  function MaxInt64Value(aValues: array of int64): int64;
  var i:integer;
  begin
    Result := 0;
    for I := low(aValues) to high(aValues) do
      if aValues[i] > 0 then
        Result := max(aValues[i],Result);
  end;

  function AvgInt64Value(aValues: array of int64): int64;
  begin
    Result := 0;
  end;

  procedure __CalcDetails;
  var
    i: Integer;
    iMinOwn, iMaxOwn, iAvgOwn: int64;
    iMinChild, iMaxChild, iAvgChild: int64;
    iMinTotal, iMaxTotal, iAvgTotal: int64;
  begin
    iMinOwn := 0; iMinChild := 0; iMinTotal:= 0;
    iMaxOwn := 0; iMaxChild := 0; iMaxTotal:= 0;
    iAvgOwn := 0; iAvgChild := 0; iAvgTotal:= 0;

    if Data.ProfiledProc <> nil then
    with Data.ProfiledProc^ do
    begin
      if Length(AllOwnProfileTimes) > 0 then
      begin
        iMinOwn   := AllOwnProfileTimes[0];
        iMinChild := AllChildProfileTimes[0];
        iMinTotal := iMinOwn + iMinChild;
      end;

      for I := low(AllOwnProfileTimes) to high(AllOwnProfileTimes) do
      begin
        if AllOwnProfileTimes[i] > 0 then
        begin
          iMinOwn := min(AllOwnProfileTimes[i],iMinOwn);
          iMaxOwn := max(AllOwnProfileTimes[i],iMaxOwn);
          iMinChild := min(AllChildProfileTimes[i],iMinChild);
          iMaxChild := max(AllChildProfileTimes[i],iMaxChild);
          iMinTotal := min(AllOwnProfileTimes[i] + AllChildProfileTimes[i],iMinTotal);
          iMaxTotal := max(AllOwnProfileTimes[i] + AllChildProfileTimes[i],iMaxTotal);
        end;
      end;
    end;

    Data.DetailedInfo.MinOwnTime := iMinOwn;
    Data.DetailedInfo.MaxOwnTime := iMaxOwn;
    Data.DetailedInfo.AvgOwnTime := iAvgOwn;
    Data.DetailedInfo.MinChildTime := iMinChild;
    Data.DetailedInfo.MaxChildTime := iMaxChild;
    Data.DetailedInfo.AvgChildTime := iAvgChild;
    Data.DetailedInfo.MinTotalTime := iMinTotal;
    Data.DetailedInfo.MaxTotalTime := iMaxTotal;
    Data.DetailedInfo.AvgTotalTime := iAvgTotal;

    Data.DetailedInfo.DataPresent := True;
  end;

  procedure __FillChart;
  var
    i: Integer;
    fTime: single;
    //c: TColor;
  begin
    OwnTimeSeries.Clear;
    ChildTimeSeries.Clear;
    Loadalldata1.Visible := False;

    if Data.ProfiledProc <> nil then
    with Data.ProfiledProc^ do
    begin
      for I := low(AllOwnProfileTimes) to high(AllOwnProfileTimes) do
      begin
        if i > 1024 then
        begin
          Loadalldata1.Visible := True;
          Break;
        end;
        
        if (AllOwnProfileTimes[i] > 0) or (AllChildProfileTimes[i] > 0) then
        begin
          fTime := AllOwnProfileTimes[i] / ProfileRun.CPUSpeed;
          OwnTimeSeries.Add(fTime);
          fTime := AllChildProfileTimes[i] / ProfileRun.CPUSpeed;
          ChildTimeSeries.Add(fTime);
        end;
      end;
    end;

    //refresh popup detail
    if frmCallChart <> nil then
      Chart1DblClick(nil);
  end;

begin
  Data := Sender.GetNodeData(Node);
  FCurrentUnitProcedure := Data;

  if Sender.GetNodeLevel(Node) = 1 then
  with ProfileRun do
  begin
    gbxDetails.Caption := ' Details ('+ Data.FunctionName + ')';

    _ClearProfiledUnitArrayOfProcArray(framProfileResultParents.ProfiledUnitArray);
    framProfileResultParents.ProfiledUnitArray :=
      _CreateProfiledUnitArrayOfProcArray(Data.ProfiledProc, Data.ProfiledProc.ParentProcs);
    framProfileResultParents.TotalCycles       := framProfileResultTree1.TotalCycles;
    framProfileResultParents.vtreeUnit.FullExpand();

    with framProfileResultChilds do
    begin
      _ClearProfiledUnitArrayOfProcArray(ProfiledUnitArray);
      ProfiledUnitArray  :=
        _CreateProfiledUnitArrayOfProcArray(Data.ProfiledProc, Data.ProfiledProc.ChildProcs);
      TotalCycles        := framProfileResultTree1.TotalCycles;
      vtreeUnit.FullExpand();
      //vtreeUnit.RootNode.States := vtreeUnit.RootNode.States + [vsExpanded];
      //vtreeUnit.InvalidateToBottom(vtreeUnit.RootNode);
    end;

    if not Data.DetailedInfo.DataPresent then
      __CalcDetails;

    __FillChart;

    with Data^ do
    if ProfiledProc <> nil then
    begin
      edtCalls.Text := IntToStr(Calls);
      edtParentCalls.Text := IntToStr(ProfiledProc.ParentCount);
      edtChildCalls.Text  := IntToStr(ProfiledProc.ChildCount);

      edtMinOwnTime.Text  := CalcTimeText(DetailedInfo.MinOwnTime / CPUSpeed);
      edtMaxOwnTime.Text  := CalcTimeText(DetailedInfo.MaxOwnTime / CPUSpeed);
      edtAvgOwnTime.Text  := CalcTimeText(DetailedInfo.AvgOwnTime / CPUSpeed);

      edtMinChildTime.Text  := CalcTimeText(DetailedInfo.MinChildTime / CPUSpeed);
      edtMaxChildTime.Text  := CalcTimeText(DetailedInfo.MaxChildTime / CPUSpeed);
      edtAvgChildTime.Text  := CalcTimeText(DetailedInfo.AvgChildTime / CPUSpeed);

      edtMinTotalTime.Text  := CalcTimeText(DetailedInfo.MinTotalTime / CPUSpeed);
      edtMaxTotalTime.Text  := CalcTimeText(DetailedInfo.MaxTotalTime / CPUSpeed);
      edtAvgTotalTime.Text  := CalcTimeText(DetailedInfo.AvgTotalTime / CPUSpeed);

      edtTotalOwnTime.Text    := CalcTimeText(Data.TotalTime);
      edtTotalChildTime.Text  := CalcTimeText(Data.TotalChildTime);
      edtTotalTotalTime.Text  := CalcTimeText(Data.TotalWithChildTime);

      edtAvgOwnTime.Text    := CalcTimeText(Data.AvgTime);
      edtAvgChildTime.Text  := CalcTimeText(Data.AvgChildTime);
      edtAvgTotalTime.Text  := CalcTimeText(Data.AvgTotalChildTime);
    end;
  end;
end;

procedure TfrmResults.framProfileResultTree1vtreeUnitDblClick(Sender: TObject);
var
  d,Data: PUnitRec;
  n,ns,Node: PVirtualNode;
begin
  with framProfileResultTree1.vtreeUnit do
  begin
    Node := GetFirstSelected;
    if Node = nil then exit;

    Data := GetNodeData(Node);
    if Assigned(Data) then
    if GetNodeLevel(Node) > 1 then
    begin
      n := GetFirst;
      while n <> nil do
      begin
        d := GetNodeData(n);
        //search unit
        if d.UnitName = Data.UnitName then
        begin
          ns := n;
          while ns <> nil do
          begin
            d := GetNodeData(ns);
            //search proc
            if (d.FunctionName = Data.FunctionName) and
               (GetNodeLevel(ns) < 2) then               //if level 2 -> search next
            begin
              FullyVisible[ns] := True;
              Selected[ns]     := True;
              FocusedNode      := ns;
              ScrollIntoView(ns, True);
              Exit;
            end;
            ns := GetNextSibling(ns);
          end;
        end;
        n := GetNext(n);
      end;
    end;
  end;
end;

function TfrmResults.ProcessProfileTimes(
  const aProfileRunArray: TProfileThreadSlotArray;
  const aThreadID: Cardinal;
  out aProfileRecords: integer;
  out aTotalCallCount: integer): int64;

var
  iArrayIndex:integer;
  iProfIndex, iChildProcIndex:integer;
  pta: TProfileTimeArray;
  ppta: PProfileTimeArray;
  iTotalCycles, iChildChildTime: int64;
  //tempstack: TStack;
  iTotalCallCount, iCallCount: integer;

  {$ifdef WriteFullDebugTrace}
  sType: string;
  strTestOutput: TStrings;
  {$endif WriteFullDebugTrace}

  function __GetNextTimeArray: PProfileTimeArray;
  begin
    Result := nil;

    while iArrayIndex < high(aProfileRunArray) do
    begin
      inc(iArrayIndex);
      if aProfileRunArray[iArrayIndex].ThreadID = aThreadID then
      begin
        iProfIndex := 0;
        Result := @(aProfileRunArray[iArrayIndex].ProfileTime);
        Break;
      end;
    end;
  end;

  function __ProcessResult(const aLevel:integer; out aProcIndex: integer; out aChildChildTime: int64; out aCallCount: integer): int64;
  var
    i :integer;
    iStartTime, iEndTime,
    iTotalTime, iResult,
    iChildChildTime,
    iChildTime,
    iOverhead: Int64;
    //iProc, iUnit: integer;
    iCallCount,
    iaProc, iaUnit, iaChildProc: integer;
    cAddress: Cardinal;
    iChildCalls: integer;
    iaChildCalls: array of integer;
    bSkip: boolean;
  begin
    Result := -1;
    aCallCount := 0;
    //end of current array? Try to continue with next
    if iProfIndex > high(pta) then
    begin
      ppta := __GetNextTimeArray;
      mmoErrorsAndWarnings.Lines.Add('Error: (ppta = nil) or (ppta^ = nil) when trying to fetch next block');
      MessageDlg('Error: (ppta = nil) or (ppta^ = nil) when trying to fetch next block', mtError, [mbOK], 0);
      if ppta = nil then Exit;
      pta  := ppta^;
      if pta = nil then Exit;
    end;
    Result := 0;
    bSkip  := True;

    if (pta[iProfIndex].Address <> nil) then
    begin
      iTotalTime := 0;
      if pta[iProfIndex].ProfileType = ptEnter then
      begin
        iStartTime := pta[iProfIndex].Time;
        cAddress   := Cardinal(pta[iProfIndex].Address);
        //tempstack.Push(pointer(cAddress));

        {get unit and proc index}
        _SearchAndAddUnitProc(cAddress, iaUnit, iaProc);
        aProcIndex := iaProc;

        {$ifdef WriteFullDebugTrace}
        //test output
        strTestOutput.Add( Format('L: %d - P: %s - T: %s - I: %d',[
            aLevel, FProfiledProcArray[iaProc]^.ProcName, 'Enter', iProfIndex{iStartTime}]));
        {$endif WriteFullDebugTrace}

        {next result}
        inc(iProfIndex);
        //end of current array? Try to continue with next
        if iProfIndex > high(pta) then
        begin
          ppta := __GetNextTimeArray;
          if (ppta = nil) or (ppta^ = nil) then
          begin
            MessageDlg('Error: (ppta = nil) or (ppta^ = nil) when trying to fetch next block', mtError, [mbOK], 0);
            mmoErrorsAndWarnings.Lines.Add('Error: (ppta = nil) or (ppta^ = nil) when trying to fetch next block');
            Exit;
          end;
          pta  := ppta^;
        end;
        inc(aProfileRecords);

        iChildTime  := 0;
        iChildCalls := 0;
        {if enter -> recursive process it}
        while (pta[iProfIndex].ProfileType = ptEnter) and
              (pta[iProfIndex].Address <> nil) do
        begin
          {recursive process child calls}
          iResult    := __ProcessResult(aLevel+1, iaChildProc, iChildChildTime, iCallCount);
          if iResult = -1 then
          begin
            //MessageDlg('Error: __ProcessResult = -1!', mtError, [mbOK], 0);
            mmoErrorsAndWarnings.Lines.Add('Error: __ProcessResult = -1!');
            break;
          end;
          aCallCount := aCallCount + iCallCount;

          {save child time}
          _AddChildProc(iaProc, iaChildProc, iResult, iChildChildTime);
          iChildTime := iChildTime + iResult;

          {save child calls in an array, so we can process parent calls for them}
          inc(iChildCalls);
          if length(iaChildCalls) <= iChildCalls then
            SetLength(iaChildCalls, iChildCalls + 25);
          iaChildCalls[iChildCalls-1] := iaChildProc;
        end;

        {calc total call time}
        iEndTime   := pta[iProfIndex].Time;
        //check end time
        if (iEndtime <= 0) then
        begin
          //iEndTime   := pta[iProfIndex-1].Time;        //get prev. time
          iEndTime   := ProfileRun.ProfileStopCycle;     //get stop time

//          if (iEndtime < 0) then
//            MessageDlg('Error: iEndtime < 0', mtError, [mbOK], 0);

          if iProfIndex >= high(pta) then
          begin
            ppta := __GetNextTimeArray;
            if (ppta = nil) or (ppta^ = nil) then
            begin
              //
            end
            else
            begin
              pta  := ppta^;
              iEndTime   := pta[iProfIndex].Time;   //get next block time
            end;
          end;

          if (iEndtime <= 0) then
          begin
            if iProfIndex > 0 then
            begin
              mmoErrorsAndWarnings.Lines.Add('Error: iEndtime <= 0! Value = ' + IntToStr(iEndtime) + '. Using previous value as endtime');
            end
            else
            begin
              mmoErrorsAndWarnings.Lines.Add('Error: iEndtime <= 0! Value = ' + IntToStr(iEndtime) + '. No previous value, exit.');
              //iEndTime := 0;
              exit;
            end;
          end;
        end;

        //own time
        if chkOverheadCompensation.Checked then
        begin
          //correct extra overhead
          iOverhead  := int64(aCallCount) * int64(ProfileRun.ProfileOverhead);
          iTotalTime := (iEndTime - iStartTime) - iChildTime - iOverhead;
        end
        else
        begin
          iTotalTime := (iEndTime - iStartTime) - iChildTime;
          iOverhead  := 0;
        end;

        inc(aCallCount);

        if iTotalTime < 0 then
        begin
          //MessageDlg('Error: iTotalTime < 0!', mtError, [mbOK], 0);
          {
          mmoErrorsAndWarnings.Lines.Add(format('Error: own iTotalTime < 0!' + #13#10 +
              'Endtime(%d) - StartTime(%d) - ChildTime(%d) - Overhead(%d) = %d',
              [iEndTime, iStartTime, iChildTime, iOverhead, iTotalTime]));
          }
          iTotalTime := 0;
        end;
        aChildChildTime := iChildTime;

        {save results}
        if (iaProc >= 0) and
             //check if end record is not corrupt, otherwise do not store
           ( ((pta[iProfIndex].ProfileType = ptLeave)) //and
              //(pta[iProfIndex].Address = pointer(cAddress)))
             or (pta[iProfIndex].Time = 0)  //end of profiling
           ) then
        begin
          bSkip := (pta[iProfIndex].Address <> pointer(cAddress));
          with FProfiledProcArray[iaProc]^ do
          begin
            inc(CallCount);
            TotalChildTime      := TotalChildTime + iChildTime;
            TotalOwnProfileTime := TotalOwnProfileTime + (iTotalTime);
            {detailed history of all times}
            if length(AllOwnProfileTimes) <= CallCount then
              SetLength(AllOwnProfileTimes, CallCount+25);
            AllOwnProfileTimes[CallCount-1]   := iTotalTime;
            if length(AllChildProfileTimes) <= CallCount then
              SetLength(AllChildProfileTimes, CallCount+25);
            AllChildProfileTimes[CallCount-1] := iChildTime;

            UnitIndex := iaUnit;
          end;

          for i := 0 to iChildCalls - 1 do
          begin
            _AddParentProc(iaProc, iaChildCalls[i], iTotalTime, iChildTime);
          end;

          {total unit time}
          if (iaUnit >= 0) and (iaProc >= 0) then
          begin
            iResult := FProfiledUnitArray[iaUnit].TotalProfileTime;
            iResult := iResult + iTotalTime;
            FProfiledUnitArray[iaUnit].TotalProfileTime := iResult;
          end;
        end
        //can occur in case of exceptions: dangling enter with no proper leave
        else
          if pta[iProfIndex].Time <> 0 then   //just end of profiling
            bSkip := True;
//            MessageDlg('Error: unknown procedure or not leaving or not the same address', mtError, [mbOK], 0);
            //debugbreak;

        iTotalCycles := iTotalCycles + iTotalTime;
        if chkOverheadCompensation.Checked then
          iTotalTime   := iEndTime - iStartTime - iOverhead
        else
          iTotalTime   := iEndTime - iStartTime;

        if iTotalTime < 0 then
        begin
          //MessageDlg('Error: iTotalTime < 0!', mtError, [mbOK], 0);
          {
          mmoErrorsAndWarnings.Lines.Add(format('Error: total iTotalTime < 0!' + #13#10 +
              'Endtime(%d) - StartTime(%d) - Overhead(%d) = %d',
              [iEndTime, iStartTime, iOverhead, iTotalTime]));
          }
          iTotalTime := 0;
        end;
      end;

      Result := iTotalTime;
    end
    else
      MessageDlg('Error: a record found with profiled address = null!', mtError, [mbOK], 0);

    //if Result < 0 then sleep(0);   //dummy to be able to set breakpoint
    {$ifdef WriteFullDebugTrace}
    //test output
    strTestOutput.Add( Format('L: %d - P: %s - T: %s - I: %d',[
        aLevel, FProfiledProcArray[iaProc]^.ProcName, 'Leave', iProfIndex {pta[iProfIndex].Time}]));
    {$endif WriteFullDebugTrace}

    {next result}
    if iProfIndex < high(pta) then
    begin
//      if (pta[iProfIndex].ProfileType <> ptLeave) then
//        MessageDlg('Error: pta[iProfIndex].ProfileType <> ptLeave!', mtError, [mbOK], 0);

      if not bSkip then
        inc(iProfIndex);
      inc(aProfileRecords);
    end
    else
    //next time array
    begin
      ppta := __GetNextTimeArray;
      if ppta = nil then Exit;
      pta  := ppta^;
      if pta = nil then Exit;
    end;
  end;

begin
  ClearProfileArrays;
  iTotalCycles    := 0;
  aProfileRecords := 0;
  iTotalCallCount := 0;
  //tempstack := TStack.Create;

  {$ifdef WriteFullDebugTrace}
  strTestOutput := TStringList.Create;
  strTestOutput.Capacity := 10000;
  {$endif WriteFullDebugTrace}

  try
    iArrayIndex := -1;

    if FMainMapFile <> nil then
    if (length(aProfileRunArray) > 0) then
    while iArrayIndex < high(aProfileRunArray) do
    begin
      ppta := __GetNextTimeArray;
      if ppta = nil then Continue;
      pta  := ppta^;
      if pta = nil then Continue;

      while (iProfIndex < high(pta)) and
            (pta[iProfIndex].ProfileType <> _uProfileTypes.ptNone) do
      begin
        __ProcessResult(1, iChildProcIndex, iChildChildTime, iCallCount);
        iTotalCallCount := iTotalCallCount + iCallCount;
      end;
    end;

    {
    for iArrayIndex := low(aProfileRunArray) to high(aProfileRunArray) do
    begin
      pta := aProfileRunArray[iArrayIndex].ProfileTime;
      if pta = nil then break;

      if aProfileRunArray[iArrayIndex].ThreadID = aThreadID then
      begin
        iProfIndex := 0;
        while (iProfIndex < high(pta)) and
              (pta[iProfIndex].ProfileType <> _uProfileTypes.ptNone) do
        begin
          __ProcessResult(1, iChildProcIndex, iChildChildTime);
          //inc(iProfIndex);
        end;
      end;
    end;
    }
  except
    on e:exception do
      MessageDlg(e.Message, mtError, [mbOK], 0);
  end;

  {$ifdef WriteFullDebugTrace}
  strTestOutput.SaveToFile('c:\test-output.txt');
  strTestOutput.Free;
  {$endif WriteFullDebugTrace}

  //tempstack.Free;
  //aProfileRecords := iProfIndex;
  Result := iTotalCycles;
  aTotalCallCount := iTotalCallCount;
end;

procedure TfrmResults.btnCompleteTraceExecuteClick(Sender: TObject);
begin
  FullTraceDump;
end;

procedure TfrmResults.btnRefreshClick(Sender: TObject);
var
  iThreadID, iProfileRecords, iTotalCallCount: integer;
  i,j:integer;
  iStartStopTime: int64;
  //sStartStopTime: string;
  iFirst, iLast, iFirstLast, iOverhead: int64;

  procedure __GetFirstLast(const aProfileTimes:TProfileThreadSlotArray; const aThreadid: Cardinal;
                           out aFirst, aLast: int64);
  var i,j:integer;
  begin
    aFirst := -1;
    aLast  := -1;

    for i := low(aProfileTimes) to high(aProfileTimes) do
    begin
      if aProfileTimes[i].ThreadID = aThreadID then
      begin
        if aFirst < 0 then
          aFirst := aProfileTimes[i].ProfileTime[0].Time;

        for j := low(aProfileTimes[i].ProfileTime) to high(aProfileTimes[i].ProfileTime) do
        begin
          if aProfileTimes[i].ProfileTime[j].Time > 0 then
            aLast := aProfileTimes[i].ProfileTime[j].Time;   
        end;
      end;
    end;
  end;

begin
  mmoProfileTimes.Clear;
  mmoErrorsAndWarnings.Clear;
  if cmbxThread.ItemIndex = -1 then exit;

  iThreadID    := Integer(cmbxThread.Items.Objects[cmbxThread.ItemIndex]);
  //ProcessProfileTimes!
  FTotalCycles := ProcessProfileTimes(Profilerun.ProfileTimes, iThreadID, iProfileRecords, iTotalCallCount);
  if FTotalCycles = 0 then FTotalCycles := 1;

  mmoInfoThread.Clear;
  mmoInfoThread.Lines.Add( Format('Thread id   : %d',[iThreadID]) );
  mmoInfoThread.Lines.Add( '' );
  iStartStopTime := ProfileRun.ProfileStopCycle - ProfileRun.ProfileStartCycle;
  if iStartStopTime <= 0 then iStartStopTime := 1;
  mmoInfoThread.Lines.Add( Format('Start/stop cycles: %d',[iStartStopTime]) );
  {
  tDiff := ProfileRun.ProfileStopTime - ProfileRun.ProfileStartTime;
  if tDiff < 1 then
    sStartStopTime := TimeToStr(tDiff)
  else
    sStartStopTime := format('%d days, %s',[Trunc(tDiff), TimeToStr(tDiff)]);
  }
  //mmoInfoThread.Lines.Add( Format('Start/stop time  : %s',[ sStartStopTime]))
  mmoInfoThread.Lines.Add( Format('Start/stop time  : %s',[trim(CalcTimeDiff(iStartStopTime))]));

  __GetFirstLast(Profilerun.ProfileTimes, iThreadID, iFirst, iLast);
  iFirstLast := iLast - iFirst;
  if iFirstLast <= 0 then iFirstLast := 1;
  mmoInfoThread.Lines.Add( Format('First/last cycles: %d',[iFirstLast]) );
  mmoInfoThread.Lines.Add( Format('First/last time  : %s',[trim(CalcTimeDiff(iFirstLast))]));
  mmoInfoThread.Lines.Add( '' );
  mmoInfoThread.Lines.Add( Format('Profile records: %d',[iProfileRecords]) );
  mmoInfoThread.Lines.Add( Format('Profile size   : %2.2fMb',[ (iProfileRecords * sizeof(TProfileTimeRecord)) / (1024*1024) ]) );
  mmoInfoThread.Lines.Add( Format('Profile cycles : %d',[FTotalCycles]) );
  mmoInfoThread.Lines.Add( Format('Profile time   : %s',[trim(CalcTimeDiff(FTotalCycles))]));
  mmoInfoThread.Lines.Add( Format('Profiled calls : %d',[iTotalCallCount]));

  mmoInfoThread.Lines.Add( Format('Time profiled  : %2.2f',[ (FTotalCycles*100)/iFirstLast]) + '%' +
                             Format(' of %s (first/last)',[trim(CalcTimeDiff(iFirstLast))]));
  mmoInfoThread.Lines.Add( Format('Time profiled  : %2.2f',[ (FTotalCycles*100)/iStartStopTime]) + '%' +
                             Format(' of %s (start/stop)',[trim(CalcTimeDiff(iStartStopTime))]));
  mmoInfoThread.Lines.Add( '' );
  mmoInfoThread.Lines.Add( format('Overhead cycles: %d',[ ProfileRun.ProfileOverhead]) );
  mmoInfoThread.Lines.Add( format('Overhead time  : %s',[ trim(CalcTimeDiff(ProfileRun.ProfileOverhead))]) );
  //iFirst    := (iProfileRecords div 2)
  //iOverhead := (iProfileRecords div 2) * ProfileRun.ProfileOverhead;
  iOverhead := int64(iProfileRecords div 2) * int64(ProfileRun.ProfileOverhead);
  mmoInfoThread.Lines.Add( format('Total overhead : %s (%2.2f',[
      trim(CalcTimeDiff(iOverhead)), (iOverhead*100)/FTotalCycles]) + '%)');
  mmoInfoThread.Lines.Add( '' );
  mmoInfoThread.Lines.Add( format('Clock type    : %s',[ ProfileRun.ProfileClockType]) );
  mmoInfoThread.Lines.Add( format('Clock speed   : %2.3f Mhz',[ ProfileRun.CPUSpeed / (1000*1000)]) );
  mmoInfoThread.Lines.Add('-----------------------------------');
  mmoInfoThread.Lines.Add('');

  mmoProfileTimes.Lines.BeginUpdate;
  for i := 0 to Length(FProfiledUnitArray)-1 do
  with FProfiledUnitArray[i] do
  begin
    mmoProfileTimes.Lines.Add( Format('Unit: %s - Procs: %d - Total time: %s - Procent: %3.2f',
        [UnitName, ProcsCount, CalcTimeText(TotalProfileTime),
         (TotalProfileTime * 100) / FTotalCycles]) );

    for j := 0 to ProcsCount-1 do
    with FProfiledUnitArray[i].ProfiledProcs[j]^ do
    begin
      mmoProfileTimes.Lines.Add( Format(' - Proc: %s - Calls: %d - Own time: %s - Child time: %s - Procent: %3.2f',
        [ProcName, CallCount, CalcTimeText(TotalOwnProfileTime), CalcTimeText(TotalChildTime),
        (TotalOwnProfileTime * 100) / FTotalCycles]) );
    end;
  end;

  mmoProfileTimes.Lines.EndUpdate;

  vtreeItems.Clear;
  vtreeItems.RootNodeCount := Length(FProfiledProcArray);

  framProfileResultTree1.TotalCycles       := FTotalCycles;
  framProfileResultTree1.ProfiledUnitArray := FProfiledUnitArray;
  framProfileResultTree1.Profilerun        := self.ProfileRun;

  vtreeItems.SortTree(6, sdDescending);                        //sort on time+childtimes
  framProfileResultTree1.vtreeUnit.SortTree(5, sdDescending);  //first sort on unit%
  framProfileResultTree1.vtreeUnit.SortTree(6, sdDescending);  //second incl childtimes

  if mmoErrorsAndWarnings.Lines.Count > 0 then
    MessageDlg('Some errors and/or warnings found during result processing!'+#13+#10+'See tab "Errors && Warnings" for details.', mtWarning, [mbOK], 0);
end;

function TfrmResults._SearchUnitResultRecord(const aUnitMapIndex:integer): integer;
var
  i:integer;
begin
  Result := -1;
  if aUnitMapIndex < 0 then exit;
  if FMainMapFile = nil then exit;

  if Length(FProfiledUnitArray)>0 then
  for i := 0 to Length(FProfiledUnitArray)-1 do
  begin
    if FProfiledUnitArray[i].UnitSegmentRecord = @FMainMapFile.UnitNames[aUnitMapIndex] then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result = -1 then
  begin
    Result := Length(FProfiledUnitArray);
    //if length(FProfiledUnitArray) <= Result then
      //SetLength( FProfiledUnitArray, Result+25);
    SetLength( FProfiledUnitArray, Result+1);
    FProfiledUnitArray[Result].UnitName   := String(FMainMapFile.UnitNames[aUnitMapIndex].UnitName);
    FProfiledUnitArray[Result].UnitSegmentRecord := @FMainMapFile.UnitNames[aUnitMapIndex];

    FProfiledUnitArray[Result].TotalProfileTime := 0;
  end;
end;

function TfrmResults._SearchProcResultRecord(
  const aUnitIndex, aProcMapIndex: integer): integer;
var
  i:integer;
begin
  {proc record}
  Result := -1;
  if aUnitIndex < 0 then exit;
  if aProcMapIndex < 0 then exit;
  if FMainMapFile = nil then exit;


  if Length(FProfiledProcArray)>0 then
  for i := 0 to Length(FProfiledProcArray)-1 do
  begin
    if FProfiledProcArray[i].ProcNameRecord = @FMainMapFile.ProcedureNames[aProcMapIndex] then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result = -1 then
  begin
    Result := Length(FProfiledProcArray);
    SetLength( FProfiledProcArray, Result+1);
    FProfiledProcArray[Result] := _NewProfiledProcRecord;
    with FProfiledProcArray[Result]^ do
    begin
      ProcName       := String(FMainMapFile.ProcedureNames[aProcMapIndex].ProcName);
      ProcNameRecord := @FMainMapFile.ProcedureNames[aProcMapIndex];
    end;

    with FProfiledUnitArray[aUnitIndex] do
    begin
      inc(ProcsCount);
      if length(ProfiledProcs) <= ProcsCount then
        SetLength( ProfiledProcs, ProcsCount+25);
      ProfiledProcs[ProcsCount-1] := FProfiledProcArray[Result];
    end;
  end;

end;

procedure TfrmResults.vtreeItemsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  if Assigned(Data) then
  begin
    Data.UnitName     := '';
    Data.ClassName    := '';
    Data.FunctionName := '';
  end;
end;

procedure TfrmResults.vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
var
  Data: PMyRec;
begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  //if Sender.GetNodeLevel(Node) = 0 then
  begin
    //ps := Data.Data;
    case Column of
      0: CellText := Data.UnitName;
      1: CellText := Data.ClassName;
      2: CellText := Data.FunctionName;
      3: CellText := IntToStr(Data.Calls);
      //
      4: CellText := Format('%3.2f',[Data.UnitProcent]);
      5: CellText := Format('%3.2f',[Data.TotalProcent]);
      6: CellText := Format('%3.2f',[Data.TotalChildProcent]);
      //
      7: CellText := CalcTimeText(Data.TotalTime);
      8: CellText := CalcTimeText(Data.TotalChildTime);
      9: CellText := CalcTimeText(Data.TotalWithChildTime);

      10: CellText := CalcTimeText(Data.AvgTime);
      11: CellText := CalcTimeText(Data.AvgWithChildTime);
    else ; end;
  end;
end;

procedure TfrmResults.vtreeItemsCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1  : PMyRec;
  Data2  : PMyRec;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
     0: Result := CompareText( Data1.UnitName, Data2.UnitName );
     1: Result := CompareText( Data1.ClassName, Data2.ClassName );
     2: Result := CompareText( Data1.FunctionName, Data2.FunctionName );
     //
     3: Result := CompareValue( Data1.Calls, Data2.Calls );
     4: Result := CompareValue( Data1.UnitProcent, Data2.UnitProcent );
     5: Result := CompareValue( Data1.TotalProcent, Data2.TotalProcent );
     6: Result := CompareValue( Data1.TotalChildProcent, Data2.TotalChildProcent );
     //
     7: Result := CompareValue( Data1.TotalTime, Data2.TotalTime );
     8: Result := CompareValue( Data1.TotalChildTime, Data2.TotalChildTime );
     9: Result := CompareValue( Data1.TotalWithChildTime, Data2.TotalWithChildTime );

     10: Result := CompareValue( Data1.AvgTime, Data2.AvgTime );
     11: Result := CompareValue( Data1.AvgWithChildTime, Data2.AvgWithChildTime );
  end;
end;

procedure TfrmResults.vtreeItemsDblClick(Sender: TObject);
var
  Data: PMyRec;
  d: PUnitRec;
  n,ns,Node: PVirtualNode;
begin
  Node := vtreeItems.GetFirstSelected;
  if Node = nil then exit;
  Data := vtreeItems.GetNodeData(Node);

  if Assigned(Data) then
  with framProfileResultTree1.vtreeUnit do
  begin
    n := GetFirst;
    while n <> nil do
    begin
      d := GetNodeData(n);
      //search unit
      if d.UnitName = Data.UnitName then
      begin
        ns := n;
        while ns <> nil do
        begin
          d := GetNodeData(ns);
          //search proc
          if (d.FunctionName = Data.FunctionName) and
             (GetNodeLevel(ns) < 2) then               //if level 2 -> search next
          begin
            Selected[ns]     := True;
            FullyVisible[ns] := True;
            ScrollIntoView(ns, True);
            pgProfileTimes.ActivePage := tsUnitOverview;
            Exit;
          end;
          ns := GetNextSibling(ns);
        end;
      end;
      n := GetNext(n);
    end;
  end;
end;


procedure TfrmResults.vtreeItemsHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
begin
  if (vtreeItems.Header.SortColumn <> HitInfo.Column) then
  begin
    vtreeItems.Header.SortColumn := HitInfo.Column;
    if HitInfo.Column >= 3 then    //calls, % etc default DESC
      vtreeItems.Header.SortDirection := sdDescending
    else                   //unit names etc default ASC
      vtreeItems.Header.SortDirection := sdAscending
  end
  else if (vtreeItems.Header.SortDirection = sdDescending) then
    vtreeItems.Header.SortDirection := sdAscending
  else
    vtreeItems.Header.SortDirection := sdDescending;

  vtreeItems.SortTree( HitInfo.Column, vtreeItems.Header.SortDirection );
end;

procedure TfrmResults.vtreeItemsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PMyRec;
//  iUnitIndex,
//  iRevIndex:integer;
//  ps: PJclMapSegmentExt;

  function __ExtractClassName(const aFullFunction: string):string;
  //var ipos, ipos2: Integer;
  begin
    //System.SysUtils.TClass.Test
    Result := ExtractClassName(aFullFunction);
//    ipos := 1;
//    while ipos > 0 do
//    begin
//      ipos  := PosEx('.', aFullFunction, ipos+1);
//      ipos2 := PosEx('.', aFullFunction, ipos+1);
//      if PosEx('.', aFullFunction, ipos2+1) <= 0 then  //last one?
//        Result := Copy(aFullFunction, ipos+1, ipos2 - ipos - 1);        //TClass
//    end;
  end;
  function __ExtractFunctionName(const aFullFunction: string):string;
  //var ipos: Integer;
  begin
    //System.SysUtils.TClass.Test
    Result := ExtractFunctionName(aFullFunction);
//    ipos := 1;
//    while ipos > 0 do
//    begin
//      ipos   := PosEx('.', aFullFunction, ipos+1);
//      if ipos > 0 then
//        Result := Copy(aFullFunction, ipos+1, Length(aFullFunction));   //Test
//    end;
  end;

begin
  try
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.
    //Data.UnitName     := FProfiledProcArray[Node.Index].UnitRecord.UnitName;
    //Data.UnitName     := FProfiledProcArray[Node.Index].UnitName;
    if (NativeUInt(Length(FProfiledProcArray)) <= Node.Index) or
       (Length(FProfiledUnitArray) <= FProfiledProcArray[Node.Index].UnitIndex) or
       (FProfiledProcArray[Node.Index].UnitIndex < 0) then exit;

    Data.UnitName     := FProfiledUnitArray[FProfiledProcArray[Node.Index].UnitIndex].UnitName;
    Data.ClassName    := __ExtractClassName( FProfiledProcArray[Node.Index].ProcName );
    Data.FunctionName := __ExtractFunctionName( FProfiledProcArray[Node.Index].ProcName );
    Data.ProfiledProc := FProfiledProcArray[Node.Index];

    if Data.ProfiledProc <> nil then
    with Profilerun, Data.ProfiledProc^ do
    begin
      Data.Calls             := Data.ProfiledProc.CallCount;
      //Data.UnitProcent       := (TotalOwnProfileTime * 100) / Data.ProfiledProc.UnitRecord.TotalProfileTime;
      if FProfiledUnitArray[FProfiledProcArray[Node.Index].UnitIndex].TotalProfileTime > 0 then
        Data.UnitProcent       := (TotalOwnProfileTime * 100) /
                                  FProfiledUnitArray[FProfiledProcArray[Node.Index].UnitIndex].TotalProfileTime;
                                //Data.ProfiledProc.UnitRecord.TotalProfileTime;
      Data.TotalProcent      := (TotalOwnProfileTime * 100) / FTotalCycles;
      Data.TotalChildProcent := ((TotalChildTime + TotalOwnProfileTime) * 100) / FTotalCycles;

      if TotalOwnProfileTime > 0 then
      begin
        Data.TotalTime           := TotalOwnProfileTime / CPUSpeed;
        Data.TotalChildTime      := TotalChildTime/ CPUSpeed;
        Data.TotalWithChildTime  := (TotalChildTime + TotalOwnProfileTime) / CPUSpeed;

        Data.AvgTime           := Data.TotalTime / Data.Calls;
        Data.AvgWithChildTime  := Data.TotalWithChildTime / Data.Calls;
      end
    end;
  end;
  except
    on e:exception do
      sleep(0);
  end;
end;

procedure TfrmResults.vtreeItemsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);

  {
  if Node.Parent = Sender.RootNode then
     TargetCanvas.Font.Style := [fsUnderline]
  else if Sender.GetNodeLevel(Node) = 2 then
     //TargetCanvas.Font.sStyle := [fsItalic];
     TargetCanvas.Font.Size := TargetCanvas.Font.Size - 1;
  }

  if Column in [5,7] then
  begin
    if (Data.TotalProcent > 50)then
      TargetCanvas.Font.Color := clRed
    else if (Data.TotalProcent > 10)then
      TargetCanvas.Font.Color := $000080FF;   //orange
  end;

  if Column in [6,9] then
  begin
    if (Data.TotalChildProcent > 50)then
      TargetCanvas.Font.Color := clRed
    else if (Data.TotalChildProcent > 10)then
      TargetCanvas.Font.Color := $000080FF;   //orange
  end;
end;

procedure TfrmResults.FormCreate(Sender: TObject);
begin
  pgProfileTimes.ActivePageIndex := 0;

  // Let the tree know how much data space we need.
  vtreeItems.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  vtreeItems.RootNodeCount := 0;

  edtCalls.Text := '';
  edtParentCalls.Text := '';
  edtChildCalls.Text  := '';
  edtMinOwnTime.Text  := '';
  edtMaxOwnTime.Text  := '';
  edtAvgOwnTime.Text  := '';
  edtMinChildTime.Text  := '';
  edtMaxChildTime.Text  := '';
  edtAvgChildTime.Text  := '';
  edtMinTotalTime.Text  := '';
  edtMaxTotalTime.Text  := '';
  edtAvgTotalTime.Text  := '';
  edtTotalOwnTime.Text    := '';
  edtTotalChildTime.Text  := '';
  edtTotalTotalTime.Text  := '';

  FProfiledUnitProcList := TGpIntegerObjectList.Create(False);

  tsText.TabVisible := False;
  tsCompleteTraceText.TabVisible := False;
end;

function TfrmResults._SearchAndAddCustomProc(
  const aDebugInfo:TDebugInfoStorage;
  const aUnitIndex: integer; const aAddr: Cardinal): integer;
var
  iCustomUnitIndex,
  iCustomProcIndex,
  i,j:integer;
  cAddr: Cardinal;
  aUnits: PSelectedUnitArray;
begin
  {proc record}
  Result := -1;
  iCustomProcIndex := -1;
  //iCustomUnitIndex := -1;
  if aUnitIndex < 0 then exit;
  cAddr  := aAddr - aDebugInfo.Offset;
  //cAddr  := aAddr;
  aUnits := aDebugInfo.SelectedUnitProcedures;

  if Length(FProfiledProcArray)>0 then
  for i := 0 to Length(FProfiledProcArray)-1 do
  begin
    if (FProfiledProcArray[i].ProcNameRecord <> nil) and
       (FProfiledProcArray[i].ProcNameRecord.Addr = cAddr) then
    begin
      Result := i;
      Break;
    end;
  end;

  if Result = -1 then
  begin

    iCustomUnitIndex := -1;
    for i := 0 to Length(aUnits^)-1 do
    begin
      if (aUnits^[i].UnitSegmentRecord <> nil) and
         (aUnits^[i].UnitSegmentRecord.StartAddr <= cAddr) and
         (aUnits^[i].UnitSegmentRecord.EndAddr >= cAddr) then
      begin
        iCustomUnitIndex := i;

        iCustomProcIndex := -1;
        if iCustomUnitIndex >= 0 then
        with aUnits^[iCustomUnitIndex] do
        for j := 0 to length(SelectedProcs)-1 do
        begin
          if (SelectedProcs[j].ProcNameRecord <> nil) and
             (SelectedProcs[j].ProcNameRecord.Addr = cAddr) then
          begin
            iCustomProcIndex := j;
            Break;
          end;
        end;

        if iCustomProcIndex >= 0 then
          Break;
      end;
    end;


    {
    with FProfiledUnitArray[aUnitIndex] do
        for j := 0 to length(SelectedProcs)-1 do
        begin
          if SelectedProcs[j].ProcNameRecord.Addr = cAddr then
          begin
            iCustomProcIndex := j;
            Break;
          end;
        end;
    }


    if iCustomProcIndex >= 0 then
    begin
      Result := Length(FProfiledProcArray);
      SetLength( FProfiledProcArray, Result+1);
      FProfiledProcArray[Result] := _NewProfiledProcRecord;
      with FProfiledProcArray[Result]^ do
      begin
        ProcName       := String(aUnits^[iCustomUnitIndex].SelectedProcs[iCustomProcIndex].ProcName);
        ProcNameRecord := aUnits^[iCustomUnitIndex].SelectedProcs[iCustomProcIndex].ProcNameRecord;
      end;
    end;

    if Result >= 0 then
    with FProfiledUnitArray[aUnitIndex] do
    begin
      inc(ProcsCount);
      if length(ProfiledProcs) <= ProcsCount then
        SetLength( ProfiledProcs, ProcsCount+ 25);
      ProfiledProcs[ProcsCount-1] := FProfiledProcArray[Result];
    end;
  end;
end;

function TfrmResults._SearchAndAddCustomUnit(
  const aDebugInfo:TDebugInfoStorage;
  const aAddr: Cardinal): integer;
var
  iCustomIndex,
  i:integer;
  cAddr: Cardinal;
  aUnits: PSelectedUnitArray;
begin
  Result := -1;
  cAddr  := aAddr - aDebugInfo.Offset;
  //cAddr  := aAddr;
  aUnits := aDebugInfo.SelectedUnitProcedures;

  if Length(FProfiledUnitArray)>0 then
  for i := 0 to Length(FProfiledUnitArray)-1 do
  begin
    if (FProfiledUnitArray[i].UnitSegmentRecord.StartAddr <= cAddr) and
       (FProfiledUnitArray[i].UnitSegmentRecord.EndAddr >= cAddr) then
    begin
      Result := i;
      Break;
    end;
  end;

  if Result = -1 then
  begin
    iCustomIndex := -1;
    for i := 0 to Length(aUnits^)-1 do
    begin
      if (aUnits^[i].UnitSegmentRecord <> nil) and
         (aUnits^[i].UnitSegmentRecord.StartAddr <= cAddr) and
         (aUnits^[i].UnitSegmentRecord.EndAddr >= cAddr) then
      begin
        iCustomIndex := i;
        Break;
      end;
    end;

    if iCustomIndex >= 0 then
    begin
      Result := Length(FProfiledUnitArray);
      SetLength( FProfiledUnitArray, Result+1);

      FProfiledUnitArray[Result].UnitName          := aUnits^[iCustomIndex].UnitName;
      FProfiledUnitArray[Result].UnitSegmentRecord := aUnits^[iCustomIndex].UnitSegmentRecord;
      FProfiledUnitArray[Result].TotalProfileTime  := 0;
    end;
  end;
end;

{
procedure TfrmResults.SetCPUSpeed(const Value: Extended);
begin
  FCPUSpeed := Value;
end;

procedure TfrmResults.SetCustomDebugInfo(const Value: TDebugInfoStorage);
begin
  FCustomDebugInfo := Value;
end;

procedure TfrmResults.SetInternalDebugInfo(const Value: TDebugInfoStorage);
begin
  FInternalDebugInfo := Value;
end;
}

function TfrmResults._SearchAndAddUnknownProc(const aUnitIndex: integer;
  const aAddr: Cardinal): integer;
var
//  iCustomUnitIndex,
  //iCustomProcIndex,
  i:integer;
  cAddr: Cardinal;
begin
  {proc record}
  Result := -1;
  if aUnitIndex < 0 then exit;
  cAddr  := aAddr;
  //iCustomProcIndex := -1;

  if Length(FProfiledProcArray)>0 then
  for i := 0 to Length(FProfiledProcArray)-1 do
  begin
    if FProfiledProcArray[i].ProcNameRecord.Addr = cAddr then
    begin
      Result := i;
      Break;
    end;
  end;

  if Result = -1 then
  begin
    //if iCustomProcIndex >= 0 then
    begin
      Result := Length(FProfiledProcArray);
      SetLength( FProfiledProcArray, Result+1);
      FProfiledProcArray[Result] := _NewProfiledProcRecord;

      with FProfiledProcArray[Result]^ do
      begin
        ProcName       := IntToStr(cAddr);
        new(ProcNameRecord);
        ProcNameRecord.Addr      := cAddr;
        ProcNameRecord.ProcName  := Ansistring(IntToStr(cAddr));
      end;
    end;

    if Result >= 0 then
    with FProfiledUnitArray[aUnitIndex] do
    begin
      inc(ProcsCount);
      if Length(ProfiledProcs) <= ProcsCount then
        SetLength( ProfiledProcs, ProcsCount+25);
      ProfiledProcs[ProcsCount-1] := FProfiledProcArray[Result];
    end;
  end;
end;

function TfrmResults._SearchAndAddUnknownUnit(
  const aAddr: Cardinal): integer;
var
//  iCustomIndex,
  i:integer;
  cAddr: Cardinal;
begin
  Result := -1;
  cAddr  := aAddr;

  if Length(FProfiledUnitArray)>0 then
  for i := 0 to Length(FProfiledUnitArray)-1 do
  begin
    if (FProfiledUnitArray[i].UnitSegmentRecord.StartAddr <= cAddr) and
       (FProfiledUnitArray[i].UnitSegmentRecord.EndAddr >= cAddr) then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result = -1 then
  if Length(FProfiledUnitArray)>0 then
  for i := 0 to Length(FProfiledUnitArray)-1 do
  begin
    if (FProfiledUnitArray[i].UnitName = 'Unknown') then
    begin
      Result := i;
      Break;
    end;
  end;

  if Result = -1 then
  begin
    //iCustomIndex := -1;
    //if iCustomIndex >= 0 then
    begin
      Result := Length(FProfiledUnitArray);
      SetLength( FProfiledUnitArray, Result+1);

      FUnknownUnitRecord.UnitName  := 'Unknown';
      FProfiledUnitArray[Result].UnitName          := 'Unknown';
      FProfiledUnitArray[Result].UnitSegmentRecord := @FUnknownUnitRecord;
      FProfiledUnitArray[Result].TotalProfileTime  := 0;
    end;
  end;
end;

procedure TfrmResults.ClearProfileArrays;
var
  i,j: Integer;
begin
  for i := low(FProfiledProcArray) to high(FProfiledProcArray) do
  begin
    for j := low(FProfiledProcArray[i]^.ChildProcs) to high(FProfiledProcArray[i]^.ChildProcs) do
    begin
      with FProfiledProcArray[i]^.ChildProcs[j]^ do
      begin
        ProcNameRecord := nil;
        ProcName       := '';
      end;
      FreeMem(FProfiledProcArray[i]^.ChildProcs[j]);
    end;
    SetLength(FProfiledProcArray[i]^.ChildProcs, 0);

    for j := low(FProfiledProcArray[i]^.ParentProcs) to high(FProfiledProcArray[i]^.ParentProcs) do
    begin
      with FProfiledProcArray[i]^.ParentProcs[j]^ do
      begin
        ProcNameRecord := nil;
        ProcName       := '';
      end;
      FreeMem(FProfiledProcArray[i]^.ParentProcs[j]);
    end;
    SetLength(FProfiledProcArray[i]^.ParentProcs, 0);

    FProfiledProcArray[i].ProcNameRecord := nil;
    FProfiledProcArray[i].ProcName       := '';
    FreeMem(FProfiledProcArray[i]);
  end;
  SetLength( FProfiledProcArray, 0);

  for i := low(FProfiledUnitArray) to high(FProfiledUnitArray) do
  begin
    FProfiledUnitArray[i].UnitSegmentRecord := nil;
    FProfiledUnitArray[i].UnitName          := '';
    SetLength( FProfiledUnitArray[i].ProfiledProcs, 0);
  end;
  SetLength( FProfiledUnitArray, 0);

  _ClearProfiledUnitProcList;
  FProfiledUnitProcList.Clear;
end;

procedure TfrmResults.cmbxThreadChange(Sender: TObject);
begin
  btnRefresh.Click;
end;

procedure TfrmResults._SearchAndAddUnitProc(const aAddr: Cardinal;
  out aUnitIndex, aProcIndex: integer);
var
  iIndex: integer;
  pr: PProfiledUnitProcListRecord;
  o: TObject;
begin
  if FMainMapFile = nil then exit;

  iIndex := FProfiledUnitProcList.IndexOf(aAddr);
  if iIndex >= 0 then
  begin
    o := FProfiledUnitProcList.Objects[iIndex];
    pr := PProfiledUnitProcListRecord(o);
    aUnitIndex := pr.UnitIndex;
    aProcIndex := pr.ProcIndex;
    exit;
  end;

  aUnitIndex := _SearchAndAddCustomUnit( FMainMapFile, aAddr );
  aProcIndex := -1;
  if aUnitIndex >= 0 then
    aProcIndex := _SearchAndAddCustomProc( FMainMapFile, aUnitIndex, aAddr );

  if (aUnitIndex < 0) or (aProcIndex < 0) then
  begin
    aUnitIndex := _SearchAndAddCustomUnit( FInternalDebugInfo, aAddr );
    if aUnitIndex >= 0 then
      aProcIndex := _SearchAndAddCustomProc( FInternalDebugInfo, aUnitIndex, aAddr )
  end;

  if (aUnitIndex < 0) or (aProcIndex < 0) then
  begin
    aUnitIndex := _SearchAndAddCustomUnit( FCustomDebugInfo, aAddr );
    if aUnitIndex >= 0 then
      aProcIndex := _SearchAndAddCustomProc( FCustomDebugInfo, aUnitIndex, aAddr )
  end;

  if (aUnitIndex < 0) or (aProcIndex < 0) then
  begin
    aUnitIndex := _SearchAndAddCustomUnit( FLoadedDllsDebugInfo, aAddr );
    if aUnitIndex >= 0 then
      aProcIndex := _SearchAndAddCustomProc( FLoadedDllsDebugInfo, aUnitIndex, aAddr )
  end;

  if (aUnitIndex < 0) or (aProcIndex < 0) then
  begin
    _FullSearchAndAddUnitProc( aAddr, aUnitIndex, aProcIndex);
  end;

  if (aUnitIndex < 0) or (aProcIndex < 0) then
  begin
    aUnitIndex := _SearchAndAddUnknownUnit(aAddr);
    aProcIndex := _SearchAndAddUnknownProc(aUnitIndex, aAddr);
  end;

  new(pr);
  pr.UnitIndex := aUnitIndex;
  pr.ProcIndex := aProcIndex;
  FProfiledUnitProcList.AddObject(aAddr,TObject(pr));
  FProfiledUnitProcList.Sorted := True;
end;

procedure TfrmResults._AddChildProc(const aProcIndex,
  aChildProcIndex: integer; const aChildTotalTime, aChildChildTime: int64);
var
  i:integer;
  iChildFound:integer;
  pChildRecord: PProfiledProc;
begin
  if aChildProcIndex < 0 then exit;  

  {save results}
  if aProcIndex >= 0 then
  with FProfiledProcArray[aProcIndex]^ do
  begin
    iChildFound := -1;

    for i := low(ChildProcs) to high(ChildProcs) do
    begin
      if ChildProcs[i].ProcNameRecord = FProfiledProcArray[aChildProcIndex].ProcNameRecord then
      begin
        iChildFound := i;
        break;
      end;
    end;

    if iChildFound < 0 then
    begin
      inc(ChildCount);
      iChildFound := Length(ChildProcs);
      SetLength(ChildProcs,iChildFound+1);
      pChildRecord := _NewProfiledProcRecord;
      ChildProcs[iChildFound] := pChildRecord;
    end
    else
      pChildRecord := ChildProcs[iChildFound];

    inc(pChildRecord.CallCount);
    pChildRecord.ProcName        := FProfiledProcArray[aChildProcIndex].ProcName;
    pChildRecord.ProcNameRecord  := FProfiledProcArray[aChildProcIndex].ProcNameRecord;
    pChildRecord.UnitIndex       := FProfiledProcArray[aChildProcIndex].UnitIndex;
    pChildRecord.TotalChildTime  := pChildRecord.TotalChildTime + aChildChildTime;
    pChildRecord.TotalOwnProfileTime := pChildRecord.TotalOwnProfileTime +
                                        (aChildTotalTime - aChildChildTime);
  end;
end;

procedure TfrmResults._AddParentProc(const aParentProcIndex,
  aChildProcIndex: integer; const aParentOwnTime, aParentChildTime: int64);
var
  i:integer;
  iParentFound:integer;
  pParentRecord: PProfiledProc;
begin
  if aParentProcIndex < 0 then exit;

  {save results}
  if aChildProcIndex >= 0 then
  with FProfiledProcArray[aChildProcIndex]^ do
  begin
    iParentFound := -1;

    for i := low(ParentProcs) to high(ParentProcs) do
    begin
      if ParentProcs[i].ProcNameRecord = FProfiledProcArray[aParentProcIndex].ProcNameRecord then
      begin
        iParentFound := i;
        break;
      end;
    end;

    if iParentFound < 0 then
    begin
      inc(ParentCount);
      iParentFound := Length(ParentProcs);
      SetLength(ParentProcs,iParentFound+1);
      pParentRecord := _NewProfiledProcRecord;
      ParentProcs[iParentFound] := pParentRecord;
    end
    else
      pParentRecord := ParentProcs[iParentFound];

    inc(pParentRecord.CallCount);
    with pParentRecord^ do
    begin
      ProcName        := FProfiledProcArray[aParentProcIndex].ProcName;
      ProcNameRecord  := FProfiledProcArray[aParentProcIndex].ProcNameRecord;
      UnitIndex       := FProfiledProcArray[aParentProcIndex].UnitIndex;
      TotalChildTime  := pParentRecord.TotalChildTime + aParentChildTime;
      TotalOwnProfileTime := pParentRecord.TotalOwnProfileTime + aParentOwnTime;
    end;
  end;
end;

procedure TfrmResults._ClearProfiledUnitArrayOfProcArray(
  const aUnitArray: TProfiledUnitArray);
begin
  if length(aUnitArray) > 0 then
  begin
    FreeMem(aUnitArray[0].UnitSegmentRecord);
    SetLength(aUnitArray[0].ProfiledProcs,0);
  end;
  //SetLength(aUnitArray,0);
end;

function TfrmResults._CreateProfiledUnitArrayOfProcArray(
  const aParentProcRecord: PProfiledProc;
  const aProcArray: TProfiledProcArray): TProfiledUnitArray;
var
  i:integer;
begin
  if aParentProcRecord = nil then exit;
  SetLength(Result,1);

  Result[0].UnitName          := aParentProcRecord.ProcName;
  Result[0].UnitSegmentRecord := new(PJclMapSegmentExt);
  Result[0].UnitSegmentRecord.UnitName := Ansistring(Result[0].UnitName);
  //Result[0].UnitSegmentRecord.StartAddr
  //Result[0].UnitSegmentRecord.EndAddr
  //Result[0].UnitSegmentRecord.UnitFile
  //Result[0].UnitSegmentRecord.Procs
  Result[0].ProcsCount        := Length(aProcArray);
  Result[0].TotalProfileTime  := aParentProcRecord.TotalOwnProfileTime + aParentProcRecord.TotalChildTime;
  SetLength(Result[0].ProfiledProcs,Length(aProcArray));

  for i := low(aProcArray) to high(aProcArray) do
  begin
    Result[0].ProfiledProcs[i] := aProcArray[i];
  end;
end;

function TfrmResults._NewProfiledProcRecord: PProfiledProc;
begin
  new(Result);
  with Result^ do
  begin
    ProcName       := '';
    ProcNameRecord := nil;
    ChildCount     := 0;
    CallCount      := 0;
    TotalOwnProfileTime := 0;
    TotalChildTime      := 0;

    ParentCount      := 0;
    ChildCount       := 0;
  end;
end;

procedure TfrmResults._ClearProfiledUnitProcList;
var
  pr: PProfiledUnitProcListRecord;
  i:integer;
begin
  for i := 0 to FProfiledUnitProcList.Count-1 do
  begin
    pr := PProfiledUnitProcListRecord(FProfiledUnitProcList.Objects[i]);
    FreeMem(pr);
    FProfiledUnitProcList.Objects[i] := nil;
  end;
end;

procedure TfrmResults._FullSearchAndAddUnitProc(const aAddr: Cardinal;
  out aUnitIndex, aProcIndex: integer);
var
  iProfUnit, iProfProc: integer;
  iUnit, iProc: integer;
  sUnit, sProc: string;
  pUnit: PJclMapSegmentExt;
  pProc: PJclMapProcNameExt;

  function __SearchUnitProc(const aInfofile: TDebugInfoStorage; out aUnitIndex, aProcIndex: integer): boolean;
  var pAddress: pointer;
  begin
    Result   := False;
    pAddress := pointer(aAddr);
    aUnitIndex := aInfofile.GetSegmentByFullAddress(aAddr);
    //aProcIndex := aInfofile.GetProcByFullAddress(pAddress);

    if aUnitIndex >= 0 then
    begin
      aProcIndex := aInfofile.GetProcByFullAddress(pAddress);
      if aProcIndex >= 0 then
      begin
        sProc  := String(aInfofile.ProcedureNames[aProcIndex].ProcName);
        pProc  := @aInfofile.ProcedureNames[aProcIndex];
        sUnit  := String(aInfofile.UnitNames[aUnitIndex].UnitName);
        pUnit  := @aInfofile.UnitNames[aUnitIndex];
        Result := True;
      end;
    end;
  end;

begin
  aUnitIndex := -1;
  aProcIndex := -1;
  iUnit := -1;
  iProc := -1;
  iProfUnit := -1;
  iProfProc := -1;

  if not __SearchUnitProc(FMainMapFile, iUnit, iProc) then
    if not __SearchUnitProc(FInternalDebugInfo, iUnit, iProc) then
      if not __SearchUnitProc(FCustomDebugInfo, iUnit, iProc) then
        if not __SearchUnitProc(FLoadedDllsDebugInfo, iUnit, iProc) then
          Exit;

  if iUnit >= 0 then
  begin
    iProfUnit := Length(FProfiledUnitArray);
    SetLength( FProfiledUnitArray, iProfUnit+1);

    FProfiledUnitArray[iProfUnit].UnitName          := sUnit;
    FProfiledUnitArray[iProfUnit].UnitSegmentRecord := pUnit;
    FProfiledUnitArray[iProfUnit].TotalProfileTime  := 0;
  end;

  if iProc >= 0 then
  begin
    iProfProc := Length(FProfiledProcArray);
    SetLength( FProfiledProcArray, iProfProc+1);
    FProfiledProcArray[iProfProc] := _NewProfiledProcRecord;
    with FProfiledProcArray[iProfProc]^ do
    begin
      ProcName       := sProc;
      ProcNameRecord := pProc;
    end;
  end;

  if (iUnit >= 0) and (iProc>= 0) then
  with FProfiledUnitArray[iProfUnit] do
  begin
    inc(ProcsCount);
    if length(ProfiledProcs) <= ProcsCount then
      SetLength( ProfiledProcs, ProcsCount+ 25);
    ProfiledProcs[ProcsCount-1] := FProfiledProcArray[iProfProc];

    aUnitIndex := iProfUnit;
    aProcIndex := iProfProc;
  end;
end;

procedure TfrmResults.framProfileResultParentsvtreeUnitDblClick(
  Sender: TObject);
var
  d,Data: PUnitRec;
  n,ns,Node: PVirtualNode;
begin
  with framProfileResultTree1.vtreeUnit do
  begin
    Node := framProfileResultParents.vtreeUnit.GetFirstSelected;
    if Node = nil then exit;

    Data := GetNodeData(Node);
    if Assigned(Data) then
    //if GetNodeLevel(Node) > 1 then
    begin
      n := GetFirst;
      while n <> nil do
      begin
        d := GetNodeData(n);
        //search unit
        if d.ClassName = Data.ClassName then
        begin
          ns := n;
          while ns <> nil do
          begin
            d := GetNodeData(ns);
            //search proc
            if (d.FunctionName = Data.FunctionName) then
               //(GetNodeLevel(ns) < 2) then               //if level 2 -> search next
            begin
              FullyVisible[ns] := True;
              Selected[ns]     := True;
              FocusedNode      := ns;
              ScrollIntoView(ns, True);
              Exit;
            end;
            ns := GetNextSibling(ns);
          end;
        end;
        n := GetNext(n);
      end;
    end;
  end;
end;

procedure TfrmResults.framProfileResultChildsvtreeUnitDblClick(
  Sender: TObject);
var
  d,Data: PUnitRec;
  n,ns,Node: PVirtualNode;
begin
  with framProfileResultTree1.vtreeUnit do
  begin
    Node := framProfileResultChilds.vtreeUnit.GetFirstSelected;
    if Node = nil then exit;

    Data := GetNodeData(Node);
    if Assigned(Data) then
    //if GetNodeLevel(Node) > 1 then
    begin
      n := GetFirst;
      while n <> nil do
      begin
        d := GetNodeData(n);
        //search unit
        if d.ClassName = Data.ClassName then
        begin
          ns := n;
          while ns <> nil do
          begin
            d := GetNodeData(ns);
            //search proc
            if (d.FunctionName = Data.FunctionName) then
               //(GetNodeLevel(ns) < 2) then               //if level 2 -> search next
            begin
              FullyVisible[ns] := True;
              Selected[ns]     := True;
              FocusedNode      := ns;
              ScrollIntoView(ns, True);
              Exit;
            end;
            ns := GetNextSibling(ns);
          end;
        end;
        n := GetNext(n);
      end;
    end;
  end;
end;

procedure TfrmResults.pgProfileTimesChange(Sender: TObject);
var
  iThreadID: integer;
begin
  if pgProfileTimes.ActivePage = tsTraceTree then
  begin
    if (cmbxThread.ItemIndex >= 0) then
    begin
      iThreadID    := Integer(cmbxThread.Items.Objects[cmbxThread.ItemIndex]);
      framProfileTraceTree1.ThreadID := iThreadID;
    end;

    framProfileTraceTree1.ProfileRun             := Profilerun;
    framProfileTraceTree1.ProfiledUnitProcList   := FProfiledUnitProcList;
    framProfileTraceTree1.FProfiledUnitArray     := FProfiledUnitArray;
    framProfileTraceTree1.FProfiledProcArray     := FProfiledProcArray;
    framProfileTraceTree1.TotalCycles            := Self.FTotalCycles;

    framProfileTraceTree1.ShowTrace(nil, 1, 0, 0);
  end;
end;

end.
