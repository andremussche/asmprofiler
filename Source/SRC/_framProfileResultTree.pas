unit _framProfileResultTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  _uProfileTypes, _uProfilerManager,
  Dialogs, VirtualTrees, Menus;

type
  TDetailedProfileInfo = record
    DataPresent: boolean;
    MinOwnTime, MaxOwnTime, AvgOwnTime: Int64;
    MinChildTime, MaxChildTime, AvgChildTime: Int64;
    MinTotalTime, MaxTotalTime, AvgTotalTime: Int64;
  end;

  PUnitRec = ^TUnitRec;
  TUnitRec = record
    UnitName,
    ClassName,
    FunctionName: String;
    //
    Calls: integer;
    UnitProcent, TotalProcent, TotalChildProcent: Single;
    TotalTime, TotalChildTime, TotalWithChildTime: Real;
    AvgTime, AvgChildTime, AvgTotalChildTime: Real;
    DetailedInfo: TDetailedProfileInfo;
    //
    ProfiledUnit: PProfiledUnit;
    ProfiledProc: PProfiledProc;
  end;

  TframProfileResultTree = class(TFrame)
    vtreeUnit: TVirtualStringTree;
    PopupMenu1: TPopupMenu;
    SavetoCSV1: TMenuItem;
    SavetoRTF1: TMenuItem;
    SavetoText1: TMenuItem;
    procedure vtreeUnitHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vtreeUnitInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtreeUnitGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtreeUnitFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeUnitCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtreeUnitPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vtreeUnitExpanding(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var Allowed: Boolean);
    procedure SavetoCSV1Click(Sender: TObject);
    procedure SavetoRTF1Click(Sender: TObject);
    procedure SavetoText1Click(Sender: TObject);
    procedure vtreeUnitHeaderDblClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
  private
    { Private declarations }
    //FProfiledProcArray: TProfiledProcArray;
    FProfiledUnitArray: TProfiledUnitArray;
    FTotalCycles: int64;
    FProfileRun: TProfilerun;
    //procedure SetProfiledProcArray(const Value: TProfiledProcArray);
    procedure SetProfiledUnitArray(const Value: TProfiledUnitArray);
    procedure SetTotalCycles(const Value: int64);
    procedure SetProfileRun(const Value: TProfilerun);
  public
    constructor Create(AOwner: TComponent); override;

    { Public declarations }
    property ProfiledUnitArray: TProfiledUnitArray read FProfiledUnitArray write SetProfiledUnitArray;
    //property ProfiledProcArray: TProfiledProcArray read FProfiledProcArray write SetProfiledProcArray;
    property TotalCycles: int64 read FTotalCycles write SetTotalCycles;

    property ProfileRun: TProfilerun read FProfileRun write SetProfileRun;
  end;

implementation

uses Math, StrUtils; //, _uAsmProfiler;

{$R *.dfm}

constructor TframProfileResultTree.Create(AOwner: TComponent);
begin
  inherited;
  
  // Let the tree know how much data space we need.
  vtreeUnit.NodeDataSize := SizeOf(TUnitRec);
  // Set an initial number of nodes.
  vtreeUnit.RootNodeCount := 0;
end;

{
procedure TframProfileResultTree.SetProfiledProcArray(const Value: TProfiledProcArray);
begin
  FProfiledProcArray := Value;
end;
}

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

procedure TframProfileResultTree.SavetoCSV1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeUnit.ContentToHTML(tstAll, 'Profiler Result HTML Export')) );
end;

procedure TframProfileResultTree.SavetoRTF1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeUnit.ContentToRTF(tstAll)) );
end;

procedure TframProfileResultTree.SavetoText1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeUnit.ContentToText(tstAll, ',')) );
end;

procedure TframProfileResultTree.SetProfiledUnitArray(const Value: TProfiledUnitArray);
begin
  FProfiledUnitArray := Value;

  vtreeUnit.Clear;
  vtreeUnit.RootNodeCount := Length(FProfiledUnitArray);
end;

procedure TframProfileResultTree.SetProfileRun(const Value: TProfilerun);
begin
  FProfileRun := Value;
end;

procedure TframProfileResultTree.SetTotalCycles(const Value: int64);
begin
  FTotalCycles := Value;
end;

procedure TframProfileResultTree.vtreeUnitCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1  : PUnitRec;
  Data2  : PUnitRec;
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
     11: Result := CompareValue( Data1.AvgTotalChildTime, Data2.AvgTotalChildTime );
  end;
end;

procedure TframProfileResultTree.vtreeUnitFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PUnitRec;
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

procedure TframProfileResultTree.vtreeUnitGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PUnitRec;

begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    //ps := Data.Data;
    case Column of
      0: CellText := Data.UnitName;
      1: CellText := '';
      2: CellText := IntToStr(Data.ProfiledUnit.ProcsCount);
      3: CellText := '';
      //
      4: CellText := Format('%3.2f',[Data.UnitProcent]);
      5: CellText := Format('%3.2f',[Data.TotalProcent]);
      6: CellText := '';
      //
      7: CellText := CalcTimeText(Data.TotalTime);
      8: CellText := '';
      9: CellText := '';

      10: CellText := '';
      11: CellText := '';
    else ; end;
  end
  else //if Sender.GetNodeLevel(Node) in [1,2] then
  begin
    //ps := Data.Data;
    case Column of
      //0: CellText := Data.UnitName;
      0: CellText := Data.FunctionName;
      1: CellText := Data.ClassName;
      //2: CellText := Data.FunctionName;
      2: CellText := '';
      3: CellText := IntToStr( Data.Calls );
      //
      4: CellText := Format('%3.2f',[Data.UnitProcent]);
      5: CellText := Format('%3.2f',[Data.TotalProcent]);
      6: CellText := Format('%3.2f',[Data.TotalChildProcent]);
      //
      7: CellText := CalcTimeText(Data.TotalTime);
      8: CellText := CalcTimeText(Data.TotalChildTime);
      9: CellText := CalcTimeText(Data.TotalWithChildTime);

      10: CellText := CalcTimeText(Data.AvgTime);
      11: CellText := CalcTimeText(Data.AvgTotalChildTime);
    else ; end;
  end;
end;

procedure TframProfileResultTree.vtreeUnitHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
begin
  if (vtreeUnit.Header.SortColumn <> HitInfo.Column) then
  begin
    vtreeUnit.Header.SortColumn := HitInfo.Column;
    if HitInfo.Column >= 3 then    //calls, % etc default DESC
      vtreeUnit.Header.SortDirection := sdDescending
    else                   //unit names etc default ASC
      vtreeUnit.Header.SortDirection := sdAscending
  end
  else if (vtreeUnit.Header.SortDirection = sdDescending) then
    vtreeUnit.Header.SortDirection := sdAscending
  else
    vtreeUnit.Header.SortDirection := sdDescending;  //default descending: higher values are more interresting

  vtreeUnit.SortTree( HitInfo.Column, vtreeUnit.Header.SortDirection );
end;

procedure TframProfileResultTree.vtreeUnitHeaderDblClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
//
end;

procedure TframProfileResultTree.vtreeUnitInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data,DataParent: PUnitRec;
  iTemp: int64;

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
//        Result := Copy(aFullFunction, ipos+1, Length(aFullFunction));  //Test
//    end;
  end;

begin
  try
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.
    if Sender.GetNodeLevel(Node) = 0 then
    begin
      Data.UnitName     := FProfiledUnitArray[Node.Index].UnitName;
      Data.ProfiledUnit := @FProfiledUnitArray[Node.Index];

      with Data.ProfiledUnit^ do
      begin
        //Data.UnitProcent       := 100;
        Data.TotalProcent      := (TotalProfileTime * 100) / FTotalCycles;
        Data.UnitProcent       := Data.TotalProcent;
        Data.TotalTime         := TotalProfileTime / Profilerun.CPUSpeed;
      end;
      ChildCount[Node] := Data.ProfiledUnit.ProcsCount;
      InvalidateToBottom(Node);
    end
    else if (Sender.GetNodeLevel(Node) >= 1) and
            (Sender.GetNodeLevel(Node) < 2) then
    begin
      DataParent := GetNodeData(Node.Parent);

      Data.UnitName     := DataParent.UnitName;
      Data.ProfiledUnit := DataParent.ProfiledUnit;
      Data.ProfiledProc := DataParent.ProfiledUnit.ProfiledProcs[Node.Index];
      if (Data.ProfiledProc = nil) then exit;
      Data.ClassName    := __ExtractClassName( Data.ProfiledProc.ProcName );
      Data.FunctionName := __ExtractFunctionName( Data.ProfiledProc.ProcName );

      with Profilerun, Data.ProfiledProc^ do
      begin
        Data.Calls             := CallCount;
        if Data.ProfiledUnit^.TotalProfileTime > 0 then
          Data.UnitProcent       := (TotalOwnProfileTime * 100) / Data.ProfiledUnit^.TotalProfileTime;
        Data.TotalProcent      := (TotalOwnProfileTime * 100) / FTotalCycles;
        Data.TotalChildProcent := ((TotalChildTime + TotalOwnProfileTime) * 100) / FTotalCycles;
        Data.TotalTime           := TotalOwnProfileTime / CPUSpeed;
        Data.TotalChildTime      := TotalChildTime/ CPUSpeed;
        Data.TotalWithChildTime  := (TotalChildTime + TotalOwnProfileTime) / CPUSpeed;

        if Data.TotalTime > 0 then
        begin
          Data.AvgTime           := Data.TotalTime / CallCount;
          Data.AvgChildTime      := Data.TotalChildTime / CallCount;
          Data.AvgTotalChildTime := Data.TotalWithChildTime / CallCount;
        end
      end;

      //if Sender.UpdateCount < 2 then
      ChildCount[Node] := Data.ProfiledProc.ChildCount;
      InvalidateToBottom(Node);
    end
    else if Sender.GetNodeLevel(Node) >= 2 then
    begin
      DataParent := GetNodeData(Node.Parent);
      if (NativeUInt(length(DataParent.ProfiledProc.ChildProcs)) <= Node.Index) or
         (length(FProfiledUnitArray) <= DataParent.ProfiledProc.ChildProcs[Node.Index].UnitIndex) then exit;

      Data.ProfiledUnit   := @FProfiledUnitArray[
                                 DataParent.ProfiledProc.ChildProcs[Node.Index].UnitIndex ];
      Data.UnitName       := Data.ProfiledUnit.UnitName;
      Data.ProfiledProc   := DataParent.ProfiledProc.ChildProcs[Node.Index];

      if (Data.ProfiledProc = nil) then exit;

      Data.ClassName    := __ExtractClassName( Data.ProfiledProc.ProcName );
      if Data.UnitName <> DataParent.UnitName then
        Data.FunctionName := Data.UnitName + '.'+ Data.ProfiledProc.ProcName;
      Data.FunctionName := __ExtractFunctionName( Data.ProfiledProc.ProcName );

      if (Data.ProfiledProc <> nil) and (FTotalCycles > 0) then
      with Profilerun, Data.ProfiledProc^ do
      begin
        Data.Calls             := CallCount;
        iTemp := DataParent.ProfiledProc^.TotalOwnProfileTime + DataParent.ProfiledProc^.TotalChildTime;
        if iTemp <> 0 then
          Data.UnitProcent       := (TotalOwnProfileTime * 100) / (iTemp);
        //Data.UnitProcent       := -1;
        Data.TotalProcent      := (TotalOwnProfileTime * 100) / FTotalCycles;
        Data.TotalChildProcent := ((TotalChildTime + TotalOwnProfileTime) * 100) / FTotalCycles;
        Data.TotalTime           := TotalOwnProfileTime / CPUSpeed;
        Data.TotalChildTime      := TotalChildTime/ CPUSpeed;
        Data.TotalWithChildTime  := (TotalChildTime + TotalOwnProfileTime) / CPUSpeed;

        if Data.TotalTime > 0 then
        begin
          Data.AvgTime            := Data.TotalTime / CallCount;
          Data.AvgChildTime       := Data.TotalChildTime / CallCount;
          Data.AvgTotalChildTime  := Data.TotalWithChildTime / CallCount;
        end;
      end;

      //ChildCount[Node] := Data.ProfiledProc.ChildCount;
      InvalidateToBottom(Node);
    end;
  end;
  except
    on e:exception do
      sleep(0);
  end;
end;

procedure TframProfileResultTree.vtreeUnitPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PUnitRec;
begin
  Data := Sender.GetNodeData(Node);

  if Node.Parent = Sender.RootNode then
     TargetCanvas.Font.Style := [fsUnderline]
  else if Sender.GetNodeLevel(Node) = 2 then
  begin
     //TargetCanvas.Font.sStyle := [fsItalic];
     TargetCanvas.Font.Size := TargetCanvas.Font.Size - 1;
     TargetCanvas.Font.Color := clDkGray;
  end;

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

procedure TframProfileResultTree.vtreeUnitExpanding(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
//var st: TVirtualNodeInitStates;
begin
  //if (Sender.GetNodeLevel(Node) >= 2) then
  {
  begin
    //Sender.ReinitNode(Node, false);
    st := [];
    vtreeUnitInitNode(vtreeUnit, Node.Parent, Node, st);
    Sender.InvalidateToBottom(Node);
  end;
  }
end;

end.
