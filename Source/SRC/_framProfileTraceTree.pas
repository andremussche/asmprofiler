unit _framProfileTraceTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, GpLists,
  _uProfileTypes,
  _uProfilerManager, Menus;

type
  TframProfileTraceTree = class(TFrame)
    vtreeTrace: TVirtualStringTree;
    PopupMenu1: TPopupMenu;
    Deletesmallitems1: TMenuItem;
    N1: TMenuItem;
    SavetoHTML1: TMenuItem;
    SavetoRTF2: TMenuItem;
    SavetoCSV2: TMenuItem;
    procedure vtreeTraceInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtreeTraceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtreeTraceFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtreeTracePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtreeTraceHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vtreeTraceCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure Deletesmallitems1Click(Sender: TObject);
    procedure SavetoHTML1Click(Sender: TObject);
    procedure SavetoRTF2Click(Sender: TObject);
    procedure SavetoCSV2Click(Sender: TObject);
  private
    FThreadID: NativeUInt;
    //FProfileThreadSlotArray: TProfileThreadSlotArray;
    FProfiledUnitProcList: TGpIntegerObjectList;
    FFProfiledUnitArray: TProfiledUnitArray;
    FFProfiledProcArray: TProfiledProcArray;
    FProfileRun: TProfileRun;
    FTotalCycles: int64;
    procedure SetThreadID(const Value: NativeUInt);
    procedure SetProfiledUnitProcList(const Value: TGpIntegerObjectList);
    procedure SetFProfiledProcArray(const Value: TProfiledProcArray);
    procedure SetFProfiledUnitArray(const Value: TProfiledUnitArray);
    procedure SetProfileRun(const Value: TProfileRun);
    procedure SetTotalCycles(const Value: int64);
    { Private declarations }
  protected
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure ShowTrace(aNode: PVirtualNode; aLevel, aArrayIndex, aProfileIndex: integer);

    property ProfileRun: TProfileRun read FProfileRun write SetProfileRun;
    //property ProfileThreadSlotArray: TProfileThreadSlotArray read FProfileThreadSlotArray write SetProfileThreadSlotArray;
    property ThreadID: NativeUInt read FThreadID write SetThreadID;
    property ProfiledUnitProcList: TGpIntegerObjectList read FProfiledUnitProcList write SetProfiledUnitProcList;
    property FProfiledUnitArray: TProfiledUnitArray read FFProfiledUnitArray write SetFProfiledUnitArray;
    property FProfiledProcArray: TProfiledProcArray read FFProfiledProcArray write SetFProfiledProcArray;
    property TotalCycles: int64 read FTotalCycles write SetTotalCycles;
  end;

  TProfiledUnitProcListRecord = record
    UnitIndex: integer;
    ProcIndex: integer;
  end;
  PProfiledUnitProcListRecord = ^TProfiledUnitProcListRecord;

  TTraceRecord = record
    ArrayIndex,
    ProfileIndex: integer;
    FunctionName: string;
    Level: integer;
    LevelChilds: integer;
    TotalLevelChilds: integer;
    Duration: Int64;
    Percentage: single;
  end;
  PTraceRecord = ^TTraceRecord;


implementation

uses Math;

{$R *.dfm}

{ TframProfileTraceTree }

constructor TframProfileTraceTree.Create(AOwner: TComponent);
begin
  inherited;
  // Let the tree know how much data space we need.
  vtreeTrace.NodeDataSize := SizeOf(TTraceRecord);
  // Set an initial number of nodes.
  vtreeTrace.RootNodeCount := 0;
end;

procedure TframProfileTraceTree.Deletesmallitems1Click(Sender: TObject);

  function __CheckChildNodes(aNode:PVirtualNode): PVirtualNode;
  var n, n2: PVirtualNode;
      data: PTraceRecord;
  begin
    Result := nil;
    if aNode = nil then Exit;

    //if aNode.ChildCount > 0 then
    //  n := aNode.FirstChild
    //else
    n := aNode;

    while n <> nil do
    begin
      data := vtreeTrace.GetNodeData(n);
      if (data <> nil) and
         (data.level = 0)
      then
        vtreeTrace.ValidateNode(n, True);

      data := vtreeTrace.GetNodeData(n);
      if (data <> nil) and
         (data.level > 0) and
         (data.Percentage < 1) then
      begin
        n2 := n;
        n  := n.NextSibling;
        vtreeTrace.DeleteNode(n2);
      end
      else
      begin
        n2 := n.FirstChild;
        __CheckChildNodes(n2);
        n := n.NextSibling;
      end;
    end;
  end;

var
  //data: PTraceRecord;
  node: PVirtualNode;
  
begin
  vtreeTrace.BeginUpdate;
  try
    node := vtreeTrace.RootNode.FirstChild;
    __CheckChildNodes(node);
  finally
    vtreeTrace.EndUpdate;
  end;
end;

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

procedure TframProfileTraceTree.SavetoCSV2Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeTrace.ContentToText(tstAll, ',')) );
end;

procedure TframProfileTraceTree.SavetoHTML1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeTrace.ContentToHTML(tstAll, 'Profiler Trace HTML Export')) );
end;

procedure TframProfileTraceTree.SavetoRTF2Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeTrace.ContentToRTF(tstAll)) );
end;

procedure TframProfileTraceTree.SetFProfiledProcArray(
  const Value: TProfiledProcArray);
begin
  FFProfiledProcArray := Value;
end;

procedure TframProfileTraceTree.SetFProfiledUnitArray(
  const Value: TProfiledUnitArray);
begin
  FFProfiledUnitArray := Value;
end;

procedure TframProfileTraceTree.SetProfiledUnitProcList(
  const Value: TGpIntegerObjectList);
begin
  FProfiledUnitProcList := Value;
end;

procedure TframProfileTraceTree.SetThreadID(const Value: NativeUInt);
begin
  FThreadID := Value;
end;

procedure TframProfileTraceTree.ShowTrace(aNode: PVirtualNode; aLevel, aArrayIndex, aProfileIndex: integer);
var
  iArrayIndex:integer;
  iProfIndex{, iChildProcIndex}:integer;
  pta: TProfileTimeArray;
  ppta: PProfileTimeArray;
  //iTotalCycles, iChildChildTime: int64;
  iIndex: Integer;

  function __GetNextTimeArray: PProfileTimeArray;
  begin
    Result := nil;

    while iArrayIndex < high(FProfileRun.ProfileTimes) do
    begin
      inc(iArrayIndex);
      if FProfileRun.ProfileTimes[iArrayIndex].ThreadID = ThreadID then
      begin
        iProfIndex := 0;
        Result := @(FProfileRun.ProfileTimes[iArrayIndex].ProfileTime);
        Break;
      end;
    end;
  end;

var
  iLevelChilds, iLevel, iLevelTotalChilds: integer;
  pNodeRoot: PVirtualNode;
  pData: PTraceRecord;
  o: TObject;
  pr: PProfiledUnitProcListRecord;
  iUnitIndex, iProcIndex: Integer;
  bFirst: boolean;
begin
  if vtreeTrace.Tag > 0 then Exit;
  vtreeTrace.Tag := 1;
  vtreeTrace.BeginUpdate;
  try
    pta := nil;

    //complete refresh?
    if (aNode = nil) then
    begin
      vtreeTrace.Clear;
    end;
    //set root
    pNodeRoot   := aNode;           //default nil

    (*
    //avoid recursive calls
    pData := vtreeTrace.GetNodeData(pNodeRoot);
    //if (pData <> nil) and (pData.FunctionName <> '') then Exit;  //already processed?
    if pNodeRoot <> nil then
    begin
      //pData := vtreeTrace.GetNodeData(pNodeRoot.Parent);
      if (pData = nil) or (pData.FunctionName = '') then Exit;   //parent not processed?
    end;
    *)

    iLevel := aLevel - 1;
    if iLevel < 0 then iLevel := 0;
    bFirst := True;
    iLevelTotalChilds := 0;
    iLevelChilds := 0;

    iArrayIndex := -1 + aArrayIndex;  //-1 because of __GetNextTimeArray
    if FProfileRun <> nil then
    while iArrayIndex < high(FProfileRun.ProfileTimes) do
    begin
      ppta := __GetNextTimeArray;
      if ppta = nil then Continue;
      pta  := ppta^;
      if pta = nil then Continue;

      if bFirst then
        iProfIndex  := aProfileIndex;
      bFirst := False;

      //while true do
      begin
        while (iProfIndex < high(pta)) and
              (pta[iProfIndex].ProfileType <> _uProfileTypes.ptNone) do
        begin
          //increase at start
          if pta[iProfIndex].ProfileType = ptEnter then
            inc(iLevel);

          //new level
          if (iLevel = aLevel) and (pta[iProfIndex].ProfileType = ptEnter) then
          begin
            //create new node
            if aNode = nil then
              pNodeRoot := vtreeTrace.AddChild(vtreeTrace.RootNode)
            //or first empty node
            else if (aNode.ChildCount = 1) and
                    (PTraceRecord(vtreeTrace.GetNodeData(aNode.FirstChild)).FunctionName = '') then
              pNodeRoot := aNode.FirstChild
            //new child
            else
              pNodeRoot := vtreeTrace.AddChild(aNode);
            pData := vtreeTrace.GetNodeData(pNodeRoot);
            vtreeTrace.ValidateNode(pNodeRoot, false);
            //set data
            pData.Level        := iLevel;
            pData.ArrayIndex   := iArrayIndex;
            pData.ProfileIndex := iProfIndex;
            pData.Duration     := pta[iProfIndex].Time;   //set start time, at leave the duration is made

            //get unit+proc name
            iIndex := ProfiledUnitProcList.IndexOf(Integer(pta[iProfIndex].Address));
            if iIndex >= 0 then
            begin
              o := ProfiledUnitProcList.Objects[iIndex];
              pr := PProfiledUnitProcListRecord(o);    
              iUnitIndex := pr.UnitIndex;
              iProcIndex := pr.ProcIndex;
              //exit;
              pData.FunctionName := FProfiledUnitArray[iUnitIndex].UnitName + ' '+ FProfiledProcArray[iProcIndex].ProcName;
            end;

            iLevelTotalChilds := 0;
            iLevelChilds := 0;
          end
          //now we have finnished current level
          else if (pNodeRoot <> nil) and
               (iLevel = aLevel) and (pta[iProfIndex].ProfileType = ptLeave) then
          begin
            //set child count of this node and level
            if iLevelChilds > 0 then
              vtreeTrace.ChildCount[pNodeRoot] := 1;
            //vtreeTrace.ChildCount[pNodeRoot] := iLevelChilds;
            pData := vtreeTrace.GetNodeData(pNodeRoot);
            pData.LevelChilds      := iLevelChilds;
            pData.TotalLevelChilds := iLevelTotalChilds;
            pData.Duration         := pta[iProfIndex].Time - pData.Duration;   //end - start = duration
            pData.Percentage       := 100 * (pData.Duration / TotalCycles);

            //if pData.Percentage < 1 then
            //  vtreeTrace.DeleteNode(pNodeRoot);
          end;

          //it is a next level, so a child of current level
          if (pta[iProfIndex].ProfileType = ptEnter) then
          begin
            if (iLevel = aLevel+1) then
            begin
              inc(iLevelChilds);
            end;
            if iLevel > aLevel then
              inc(iLevelTotalChilds);
          end;

          if (aLevel > 1) and (iLevel < aLevel) then Break;
          //decrease at end, so we have proper level in code above
          if pta[iProfIndex].ProfileType = ptLeave then
            dec(iLevel);

          inc(iProfIndex);
        end;

      end;
    end;

  finally
    vtreeTrace.EndUpdate;
    vtreeTrace.Tag := 0;
  end;
end;

procedure TframProfileTraceTree.vtreeTraceInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PTraceRecord;
begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  //get parent
  if Assigned(Data) then
    Data := Sender.GetNodeData(Node.Parent);
  //init child with parent data
  if Assigned(Data) then
  begin
    ShowTrace(Node.Parent, Data.Level+1,   //next level
              Data.ArrayIndex, Data.ProfileIndex+1);
  end;
end;

procedure TframProfileTraceTree.vtreeTraceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PTraceRecord;
begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Column of
      0: CellText := Data.FunctionName;
      1: CellText := IntToStr(Data.LevelChilds);
      2: CellText := IntToStr(Data.TotalLevelChilds);
      3: CellText := CalcTimeText(Data.Duration / ProfileRun.CPUSpeed);
      4: CellText := Format('%4.2f', [100 * (Data.Duration / TotalCycles) ]);
    else
    end;
  end;
end;

procedure TframProfileTraceTree.vtreeTraceFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTraceRecord;
begin
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  if Assigned(Data) then
  begin
    Data.FunctionName     := '';
  end;
end;

procedure TframProfileTraceTree.SetProfileRun(const Value: TProfileRun);
begin
  FProfileRun := Value;
end;

procedure TframProfileTraceTree.SetTotalCycles(const Value: int64);
begin
  FTotalCycles := Value;
end;

procedure TframProfileTraceTree.vtreeTracePaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PTraceRecord;
begin
  Data := Sender.GetNodeData(Node);

  if Node.Parent = Sender.RootNode then
     TargetCanvas.Font.Style := [fsUnderline];
  (*
  else if Sender.GetNodeLevel(Node) = 2 then
  begin
     //TargetCanvas.Font.sStyle := [fsItalic];
     TargetCanvas.Font.Size := TargetCanvas.Font.Size - 1;
     TargetCanvas.Font.Color := clDkGray;
  end;
  *)

  if Column in [4] then
  begin
    if (Data.Percentage > 25)then
      TargetCanvas.Font.Color := clRed
    else if (Data.Percentage > 5)then
      TargetCanvas.Font.Color := $000080FF;   //orange
  end;
end;

procedure TframProfileTraceTree.vtreeTraceHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
begin
  if (vtreeTrace.Header.SortColumn <> HitInfo.Column) then
  begin
    vtreeTrace.Header.SortColumn := HitInfo.Column;
    if HitInfo.Column >= 1 then    //calls, % etc default DESC
      vtreeTrace.Header.SortDirection := sdDescending
    else                   //unit names etc default ASC
      vtreeTrace.Header.SortDirection := sdAscending
  end
  else if (vtreeTrace.Header.SortDirection = sdDescending) then
    vtreeTrace.Header.SortDirection := sdAscending
  else
    vtreeTrace.Header.SortDirection := sdDescending;  //default descending: higher values are more interresting

  vtreeTrace.SortTree( HitInfo.Column, vtreeTrace.Header.SortDirection );
end;

procedure TframProfileTraceTree.vtreeTraceCompareNodes(
  Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  Data1  : PTraceRecord;
  Data2  : PTraceRecord;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
     0: Result := CompareText( Data1.FunctionName, Data2.FunctionName );
     1: Result := CompareValue( Data1.LevelChilds, Data2.LevelChilds );
     2: Result := CompareValue( Data1.TotalLevelChilds, Data2.TotalLevelChilds );
     3: Result := CompareValue( Data1.Duration, Data2.Duration );
     4: Result := CompareValue( Data1.Duration, Data2.Duration );
  end;

  (*
      0: CellText := Data.FunctionName;
      1: CellText := IntToStr(Data.LevelChilds);
      2: CellText := IntToStr(Data.TotalLevelChilds);
      3: CellText := CalcTimeText(Data.Duration / ProfileRun.CPUSpeed);
      4: CellText := Format('%4.2f', [100 * (Data.Duration / TotalCycles) ]);
  *)


end;

end.
