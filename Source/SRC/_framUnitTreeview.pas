unit _framUnitTreeview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, StdCtrls, ExtCtrls,
  _uProfileClasses, Menus;

type
  TframUnitTreeview = class(TFrame)
    vtreeItems: TVirtualStringTree;
    Panel1: TPanel;
    Label2: TLabel;
    lblIncrSearch: TLabel;
    PopupMenu1: TPopupMenu;
    SavetoHTML1: TMenuItem;
    SavetoRTF1: TMenuItem;
    SavetoText1: TMenuItem;
    procedure vtreeItemsChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtreeItemsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: UnicodeString);
//    procedure vtreeItemsIncrementalSearch(Sender: TBaseVirtualTree;
//      Node: PVirtualNode; const SearchText: WideString;
//      var Result: Integer);
    procedure vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtreeItemsStateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
    procedure SavetoHTML1Click(Sender: TObject);
    procedure SavetoRTF1Click(Sender: TObject);
    procedure SavetoText1Click(Sender: TObject);
    procedure vtreeItemsHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vtreeItemsIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: string; var Result: Integer);
  private
    FSelectedItemsCount: integer;
    FDebugInfoStorage: TDebugInfoStorage;
    procedure SetDebugInfoStorage(const Value: TDebugInfoStorage);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    function  GetSelectedItemsCount: integer;
    procedure SaveSelectedItemsList;
    procedure LoadSelectedItemsList;

    property DebugInfoStorage: TDebugInfoStorage read FDebugInfoStorage write SetDebugInfoStorage;
  end;

implementation

uses _uProfileTypes, JclMath, Math;

{$R *.dfm}

type
  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: String;
    Data: Pointer;
  end;

constructor TframUnitTreeview.Create(AOwner: TComponent);
begin
  inherited;

  // Let the tree know how much data space we need.
  vtreeItems.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  vtreeItems.RootNodeCount := 0;
end;

procedure TframUnitTreeview.SetDebugInfoStorage(const Value: TDebugInfoStorage);
begin
  FDebugInfoStorage := Value;

  vtreeItems.Clear;
  if Value <> nil then
    vtreeItems.RootNodeCount := Length(FDebugInfoStorage.UnitNames);
  LoadSelectedItemsList;
end;

procedure TframUnitTreeview.vtreeItemsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
//  n:PVirtualNode;
  na: TNodeArray;
  i:integer;

  procedure  __CheckNode(aNode:PVirtualNode);
  var
    n:PVirtualNode;
  begin
    aNode.CheckState := Node.CheckState;

    if (Sender.GetNodeLevel(aNode) = 0) and
       (aNode.ChildCount > 0) then
    begin
      n := aNode.FirstChild;
      repeat
        n.CheckState := aNode.CheckState;
        n := n.NextSibling;
      until n = nil;
    end;

    Sender.InvalidateToBottom(aNode);
  end;

begin
  if Sender.SelectedCount > 1 then
  begin
    na := Sender.GetSortedSelection(True);
    for i := 0 to Sender.SelectedCount-1 do
    begin
      __CheckNode(na[i]);
    end;
  end
  else
    __CheckNode(Node);
end;

procedure TframUnitTreeview.vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1  : PMyRec;
  Data2  : PMyRec;
  ps1, ps2: PJclMapSegmentExt;
  pp1, pp2: PJclMapProcNameExt;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  if (Sender.GetNodeLevel(Node1) = 0) and
     (Sender.GetNodeLevel(Node2) = 0) then
  begin
    ps1 := Data1.Data;
    ps2 := Data2.Data;

    case Column of
       0: Result := CompareText( Data1.Caption, Data2.Caption );
       1: Result := CompareValue( ps1.StartAddr, ps2.StartAddr );
       2: Result := CompareValue( ps1.EndAddr, ps2.EndAddr );
       3: Result := CompareText( String(ps1.UnitFile), String(ps2.UnitFile) );
    end;
  end
  else if (Sender.GetNodeLevel(Node1) = 1) and
          (Sender.GetNodeLevel(Node2) = 1) then
  begin
    pp1 := Data1.Data;
    pp2 := Data2.Data;

    case Column of
       0: Result := CompareText( Data1.Caption, Data2.Caption );
       1: Result := CompareValue( pp1.Addr, pp2.Addr );
       //2: Result := CompareValue( ps1.EndAddr, ps2.EndAddr );
       //3: Result := CompareText( ps1.UnitFile, ps2.UnitFile );
    end;
  end;
end;

procedure TframUnitTreeview.vtreeItemsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  if Assigned(Data) then
    Data.Caption := '';
end;

procedure TframUnitTreeview.vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: UnicodeString);
var
  Data: PMyRec;
  ps: PJclMapSegmentExt;
  pp: PJclMapProcNameExt;
begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    ps := Data.Data;
    case Column of
      0: CellText := Data.Caption;
      1: CellText := IntToStr(ps.StartAddr);
      2: CellText := IntToStr(ps.EndAddr);
      3: CellText := String(ps.UnitFile);
    else ; end;
  end
  else
  begin
    pp := Data.Data;
    case Column of
      0: CellText := Data.Caption;
      1: CellText := IntToStr(pp.Addr);
      2: CellText := '';
      3: CellText := '';
    else ; end;
  end;
end;

procedure TframUnitTreeview.vtreeItemsHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
begin
  if (vtreeItems.Header.SortColumn <> HitInfo.Column) then
    vtreeItems.Header.SortColumn := HitInfo.Column
  else if (vtreeItems.Header.SortDirection = sdAscending) then
    vtreeItems.Header.SortDirection := sdDescending
  else
    vtreeItems.Header.SortDirection := sdAscending;

  vtreeItems.SortTree( HitInfo.Column, vtreeItems.Header.SortDirection );
end;

procedure TframUnitTreeview.vtreeItemsIncrementalSearch(
  Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string;
  var Result: Integer);

    function Min(const A, B: Integer): Integer;  {save us linking in math.pas}
    begin
      if A < B then
        Result := A
      else
        Result := B;
    end;

var
  sCompare1, sCompare2 : string;
  DisplayText: String;

begin
  vtreeItems.IncrementalSearchDirection := sdForward;   // note can be backward
  lblIncrSearch.Caption := SearchText;

  // Note: This code requires a proper Unicode/WideString comparation routine which I did not want to link here for
  // size and clarity reasons. For now strings are (implicitely) converted to ANSI to make the comparation work.
  // Search is not case sensitive.
  vtreeItemsGetText( Sender, Node, 0 {Column}, ttNormal, DisplayText );
  sCompare1 := SearchText;
  sCompare2 := DisplayText;

  // By using StrLIComp we can specify a maximum length to compare. This allows us to find also nodes
  // which match only partially. Don't forget to specify the shorter string length as search length.
  Result := StrLIComp( pchar(sCompare1), pchar(sCompare2), Min(Length(sCompare1), Length(sCompare2)) )
end;

procedure TframUnitTreeview.vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data,DataParent: PMyRec;
  iRevIndex:integer;
  ps: PJclMapSegmentExt;
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.
    //Data.Caption := Format('Level %d, Index %d', [GetNodeLevel(Node), Node.Index]);
    CheckType[Node] := ctCheckBox;

    if GetNodeLevel(Node) = 0 then
    begin
      iRevIndex := vtreeItems.RootNodeCount - Node.Index - 1;
      Data.Caption     := String(FDebugInfoStorage.UnitNames[iRevIndex].UnitName);
      Data.Data        := @FDebugInfoStorage.UnitNames[iRevIndex];
      ChildCount[Node] := Length(FDebugInfoStorage.UnitNames[iRevIndex].Procs);
      InvalidateToBottom(Node);
    end
    else if GetNodeLevel(Node) = 1 then
    begin
      //iRevIndex := vtreeItems.RootNodeCount - ParentNode.Index - 1;
      DataParent := GetNodeData(ParentNode);
      ps := DataParent.Data;
      Data.Caption    := String(ps.Procs[ Node.Index ].ProcName);
      //Data.Caption    := FDebugInfoStorage.UnitNames[iRevIndex].Procs[ Node.Index ].ProcName;  // + ' ' +
                         //IntToStr(FDebugInfoStorage.UnitNames[iRevIndex].Procs[ Node.Index ].Addr);
      //Data.Data       := @FDebugInfoStorage.UnitNames[iRevIndex].Procs[ Node.Index ];
      Data.Data       := @ps.Procs[ Node.Index ];
      Node.CheckState := ParentNode.CheckState;
    end;
  end;
end;

procedure TframUnitTreeview.vtreeItemsStateChange(Sender: TBaseVirtualTree; Enter,
  Leave: TVirtualTreeStates);
begin
  if (tsIncrementalSearching in Leave) then
     //(tsIncrementalSearchPending in Leave) then
    lblIncrSearch.Caption := '';
end;

procedure TframUnitTreeview.SaveSelectedItemsList;
var
  nParent, n: PVirtualNode;
//  p:TJclMapProcNameExt;
  Data, pData: PMyRec;
  iUnit,
  iProc: integer;
begin
  FDebugInfoStorage.ClearSelection;
  FSelectedItemsCount := 0;
  //SetLength(Result, 0);

  with vtreeItems do
  begin
    n := GetFirstChecked(csCheckedNormal);
    while n <> nil do
    begin
      Data := GetNodeData(n);
      //if Length(Result) = 0 then
      //  SetLength(Result, 1);
      //iResult := 1;

      if GetNodeLevel(n) = 0 then
      begin
        //
      end
      else
      begin
        nParent := n.Parent;
        {add unit record if not exist already}
        if GetNodeLevel(nParent) = 0 then
        begin
          //iResult := Length(Result);
          pData   := GetNodeData(nParent);

          iUnit := FDebugInfoStorage.GetSegmentByName( pData.Caption );
          iProc := FDebugInfoStorage.GetProcByName(Data.Caption,iUnit);
          FDebugInfoStorage.AddSelectedProcedure(iUnit,iProc);
        end;
      end;

      n := GetNextChecked(n,csCheckedNormal);
    end;
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

procedure TframUnitTreeview.SavetoHTML1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeItems.ContentToHTML(tstAll, 'Profiler Unit HTML Export')) );
end;

procedure TframUnitTreeview.SavetoRTF1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeItems.ContentToRTF(tstAll)) );
end;

procedure TframUnitTreeview.SavetoText1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( String(vtreeItems.ContentToText(tstAll, ',')) );
end;

function TframUnitTreeview.GetSelectedItemsCount: integer;
begin
  //if FSelectedItemsCount = 0 then
  //  GetSelectedItemsList)
  Result := FSelectedItemsCount;
end;

procedure TframUnitTreeview.LoadSelectedItemsList;
var
  i,j:integer;
  nParent, n: PVirtualNode;
  Data: PMyRec;
  sa: PSelectedUnitArray;
begin
  sa := FDebugInfoStorage.SelectedUnitProcedures;

  with vtreeItems do
  for i := low(sa^) to high(sa^) do
  begin
    n := vtreeItems.RootNode.FirstChild;

    while n <> nil do
    begin
      Data := GetNodeData(n);
      if (Data = nil) or (Data.Caption = '') then vtreeItems.ReinitNode(n,false);
      //Data := GetNodeData(n);

      if (Data <> nil) and (Data.Caption = sa^[i].UnitName) then
      begin
        if sa^[i].SelectedProcsCount = sa^[i].TotalProcsCount then
        begin
          n.CheckState := csCheckedNormal;
          vtreeItems.ReinitNode(n,true);
        end
        else
        begin
          nParent := n;
          nParent.CheckState := csUnCheckedPressed;
          vtreeItems.ReinitNode(n,true);

          n       := nParent.FirstChild;
          while n <> nil do
          begin
            n.CheckState := csUncheckedNormal;
            n := n.NextSibling;
          end;

          for j := 0 to sa^[i].SelectedProcsCount-1 do
          begin
            n       := nParent.FirstChild;
            while n <> nil do
            begin
              Data := GetNodeData(n);
              //if (Data = nil) or (Data.Caption = '') then vtreeItems.ReinitNode(n,false);
              if Data.Caption = String(sa^[i].SelectedProcs[j].ProcName) then
              begin
                n.CheckState := csCheckedNormal;
                Break;
              end;
              n := n.NextSibling;
            end;
          end;
        end;

        Break;
      end;

      n := n.NextSibling;
    end;
  end;
end;

end.

