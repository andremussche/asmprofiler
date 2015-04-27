unit ResultsFunctionOverviewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTreeFrame, VirtualTrees,
  mcSamplingResult, uResultTypes, Menus;

type
  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PTreeFunctionRec = ^TTreeFunctionRec;
  TTreeFunctionRec = record
    ModuleName,
//    UnitName,
//    ClassName,
    FunctionName: String;
    //
    Calls: integer;
    TotalProcent, TotalChildProcent: Single;
    TotalTime, TotalChildTime, TotalWithChildTime: Real;
    //
    //ProfiledProc: PProfiledProc;
    ProfiledFunction: TFlatFunction;
  end;

  TframFunctionOverview = class(TframBaseTree)
    procedure vtreeItemsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtreeItemsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure vtreeItemsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
    procedure vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FThreadItem: TThreadResult;
    procedure SetThreadItem(const Value: TThreadResult);
    { Private declarations }
  public
    { Public declarations }
    procedure AfterConstruction;override;

    function GetTreeNodeData(aNode: PVirtualNode): PTreeFunctionRec;

    property ThreadItem: TThreadResult read FThreadItem write SetThreadItem;
  end;


implementation

uses
  Math;

{$R *.dfm}

{ TframBaseTree1 }

procedure TframFunctionOverview.AfterConstruction;
begin
  inherited;
  // Let the tree know how much data space we need.
  vtreeItems.NodeDataSize := SizeOf(TTreeFunctionRec);
  // Set an initial number of nodes.
  vtreeItems.RootNodeCount := 0;
end;

function TframFunctionOverview.GetTreeNodeData(
  aNode: PVirtualNode): PTreeFunctionRec;
begin
  Result := vtreeItems.GetNodeData(aNode);
end;

procedure TframFunctionOverview.SetThreadItem(const Value: TThreadResult);
begin
  FThreadItem := Value;

  vtreeItems.Clear;
  vtreeItems.RootNodeCount := FThreadItem.FlatFunctions.Count;

  //sort on calls
  vtreeItems.SortTree(4, sdDescending);
end;

procedure TframFunctionOverview.vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  Data1  : PTreeFunctionRec;
  Data2  : PTreeFunctionRec;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
     0: Result := CompareText( Data1.ModuleName, Data2.ModuleName );
     1: Result := CompareText( Data1.FunctionName, Data2.FunctionName );

     2: Result := CompareValue( Data1.Calls, Data2.Calls );
     3: Result := CompareValue( Data1.TotalProcent, Data2.TotalProcent );
     4: Result := CompareValue( Data1.TotalChildProcent, Data2.TotalChildProcent );

     5: Result := CompareValue( Data1.TotalTime, Data2.TotalTime );
     6: Result := CompareValue( Data1.TotalChildTime, Data2.TotalChildTime );
     7: Result := CompareValue( Data1.TotalWithChildTime, Data2.TotalWithChildTime );
  end;
end;

procedure TframFunctionOverview.vtreeItemsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeFunctionRec;
begin
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  if Assigned(Data) then
  begin
    Data.ModuleName   := '';
//    Data.UnitName     := '';
//    Data.ClassName    := '';
    Data.FunctionName := '';
  end;
end;

procedure TframFunctionOverview.vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: UnicodeString);
var
  Data: PTreeFunctionRec;
begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  try
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then
    //if Sender.GetNodeLevel(Node) = 0 then
    begin
      //ps := Data.Data;
      case Column of
        0: CellText := Data.ModuleName;
        1: CellText := Data.FunctionName;
        2: CellText := IntToStr(Data.Calls);
        //
        3: CellText := Format('%3.2f',[Data.TotalProcent]);
        4: CellText := Format('%3.2f',[Data.TotalChildProcent]);
        //
        5: CellText := Format('%4.2fms',[Data.TotalTime]);
        6: CellText := Format('%4.2fms',[Data.TotalChildTime]);
        7: CellText := Format('%4.2fms',[Data.TotalWithChildTime]);
      else ;
      end;
    end;
  except
    Sleep(0);
  end;
end;

procedure TframFunctionOverview.vtreeItemsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
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

procedure TframFunctionOverview.vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PTreeFunctionRec;
  fi: TFlatFunction;
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.

    if ThreadItem.FlatFunctions.Count <= Integer(Node.Index) then exit;
    fi := ThreadItem.FlatFunctions[Node.Index];

    Data.ModuleName   := ExtractFileName(fi.ModuleName);
    Data.FunctionName := ExtractFileName(fi.FunctionName);
    //__ExtractPartsOfFullNameAndFill(fi.FunctionName);
    Data.Calls        := fi.Count;

    Data.TotalProcent       := (fi.TotalOwnTime * 100) / ThreadItem.Duration;
    Data.TotalChildProcent  := (fi.TotalTime * 100)    / ThreadItem.Duration;
//    Data.TotalProcent       := (fi.Count * 100) / ThreadItem.SampleCount;
//    Data.TotalChildProcent  := (fi.Count * 100) / ThreadItem.SampleCount;

    Data.TotalTime          := fi.TotalOwnTime;
    Data.TotalChildTime     := fi.TotalChildTime;
    Data.TotalWithChildTime := fi.TotalTime;

    Data.ProfiledFunction := fi;
  end;
end;

procedure TframFunctionOverview.vtreeItemsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PTreeFunctionRec;
begin
  Data := Sender.GetNodeData(Node);

  if Column in [3,5] then
  begin
    if (Data.TotalProcent > 50)then
      TargetCanvas.Font.Color := clRed
    else if (Data.TotalProcent > 10)then
      TargetCanvas.Font.Color := $000080FF;   //orange
  end;

  if Column in [4,7] then
  begin
    if (Data.TotalChildProcent > 50)then
      TargetCanvas.Font.Color := clRed
    else if (Data.TotalChildProcent > 10)then
      TargetCanvas.Font.Color := $000080FF;   //orange
  end;
end;

end.
