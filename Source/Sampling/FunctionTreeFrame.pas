unit FunctionOverviewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTreeFrame, VirtualTrees;

type
  TframFunctionOverview = class(TframBaseTree)
    procedure vtreeItemsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtreeItemsHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure vtreeItemsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: UnicodeString);
    procedure vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FThreadItem: TThreadItem;
    procedure SetThreadItem(const Value: TThreadItem);
    { Private declarations }
  public
    { Public declarations }
    procedure AfterConstruction;override;

    property ThreadItem: TThreadItem read FThreadItem write SetThreadItem;
  end;

implementation

uses
  Math;

{$R *.dfm}

type
  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    ModuleName,
    UnitName,
    ClassName,
    FunctionName: String;
    //
    Calls: integer;
    TotalProcent, TotalChildProcent: Single;
    TotalTime, TotalChildTime, TotalWithChildTime: Real;
    //
    //ProfiledProc: PProfiledProc;
  end;

{ TframBaseTree1 }

procedure TframFunctionOverview.AfterConstruction;
begin
  inherited;
  // Let the tree know how much data space we need.
  vtreeItems.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  vtreeItems.RootNodeCount := 0;
end;

procedure TframFunctionOverview.SetThreadItem(const Value: TThreadItem);
begin
  FThreadItem := Value;

  vtreeItems.Clear;
  vtreeItems.RootNodeCount := Length(FThreadItem.ThreadResult);

  //sort on calls
  vtreeItems.SortTree(4, sdDescending);
end;

procedure TframFunctionOverview.vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  Data1  : PMyRec;
  Data2  : PMyRec;
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
  Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  if Assigned(Data) then
  begin
    Data.ModuleName   := '';
    Data.UnitName     := '';
    Data.ClassName    := '';
    Data.FunctionName := '';
  end;
end;

procedure TframFunctionOverview.vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: UnicodeString);
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
      0: CellText := Data.ModuleName;
      1: CellText := Data.FunctionName;
      2: CellText := IntToStr(Data.Calls);
      //
      3: CellText := Format('%3.2f',[Data.TotalProcent]);
      4: CellText := Format('%3.2f',[Data.TotalChildProcent]);
      //
      5: CellText := CalcTimeText(Data.TotalTime);
      6: CellText := CalcTimeText(Data.TotalChildTime);
      7: CellText := CalcTimeText(Data.TotalWithChildTime);
    else ;
    end;
  end;
end;

procedure TframFunctionOverview.vtreeItemsHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (vtreeItems.Header.SortColumn <> Column) then
  begin
    vtreeItems.Header.SortColumn := Column;
    if Column >= 3 then    //calls, % etc default DESC
      vtreeItems.Header.SortDirection := sdDescending
    else                   //unit names etc default ASC
      vtreeItems.Header.SortDirection := sdAscending
  end
  else if (vtreeItems.Header.SortDirection = sdDescending) then
    vtreeItems.Header.SortDirection := sdAscending
  else
    vtreeItems.Header.SortDirection := sdDescending;

  vtreeItems.SortTree( Column, vtreeItems.Header.SortDirection );
end;

procedure TframFunctionOverview.vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
/
end;

procedure TframFunctionOverview.vtreeItemsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PMyRec;
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
