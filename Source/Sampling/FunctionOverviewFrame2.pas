unit FunctionOverviewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, mcProcessSampler, mcSamplingResult;

type
  TFrame1 = class(TFrame)
    vtreeItems: TVirtualStringTree;
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
//    property ProcessObject: TProcessSampler read FProcessObject write SetProcessObject;
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

function CalcTimeText(const aTime: single): string;
begin
  {
  if aTime >= 1 then
    Result := Format('%8.2fs',[aTime])
  else if aTime >= 0.0001 then
    Result := Format('%8.2fms',[aTime * 1000])
  else if aTime >= 0.0000001 then
    Result := Format('%8.2fµs',[aTime * 1000 * 1000])
  else //if tDiff >= 0.0000001 then
    Result := Format('%8.2fns',[aTime * 1000 * 1000 * 1000])
  }
  Result := Format('%8.2fms',[aTime]);
end;
  

procedure TFrame1.AfterConstruction;
begin
  inherited;

  // Let the tree know how much data space we need.
  vtreeItems.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  vtreeItems.RootNodeCount := 0;
end;

procedure TFrame1.SetThreadItem(const Value: TThreadItem);
begin
  FThreadItem := Value;

  vtreeItems.Clear;
  vtreeItems.RootNodeCount := Length(FThreadItem.ThreadResult);

  //sort on calls
  vtreeItems.SortTree(4, sdDescending);
end;

procedure TFrame1.vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  Data1  : PMyRec;
  Data2  : PMyRec;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
     0: Result := CompareText( Data1.ModuleName, Data2.ModuleName );
     1: Result := CompareText( Data1.UnitName, Data2.UnitName );
     2: Result := CompareText( Data1.ClassName, Data2.ClassName );
     3: Result := CompareText( Data1.FunctionName, Data2.FunctionName );

     4: Result := CompareValue( Data1.Calls, Data2.Calls );
     5: Result := CompareValue( Data1.TotalProcent, Data2.TotalProcent );
     6: Result := CompareValue( Data1.TotalChildProcent, Data2.TotalChildProcent );

     8: Result := CompareValue( Data1.TotalTime, Data2.TotalTime );
     9: Result := CompareValue( Data1.TotalChildTime, Data2.TotalChildTime );
     10: Result := CompareValue( Data1.TotalWithChildTime, Data2.TotalWithChildTime );
  end;
end;

procedure TFrame1.vtreeItemsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
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

procedure TFrame1.vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
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
      1: CellText := Data.UnitName;
      2: CellText := Data.ClassName;
      3: CellText := Data.FunctionName;
      4: CellText := IntToStr(Data.Calls);
      //
      5: CellText := Format('%3.2f',[Data.TotalProcent]);
      6: CellText := Format('%3.2f',[Data.TotalChildProcent]);
      //
      7: CellText := CalcTimeText(Data.TotalTime);
      8: CellText := CalcTimeText(Data.TotalChildTime);
      9: CellText := CalcTimeText(Data.TotalWithChildTime);
    else ;
    end;
  end;
end;

procedure TFrame1.vtreeItemsHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
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

procedure TFrame1.vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PMyRec;

  procedure __ExtractPartsOfFullNameAndFill(const aFullFunction: string);
  var i: Integer;
  begin
    //Forms.TApplication.ProcessMessages
    i := Pos('.', aFullFunction);      //Forms
    Data.UnitName     := Copy(aFullFunction, 0, i-1);

    Data.FunctionName := Copy(aFullFunction, i+1, 255);
    i := Pos('.', Data.FunctionName);  //TApplication
    Data.ClassName    := Copy(Data.FunctionName, 0, i-1);

    Data.FunctionName := Copy(Data.FunctionName, i+1, 255); //ProcessMessages
  end;

var
  fi: TFunctionItem;
  tr: TThreadResultArray;
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.

    tr := ThreadItem.ThreadResult;
    if (Length(tr) <= Node.Index) then exit;
    fi := tr[Node.Index];

    Data.ModuleName   := ExtractFileName(fi.ModuleName);
    __ExtractPartsOfFullNameAndFill(fi.FunctionName);
    Data.Calls        := fi.Count;

    Data.TotalProcent       := (fi.TotalOwnTime * 100) / ThreadItem.SampleCount;
    Data.TotalChildProcent  := (fi.TotalTime * 100)    / ThreadItem.SampleCount;

    Data.TotalTime          := fi.TotalOwnTime;
    Data.TotalChildTime     := fi.TotalChildTime;
    Data.TotalWithChildTime := fi.TotalTime;
  end;
end;

procedure TFrame1.vtreeItemsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);

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

end.
