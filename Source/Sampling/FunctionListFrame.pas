unit FunctionListFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees,
  mcSamplingResult, uResultTypes;

type
  TframFunctionList = class(TFrame)
    vtreeItems: TVirtualStringTree;
    procedure vtreeItemsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure vtreeItemsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FFunctionItems: TSubFunctionList;
//    FSampleCount: Integer;
    FDuration: Double;
    procedure SetFunctionItems(const Value: TSubFunctionList);
    { Private declarations }
  public
    { Public declarations }
    procedure AfterConstruction;override;

    property FunctionItems: TSubFunctionList read FFunctionItems write SetFunctionItems;
//    property SampleCount: Integer read FSampleCount write FSampleCount;
    property Duration: Double read FDuration write FDuration; 
//    property ThreadItem: TThreadItem read FThreadItem write SetThreadItem;
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
    FunctionName: String;
    //
//    Calls: integer;
    TotalProcent, TotalChildProcent: Single;
//    TotalTime, TotalChildTime, TotalWithChildTime: Real;
    FunctionRef: TSubFunction;
  end;
  
procedure TframFunctionList.AfterConstruction;
begin
  inherited;
  // Let the tree know how much data space we need.
  vtreeItems.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  vtreeItems.RootNodeCount := 0;
end;

procedure TframFunctionList.SetFunctionItems(const Value: TSubFunctionList);
begin
  FFunctionItems := Value;

  vtreeItems.Clear;
  vtreeItems.RootNodeCount := FFunctionItems.Count;

  //sort on calls
  vtreeItems.SortTree(4, sdDescending);
end;

procedure TframFunctionList.vtreeItemsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  Data1  : PMyRec;
  Data2  : PMyRec;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
     0: Result := CompareText( Data1.ModuleName,   Data2.ModuleName );
     1: Result := CompareText( Data1.FunctionName, Data2.FunctionName );

     2: Result := CompareValue( Data1.FunctionRef.Count, Data2.FunctionRef.Count );
     3: Result := CompareValue( Data1.TotalProcent,      Data2.TotalProcent );
     4: Result := CompareValue( Data1.TotalChildProcent, Data2.TotalChildProcent );

     5: Result := CompareValue( Data1.FunctionRef.TotalOwnTime,   Data2.FunctionRef.TotalOwnTime );
     6: Result := CompareValue( Data1.FunctionRef.TotalChildTime, Data2.FunctionRef.TotalChildTime );
     7: Result := CompareValue( Data1.FunctionRef.TotalTime,      Data2.FunctionRef.TotalTime );
  end;
end;

procedure TframFunctionList.vtreeItemsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
    Data.FunctionName := '';
  end;
end;

procedure TframFunctionList.vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
var
  Data: PMyRec;
begin
  try
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Column of
      0: CellText := Data.ModuleName;
      1: CellText := Data.FunctionName;
      2: CellText := IntToStr(Data.FunctionRef.Count);
      //
      3: CellText := Format('%3.2f',[Data.TotalProcent]);
      4: CellText := Format('%3.2f',[Data.TotalChildProcent]);
      //
      5: CellText := Format('%4.2fms',[Data.FunctionRef.TotalOwnTime]);
      6: CellText := Format('%4.2fms',[Data.FunctionRef.TotalChildTime]);
      7: CellText := Format('%4.2fms',[Data.FunctionRef.TotalTime]);
    else ;
    end;
  end;
  except
    Sleep(0);
  end;
end;

procedure TframFunctionList.vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PMyRec;
  ff: TSubFunction;
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.

    if FunctionItems.Count <= Integer(Node.Index) then exit;
    ff := TSubFunction(FunctionItems.Items[Node.Index]);
    Data.FunctionRef  := ff;

    Data.ModuleName   := ExtractFileName(ff.FunctionRef.ModuleName);
    Data.FunctionName := ExtractFileName(ff.FunctionRef.FunctionName);

//    Data.TotalProcent       := (ff.TotalOwnTime * 100) / SampleCount;
//    Data.TotalChildProcent  := (ff.TotalTime * 100)    / SampleCount;
    Data.TotalProcent       := (ff.TotalOwnTime * 100) / Duration;
    Data.TotalChildProcent  := (ff.TotalTime * 100)    / Duration;

    {
    //__ExtractPartsOfFullNameAndFill(fi.FunctionName);
    Data.Calls        := ff.Count;

    Data.TotalTime          := ff.TotalOwnTime;
    Data.TotalChildTime     := ff.TotalChildTime;
    Data.TotalWithChildTime := ff.TotalTime;
    }
  end;
end;

procedure TframFunctionList.vtreeItemsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PMyRec;
begin
  Data := Sender.GetNodeData(Node);

  //weird bug? remove bold!
  TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];

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
