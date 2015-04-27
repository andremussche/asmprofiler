unit ResultsDetailedViewFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseTreeFrame, VirtualTrees,
  mcSamplingResult, uResultTypes,
  StdCtrls, ExtCtrls, FunctionListFrame, Menus;

type
  TframDetailedView = class(TframBaseTree)
    Panel1: TPanel;
    Splitter1: TSplitter;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Splitter2: TSplitter;
    framFunctionListChilds: TframFunctionList;
    framFunctionListParents: TframFunctionList;
    procedure vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vtreeItemsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeItemsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FThreadItem: TThreadResult;
    procedure SetThreadItem(const Value: TThreadResult);
  protected
  public
    procedure AfterConstruction;override;

    property ThreadItem: TThreadResult read FThreadItem write SetThreadItem;
  end;

implementation

{$R *.dfm}

type
  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    ModuleRec   : TModule;
    UnitRec     : TUnit;
    FunctionRec : TFlatFunction;
    LineRec     : TFunctionLine;
    TotalProcent, TotalChildProcent: Single;
  end;

{todo
  show red colors
  full expand node if one child
  expand modules 1 level at init
  parent + child calls
}

procedure IncF(var aFloat: Real; aIncrement: Real);
begin
  aFloat := aFloat + aIncrement;
end;

  { TframBaseTree1 }

procedure TframDetailedView.AfterConstruction;
begin
  inherited;
  // Let the tree know how much data space we need.
  vtreeItems.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  vtreeItems.RootNodeCount := 0;
end;

(*
procedure TframDetailedView.MakeModuleArray;
var
  iModules, iUnits, iFunctions,
  i: Integer;
  fi: TFlatFunction;
  iModule, iUnit, iFunction: Integer;
  bFound: boolean;
  _module: TModule;
  _unit: TUnit;
  _function: TFlatFunction;
begin
  iModules := 0;
  SetLength(FModules, iModules);

  for i := 0 to High(FThreadItem.ThreadResult) do
  begin
    fi := @FThreadItem.ThreadResult[i];

    //MODULE
    bFound  := False;
    _module := nil;
    for iModule := 0 to High(FModules) do
    begin
      if FModules[iModule].ModuleName = fi.ModuleName then
      begin
        bFound  := True;
        _module := @FModules[iModule];
        Break;
      end;
    end;
    if not bFound then
    begin
      inc(iModules);
      SetLength(FModules, iModules);
      FModules[iModules-1].ModuleName := fi.ModuleName;
      _module := @FModules[iModules-1];
    end;
    inc(_module.Calls, fi.Count);
    IncF(_module.TotalTime, fi.TotalTime);
    IncF(_module.TotalOwnTime, fi.TotalOwnTime);
    IncF(_module.TotalChildTime, fi.TotalChildTime);

    //UNIT
    iUnits  := Length(_module.Units);
    bFound  := False;
    _unit   := nil;
    for iUnit := 0 to High(_module.Units) do
    begin
      if _module.Units[iUnit].UnitName = fi.UnitName then
      begin
        bFound := True;
        _unit := @_module.Units[iUnit];
        Break;
      end;
    end;
    if not bFound then
    begin
      inc(iUnits);
      SetLength(_module.Units, iUnits);
      _module.Units[iUnits-1].UnitName := fi.UnitName;
      _unit := @_module.Units[iUnits-1];
    end;
    inc(_unit.Calls, fi.Count);
    IncF(_unit.TotalTime, fi.TotalTime);
    IncF(_unit.TotalOwnTime, fi.TotalOwnTime);
    IncF(_unit.TotalChildTime, fi.TotalChildTime);

    //FUNCTIONS
    iFunctions := Length(_unit.Functions);
    bFound     := False;
    _function  := nil;
    for iFunction := 0 to High(_unit.Functions) do
    begin
      if _unit.Functions[iFunction].FunctionName = fi.FunctionName then
      begin
        bFound := True;
        _function  := @_unit.Functions[iFunction];
        Break;
      end;
    end;
    if not bFound then
    begin
      inc(iFunctions);
      SetLength(_unit.Functions, iFunctions);
      _unit.Functions[iFunctions-1].FunctionName := fi.FunctionName;
      _function  := @_unit.Functions[iFunctions-1];
    end;
    _function.FunctionLines := fi.FunctionLines;
    inc(_function.Calls, fi.Count);
    IncF(_function.TotalTime, fi.TotalTime);
    IncF(_function.TotalOwnTime, fi.TotalOwnTime);
    IncF(_function.TotalChildTime, fi.TotalChildTime);

    //child calls
    if Length(_function.ChildCalls) <> Length(fi.ChildCalls) then
    begin
      SetLength(_function.ChildCalls, Length(fi.ChildCalls));
      for iFunction := 0 to High(fi.ChildCalls) do
      begin
        _function.ChildCalls[iFunction].ModuleName   := fi.ChildCalls[iFunction].ModuleName;
        _function.ChildCalls[iFunction].FunctionName := fi.ChildCalls[iFunction].FunctionName;
        _function.ChildCalls[iFunction].Count        := fi.ChildCalls[iFunction].Count;
        _function.ChildCalls[iFunction].TotalTime    := fi.ChildCalls[iFunction].TotalTime;
      end;
    end;

    //parent calls
    if Length(_function.ParentCalls) <> Length(fi.ParentCalls) then
    begin
      SetLength(_function.ParentCalls, Length(fi.ParentCalls));
      for iFunction := 0 to High(fi.ParentCalls) do
      begin
        _function.ParentCalls[iFunction].ModuleName   := fi.ParentCalls[iFunction].ModuleName;
        _function.ParentCalls[iFunction].FunctionName := fi.ParentCalls[iFunction].FunctionName;
        _function.ParentCalls[iFunction].Count        := fi.ParentCalls[iFunction].Count;
        _function.ParentCalls[iFunction].TotalTime    := fi.ParentCalls[iFunction].TotalTime;
      end;
    end;

    {
  TSubFunctionItem = record
    ModuleName,
    FunctionName: String
    Count: Integer;
    TotalTime: Integer;
    }

  end;
end;
*)

procedure TframDetailedView.SetThreadItem(const Value: TThreadResult);
begin
  FThreadItem := Value;

  vtreeItems.Clear;
  vtreeItems.RootNodeCount := FThreadItem.ModuleResult.Count;

  //sort on calls
  vtreeItems.SortTree(4, sdDescending);
end;

procedure TframDetailedView.vtreeItemsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PMyRec;
begin
  //function
  if Sender.GetNodeLevel(Node) = 2 then
  begin
    Data := Sender.GetNodeData(Node);

//    framFunctionListChilds.SampleCount    := Self.ThreadItem.SampleCount;
//    framFunctionListParents.SampleCount   := Self.ThreadItem.SampleCount;
    framFunctionListChilds.Duration    := Self.ThreadItem.Duration;
    framFunctionListParents.Duration   := Self.ThreadItem.Duration;

    framFunctionListChilds.FunctionItems  := Data.FunctionRec.ChildCalls;
    framFunctionListParents.FunctionItems := Data.FunctionRec.ParentCalls;
  end;
end;

procedure TframDetailedView.vtreeItemsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if Sender.ChildCount[Node] <= 1 then
    Sender.Expanded[Node.FirstChild] := True;
end;

procedure TframDetailedView.vtreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
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
      2: CellText := Format('%3.2f',[Data.TotalProcent]);
      3: CellText := Format('%3.2f',[Data.TotalChildProcent]);
    end;

    if Sender.GetNodeLevel(Node) = 0 then
    case Column of
      0: CellText := ExtractFileName(Data.ModuleRec.ModuleName);
      1: CellText := IntToStr(Data.ModuleRec.Calls);
      4: CellText := Format('%4.2fms',[Data.ModuleRec.TotalOwnTime]);
      5: CellText := Format('%4.2fms',[Data.ModuleRec.TotalChildTime]);
      6: CellText := Format('%4.2fms',[Data.ModuleRec.TotalTime]);
    end
    else if Sender.GetNodeLevel(Node) = 1 then
    case Column of
      0: CellText := ExtractFileName(Data.UnitRec.UnitName);
      1: CellText := IntToStr(Data.UnitRec.Calls);
      4: CellText := Format('%4.2fms',[Data.UnitRec.TotalOwnTime]);
      5: CellText := Format('%4.2fms',[Data.UnitRec.TotalChildTime]);
      6: CellText := Format('%4.2fms',[Data.UnitRec.TotalTime]);
    end
    else if Sender.GetNodeLevel(Node) = 2 then
    case Column of
      0: CellText := Data.FunctionRec.FunctionName;
      1: CellText := IntToStr(Data.FunctionRec.Count);
      4: CellText := Format('%4.2fms',[Data.FunctionRec.TotalOwnTime]);
      5: CellText := Format('%4.2fms',[Data.FunctionRec.TotalChildTime]);
      6: CellText := Format('%4.2fms',[Data.FunctionRec.TotalTime]);
    end
    else if Sender.GetNodeLevel(Node) = 3 then
    case Column of
      0: CellText := Data.LineRec.LineString;
      1: CellText := IntToStr(Data.LineRec.Count);
//      4: CellText := Format('%4.2fms',[Data.FunctionRec.TotalOwnTime]);
//      5: CellText := Format('%4.2fms',[Data.FunctionRec.TotalChildTime]);
      6: CellText := Format('%4.2fms',[Data.LineRec.Time]);
    else
      CellText := '';
    end;
  end;
  except
    Sleep(0);
  end;
end;

procedure TframDetailedView.vtreeItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  ParentData,
  Data: PMyRec;
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.

    //MODULE
    if Sender.GetNodeLevel(Node) = 0 then
    begin
      if ThreadItem.ModuleResult.Count <= Integer(Node.Index) then exit;
      Data.ModuleRec          := TModule(ThreadItem.ModuleResult.Items[Node.Index]);
//      Data.TotalProcent       := (Data.ModuleRec.TotalOwnTime * 100) / ThreadItem.SampleCount;
//      Data.TotalChildProcent  := (Data.ModuleRec.TotalTime * 100)    / ThreadItem.SampleCount;
      Data.TotalProcent       := (Data.ModuleRec.TotalOwnTime * 100) / ThreadItem.Duration;
      Data.TotalChildProcent  := (Data.ModuleRec.TotalTime * 100)    / ThreadItem.Duration;
      ChildCount[Node]        := Data.ModuleRec.Units.Count;
    end
    //UNIT
    else if Sender.GetNodeLevel(Node) = 1 then
    begin
      ParentData := Sender.GetNodeData(Node.Parent);

      if (ParentData.ModuleRec.Units.Count <= Integer(Node.Index)) then exit;
      Data.UnitRec            := TUnit(ParentData.ModuleRec.Units.Items[Node.Index]);
      Data.TotalProcent       := (Data.UnitRec.TotalOwnTime * 100) / ThreadItem.Duration;
      Data.TotalChildProcent  := (Data.UnitRec.TotalTime * 100)    / ThreadItem.Duration;
      ChildCount[Node]        := Data.UnitRec.Functions.Count;
    end
    //FUNCTION
    else if Sender.GetNodeLevel(Node) = 2 then
    begin
      ParentData := Sender.GetNodeData(Node.Parent);

      if (ParentData.UnitRec.Functions.Count <= Integer(Node.Index)) then exit;
      Data.FunctionRec        := TFlatFunction(ParentData.UnitRec.Functions.Items[Node.Index]);
      Data.TotalProcent       := (Data.FunctionRec.TotalOwnTime * 100) / ThreadItem.Duration;
      Data.TotalChildProcent  := (Data.FunctionRec.TotalTime * 100)    / ThreadItem.Duration;
      ChildCount[Node]        := Data.FunctionRec.FunctionLines.Count;
    end
    //LINE
    else if Sender.GetNodeLevel(Node) = 3 then
    begin
      ParentData := Sender.GetNodeData(Node.Parent);

      if (ParentData.FunctionRec.FunctionLines.Count <= Integer(Node.Index)) then exit;
      Data.LineRec            := TFunctionLine(ParentData.FunctionRec.FunctionLines.Items[Node.Index]);
      Data.TotalProcent       := (Data.LineRec.Time * 100) / ThreadItem.Duration;
//      Data.TotalChildProcent  := (Data.LineRec.TotalTime * 100)    / ThreadItem.SampleCount;
//      ChildCount[Node]        := Length(Data.FunctionRec.FunctionLines);
    end;
  end;
end;

end.
