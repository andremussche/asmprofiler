unit _framProfileResultProcsTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  _uProfileTypes,  
  Dialogs, VirtualTrees, Menus;

type
  PUnitRec = ^TUnitRec;
  TUnitRec = record
    UnitName,
    ClassName,
    FunctionName: String;
    //
    Calls: integer;
    UnitProcent, TotalProcent, TotalChildProcent: Single;
    AvgTime, AvgChildTime, AvgWithChildTime: Real;
    //
    ProfiledUnit: PProfiledUnit;
    ProfiledProc: PProfiledProc;
  end;
  
  TframProfileResultProcsTree = class(TFrame)
    vtreeUnit: TVirtualStringTree;
    PopupMenu1: TPopupMenu;
    SavetoCSV1: TMenuItem;
    SavetoRTF1: TMenuItem;
    SavetoText1: TMenuItem;
    procedure vtreeUnitInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

//    procedure vtreeUnitHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
//      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//    procedure vtreeUnitGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
//      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);

    procedure vtreeUnitFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeUnitCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure SavetoCSV1Click(Sender: TObject);
    procedure SavetoRTF1Click(Sender: TObject);
    procedure SavetoText1Click(Sender: TObject);

    procedure vtreeUnitGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtreeUnitHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
  private
    FProfiledProcArray: TProfiledProcArray;
    FProfiledUnitArray: TProfiledUnitArray;
    FTotalCycles: int64;
    procedure SetProfiledProcArray(const Value: TProfiledProcArray);
    procedure SetProfiledUnitArray(const Value: TProfiledUnitArray);
    procedure SetTotalCycles(const Value: int64);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;

    { Public declarations }
    property ProfiledUnitArray: TProfiledUnitArray read FProfiledUnitArray write SetProfiledUnitArray;
    //property ProfiledProcArray: TProfiledProcArray read FProfiledProcArray write SetProfiledProcArray;
    property TotalCycles: int64 read FTotalCycles write SetTotalCycles;
  end;

implementation

uses Math, _uAsmProfiler, StrUtils;

{$R *.dfm}

constructor TframProfileResultProcsTree.Create(AOwner: TComponent);
begin
  inherited;
  
  // Let the tree know how much data space we need.
  vtreeUnit.NodeDataSize := SizeOf(TUnitRec);
  // Set an initial number of nodes.
  vtreeUnit.RootNodeCount := 0;
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

procedure TframProfileResultProcsTree.SavetoCSV1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( vtreeUnit.ContentToHTML(tstAll, 'Profiler Result Tree HTML Export') );
end;

procedure TframProfileResultProcsTree.SavetoRTF1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( vtreeUnit.ContentToRTF(tstAll) );
end;

procedure TframProfileResultProcsTree.SavetoText1Click(Sender: TObject);
begin
  SaveStringToFile_Ask( vtreeUnit.ContentToText(tstAll, ',') );
end;

procedure TframProfileResultProcsTree.SetProfiledProcArray(const Value: TProfiledProcArray);
begin
  FProfiledProcArray := Value; 
end;

procedure TframProfileResultProcsTree.SetProfiledUnitArray(const Value: TProfiledUnitArray);
begin
  FProfiledUnitArray := Value;

  vtreeUnit.Clear;
  vtreeUnit.RootNodeCount := Length(FProfiledUnitArray);
end;

procedure TframProfileResultProcsTree.SetTotalCycles(const Value: int64);
begin
  FTotalCycles := Value;
end;

procedure TframProfileResultProcsTree.vtreeUnitCompareNodes(Sender: TBaseVirtualTree; Node1,
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
     7: Result := CompareValue( Data1.AvgTime, Data2.AvgTime );
     8: Result := CompareValue( Data1.AvgChildTime, Data2.AvgChildTime );
     9: Result := CompareValue( Data1.AvgWithChildTime, Data2.AvgWithChildTime );
  end;
end;

procedure TframProfileResultProcsTree.vtreeUnitFreeNode(Sender: TBaseVirtualTree;
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

procedure TframProfileResultProcsTree.vtreeUnitGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PUnitRec;

  function __CalcTimeDiff(const aDiff: single): string;
  begin
    if aDiff >= 1 then
      Result := Format('%8.2fs',[aDiff])
    else if aDiff >= 0.0001 then
      Result := Format('%8.2fms',[aDiff * 1000])
    else if aDiff >= 0.0000001 then
      Result := Format('%8.2fµs',[aDiff * 1000 * 1000])
    else //if tDiff >= 0.0000001 then
      Result := Format('%8.2fns',[aDiff * 1000 * 1000 * 1000])
  end;

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
      7: CellText := __CalcTimeDiff(Data.AvgTime);
      8: CellText := '';
      9: CellText := '';
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
      7: CellText := __CalcTimeDiff(Data.AvgTime);
      8: CellText := __CalcTimeDiff(Data.AvgChildTime);
      9: CellText := __CalcTimeDiff(Data.AvgWithChildTime);
    else ; end;
  end;
end;

procedure TframProfileResultProcsTree.vtreeUnitHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
begin
  if (vtreeUnit.Header.SortColumn <> HitInfo.Column) then
    vtreeUnit.Header.SortColumn := HitInfo.Column
  else if (vtreeUnit.Header.SortDirection = sdAscending) then
    vtreeUnit.Header.SortDirection := sdDescending
  else
    vtreeUnit.Header.SortDirection := sdAscending;

  vtreeUnit.SortTree( HitInfo.Column, vtreeUnit.Header.SortDirection );
end;

procedure TframProfileResultProcsTree.vtreeUnitInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data,DataParent: PUnitRec;

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
//        Result := Copy(aFullFunction, ipos+1, Length(aFullFunction));    //Test
//    end;
  end;

begin
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
        Data.AvgTime           := TotalProfileTime / GCPUSpeed;
      end;
      ChildCount[Node] := Data.ProfiledUnit.ProcsCount;
      InvalidateToBottom(Node);
    end
    else if Sender.GetNodeLevel(Node) = 1 then
    begin
      DataParent := GetNodeData(Node.Parent);

      Data.UnitName     := DataParent.UnitName;
      Data.ProfiledUnit := DataParent.ProfiledUnit;
      Data.ProfiledProc := DataParent.ProfiledUnit.ProfiledProcs[Node.Index];
      Data.ClassName    := __ExtractClassName( Data.ProfiledProc.ProcName );
      Data.FunctionName := __ExtractFunctionName( Data.ProfiledProc.ProcName );

      with Data.ProfiledProc^ do
      begin
        Data.Calls             := CallCount;
        Data.UnitProcent       := (TotalOwnProfileTime * 100) / Data.ProfiledUnit^.TotalProfileTime;
        Data.TotalProcent      := (TotalOwnProfileTime * 100) / FTotalCycles;
        Data.TotalChildProcent := ((TotalChildTime + TotalOwnProfileTime) * 100) / FTotalCycles;
        Data.AvgTime           := TotalOwnProfileTime / GCPUSpeed;
        Data.AvgChildTime      := TotalChildTime/ GCPUSpeed;
        Data.AvgWithChildTime  := (TotalChildTime + TotalOwnProfileTime) / GCPUSpeed;
      end;

      ChildCount[Node] := Data.ProfiledProc.ChildCount;
      InvalidateToBottom(Node);
    end
    else if Sender.GetNodeLevel(Node) = 2 then
    begin
      DataParent := GetNodeData(Node.Parent);

      Data.ProfiledUnit   := @FProfiledUnitArray[
                                 DataParent.ProfiledProc.ChildProcs[Node.Index].UnitIndex ];
      Data.UnitName       := Data.ProfiledUnit.UnitName;
      Data.ProfiledProc   := DataParent.ProfiledProc.ChildProcs[Node.Index];

      Data.ClassName    := __ExtractClassName( Data.ProfiledProc.ProcName );
      if Data.UnitName <> DataParent.UnitName then
        Data.FunctionName := Data.UnitName + '.'+ Data.ProfiledProc.ProcName;
      Data.FunctionName := __ExtractFunctionName( Data.ProfiledProc.ProcName );

      with Data.ProfiledProc^ do
      begin
        Data.Calls             := CallCount;
        Data.UnitProcent       := (TotalOwnProfileTime * 100) / (DataParent.ProfiledProc^.TotalOwnProfileTime + DataParent.ProfiledProc^.TotalChildTime);
        //Data.UnitProcent       := -1;
        Data.TotalProcent      := (TotalOwnProfileTime * 100) / FTotalCycles;
        Data.TotalChildProcent := ((TotalChildTime + TotalOwnProfileTime) * 100) / FTotalCycles;
        Data.AvgTime           := TotalOwnProfileTime / GCPUSpeed;
        Data.AvgChildTime      := TotalChildTime/ GCPUSpeed;
        Data.AvgWithChildTime  := (TotalChildTime + TotalOwnProfileTime) / GCPUSpeed;
      end;
    end;
  end;
end;

end.
