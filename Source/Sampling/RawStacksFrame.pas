unit RawStacksFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls,
  uResultTypes, mcThreadSampler, mcProcessSampler;

type
  TframRawStacks = class(TFrame)
    lbStackDumps: TListBox;
    mmoStacks: TMemo;
    procedure lbStackDumpsClick(Sender: TObject);
  private
    FThreadSampler: TThreadSampler;
    FProcessObject: TProcessSampler;
    FSelectProcedure: string;
    procedure SetThreadSampler(const Value: TThreadSampler);
    procedure MakeStackTrace(aStack: PSnapshotStack);
    { Private declarations }
  public
    { Public declarations }
    property ThreadSampler: TThreadSampler read FThreadSampler write SetThreadSampler;
    property ProcessObject: TProcessSampler read FProcessObject write FProcessObject;

    procedure FilterStackNrs(aSnapshotNrs: TIntegerList; const aSelectProcedure: string = '');
  end;

implementation

uses
  jclDebug;

{$R *.dfm}

{ TframRawStacks }

procedure TframRawStacks.SetThreadSampler(const Value: TThreadSampler);
begin
  FThreadSampler := Value;

  mmoStacks.Clear;
  FilterStackNrs(nil);
end;

procedure TframRawStacks.FilterStackNrs(aSnapshotNrs: TIntegerList; const aSelectProcedure: string = '');
var
  i, j, iNr: Integer;
  bAdd: boolean;
begin
  lbStackDumps.Clear;
  mmoStacks.Clear;
  FSelectProcedure := aSelectProcedure;

  if FThreadSampler <> nil then
  begin
    for i := 0 to FThreadSampler.SnapshotCount - 1 do
    begin
      if aSnapshotNrs <> nil then
      begin
        bAdd := False;
        iNr  := FThreadSampler.SnapshotStack[i].SnapshotNr;
        for j := 0 to aSnapshotNrs.Count - 1 do
        begin
          if aSnapshotNrs.Items[j] = iNr then
          begin
            bAdd := True;
            Break;
          end;
        end;
      end
      else
        bAdd := True;

      if bAdd then
        lbStackDumps.AddItem( IntToStr(FThreadSampler.SnapshotStack[i].SnapshotNr),
                              TObject(FThreadSampler.SnapshotStack[i]) );
    end;
  end;

  if lbStackDumps.Count > 0 then
    lbStackDumps.ItemIndex := 0;
  //load first dum
  lbStackDumpsClick(nil);
end;

procedure TframRawStacks.lbStackDumpsClick(Sender: TObject);
var
  psnapshot: PSnapshotStack;
  iPos : Integer;
begin
  if lbStackDumps.ItemIndex >= 0 then
  begin
    psnapshot := PSnapshotStack(lbStackDumps.Items.Objects[lbStackDumps.ItemIndex]);
    MakeStackTrace(psnapshot);
  end;

  //select text
  iPos := Pos(FSelectProcedure, mmoStacks.Text);
  if ipos > 0 then
  begin
    mmoStacks.SelStart  := iPos-1;
    mmoStacks.SelLength := Length(FSelectProcedure);
    mmoStacks.HideSelection := False;

    // scroll caret into view,
    // the wParam, lParam parameters are ignored
    SendMessage(mmoStacks.handle, EM_SCROLLCARET,0,0);
  end;
end;

procedure TframRawStacks.MakeStackTrace(aStack: PSnapshotStack);
var
//  isnapshot: Integer;
//  stack: TSnapshotStack;
  istack: Integer;
  info: TJclLocationInfo;
//  ff: TFlatFunction;
  sfunction: string;
//  fl: TFunctionLine;
//  dumpinfo: TStackDumpInfo;

//  childfunction: TFlatFunction;
//  parentsub, childsub: TSubFunction;
//  I: Integer;
//  iprevtime, idifftime: int64;
//  ftime: Double;
begin
  mmoStacks.Clear;
  mmoStacks.Lines.BeginUpdate;
  try

    //process stack functions
    for istack := Low(aStack.Functions) to High(aStack.Functions) do
    begin
      if not FProcessObject.GetLocationInfoRecord(aStack.Functions[istack], info) then
      begin
        Continue;
      end;

      sfunction := info.ProcedureName;
      if Pos(info.UnitName + '.', sfunction) = 1 then
        sfunction := Copy(sfunction, Length(info.UnitName) + 2,
                                     Length(sfunction) - Length(info.UnitName) - 1);
      if sfunction = '' then
        sfunction := IntToHex(Integer(info.Address), 8);

  {
    TJclLocationInfo = record
      Address: Pointer;               // Error address
      UnitName: string;               // Name of Delphi unit
      ProcedureName: string;          // Procedure name
      OffsetFromProcName: Integer;    // Offset from Address to ProcedureName symbol location
      LineNumber: Integer;            // Line number
      OffsetFromLineNumber: Integer;  // Offset from Address to LineNumber symbol location
      SourceName: string;             // Module file name
      DebugInfo: TJclDebugInfoSource; // Location object
      BinaryFileName: string;         // Name of the binary file containing the symbol
    end;
  }
  //[004AE713]{FobisPM -3551.exe} JclHookExcept.DoExceptNotify (Line 265, "JclHookExcept.pas" + 21)

      mmoStacks.Lines.Add( Format('[%p] {%s} %s (Line %d, "%s" + %d)',
                                  [info.Address,
                                   info.BinaryFileName,
                                   sfunction, info.LineNumber, info.SourceName,
                                   info.OffsetFromProcName]) );
    end;

    mmoStacks.SelStart  := 0;
    mmoStacks.SelLength := 0;
  finally
    mmoStacks.Lines.EndUpdate;
  end;
end;

end.
