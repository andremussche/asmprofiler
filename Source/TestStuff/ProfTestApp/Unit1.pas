unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jclDebug, Buttons, CheckLst,
  ComCtrls, VirtualTrees;

type
  PJclMapProcNameExt = ^TJclMapProcNameExt;
  TJclMapProcNameExt = record
    Addr: DWORD;
    ProcName: String;
  end;

  PJclMapSegmentExt = ^TJclMapSegmentExt;
  TJclMapSegmentExt = record
    StartAddr: DWORD;
    EndAddr: DWORD;
    UnitName: String;
    UnitFile: String;
    Procs: array of TJclMapProcNameExt;
  end;

  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: WideString;
    Data: Pointer;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    btnProfileSelected: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    btnLoad: TBitBtn;
    Button3: TButton;
    btnAllBusyFunctionsOnce_1861msec: TButton;
    btnAllSleepFunctionsOnce_1861msec: TButton;
    btn10xAllBusyFunctions: TButton;
    btn10xAllSleepFunctions: TButton;
    BitBtn1: TBitBtn;
    BitBtn5: TBitBtn;
    Button4: TButton;
    Edit1: TEdit;
    BitBtn6: TBitBtn;
    Button5: TButton;
    BitBtn7: TBitBtn;
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAllBusyFunctionsOnce_1861msecClick(Sender: TObject);
    procedure btnAllSleepFunctionsOnce_1861msecClick(Sender: TObject);
    procedure btn10xAllBusyFunctionsClick(Sender: TObject);
    procedure btn10xAllSleepFunctionsClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
  private
    { Private declarations }
//    FMapFile: TJclMapParser;

//    FProcNamesCnt: Integer;
//    FTopValidAddr: Integer;
    //FMemoryOffset: Integer;

    //FLineNumbers: array of TJclMapLineNumber;
//    FProcNames: array of TJclMapProcNameExt;
//    FSegments: array of TJclMapSegmentExt;
    //FSourceNames: array of TJclMapProcName;
  protected
    procedure TestSleep;
  public
    { Public declarations }
  end;

  //function InitTest: boolean;

  procedure ExecuteProfileTest;
  function ProfileTest(const aParam1: integer; var aParam2, aParam3, aParam4: integer): integer;

var
  Form1: TForm1;
  //GTrampoline_ProfileTest: pointer;

implementation

uses
  AsmDetours, _uAsmProfiler,
  Math, Dateutils,
  //_uProfileTypes,
  _uDllImplementation,
  _uProfileTools,
//  _uDbgLoader,
  _uTestFunctions, _uProfileClasses;

{$R *.dfm}

function ProfileTest(const aParam1: integer; var aParam2, aParam3, aParam4: integer): integer;
begin
  Result  := aParam1 + aParam2 + aParam3 + aParam4;

  aParam2 := aParam3;
  aParam3 := aParam4;
  aParam4 := aParam1;
end;

procedure ExecuteProfileTest;
var
  i,i2,i3,i4: integer;
begin
  i  := 1;
  i2 := 2;
  i3 := 3;
  i4 := 4;
  i  := ProfileTest(i,i2,i3,i4);

  assert(i = 1+2+3+4);
  assert(i2 = 3);
  assert(i3 = 4);
  assert(i4 = 1);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i1,i2, i3,iDiff: Int64;
begin
  //CreateProfileIntercept(@ExecuteProfileTest);
  //CreateProfileIntercept(@ProfileTest);
  //CreateProfileIntercept(@DummyProfileMarker);
  //GTrampoline_ProfileTest := KOLDetours.InterceptCreate(@ProfileTest, @ProfileStart);

  ExecuteProfileTest;
  //DumpProfileResults;

  i1 := ClockTicks;
  i3 := DummyProfileMarker;
  i2 := ClockTicks;

  iDiff := i3 - i1;
  Memo1.Lines.Add(Format('Overhead: %d',[iDiff]));
  iDiff := i2 - i3;
  Memo1.Lines.Add(Format('Overhead: %d',[iDiff]));
  iDiff := i2 - i1;
  Memo1.Lines.Add(Format('Overhead: %d',[iDiff]));
end;


type
  PMethod = ^TMethod;
  TObjectProcedure = procedure of object;

procedure TForm1.BitBtn1Click(Sender: TObject);
{
var
  sMap:string;
//  sExe:string;
  i,i2:integer;
  p,p2:pointer;
//  m:TMethod;
  op: TObjectProcedure;
//  mp: PMethod;
}
begin
  {
  sMap     := ChangeFileExt(Application.ExeName,'.map');
  FMapFile := TJclMapParser.Create( sMap );

  SetLength(FSegments, 0);
  SetLength(FProcNames, 0);

  FMapFile.OnSegment        := Self.JclMapSegmentEvent;
  FMapFile.OnClassTable     := Self.JclMapClassTableEvent;
//  FMapFile.OnPublicsByValue := Self.JclMapPublicsEvent;
  FMapFile.OnPublicsByName  := Self.JclMapPublicsEvent;
  FMapFile.OnLineNumberUnit := Self.JclMapLineNumberUnitItem;
  FmapFile.Parse;

  p := @FMapFile.OnClassTable;
  label1.Caption := format('%s: %d / %p',['JclMapClassTableEvent',Integer(p), p]);
  }

  {
  op := DumpSegmentProcs;
  p  := @op;
  i  := _GetProcByName('TForm1.DumpSegmentProcs');
  i2 := FProcNames[i].Addr;
  p2 := Pointer(i2);
  i2 := Integer(p) - Integer(p2);
  lblOffSet.Caption := format('%s: %p - %p = %p',['TJclMapParser.OnClassTable',p, p2, Pointer(i2) ]);

  DumpSegmentProcs;

  FMapFile.Free;
  }
end;

threadvar
  t:integer;

procedure TForm1.Button2Click(Sender: TObject);
var
  i1,i2,iDiff: Int64;
  cThread: cardinal;
  rc, re,ru,rk: TFileTime;
begin
  i1 := ClockTicks;
  i2 := ClockTicks;
  iDiff := i2 - i1;
  MessageDlg('RDTSC diff: ' + IntToStr(iDiff), mtWarning, [mbOK], 0);

  i1 := ClockTicks;
  t  := 1;
  i2 := ClockTicks;
  iDiff := i2 - i1;
  MessageDlg('Threadvar diff: ' + IntToStr(iDiff), mtWarning, [mbOK], 0);

  i1 := ClockTicks;
  cThread := GetCurrentThreadId;
  i2 := ClockTicks;
  iDiff := i2 - i1;
  MessageDlg('GetCurrentThreadId diff: ' + IntToStr(iDiff), mtWarning, [mbOK], 0);

  i1 := ClockTicks;
  GetThreadTimes(cThread,rc,re,rk,ru);
  i2 := ClockTicks;
  iDiff := i2 - i1;
  MessageDlg('GetThreadTimes diff: ' + IntToStr(iDiff), mtWarning, [mbOK], 0);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
//  CreateProfileIntercept( @sleep, 'sleep' );
//  CreateProfileIntercept( @sleep, 'sleep' );
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  TestSleep;
end;

procedure TForm1.TestSleep;
begin
  {sleep(100);
  sleep(200); }
  Windows.Beep(1,200);
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  AddCustomFunction('TForm1','TestSleep',@TForm1.TestSleep);
end;

procedure TForm1.btn10xAllBusyFunctionsClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 10 do
    btnAllBusyFunctionsOnce_1861msec.Click;
end;

procedure TForm1.btn10xAllSleepFunctionsClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 10 do
    btnAllSleepFunctionsOnce_1861msec.Click;
end;

procedure TForm1.btnAllBusyFunctionsOnce_1861msecClick(Sender: TObject);
begin
  Busy1msec;
  Busy10msec;
  Busy100msec;
  Busy250msec;
  Busy500msec;
  Busy1000msec;            //1861
end;

procedure TForm1.btnAllSleepFunctionsOnce_1861msecClick(Sender: TObject);
begin
  Sleep1msec;
  Sleep10msec;
  Sleep100msec;
  Sleep250msec;
  Sleep500msec;
  Sleep1000msec;         //1861
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  //
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  str: TStrings;
begin
  str := TStringList.Create;
  //_uDbgLoader.LoadDbgFile(ChangeFileExt(Application.ExeName, '.dbg'), str);
//  _uDbgLoader.LoadDbgFile('FPCamera.pdb', str);
  str.SaveToFile('dbg details.txt');
  str.free;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  FLoadedDllsInfo: TProgramLoadedDllsStorage;
  t: tdatetime;
begin
  t := now;

  FLoadedDllsInfo := TProgramLoadedDllsStorage.Create;
  //FLoadedDllsInfo.LoadProgramLoadedDlls;
  FLoadedDllsInfo.LoadSelectionFromFile('LoadedDllsSelectedProcs.ini');

  t := now - t;
  MessageDlg('create+selection: ' + formatdatetime('hh:nn:ss:zzz',t), mtWarning, [mbOK], 0);

  t := now;
  FLoadedDllsInfo.LoadProgramLoadedDlls;
  t := now - t;
  MessageDlg('LoadProgramLoadedDlls: ' + formatdatetime('hh:nn:ss:zzz',t), mtWarning, [mbOK], 0);

  FLoadedDllsInfo.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  now
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
  LoadLibrary(pchar('C:\Program Files\GExperts for Delphi 7\GExpertsD7.dll'));
  LoadLibrary(pchar('C:\Program Files\Borland\Delphi7\Bin\EXPTDEMO.DLL'));

end;

function DummyTest: integer;
var i,j: integer;
begin
  i := 0;
  i := i + 1;
  j := i * 2;
  Result := i + j;
end;

function DummyTest2: integer;
var i, j: integer;
begin
  i := 0;
  j := 1;
  i := i + 1;
  i := i + 1;
  i := i + 1;
  i := i + 1;
  Result := i + j;
end;

procedure DummyTest1000;
var i:integer;
begin
  for i := 1 to 1000 do
    DummyTest;
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
var i, iCount :integer;
begin
  if TryStrToInt(Edit1.Text, iCount) then
  for i := 1 to iCount do
  begin
    DummyTest1000;
    DummyTest2;
  end;
end;

var bException: boolean;

procedure DummyTestLevel6;
begin
  if bException then
    raise Exception.Create('test');                //system
  sleep(0);
end;

procedure DummyTestLevel5;
begin
  DummyTestLevel6;
  sleep(1);
  DummyTestLevel6;
  sleep(1);
  DummyTestLevel6;
  sleep(1);
  DummyTestLevel6;
  sleep(1);
  DummyTestLevel6;
  sleep(1);
  DummyTestLevel6;
end;

procedure DummyTestLevel4;
begin
  DummyTestLevel5;
  DummyTestLevel5;
  DummyTestLevel5;
  DummyTestLevel6;
  DummyTestLevel5;
  DummyTestLevel5;
end;

procedure DummyTestLevel3;
begin
  DummyTestLevel4;
  DummyTestLevel4;
  DummyTestLevel5;
  DummyTestLevel4;
  DummyTestLevel4;
end;

procedure DummyTestLevel2;
begin
  DummyTestLevel3;
  DummyTestLevel3;
  DummyTestLevel4;
  DummyTestLevel3;
end;

procedure DummyTestLevel1;
begin
  DummyTestLevel2;
  DummyTestLevel3;
  DummyTestLevel2;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  StartProfiler(False);
  DummyTestLevel1;
end;

procedure DummyTestWithException;
begin
  raise Exception.Create('test');                //system
end;

procedure DummyTestExceptionHandled;
begin
  sleep(0);
end;

procedure DummyTestAfterException;
begin
  sleep(0);
end;

procedure TForm1.BitBtn7Click(Sender: TObject);
begin
  try
    bException := True;
    DummyTestLevel1;
    //DummyTestWithException;
  except
    bException := False;
    DummyTestExceptionHandled;
  end;
  DummyTestAfterException;
end;

end.

