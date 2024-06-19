unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    btnLoadDll: TBitBtn;
    btnShowProfiler: TBitBtn;
    btnExecuteFunction: TBitBtn;
    OpenDialog1: TOpenDialog;
    btnAllBusyFunctionsOnce_1861msec: TButton;
    btnAllSleepFunctionsOnce_1861msec: TButton;
    btn10xAllBusyFunctions: TButton;
    btn10xAllSleepFunctions: TButton;
    BitBtn1: TBitBtn;
    btnAddFunction: TBitBtn;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    procedure btnLoadDllClick(Sender: TObject);
    procedure btnShowProfilerClick(Sender: TObject);
    procedure btnAddFunctionClick(Sender: TObject);
    procedure btnExecuteFunctionClick(Sender: TObject);
    procedure btnAllBusyFunctionsOnce_1861msecClick(Sender: TObject);
    procedure btnAllSleepFunctionsOnce_1861msecClick(Sender: TObject);
    procedure btn10xAllBusyFunctionsClick(Sender: TObject);
    procedure btn10xAllSleepFunctionsClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    hDll:THandle;
    { Private declarations }
  protected
    procedure TestBeep;overload;
    procedure TestBeep2(const aFreq, aDuration: Cardinal);overload;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses _uAsmProfDllInterface,
   //_uProfilerManager,
   //_uAsmProfDllLoader,
   //_frmResults,
  _uTestFunctions,
  _uAsmProfDllLoader;

{$R *.dfm}

procedure TForm1.btnLoadDllClick(Sender: TObject);
begin
 hDll := LoadLibrary('..\..\..\DllVersion\Release\AsmProfiler.dll');
  if hDll <= 0 then
    hDll := LoadLibrary('AsmProfiler.dll');

  if hDll <= 0 then
    MessageDlg('Dll load failed', mtError, [mbOK], 0)
  else
  begin
    LoadProfilerDll;
    MessageDlg('Dll loaded', mtInformation, [mbOK], 0);
  end;
end;

procedure TForm1.btnShowProfilerClick(Sender: TObject);
var
  pCreate:TCreateProfileForm;
  pShow:TShowProfileForm;
begin
  if hDll > 0 then
  begin
    pCreate := GetProcAddress(hdll,'CreateProfileForm');
    pCreate;
    
    pShow := GetProcAddress(hdll,'ShowProfileForm');
    pShow;
  end;
end;

procedure subprocedure2;
begin
  raise exception.Create('test');
  sleep(0);
end;

procedure finallyend;
begin
  sleep(0);
end;

procedure subprocedure;
begin
  try
    subprocedure2;
  finally
    finallyend;
  end;
end;

procedure exceptend;
begin
  sleep(0);
end;

type
  tprocedure = procedure;

procedure TForm1.Button1Click(Sender: TObject);
var
  h: cardinal;
  p: pointer;
begin
  h := LoadLibrary(pchar('kernel32.dll'));
  p := GetProcAddress(h, 'LoadLibraryA');
  if p <> nil then
    tprocedure(p)();

  try
    subprocedure;
  except
    exceptend;
  end;
end;

function ParamTest(a1, a2, a3, a4: integer; var a5: integer; out a6: integer): integer;
begin
  a5 := 5;
  a6 := 6;
  Result := a1 + a2 + a3 + a4;
end;

procedure DoParamTest;
var
  iResult, i5, i6: integer;
begin
  iResult := ParamTest(1,2,3,4, i5, i6);
  assert(iResult = 1+2+3+4);
  assert(i5      = 5);
  assert(i6      = 6);
end;

procedure ReCursiveTest(var aDeep: integer);
begin
  dec(aDeep);
  if aDeep > 0 then
    ReCursiveTest(aDeep);
  DoParamTest;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  i := 10;
  ReCursiveTest(i);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  //Unit2.TForm2.Create(Self).Show;
end;

{
ESP           EBP
F510          540  >                  btn
F4E8          530    >                sub
F4D4          508      >              sub2
EC0C          508        >            f.e
EC0C          508        <            f.e
              ???      <
              ???    <
F03C          530    >                e.e
F03C          530    <                e.e
F510          540  <
}

procedure TForm1.btnAddFunctionClick(Sender: TObject);
var
  pAdd:TAddCustomFunction;
begin
  if hDll > 0 then
  begin
    pAdd := GetProcAddress(hdll,'AddCustomFunction');

    pAdd('Unit1','btnExecuteFunctionClick',@TForm1.btnExecuteFunctionClick);
    pAdd('Unit1','TestBeep',@TForm1.TestBeep);
    pAdd('Unit1','TestBeep2',@TForm1.TestBeep2);

  end;
  //MessageDlg(format('WaitMessage: %d',[Cardinal(@WaitMessage)]), mtWarning, [mbOK], 0);
end;

procedure TForm1.btnExecuteFunctionClick(Sender: TObject);
begin
  TestBeep;
  TestBeep2(20, 200);
end;

procedure TForm1.TestBeep;
begin
  Beep;
end;

procedure TForm1.TestBeep2(const aFreq, aDuration: Cardinal);
begin
  Windows.Beep(aFreq,aDuration);
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

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  h: thandle;
begin
  h := LoadLibrary(pchar('..\DllVersion\AsmProfiler_inject.dll'));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //if not LoadProfilerDll then
  begin
    btnLoadDllClick(nil);
    btnShowProfilerClick(nil);
  end;
  ShowProfileForm;
end;

end.
