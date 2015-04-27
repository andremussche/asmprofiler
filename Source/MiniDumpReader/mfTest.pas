unit mfTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMinidumpTest = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMinidumpTest: TfrmMinidumpTest;

implementation

uses
  uMakeMinidump, mcTestThreads;

{$R *.dfm}

procedure TfrmMinidumpTest.Button1Click(Sender: TObject);
var
  t1, t2: TThread;
begin
  t1 := TTestThread.Create(False{direct run});
  t2 := TTestThread2.Create(False{direct run});
  try
    Sleep(10); //make sure test threads are running
    uMakeMinidump.MakeMinidump(GetCurrentProcessId, 'minidumptest.dmp');

    MessageDlg('Minidump created!', mtInformation, [mbOK], 0);
  finally
    t1.FreeOnTerminate := True;
    t1.Terminate;
    t2.FreeOnTerminate := True;
    t2.Terminate;
  end;
end;

end.
