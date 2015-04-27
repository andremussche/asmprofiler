program MinidumpTest;

uses
  Forms,
  mfTest in 'mfTest.pas' {frmMinidumpTest},
  uMakeMinidump in 'uMakeMinidump.pas',
  mcTestThreads in 'mcTestThreads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMinidumpTest, frmMinidumpTest);
  Application.Run;
end.
