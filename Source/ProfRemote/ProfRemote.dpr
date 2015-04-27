program ProfRemote;

uses
  Forms,
  _frmMain in '_frmMain.pas' {frmMain},
  _uIPC in '_uIPC.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
