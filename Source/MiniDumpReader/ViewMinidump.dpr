program ViewMinidump;

uses
  Forms,
  mfMainForm in 'mfMainForm.pas' {frmMain},
  mcOfflineStackInfo in 'mcOfflineStackInfo.pas',
  mcOfflineDebugInfo in 'mcOfflineDebugInfo.pas',
  mcMiniDumpReader in 'mcMiniDumpReader.pas',
  mcExtraImageHlp in 'mcExtraImageHlp.pas',
  uDragDrop in 'uDragDrop.pas';

{$R *.res}

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
