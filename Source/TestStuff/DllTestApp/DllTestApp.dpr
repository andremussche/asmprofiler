program DllTestApp;

uses
  //ShareMem,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  _uTestFunctions in '..\ProfTestApp\_uTestFunctions.pas',
  _uAsmProfDllInterface in '..\..\DllVersion\_uAsmProfDllInterface.pas',
  _uAsmProfDllLoader in '..\..\DllVersion\_uAsmProfDllLoader.pas';

//_uAsmProfDllInterface in '_uAsmProfDllInterface.pas',
  //_uAsmProfDllLoader in '_uAsmProfDllLoader.pas',
  //_uTestFunctions in '_uTestFunctions.pas';

{$R *.res}

begin
  Application.Initialize;

  //if LoadProfilerDll then
  //  ShowProfileForm;

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
