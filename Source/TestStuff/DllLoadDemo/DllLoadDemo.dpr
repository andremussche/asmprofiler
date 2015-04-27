program DllLoadDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Forms,
  _uAsmProfDllInterface in '_uAsmProfDllInterface.pas',
  _uAsmProfDllLoader in '_uAsmProfDllLoader.pas';

var i: Integer;
begin
  Writeln('Trying to load AsmProfiler.dll...');
  //try to load AsmProfiler.dll
  //Must be in a search path or same directory
  if LoadProfilerDll then
  begin
    Writeln('Dll loaded!');
  end
  else
  begin
    Writeln('Dll not loaded! Maybe dll not found or error during loading');
    Writeln('Press enter to stop');
    Readln;
    Exit;
  end;

  //you can also use command line params:
  //-profile:      load dll and show form
  //-profilestart: load dll, show form, direct start profiling

  Writeln('');
  Writeln('We will now do some profiling, press enter to continue');
  Readln;

  Writeln('Showing profiler form.');
  ShowProfileForm;  //not needed for bare profiling, just to show the form :)
  Application.ProcessMessages;

  //we add fixed in code a function here, normally you do it manually via "Select items"
  //dialog in the profiler popup form
  if _uAsmProfDllLoader.SelectFunction('Windows', 'Sleep') < 0 then
    Writeln('"Sleep" function could not be profiled!');
  //if _uAsmProfDllLoader.SelectFunction('Windows', '*' {everthing!}) < 0 then
  //  Writeln('"Windows" unit could not be profiled!');

  _uAsmProfDllLoader.StartProfiler(false);
  try
    for i := 0 to 100 do
      Sleep(10);
  finally
    _uAsmProfDllLoader.StopProfiler;
  end;
  Writeln('Profiling finished.');

  Writeln('Showing results');
  _uAsmProfDllLoader.ShowResultsForm;

  while not Application.Terminated do
    Application.HandleMessage;

  _uAsmProfDllLoader.HideProfileForm;
  Application.ProcessMessages;

  Writeln('');
  Writeln('Press enter to stop');
  Readln;

  _uAsmProfDllLoader.UnLoadProfilerDll;
end.
