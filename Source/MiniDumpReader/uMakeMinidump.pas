unit uMakeMinidump;

interface

uses
  Dialogs,
  SysUtils, Forms, Windows;

  procedure GetDebugPrivileges;
  procedure MakeMinidump(aPID:integer; const aOutputFile: string);

implementation

uses
  JwaImageHlp, mcExtraImageHlp;

function MiniDumpWriteDump(hProcess: THANDLE; ProcessId: DWORD; hFile: THANDLE; DumpType: DWORD; ExceptionParam: pointer; UserStreamParam: pointer; CallbackParam: pointer): BOOL; stdcall;
    external 'dbghelp.dll' name 'MiniDumpWriteDump';
{$EXTERNALSYM MiniDumpWriteDump}

const
  SE_DEBUG_NAME = 'SeDebugPrivilege' ;

procedure GetDebugPrivileges;
var
  hToken: THandle;
  tkp: TTokenPrivileges;
  retval: cardinal;
begin
  if (OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or  TOKEN_QUERY, hToken)) then
  begin
    LookupPrivilegeValue(nil, SE_DEBUG_NAME, tkp.Privileges[0].Luid);
    tkp.PrivilegeCount := 1;
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    AdjustTokenPrivileges(hToken, false, tkp, 0, nil, retval);
  end;
end;

procedure MakeMinidump(aPID:integer; const aOutputFile: string);
var
  hProc,
  hFile: THandle;
  sFile: string;
begin
  GetDebugPrivileges;

  sFile  := aOutputFile;
  hProc  := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, aPID);
  hFile  := CreateFile(PChar(sFile),
                      GENERIC_WRITE,FILE_SHARE_WRITE,nil,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,0);
  try
    if not MiniDumpWriteDump(
        hProc,
        aPID,
        hFile,
        MiniDumpNormal or MiniDumpWithHandleData or
          MiniDumpWithProcessThreadData or MiniDumpWithThreadInfo,
        nil, nil ,nil)
    then
      RaiseLastOSError;
  finally
    FileClose(hfile);
  end;
end;

end.
