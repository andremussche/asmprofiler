unit _uToolHelpWrappperFunction;

interface

uses windows;

{
  Author : Rezmond
  URL    : www.projectbionet.com
  Email  : rezmond@projectbionet.com

  Freeware for non commerical use

  ToolHelp Wrappper functions
  As well as a procedure to get Debug Privalages under win NT
}

 const
   TH32CS_SnapProcess = 2;

type  TProcessEntry32 = record
      dwSize              : DWORD;
      cntUsage            : DWORD;
      th32ProcessID       : DWORD;
      th32DefaultHeapID   : DWORD;
      th32ModuleID        : DWORD;
      cntThreads          : DWORD;
      th32ParentProcessID : DWORD;
      pcPriClassBase      : integer;
      dwFlags             : DWORD;
      szExeFile           : array [0..MAX_PATH-1] of char;
end;

 function CreateToolhelp32Snapshot (dwFlags,th32ProcessID: cardinal) : cardinal;
 function Process32First(hSnapshot: cardinal; var lppe: TProcessEntry32) : bool;
 function Process32Next(hSnapshot: cardinal; var lppe: TProcessEntry32) : bool;
 function FindProcess( Name : string) : dword;
 procedure GetDebugPrivs;
 procedure killbyPID( PID : DWORD);
 
implementation

uses SysUtils;


 
procedure killbyPID( PID : DWORD);
var hp : THANDLE;
begin
 hp := OpenProcess( PROCESS_TERMINATE , false, PID) ;
 TerminateProcess(hp,0);
end;



Const SE_DEBUG_NAME = 'SeDebugPrivilege' ;

procedure GetDebugPrivs;
var
  hToken: THandle;
  tkp: TTokenPrivileges;
  retval: dword;
begin

  if  (OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or  TOKEN_QUERY, hToken)) then
  begin
    if not LookupPrivilegeValue(nil, SE_DEBUG_NAME  , tkp.Privileges[0].Luid) then
      Win32Check(false);
    tkp.PrivilegeCount := 1;
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    if not AdjustTokenPrivileges(hToken, false, tkp, 0, nil, retval) then
      Win32Check(false);
  end
  else
    Win32Check(false)
end;


function FindProcess( Name : string) : dword;
var
   FSnapshotHandle : THandle;
   FProcessEntry32 : TProcessEntry32;
   ContinueLoop:BOOL;
   hp : Thandle;
begin

   FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
   FProcessEntry32.dwSize:=Sizeof(FProcessEntry32);
   ContinueLoop := Process32First(FSnapshotHandle,FProcessEntry32);
   while ContinueLoop do
   begin
     if Name = FProcessEntry32.szExeFile  then
        begin
           result := FProcessEntry32.th32ProcessID ;
           CloseHandle(FSnapshotHandle);
           exit;
        end;

       ContinueLoop := Process32Next(FSnapshotHandle,FProcessEntry32);
   end;
   CloseHandle(FSnapshotHandle);
end;

(*
function FindMainThreadId( PID : cardinal) : cardinal;
var
   FSnapshotHandle : THandle;
   FProcessEntry32 : TProcessEntry32;
   ContinueLoop:BOOL;
   hp : Thandle;
begin
   FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
   FProcessEntry32.dwSize:=Sizeof(FProcessEntry32);
   ContinueLoop := Process32First(FSnapshotHandle,FProcessEntry32);
   while ContinueLoop do
   begin
     if FProcessEntry32.th32ProcessID = aPID  then
        begin
           result := FProcessEntry32.cntThreads ;
           CloseHandle(FSnapshotHandle);
           exit;
        end;

       ContinueLoop := Process32Next(FSnapshotHandle,FProcessEntry32);
   end;
   CloseHandle(FSnapshotHandle);
end;
*)


var
 pCreateToolhelp32Snapshot : function (dwFlags,th32ProcessID: cardinal) : cardinal; stdcall = nil;
 pProcess32First :  function (hSnapshot: cardinal; var lppe: TProcessEntry32) : bool; stdcall = nil;
 pProcess32Next  :  function (hSnapshot: cardinal; var lppe: TProcessEntry32) : bool; stdcall = nil;

function TestToolhelpFunctions : boolean;
var c1 : cardinal;
begin
  c1:=GetModuleHandle('kernel32');
  @pCreateToolhelp32Snapshot:=GetProcAddress(c1,'CreateToolhelp32Snapshot');
  @pProcess32First          :=GetProcAddress(c1,'Process32First'          );
  @pProcess32Next           :=GetProcAddress(c1,'Process32Next'           );
  result := (@pCreateToolhelp32Snapshot<>nil) and (@pProcess32First<>nil) and (@pProcess32Next<>nil);
end;


 function CreateToolhelp32Snapshot (dwFlags,th32ProcessID: cardinal) : cardinal;
 begin
   result := 0;
   if @pCreateToolhelp32Snapshot = nil then if not TestToolhelpFunctions then exit;
   result := pCreateToolhelp32Snapshot( dwFlags , th32ProcessID );
 end;

 function Process32First(hSnapshot: cardinal; var lppe: TProcessEntry32) : bool;
 begin
   result := false;
   if @pProcess32First = nil then if not TestToolhelpFunctions then exit;
   result := pProcess32First(hSnapshot,lppe);
 end;

 function Process32Next(hSnapshot: cardinal; var lppe: TProcessEntry32) : bool;
 begin
    result := false;
    if @pProcess32Next = nil then if not TestToolhelpFunctions then exit;
    result := pProcess32Next(hSnapshot,lppe);
 end;

end.

