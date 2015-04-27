unit uDebugUtils;

interface

  procedure GetDebugPrivs;

implementation

uses
  Types,
  jwaWinBase, jwaWinNt;

{
function EnableThreadPrivilege(const Enable: Boolean; const Privilege: string): Boolean;
// utility function adapted from the JCL (http://sourceforge.net/projects/jcl)
const
  PrivAttrs: array [Boolean] of DWORD = (0, SE_PRIVILEGE_ENABLED);
var
  Token: THandle;
  TokenPriv: TTokenPrivileges;
  HaveToken: Boolean;
  Length: Cardinal;
begin
  Token := 0;
  HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_ADJUST_PRIVILEGES, False, Token);
  if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
  begin
    HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token);
  end;
  if HaveToken then
  begin
    TokenPriv.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privileges[0].Luid);
    TokenPriv.Privileges[0].Attributes := PrivAttrs[Enable];
    AdjustTokenPrivileges(Token, False, @TokenPriv, SizeOf(TokenPriv), nil, @Length);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(Token);
  end
  else Result := False;
end;
}

const SE_DEBUG_NAME = 'SeDebugPrivilege' ;

procedure GetDebugPrivs;
var
  hToken: THandle;
  tkp: TOKEN_PRIVILEGES;
  retval: DWord;
begin
  if (OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or  TOKEN_QUERY, hToken)) then
  begin
    LookupPrivilegeValue(nil, SE_DEBUG_NAME, tkp.Privileges[0].Luid);
    tkp.PrivilegeCount := 1;
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    AdjustTokenPrivileges(hToken, false, @tkp, 0, nil, @retval);
  end;
end;


end.
