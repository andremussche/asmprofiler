unit _AsmDllInjectionHook;

interface

uses
  Forms,
  Windows, //Dialogs, Forms,
  JwaWinUser, JwaTlHelp32;

  procedure StartDllInjectionHook;

implementation

uses
  {$if CompilerVersion >= 24}  //Delphi XE3
  UITypes,
  {$ifend}
  _uAsmProfiler, AsmDetours, JwaWinType, JwaNative, Dialogs;

type
  TGetMessage = function(lpMsg: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall;
  TPeekMessage = function(var lpMsg: MSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall;
  TInterceptZwYieldExecution = function: NTSTATUS; stdcall;

var
  pGetMessage_Trampoline,
  pPeekMessage_Trampoline,
  pZwYieldExecution_Trampoline,
  pInterceptGetMessage,
  pInterceptPeekMessage,
  pInterceptZwYieldExecution: pointer;
  //str:Tstrings;
  cMainThreadID: Cardinal;

function  InterceptZwYieldExecution(): NTSTATUS; stdcall;
begin
  if GetCurrentThreadId = cMainThreadID then
  begin
    ShowProfileForm;
    Result := TInterceptZwYieldExecution(pZwYieldExecution_Trampoline);

    //AsmDetours.InterceptRemove(pZwYieldExecution_Trampoline, pInterceptZwYieldExecution);
    AsmDetours.InterceptRemove(pGetMessage_Trampoline, pInterceptGetMessage);
    AsmDetours.InterceptRemove(pPeekMessage_Trampoline, pInterceptPeekMessage);
    //showmessage(Application.exename);
  end
  else
    Result := TInterceptZwYieldExecution(pZwYieldExecution_Trampoline);
end;

function InterceptGetMessage(lpMsg: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall;
begin
  //str.add( 'GetMsg - Main = %s - Current = %d' + IntToStr(cMainThreadID) + ' - ' + IntToStr(GetCurrentThreadId) );
  //str.SaveToFile('c:\dump.txt');

  if GetCurrentThreadId = cMainThreadID then
  begin
    ShowProfileForm;
    Result := TGetMessage(pGetMessage_Trampoline)(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax);

    //AsmDetours.InterceptRemove(pZwYieldExecution_Trampoline, pInterceptZwYieldExecution);
    AsmDetours.InterceptRemove(pGetMessage_Trampoline, pInterceptGetMessage);
    AsmDetours.InterceptRemove(pPeekMessage_Trampoline, pInterceptPeekMessage);

    //showmessage(Application.exename);
  end
  else
    Result := TGetMessage(pGetMessage_Trampoline)(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax);
end;

function InterceptPeekMessage(var lpMsg: MSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall;
begin
  if GetCurrentThreadId = cMainThreadID then
  begin
    ShowProfileForm;
    Result := TPeekMessage(pPeekMessage_Trampoline)(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax, wRemoveMsg);

    //AsmDetours.InterceptRemove(pZwYieldExecution_Trampoline, pInterceptZwYieldExecution);
    AsmDetours.InterceptRemove(pGetMessage_Trampoline, pInterceptGetMessage);
    AsmDetours.InterceptRemove(pPeekMessage_Trampoline, pInterceptPeekMessage);

    //showmessage(Application.exename);
  end
  else
    Result := TPeekMessage(pPeekMessage_Trampoline)(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax, wRemoveMsg);
end;

procedure StartDllInjectionHook;
var
  FSnapshotHandle: THandle;
  t: THREADENTRY32;
  b:boolean;
  sError:string;
begin
  //debugbreak;
  Windows.MessageBox(0, 'AsmProfiler Dll injected, trying to hook message queue to show profiler form...', 'Injected', mb_ok);

  FSnapshotHandle := JwaTlHelp32.CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, GetCurrentProcessId);
  t.dwSize := Sizeof(t);
  b := JwaTlHelp32.Thread32First(FSnapshotHandle, t);
  while b do
  begin
    if t.th32OwnerProcessID = GetCurrentProcessId then
    begin
      cMainThreadID   := t.th32ThreadID;
      Break;
    end;
    b := Thread32Next(FSnapshotHandle, t);
  end;
  CloseHandle(FSnapshotHandle);

  //cMainThreadID   := MainThreadID;

  pInterceptGetMessage  := @InterceptGetMessage;
  pInterceptPeekMessage := @InterceptPeekMessage;
  pInterceptZwYieldExecution := @InterceptZwYieldExecution;

  pGetMessage_Trampoline   := AsmDetours.InterceptCreate(@GetMessage, pInterceptGetMessage, sError);
  pPeekMessage_Trampoline  := AsmDetours.InterceptCreate(@PeekMessage, pInterceptPeekMessage, sError);
  //pZwYieldExecution_Trampolin := AsmDetours.InterceptCreate(@ZwYieldExecution, pInterceptZwYieldExecution);

  MessageDlg('Hook successful, profiler form should appear in the upper left corner of the screen', mtInformation, [mbOK], 0);
end;

end.
