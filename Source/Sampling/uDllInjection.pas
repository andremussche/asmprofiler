unit uDllInjection;

{
 Author  : Rezmond
 URL     : www.projectbionet.com
 Email   : rezmond@projectbionet.com
 Relesed : Friday 11th Oct 2002
 Version : 1.00

 Freeware for non commerical use

 Note this only works under NT

 This program demonstrates how to inject a DLL into a remote process

 When working with code in a target you must be
 carefull with the variables you use

 How this program works!

  The CreateRemoteThread procedure will make the target process
  create a thread an execute it.

  What we simply do is use VirtualAllocEx to allocate some extra memory in
  the target process

  The next step is to copy our code ( in this case InjectedProc )
  into that memory.

  We then get the target process to execute that code

  Note:
   In order to pass parameters to the remote thread we must
   copy the data over to the remote process  with  VirtualAllocEx
   and WriteProcessMemory.
   We cannot referance local variables in our inject application from the remote process

 Writing Remote Thread code :
  When writing code that will be injected to a remote process you must be carefull
  that you do not make any referances to local variables determined by your app
  This include function calls!! if you wish to call functions
  you must load the libraries first using LoadLibrary and GetProcAddress

  For example writing
    MessageBox( 0, 'hey\0','hey\0' , 0);
  in the thread we want to inject would cause an error

  Your best bet is to use the inline assembler when coding.
  Delphi code is not always safe
}

interface

uses
  Types;

  function InjectDllIntoProcess(const aDllName, aProcName: String; aTargetProcessID: Cardinal): boolean;overload;

implementation

uses
  SysUtils,
  windows,
  Forms,
  shellapi,
  _uAsmProfDllInterface,
  uDebugUtils;

type
{
 this structure contains pointers to variables
 that will be passed to and used by the InjectedProc
}
  TInjectDllData = record
    pLoadLibrary     : pointer;  //pointer to the loadLibrary function
    pGetProcAddress  : pointer;  //pointer to the GetProcAddress function
    //pGetModuleHandle : pointer;  //pointer to the GetModulhandle function

    lib_name         : PAnsiChar;      //pointer to the name of the dll we will load
    proc_name        : PAnsiChar;      //pointer to the name of procedure
//    kernel32_name    : pointer;
//    loadlibrary_name : pointer;
    errorcode: Integer;
  end;
  PInjectDllData = ^TInjectDllData;

procedure InjectedProc(parameter: Pointer); stdcall;
var
  InjectDllData : TInjectDllData;
begin
  InjectDllData := TInjectDllData(parameter^);   //copy to stack
  asm
    push InjectDllData.lib_name
    call InjectDllData.pLoadLibrary       //load library

    test EAX, EAX                         //handle <> 0
    jz @@errorload
    push InjectDllData.proc_name
    push eax
    call InjectDllData.pGetProcAddress    //get proc address

    test EAX, EAX                         //proc <> nil
    jz @@errorproc
    call eax;                             //call proc
    mov InjectDllData.errorcode, 0
    jmp @@exit

    @@errorload:
    mov InjectDllData.errorcode, 1
    int 3;  //breakpoint

    @@errorproc:
    mov InjectDllData.errorcode, 2
    int 3;  //breakpoint

    {
      you could easily call a function inside the library we just loaded
    }
    @@exit:
  end;
  TInjectDllData(parameter^) := InjectDllData;   //copy from stack to "shared" memory so results can be read by remote caller
end;

{
 this basically allocates some memory in the target process
 is then uses create remote thread to execute it
 it will then wait for the code to finish executing
 once finished it will free the memory used.
}
function InjectDllToTarget(const aDllName, aProcName: ansistring; TargetProcessID: Cardinal; aCode: pointer; aCodeSize: Cardinal): boolean;overload;
var
  InitDataAddr , WriteAddr : pointer;
  hProcess  , ThreadHandle : Thandle;
  BytesWritten: {$if CompilerVersion >= 23} (*Delphi XE2*) NativeUInt {$else} Cardinal {$ifend};
  ThreadID : Cardinal;
  InitData : TInjectDllData;
begin
  result := False;
  FillChar(InitData, SizeOf(InitData), 0);
  InitDataAddr := nil;
  GetDebugPrivs;

  // it would probably be a good idea to set these
  // from the IAT rather than assuming kernel32.dll
  // is loaded in the same place in the remote process
  InitData.pLoadLibrary      := GetProcAddress(LoadLibrary('kernel32.dll'), 'LoadLibraryA');
  InitData.pGetProcAddress   := GetProcAddress(LoadLibrary('kernel32.dll'), 'GetProcAddress');
  //InitData.pGetModuleHandle  := GetProcAddress(LoadLibrary('kernel32.dll'), 'GetModuleHandleA');
  InitData.errorcode := -1;

  hProcess := OpenProcess( PROCESS_ALL_ACCESS, FALSE, TargetProcessID );
  if (hProcess = 0) then exit;
  try
    // allocate and write the dll name to the remote process
    InitData.lib_name := VirtualAllocEx(hProcess, nil, length(aDllName) + 5, MEM_COMMIT, PAGE_READWRITE);
    if (InitData.lib_name <> nil) then
      WriteProcessMemory(hProcess, InitData.lib_name, pansichar(aDllName), length(aDllName), BytesWritten );

    //allocate proc name
    InitData.proc_name := VirtualAllocEx(hProcess, nil, length(aProcName) + 5, MEM_COMMIT, PAGE_READWRITE);
    if InitData.proc_name <> nil then
      WriteProcessMemory(hProcess, InitData.proc_name, pansichar(aProcName), length(aProcName), BytesWritten);

    // write the initdata strucutre to the remote prcess
    InitDataAddr := VirtualAllocEx(hProcess, nil, sizeof(InitData), MEM_COMMIT, PAGE_READWRITE);
    if (InitDataAddr <> nil) then
      WriteProcessMemory(hProcess, InitDataAddr, (@InitData), sizeof(InitData), BytesWritten);

    // write our proc that loads the dll into the remote process
    // then execute it
    WriteAddr := VirtualAllocEx(hProcess, nil, aCodeSize, MEM_COMMIT, PAGE_READWRITE) ;
    try
      if (WriteAddr <> nil) then
      begin
        WriteProcessMemory(hProcess, WriteAddr, aCode, aCodeSize, BytesWritten);

        if BytesWritten = aCodeSize then
        begin
          ThreadHandle := CreateRemoteThread(hProcess, nil, 0, WriteAddr, InitDataAddr, 0, ThreadID);

          Sleep(100);
          WaitForSingleObject(ThreadHandle, INFINITE);  //wait for the thread to execute
          if ReadProcessMemory(hProcess, InitDataAddr, @InitData, sizeof(InitData), BytesWritten) and
             (InitData.errorcode <> 0) then
          begin
            if InitData.errorcode = 1 then
              raise Exception.CreateFmt('Dll hook injection failed: could not load dll "%s"', [string(InitData.lib_name)])
            else if InitData.errorcode = 2 then
              raise Exception.CreateFmt('Dll hook injection failed: could not find function "%s" in "%s"', [string(InitData.proc_name), string(InitData.lib_name)])
            else
              raise Exception.CreateFmt('Dll hook injection failed with errorcode "%d"', [InitData.errorcode]);
          end;

          result := true;
        end;
      end;
    finally
      VirtualFreeEx(hProcess, WriteAddr, 0, MEM_RELEASE); // free the memory we allocated
    end;

  finally
    // free the memory we allocated for the dll name
    VirtualFreeEx( hProcess , InitDataAddr ,  0 , MEM_RELEASE);
    VirtualFreeEx( hProcess , InitData.lib_name ,  0 , MEM_RELEASE);
    VirtualFreeEx( hProcess , InitData.proc_name ,  0 , MEM_RELEASE);
    CloseHandle(hProcess);
  end;
end;

function InjectDllIntoProcess(const aDllName, aProcName: String; aTargetProcessID: Cardinal): boolean;
begin
  Result := InjectDllToTarget(AnsiString(aDllName), AnsiString(aProcName), aTargetProcessID, @InjectedProc, 1000);
end;


(*
var InitData : TInjectDllData;
   PID       : Dword;
   i:integer;
   s, dll_to_inject : string;
begin
  GetDebugPrivs; // give us debug privalages

 //dll_to_inject := 'wsock.dll'; //just an example, it can be anything
 dll_to_inject := ExtractFilePath(Application.ExeName) + 'AsmProfiler.dll';

 {
  for the perous of this demo we will just inject into
  our own process but PID could be any process we want
 }
  writeLn('Enter PID of target process:');
  ReadLn(s);
  if TryStrToInt(s,i) then
    PID := i
  else
    PID := GetCurrentProcessID();

  if InjectDllToTarget(dll_to_inject, C_StartDllInjectionHook, PID, @InjectedProc, 1000)  then
    writeLn('Dll Injected' )
  else
    writeLn('Dll Inject failed');

  writeLn('Press enter to exit');

  ReadLn(dll_to_inject);
*)

end.

