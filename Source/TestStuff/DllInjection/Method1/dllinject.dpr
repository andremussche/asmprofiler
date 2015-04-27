program dllinject;

{$APPTYPE CONSOLE}                  

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

uses
  SysUtils,
  windows,
  Forms,
  shellapi,
  procs in 'procs.pas',
  _uAsmProfDllInterface in '..\..\DllVersion\_uAsmProfDllInterface.pas';

type

{
 this structure contains pointers to variables
 that will be passed to and used by the InjectedProc
}
  TInjectDllData = record
    pLoadLibrary     : pointer;  //pointer to the loadLibrary function
    pGetProcAddress  : pointer;  //pointer to the GetProcAddress function
    //pGetModuleHandle : pointer;  //pointer to the GetModulhandle function

    lib_name         : pointer;      //pointer to the name of the dll we will load
    proc_name        : pointer;      //pointer to the name of procedure
//    kernel32_name    : pointer;
//    loadlibrary_name : pointer;
  end;

procedure InjectedProc(parameter: Pointer); stdcall;
var InjectDllData : TInjectDllData;
    h:THandle;
begin
  InjectDllData :=  TInjectDllData(parameter^);
  asm
   push InjectDllData.lib_name
   call InjectDllData.pLoadLibrary       //load library

   test EAX, EAX                         //handle <> 0
   jz @@exit
   push InjectDllData.proc_name
   push eax
   call InjectDllData.pGetProcAddress    //get proc address

   test EAX, EAX                         //proc <> nil
   jz @@exit
   call eax;                             //call proc
 {
   you could easily call a function inside the library we just loaded
 }
@@exit:
  end;
  //while True do ;
end;


{
 this basically allocates some memory in the target process
 is then uses create remote thread to execute it
 it will then wait for the code to finish executing
 once finished it will free the memory used.
}
function InjectDllToTarget(const aDllName, aProcName: string; TargetProcessID: DWORD ; code: pointer; CodeSize: integer): boolean;
 var
  InitDataAddr , WriteAddr : pointer;
  hProcess  , ThreadHandle : Thandle;
  BytesWritten , TheadID : DWORD;
  InitData : TInjectDllData;
begin
 result := false;

 // it would probably be a good idea to set these
 // from the IAT rather than assuming kernel32.dll
 // is loaded in the same place in the remote process
 InitData.pLoadLibrary      := GetProcAddress(LoadLibrary('kernel32.dll'), 'LoadLibraryA');
 InitData.pGetProcAddress   := GetProcAddress(LoadLibrary('kernel32.dll'), 'GetProcAddress');
 //InitData.pGetModuleHandle  := GetProcAddress(LoadLibrary('kernel32.dll'), 'GetModuleHandleA');

 hProcess := OpenProcess( PROCESS_ALL_ACCESS, FALSE, TargetProcessID );
 if (hProcess = 0) then exit;

 // allocate and write the dll name to the remote process
 InitData.lib_name := VirtualAllocEx(hProcess , 0, length(aDllName) + 5  , MEM_COMMIT , PAGE_READWRITE) ;
 if ( InitData.lib_name <> nil) then
 begin
    WriteProcessMemory(hProcess ,  InitData.lib_name , pchar(aDllName) , length(aDllName) , BytesWritten );
 end;

 //allocate proc name
 InitData.proc_name := VirtualAllocEx(hProcess, 0, length(aProcName) + 5, MEM_COMMIT, PAGE_READWRITE) ;
 if InitData.proc_name <> nil then
 begin
    WriteProcessMemory(hProcess, InitData.proc_name, pchar(aProcName), length(aProcName), BytesWritten);
 end;

 {
 //kernel32.dll
 InitData.kernel32_name := VirtualAllocEx(hProcess , 0, length('kernel32.dll') + 5  , MEM_COMMIT , PAGE_READWRITE) ;
 if ( InitData.lib_name <> nil) then
 begin
    WriteProcessMemory(hProcess ,  InitData.kernel32_name , pchar('kernel32.dll') , length('kernel32.dll') , BytesWritten );
 end ;

 //LoadLibraryA
 InitData.loadlibrary_name := VirtualAllocEx(hProcess , 0, length('LoadLibraryA') + 5  , MEM_COMMIT , PAGE_READWRITE) ;
 if ( InitData.lib_name <> nil) then
 begin
    WriteProcessMemory(hProcess ,  InitData.loadlibrary_name , pchar('LoadLibraryA') , length('LoadLibraryA') , BytesWritten );
 end ;
 }

// write the initdata strucutre to the remote prcess
 InitDataAddr := VirtualAllocEx(hProcess, 0, sizeof(InitData), MEM_COMMIT, PAGE_READWRITE) ;
 if (InitDataAddr <> nil) then
 begin
  WriteProcessMemory(hProcess, InitDataAddr, (@InitData), sizeof(InitData), BytesWritten);
 end ;

 {
 InitData.pLoadLibrary      := GetProcAddress(LoadLibrary('kernel32.dll'), 'LoadLibraryA');
 asm
   push InitData.kernel32_name
   call InitData.pLoadLibrary

   push InitData.lib_name
   call InitData.pLoadLibrary
 end;
 while True do ;
 }

 {
 asm
   push InitData.lib_name
   call InitData.pLoadLibrary
  end;
  }

//  InjectedProc(@InitData);
//InjectedProc(InitDataAddr);


// write our proc that loads the dll into the remote process
// then execute it
 WriteAddr := VirtualAllocEx(hProcess, 0, CodeSize, MEM_COMMIT, PAGE_READWRITE) ;
 if (WriteAddr <> nil) then
 begin
   WriteProcessMemory(hProcess, WriteAddr, code, CodeSize, BytesWritten);

   if BytesWritten = CodeSize then
   begin
      ThreadHandle := CreateRemoteThread(hProcess, nil, 0, WriteAddr, InitDataAddr, 0, TheadID);

      WaitForSingleObject(ThreadHandle, INFINITE);  //wait for the thread to execute

      VirtualFreeEx(hProcess, WriteAddr, 0, MEM_RELEASE); // free the memory we allocated
      result := true;
   end;
 end;

 // free the memory we allocated for the dll name
 VirtualFreeEx( hProcess , InitDataAddr ,  0 , MEM_RELEASE);
 VirtualFreeEx( hProcess , InitData.lib_name ,  0 , MEM_RELEASE);
 CloseHandle(hProcess);
end;


var InitData : TInjectDllData;
   PID       : Dword;
   i:integer;
   s, dll_to_inject : string;
begin
 GetDebugPrivs; // give us debug privalages

 //dll_to_inject := 'wsock.dll'; //just an example, it can be anything
// dll_to_inject := ExtractFilePath(Application.ExeName) + 'AsmProfiler_inject.dll';
// dll_to_inject := ExtractFilePath(Application.ExeName) + 'AsmProfiler.dll';
  dll_to_inject := 'C:\Andre\AsmProfiler\DllVersion\AsmProfiler.dll';

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
end.
