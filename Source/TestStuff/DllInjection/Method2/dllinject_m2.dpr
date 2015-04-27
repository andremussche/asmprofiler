program dllinject_m2;

{$APPTYPE CONSOLE}

{
 Author  : Rezmond
 URL     : www.projectbionet.com
 Email   : rezmond@projectbionet.com
 Relesed : Friday 11th Oct 2002
 Version : 1.00

 Freeware for non commerical use
 Note this only works under NT

 This method is another way of injecting a DLL
 It is limited to just calling Loadlibrary
 Method 1 showed you how to copy code to the
 remote process and use create thread to execute it

 This method simply uses the remote LoadlLibrary function
 as if it was the thread procedure

 IT calls CreateRemotethread on the address of Loadlibrary in the target process
 passing the dll name.


this snippet was from Ivo Ivanov API hookin tutorials
it explains the basics of how method 2 works 
You must understand how method 1 works before reading this....


Here comes the trick – have a look at the signature of thread function,
whose pointer is passed as parameter
(i.e. LPTHREAD_START_ROUTINE) to the CreateRemoteThread():

DWORD WINAPI ThreadProc(LPVOID lpParameter);


And here is the prototype of LoadLibrary() API 

HMODULE LoadLibrary(LPCTSTR lpFileName);

Yes, they do have "identical" pattern. 
They use the same calling convention WINAPI, they both accept
 one parameter and the size of returned value is the same.
 This match gives us a hint that we can use LoadLibrary() as
 thread function, which will be executed after the remote
 thread has been created. Let's have a look at the following sample code:


HMODULE pfnLoadLibrary = 
	GetProcAddress(LoadLibrary("kernel32.dll"), "LoadLibraryA" );

hThread = ::CreateRemoteThread(hProcessForHooking,
                                      NULL, 
                                      0, 
                                      pfnLoadLibrary, 
                                      "C:\\HookTool.dll", 
                                      0, 
                                      NULL);

By using GetProcAddress() API we get the address of the LoadLibrary() API. 
The dodgy thing here is that Kernel32.DLL is mapped always to the same
address space of each process, thus the address of LoadLibrary()
function has the same value in address space of any running process.
This ensures that we pass a valid pointer (i.e. pfnLoadLibrary)
as parameter of CreateRemoteThread().


--- end snippet  -----


}

uses
  SysUtils,
  windows,
  shellapi,
  procs in 'procs.pas';

function InjectDllToTarget(dllName : string; TargetProcessID : DWORD ): boolean;
 var
  LibName  : pointer;
  hProcess , ThreadHandle : Thandle;
  BytesWritten , TheadID : DWORD;
begin
 result := false;

 hProcess := OpenProcess( PROCESS_ALL_ACCESS, FALSE, TargetProcessID );
 if (hProcess = 0) then exit;

  // alocate and write the dll name to the remote process
 LibName := VirtualAllocEx(hProcess , 0, length(dllName) + 5  , MEM_COMMIT , PAGE_READWRITE) ;
 if ( LibName <> nil) then
 begin
    WriteProcessMemory(hProcess , LibName, pchar(dllName) , length(dllName) , BytesWritten );
 end ;

 ThreadHandle := CreateRemoteThread( hProcess , nil , 0,   GetProcAddress(LoadLibrary('kernel32.dll'), 'LoadLibraryA') , LibName ,0 , TheadID );
 result := ThreadHandle <> 0;
 WaitForSingleObject( ThreadHandle , INFINITE);  //wait for the thread to execute

 // free the memory we allocated for the dll name
 VirtualFreeEx( hProcess , LibName ,  0 , MEM_RELEASE);
 CloseHandle(hProcess);
end;


var
   PID       : Dword;
   dll_to_inject : string;
begin

 GetDebugPrivs; // give us debug privalages

 dll_to_inject := 'wsock.dll'; //just an example, it can be anything

 {
  for the perous of this demo we will just inject into
  our own process but PID could be any process we want
 }
  PID := GetCurrentProcessID();


 if  InjectDllToTarget(dll_to_inject, PID)  then
   writeLn('Dll Injected' )
  else
    writeLn('Dll Inject failed');

 writeLn('Press enter to exit');

 ReadLn(dll_to_inject);
end.
