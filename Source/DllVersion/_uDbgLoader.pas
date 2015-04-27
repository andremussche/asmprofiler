unit _uDbgLoader;

interface

uses
  Windows, Classes,
  JwaImageHlp;

  function LoadDbgFile(const aFile:string; var aResultList: Tstrings): boolean;

implementation

uses SysUtils;

const
  SymTagNull = 0;
  SymTagExe = 1;
  SymTagCompiland = 2;
  SymTagCompilandDetails = 3;
  SymTagCompilandEnv = 4;
  SymTagFunction = 5;
  SymTagBlock = 6;
  SymTagData = 7;
  SymTagAnnotation = 8;
  SymTagLabel = 9;
  SymTagPublicSymbol = 10;
  SymTagUDT = 11;
  SymTagEnum = 12;
  SymTagFunctionType = 13;
  SymTagPointerType = 14;
  SymTagArrayType = 15;
  SymTagBaseType = 16;
  SymTagTypedef = 17;
  SymTagBaseClass = 18;
  SymTagFriend = 19;
  SymTagFunctionArgType = 20;
  SymTagFuncDebugStart = 21;
  SymTagFuncDebugEnd = 22;
  SymTagUsingNamespace = 23;
  SymTagVTableShape = 24;
  SymTagVTable = 25;
  SymTagCustom = 26;
  SymTagThunk = 27;
  SymTagCustomType = 28;
  SymTagManagedType = 29;
  SymTagDimension = 30;
  SymTagMa = 31;

function TagToStr(const aTag: cardinal): string;
begin
  case aTag of
    SymTagNull: result := 'Null';
    SymTagExe: result := 'Exe';
    SymTagCompiland: result := 'Compiland';
    SymTagCompilandDetails: result := 'CompilandDetails';
    SymTagCompilandEnv: result := 'CompilandEnv';
    SymTagFunction: result := 'Function';
    SymTagBlock: result := 'Block';
    SymTagData: result := 'Data';
    SymTagAnnotation: result := 'Annotation';
    SymTagLabel: result := 'Label';
    SymTagPublicSymbol: result := 'PublicSymbol';
    SymTagUDT: result := 'UDT';
    SymTagEnum: result := 'Enum';
    SymTagFunctionType: result := 'FunctionType';
    SymTagPointerType: result := 'PointerType';
    SymTagArrayType: result := 'ArrayType';
    SymTagBaseType: result := 'BaseType';
    SymTagTypedef: result := 'Typedef';
    SymTagBaseClass: result := 'BaseClass';
    SymTagFriend: result := 'Friend';
    SymTagFunctionArgType: result := 'FunctionArgType';
    SymTagFuncDebugStart: result := 'FuncDebugStart';
    SymTagFuncDebugEnd: result := 'FuncDebugEnd';
    SymTagUsingNamespace: result := 'UsingNamespace';
    SymTagVTableShape: result := 'VTableShape';
    SymTagVTable: result := 'VTable';
    SymTagCustom: result := 'Custom';
    SymTagThunk: result := 'Thunk';
    SymTagCustomType: result := 'CustomType';
    SymTagManagedType: result := 'ManagedType';
    SymTagDimension: result := 'Dimension';
  else
    result := 'Unknown';
  end;
end;

function MyEnumSymbolsCallback(aSymInfo: PSymbolInfo; aSymbolSize: Cardinal; aUserContext: pointer): boolean; stdcall;
var
  pName: pointer;
  cOffset: cardinal;
  rLine: _IMAGEHLP_LINE64;
  s:string;
begin
  if aSymInfo <> nil then
  begin
    s := format('Name: %s | Symbol: %s | Address: %x | Size: %d ',
            [aSymInfo.Name,
             TagToStr(aSymInfo.Tag), aSymInfo.Address, aSymInfo.Size]);

    {
    if SymGetTypeInfo( GetCurrentProcess, cModBase, aSymInfo.TypeIndex, TI_GET_SYMNAME, pName) then
    begin
      s := s + pchar(pName);
      localfree(Cardinal(pName));
    end;
    }
    rLine.SizeOfStruct := sizeof(rLine);
    if SymGetLineFromAddr64( GetCurrentProcess, aSymInfo.Address, cOffset, rLine) then
    begin
      s := Format('%s | Line: %d | File: %s',[s, rLine.LineNumber, rLine.FileName]);
    end;

    if aUserContext <> nil then
      TStrings(pointer(aUserContext)^).Add( s );
  end;

  Result := True;
end;

function MyEnumSourcefilesCallback(aSourceFile: PSOURCEFILE; aUserContext: pointer): boolean;stdcall;
var
  s:string;
begin
  if aSourceFile <> nil then
  begin
    if aUserContext <> nil then
    begin
      s := format('File: %s | Base: %x', [aSourceFile.FileName, aSourceFile.ModBase]);
      TStrings(pointer(aUserContext)^).Add( s );
    end;
  end;
  Result := True;
end;

function GetSizeOfFile(const aFile:string): Cardinal;
var
  hFile: THandle;
begin
  hFile := CreateFile( pchar(aFile), GENERIC_READ, FILE_SHARE_READ,
	                     nil, OPEN_EXISTING, 0, 0);
  GetFileSize(hFile, nil);
  FileClose(hFile);
end;

function LoadDbgFile(const aFile:string; var aResultList: Tstrings): boolean;
var
  cModBase,
  cFileSize, cPdbBase,
  cOptions: Cardinal;
  rModInfo: IMAGEHLP_MODULE64;
begin
	// Set SYMOPT_DEBUG option
  cOptions := SymGetOptions;
	cOptions := cOptions OR SYMOPT_DEBUG OR SYMOPT_LOAD_LINES;
	SymSetOptions( cOptions );

  if not JwaImageHlp.SymInitialize(
            GetCurrentProcess,              // Process handle of the current process
            nil,                            // No user-defined search path -> use default
            False)                          // Do not load symbols for modules in the current process
  then
    RaiseLastWin32Error;

  if lowercase(ExtractFileExt(aFile)) = '.pdb' then
  begin
    cFileSize := GetSizeOfFile(aFile);
    cPdbBase  := $10000000;  // it can be any non-zero value, but if we load symbols
		                         // from more than one file, memory regions specified
	    	                     // for different files should not overlap
		                         // (region is "base address + file size")
  end
  else
  begin
    cFileSize := 0;
    cPdbBase  := 0;
  end;

  cModBase := SymLoadModule64(
                  GetCurrentProcess,              // Process handle of the current process
                  0,                              // Handle to the module's image file (not needed)
                  pchar(afile),                   // Path/name of the file
                  nil,                            // User-defined short name of the module (it can be NULL)
                  cPdbBase,                       // Base address of the module (cannot be NULL if .PDB file is used, otherwise it can be NULL)
                  cFileSize);                     // Size of the file (cannot be NULL if .PDB file is used, otherwise it can be NULL)

  if cModBase = 0 then
    RaiseLastWin32Error;

	FillChar(rModInfo, sizeof(rModInfo), 0);
	rModInfo.SizeOfStruct := sizeof(rModInfo);

  if not SymGetModuleInfo64(
            GetCurrentProcess,
            cModBase,
            rModInfo)
  then
    RaiseLastWin32Error;

  if not SymEnumSymbols(
                GetCurrentProcess,                // Process handle of the current process
                cModBase,                         // Base address of the module
                nil,                              // Mask (NULL -> all symbols)
                @MyEnumSymbolsCallback,           // The callback function
                @aResultList)                     // A used-defined context can be passed here, if necessary
                //nil)                              // A used-defined context can be passed here, if necessary
  then ;
    //RaiseLastWin32Error;

  if not SymEnumSourceFiles(
                GetCurrentProcess,                // Process handle of the current process
                cModBase,                         // Base address of the module
                '',                               // Mask (NULL -> all symbols)
                @MyEnumSourcefilesCallback,       // The callback function
                @aResultList)                     // A used-defined context can be passed here, if necessary
  then
    ;//Win32Check(false);

  SymUnloadModule64( GetCurrentProcess, cModBase );
  SymCleanup( GetCurrentProcess );
end;


end.
