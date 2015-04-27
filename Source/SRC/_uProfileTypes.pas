unit _uProfileTypes;

interface

uses
  Classes,
  Types;

const
  C_MEMORY_OFFSET         = $401000;
  C_MAX_THREADS           = 128;                 //max 256 threads can be handled
  //C_PROFILETIMEARRAY_SIZE = 256 * 1024 * 1;    //256 kb * Sizeof(TProfileTimeRecord) = 256 * 13 (or 16) = 3.4mb
  C_PROFILETIMEARRAY_SIZE = 160 * 1024 * 1;      //160 kb * Sizeof(TProfileTimeRecord) = 160 * 13 (or 16) = 2mb (or 2,5mb)

type
  TThreadNameInfo = record                 
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
  PThreadNameInfo = ^TThreadNameInfo;

  //*============================*//

  TThreadName = record
    ThreadID: Cardinal;
    Name: Ansistring;
  end;

  PJclMapProcNameExt = ^TJclMapProcNameExt;
  TJclMapProcNameExt = packed record
    Addr: DWORD;
    ProcName: AnsiString;
  end;
  TJclMapProcNameExtArray = array of TJclMapProcNameExt;
  PJclMapProcNameExtArray = ^TJclMapProcNameExtArray;

  PJclMapSegmentExt = ^TJclMapSegmentExt;
  TJclMapSegmentExt = packed record
    StartAddr: DWORD;
    EndAddr: DWORD;
    UnitName: AnsiString;
    UnitFile: AnsiString;
    Procs: TJclMapProcNameExtArray;
  end;
  TJclMapSegmentExtArray  = array of TJclMapSegmentExt;

  TSelectedProc = record
    ProcName: Ansistring;
    //Addr: Cardinal;
    ProcNameRecord: PJclMapProcNameExt;
  end;

  TSelectedUnit = record
    UnitName: string;
    //StartAddr: Cardinal;
    //EndAddr: Cardinal;
    UnitSegmentRecord: PJclMapSegmentExt;
    SelectedProcsCount: integer;
    TotalProcsCount: integer;
    SelectedProcs : array of TSelectedProc;
  end;
  PSelectedUnit = ^TSelectedUnit;
  TSelectedUnitArray = array of TSelectedUnit;
  PSelectedUnitArray = ^TSelectedUnitArray;

  {
  TProfileType2 = (ptEnter2, ptLeave2);
  TProfileTimeRecord2 = packed record
    ProfileType: TProfileType2;
    Address: word;
    Time: integer;
  end;
}

  TProfileType = (ptNone, ptEnter, ptLeave, ptTime);

  TProfileTimeRecord = packed record
    ProfileType: TProfileType;
    //ThreadID: Cardinal;
    Address: Pointer;
    Time: Int64;
  end;
  PProfileTimeRecord = ^TProfileTimeRecord;

  TProfileTimeArray = array of TProfileTimeRecord;
  PProfileTimeArray = ^TProfileTimeArray;

  TProfileRunRecord = record
    Time: TDateTime;
    ThreadID: Cardinal;
    ProfileTimeArray: TProfileTimeArray;
  end;
  TProfileRunRecordArray = array of TProfileRunRecord;

  TThreadNameArray = array[0..C_MAX_THREADS] of TThreadName;

  //*============================*//
  PProfiledUnit = ^TProfiledUnit;

  PProfiledProc = ^TProfiledProc;
  TProfiledProcArray = array of PProfiledProc;

  TProfiledProc = record
    ProcName: string;
    ProcNameRecord: PJclMapProcNameExt;
    UnitIndex: integer;
    //
    CallCount: integer;
    TotalOwnProfileTime: int64;
    TotalChildTime: int64;
    //
    ParentCount: integer;
    ParentProcs: TProfiledProcArray;
    //
    ChildCount: integer;
    ChildProcs: TProfiledProcArray;
    //
    AllOwnProfileTimes, AllChildProfileTimes: array of int64;
  end;

  TProfiledUnit = record
    UnitName: string;
    UnitSegmentRecord: PJclMapSegmentExt;
    //
    ProcsCount: integer;
    ProfiledProcs : TProfiledProcArray;
    //
    TotalProfileTime: int64;
  end;

  TProfiledUnitArray = array of TProfiledUnit;
  TObjectProcedure = procedure of object;
  TCardinalArray = array of cardinal;

  function CalcTimeText(const aTime: single): string;

const
  msvcrt = 'msvcrt.dll';

  procedure c_free(p: Pointer); cdecl; external msvcrt name 'free';
  function malloc(Size: Integer): Pointer; cdecl; external msvcrt;
  function calloc(Number, Size: Integer): Pointer; cdecl; external msvcrt;
  function realloc(p: Pointer; Size: Integer): Pointer; cdecl; external msvcrt;

  procedure Save_TSelectedUnitArray_ToFile(const aArray:TSelectedUnitArray; const aFile:string);
  function Load_TSelectedUnitArray_FromFile(const aFile:string):TSelectedUnitArray;

  function ExtractClassName(const aFullFunction: string):string;
  function ExtractFunctionName(const aFullFunction: string):string;


//  function Save_TProfileTimeArray_ToStream(const aArray:TProfileTimeArray; aStream:TStream): integer;
//  function Load_TProfileTimeArray_FromStream(var aArray:TProfileTimeArray; aStream:TStream): integer;

implementation

uses Math, SysUtils;

function PosCharReverse(ch: Char; const S: string; Offset: Integer): Integer;
var
  i: integer;
begin
  Result := 0;
  for i := Min(Length(S), Offset) downto 1 do
    if S[i] = ch then
    begin
      Result := i;
      exit;
    end;
end;

function ExtractClassName(const aFullFunction: string):string;
var
  lLast, lPrev: Integer;
begin
  //System.SysUtils.TClass.Test

  lLast := PosCharReverse('.', aFullFunction, Length(aFullFunction));
  lPrev := PosCharReverse('.', aFullFunction, lLast - 1);

  if lLast = 0 then
    Result := ''
  else if lPrev = 0 then  //Windows.Sleep
    Result := '' //Copy(aFullFunction, 1, lLast - 1)
  else
    Result := Copy(aFullFunction, lPrev + 1, lLast - lPrev - 1);     //TClass
end;

function ExtractFunctionName(const aFullFunction: string):string;
var
  lLast: Integer;
begin
  //System.SysUtils.TClass.Test

  lLast := PosCharReverse('.', aFullFunction, Length(aFullFunction));
  Result := Copy(aFullFunction, lLast + 1, Length(aFullFunction));     //Test
end;


procedure Save_TSelectedUnitArray_ToFile(const aArray:TSelectedUnitArray; const aFile:string);
var
  //sFile:string;
  i,j:integer;
  strm:TMemoryStream;
  fs: TFileStream;
  //iResultCount:integer;
  iProcs:integer;
  iCount:integer;
  rUnit: TSelectedUnit;
  rProc: TSelectedProc;

  procedure __WriteString(const aString:ansistring);
  var iStringLength: integer;
  begin
    iStringLength := Length(aString) * StringElementSize(aString);
    strm.Write(iStringLength, sizeof(iStringLength));
    if iStringLength > 0 then
      strm.Write(aString[1], iStringLength);
  end;

begin
  strm  := TMemoryStream.Create;
  strm.SetSize(1024 * 1024);      //default 1mb size
  //nr of results
  iCount   := Length(aArray);
  strm.Write(iCount, Sizeof(iCount) );
  //iResultCount := 0;

  for i := 0 to iCount-1 do
  begin
    rUnit := aArray[i];

    {if less units than size of array}
    if (i>0) and (rUnit.UnitName = '') and
                 ( (rUnit.UnitSegmentRecord = nil) or
                   (rUnit.UnitSegmentRecord.UnitName = '')) then
    begin
      //strm.Position := iSizePos;
      strm.Position := 0;
      strm.Write(i, Sizeof(i) );
      strm.Position := strm.Size;
      Break;
    end;

    {  TSelectedUnit = record
    //UnitName: string;
    UnitSegmentRecord: PJclMapSegmentExt;
    SelectedProcsCount,
    //TotalProcsCount: integer;
    SelectedProcs : array of record
      ProcName: string;
      ProcNameRecord: PJclMapProcNameExt;
    end;
    }

    with rUnit do
    begin
      UnitSegmentRecord := nil;
      UnitName          := '';
      SelectedProcs     := nil;
    end;
    strm.Write(rUnit, Sizeof(rUnit) );

    __WriteString(Ansistring(aArray[i].UnitName));
    //if (aArray[i].UnitName <> '') then
    //  inc(iResultCount);

    {TSelectedProc = record
      ProcName: string;
      ProcNameRecord: PJclMapProcNameExt;
    end;}
    iProcs := Length(aArray[i].SelectedProcs);
    strm.Write(iProcs, Sizeof(iProcs) );

    for j := 0 to iProcs-1 do
    begin
      rProc := aArray[i].SelectedProcs[j];
      rProc.ProcName := '';
      rProc.ProcNameRecord := nil;

      strm.Write(rProc, Sizeof(rProc) );
      __WriteString(aArray[i].SelectedProcs[j].ProcName);
    end;
  end;

  //{only save if we have results}
  //if iResultCount > 0 then
  begin
    ForceDirectories(ExtractFilePath(aFile));
    fs := TFileStream.Create(aFile, fmCreate or fmShareExclusive);
    strm.SetSize(strm.position);      //resize back to actual size
    strm.Position := 0;
    fs.CopyFrom(strm, strm.Size);
    fs.Free;
  end;

  strm.Free;
end;

function Load_TSelectedUnitArray_FromFile(const aFile:string):TSelectedUnitArray;
var
  strm: TMemoryStream;
  fs: TFileStream;
  i,j:integer;
  //iStringLength,
  //iTotalProcCount,
  iProcCount,
  iUnitCount:integer;
  //sFile:string;
  sString: string;

  //rUnit: TSelectedUnit;
//  rProc: TSelectedProc;

  function __ReadString:ansistring;
  var iStringLength: integer;
  begin
    strm.Read(iStringLength,Sizeof(iStringLength));
    SetLength(Result, iStringLength);
    if iStringLength > 0 then
      strm.Read(Result[1], iStringLength)
    else
      Result := '';
  end;

begin
  SetLength(Result,0);
  if not FileExists(aFile) then exit;
  
  fs    := TFileStream.Create(aFile,fmOpenRead or fmShareDenyNone	);
  fs.Position   := 0;

  strm := TMemoryStream.Create;
  strm.CopyFrom(fs, fs.Size);
  strm.Position := 0;
  fs.Free;

  strm.Read(iUnitCount,Sizeof(iUnitCount));
  SetLength(Result,iUnitCount);
  //iTotalProcCount := 0;

  for i := 0 to iUnitCount-1 do
  begin
    strm.Read(Result[i], sizeof(TSelectedUnit));

    {TSelectedUnit = record
    UnitName: string;
    UnitSegmentRecord: PJclMapSegmentExt;
    SelectedProcsCount,
    TotalProcsCount: integer;
    SelectedProcs : array of record
      ProcName: string;
      ProcNameRecord: PJclMapProcNameExt;
    end;}

    sString := String(__ReadString);
    Result[i].UnitName := sString;

    strm.Read(iProcCount,Sizeof(iProcCount));
    SetLength(Result[i].SelectedProcs,iProcCount);
    //iTotalProcCount := iTotalProcCount + iProcCount;

    for j := 0 to iProcCount-1 do
    begin
      strm.Read(Result[i].SelectedProcs[j], sizeof(TSelectedProc));
      {
      TSelectedProc = record
        ProcName: string;
        ProcNameRecord: PJclMapProcNameExt;
      end;
      }

      sString := String(__ReadString);
      Result[i].SelectedProcs[j].ProcName := Ansistring(sString);
    end;
  end;

  strm.Free;
end;

function Save_TProfileTimeArray_ToStream(const aArray:TProfileTimeArray; aStream:TStream): integer;
var
//  sFile:string;
  iPrevPos,iSizePos,j:integer;
//  fs: TFileStream;
  pta: TProfileTimeArray;
  //iTotalResultCount,
  iResultCount:integer;
  iTimes:integer;
begin
  pta      := aArray;
  iTimes   := Length(pta);
  iSizePos := aStream.Position;
  //nr of results
  aStream.Write(iTimes, Sizeof(iTimes) );

  iResultCount := 0;

  for j := 0 to iTimes-1 do
  begin
    if (j>0) and (pta[j].Address = nil) then
    begin
      iPrevPos         := aStream.Position;
      aStream.Position := iSizePos;
      aStream.Write(j, Sizeof(j) );
      aStream.Position := iPrevPos;
      Break;
    end;
    {TProfileTimeRecord = record
      ProfileType: TProfileType;
      //ThreadID: Cardinal;
      Address: Pointer;
      Time: Int64;
    end;}
    if (pta[j].Address <> nil) then
      inc(iResultCount);

    if aStream.Position + 1024 >= aStream.Size then
      (aStream as TMemoryStream).SetSize(aStream.Size + 1024 * 1024);

    aStream.Write(pta[j], Sizeof(TProfileTimeRecord) );
  end;

  Result := iResultCount;
end;

function Load_TProfileTimeArray_FromStream(var aArray:TProfileTimeArray; aStream:TStream): integer;
var
//  sFile:string;
  j:integer;
//  fs: TFileStream;
  //iTotalResultCount,
  iResultCount:integer;
  iTimes:integer;
begin
  //nr of results
  aStream.Read(iTimes, Sizeof(iTimes) );
  SetLength(aArray,iTimes);

  iResultCount := 0;

  for j := 0 to iTimes-1 do
  begin
    {TProfileTimeRecord = record
      ProfileType: TProfileType;
      //ThreadID: Cardinal;
      Address: Pointer;
      Time: Int64;
    end;}
    aStream.Read(aArray[j], Sizeof(aArray[j]) );

    if (aArray[j].Address <> nil) then
      inc(iResultCount);
  end;

  Result := iResultCount;
end;

function CalcTimeText(const aTime: single): string;
begin
  if aTime >= 1 then
    Result := Format('%8.2fs',[aTime])
  else if aTime >= 0.0001 then
    Result := Format('%8.2fms',[aTime * 1000])
  else if aTime >= 0.0000001 then
    Result := Format('%8.2fµs',[aTime * 1000 * 1000])
  else //if tDiff >= 0.0000001 then
    Result := Format('%8.2fns',[aTime * 1000 * 1000 * 1000])
end;

initialization
  //if sizeof(TProfileTimeRecord2) > 0 then ;
  //if sizeof(TProfileTimeRecord) > 0 then sleep(0); 


end.
