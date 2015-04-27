unit uOfflineDebugInfo;

interface

uses
  Classes, SysUtils,
  jclDebug,
  uResultTypes;

type
  TOfflineDebugInfo = class
  protected
    FProcessResult: TProcessResult;

    FstrModules,
    FstrUnits,
    FstrFunctions,
    FstrLines: Tstrings;
  public
    constructor Create(aProcessResult: TProcessResult);
    destructor  Destroy;override;

    procedure LoadDebugInfoFromFile(const aFile: string);
    procedure SaveDebugInfoToFile(const aFile: string);

    function GetProcessLocationInfo(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;
  end;

implementation

uses
  StrUtils;

constructor TOfflineDebugInfo.Create(aProcessResult: TProcessResult);
begin
  inherited Create;
  FProcessResult := aProcessResult;

  FstrModules   := TStringList.Create;
  FstrUnits     := TStringList.Create;
  FstrFunctions := TStringList.Create;
  FstrLines     := TStringList.Create;
end;

destructor TOfflineDebugInfo.Destroy;
begin
  FstrModules.Free;
  FstrUnits.Free;
  FstrFunctions.Free;
  FstrLines.Free;
  inherited;
end;

type
  TLine = class
    //Address: Pointer;
    MinAddr,
    MaxAddr: Cardinal;
    iLine: Integer;
    iModule, iUnit, iFunction: Integer;
  end;

function TOfflineDebugInfo.GetProcessLocationInfo(const aProcessAddr: Pointer; var Info: TJclLocationInfo): Boolean;
var
  i,
  iLine, iModule, iUnit: Integer;
  sAddr, sValues: string;
//  addr: Pointer;
  l: TLine;
begin
  Result := False;
  FillChar(info, SizeOf(info), 0);

  if FstrLines.Count = 0 then Exit;
  if FstrLines.Objects[0] = nil then
  begin
    for i := 0 to FstrLines.Count - 1 do
    begin
      sAddr   := FstrLines.Names[i];
      sValues := FstrLines.ValueFromIndex[i];

      l := TLine.Create;
//      l.Address := Pointer(StrToInt(sAddr));
      l.MinAddr := StrToInt(Copy(sAddr, 0, 9));  //$%p
      l.MaxAddr := StrToInt(Copy(sAddr, 11, 9));  //$%p
      iLine := Pos(';', sValues);
      if iLine > 0 then
      begin
        l.iLine  := StrToInt(Copy(sValues, 0, iLine-1));

        iModule  := PosEx(',', sValues, iLine+1);
        if iModule > 0 then
          l.iModule  := StrToInt(Copy(sValues, iLine+1, iModule-iLine-1));
        iUnit  := PosEx(',', sValues, iModule+1);
        if iUnit > 0 then
          l.iUnit  := StrToInt(Copy(sValues, iModule+1, iUnit-iModule-1));
        //iFunction := PosEx(',', sValues, iUnit+1);
        //if iFunction > 0 then
          l.iFunction  := StrToInt(Copy(sValues, iUnit+1, 255));
      end;

      //FstrLines.Objects[i] := TObject(addr);
      FstrLines.Objects[i] := l;
    end;
  end;

  for i := 0 to FstrLines.Count - 1 do
  begin
    l := TLine(FstrLines.Objects[i]);
    if (Cardinal(aProcessAddr) >= l.MinAddr) and
       (Cardinal(aProcessAddr) <= l.MaxAddr) then
//    if (aProcessAddr = l.Address) then
    begin
      info.Address        := aProcessAddr;
      info.BinaryFileName := FstrModules[l.iModule];
      info.UnitName       := FstrUnits[l.iUnit];
      info.ProcedureName  := FstrFunctions[l.iFunction];
      info.LineNumber     := l.iLine; //StrToInt(FstrLines[l.iLine]);
      Result := True;
      Exit;
    end;
  end;

  {
    sline := Format('$%p-$%p=%d;%d,%d,%d',
                    [_line.Address,
                     _line.Line,
                     iModule, iUnit, iFunction]);
    strResult.Add('=Lines=(Address=Line;Module;Unit;Function====================================');
  }
end;

procedure TOfflineDebugInfo.LoadDebugInfoFromFile(const aFile: string);
var
  strResult: Tstrings;
  iHeader, iOffset,
  iModules, iUnits, iFunctions, iLines: integer;
  i: Integer;
begin
  strResult    := TStringList.Create;
  try
    strResult.LoadFromFile(aFile);

    FProcessResult.ProcessExe := strResult.Values['Application'];
    iHeader    := StrToIntDef(strResult.Values['Header'], 4);
    iModules   := StrToIntDef(strResult.Values['Modules'], 0);
    iUnits     := StrToIntDef(strResult.Values['Units'], 0);
    iFunctions := StrToIntDef(strResult.Values['Functions'], 0);
    iLines     := StrToIntDef(strResult.Values['Lines'], 0);

    FstrModules.Capacity := iModules;
    iOffset    := 1 {app} + 2 {header} + iHeader + 1 {modules};
    for i := 0 to iModules-1 do
      FstrModules.Add(strResult.Strings[i + iOffset]);

    iOffset    := iOffset + 1 {header} + iModules;
    for i := 0 to iUnits-1 do
      FstrUnits.Add(strResult.Strings[i + iOffset]);

    iOffset    := iOffset + 1 {header} + iUnits;
    for i := 0 to iFunctions-1 do
      FstrFunctions.Add(strResult.Strings[i + iOffset]);

    iOffset    := iOffset + 1 {header} + iFunctions;
    for i := 0 to iLines-1 do
      FstrLines.Add(strResult.Strings[i + iOffset]);
  finally
    strResult.Free;
  end;
  //
  {
    strResult.Capacity := FstrModules.Count + FstrUnits.Count + FstrFunctions.Count + FstrLines.Count;
    strResult.Add('Application='   + FProcessResult.ProcessExe);
    strResult.Add('Header='    + IntToStr(4));
    strResult.Add('=Header=====================================');
    strResult.Add('Modules='   + IntToStr(FstrModules.Count));
    strResult.Add('Units='     + IntToStr(FstrUnits.Count));
    strResult.Add('Functions=' + IntToStr(FstrFunctions.Count));
    strResult.Add('Lines='     + IntToStr(FstrLines.Count));
    strResult.Add('=Modules=====================================');
    strResult.AddStrings(FstrModules);
    strResult.Add('=Units=====================================');
    strResult.AddStrings(FstrUnits);
    strResult.Add('=Functions=====================================');
    strResult.AddStrings(FstrFunctions);
    strResult.Add('=Lines=(Address=Line;Module;Unit;Function====================================');
    strResult.AddStrings(FstrLines);

    strResult.SaveToFile(aFile);
}
end;

procedure TOfflineDebugInfo.SaveDebugInfoToFile(const aFile: string);
var
  FMainModule: TModuleList;
  strResult: Tstrings;
//  strModules, strUnits, strFunctions, strLines: Tstrings;
  _module: TModule;
  _unit  : TUnit;
  _function: TFlatFunction;
  _line    : TFunctionLine;
  sline: string;
  iModule: Integer;
  iUnit: Integer;
  iFunction: Integer;
  iLine: Integer;

  function _GetMainModuleResult: TModuleList;
  var
//    m: TModuleList;
    i: Integer;
  begin
    Result := nil;
    for i := 0 to FProcessResult.ThreadResults.Count - 1 do
    begin
      if FProcessResult.ThreadResults[i].ThreadId = 0 then
      begin
        Result := FProcessResult.ThreadResults[i].ModuleResult;
        Break;
      end;
    end;
  end;

begin
  FMainModule  := _GetMainModuleResult;

  FstrModules.Clear;
  FstrUnits.Clear;
  FstrFunctions.Clear;
  FstrLines.Clear;
  strResult    := TStringList.Create;
  try

    for iModule := 0 to FMainModule.Count-1 do
    begin
      _module := FMainModule.Modules[iModule];
      FstrModules.Add(_module.ModuleName);

      for iUnit := 0 to _module.Units.Count-1 do
      begin
        _unit := _module.Units[iUnit];
        FstrUnits.Add(_unit.UnitName);

        for iFunction := 0 to _unit.Functions.Count-1 do
        begin
          _function := _unit.Functions[iFunction];
          FstrFunctions.Add(_function.FunctionName);

          for iLine := 0 to _function.FunctionLines.Count-1 do
          begin
            _line := _function.FunctionLines[iline];
            sline := Format('$%p-$%p=%d;%d,%d,%d',
                            [Pointer(_line.MinAddr), Pointer(_line.MaxAddr),
//            sline := Format('$%p=%d;%d,%d,%d',
//                            [_line.Address,
                             _line.Line,
//                             iModule, iUnit, iFunction]);
                             FstrModules.Count-1,
                             FstrUnits.Count-1,
                             FstrFunctions.Count-1]);
            FstrLines.Add(sline);
          end;
        end;
      end;
    end;

    strResult.Capacity := FstrModules.Count + FstrUnits.Count + FstrFunctions.Count + FstrLines.Count;
    strResult.Add('Application='   + FProcessResult.ProcessExe);
    strResult.Add('Header='    + IntToStr(4));
    strResult.Add('=Header=====================================');
    strResult.Add('Modules='   + IntToStr(FstrModules.Count));
    strResult.Add('Units='     + IntToStr(FstrUnits.Count));
    strResult.Add('Functions=' + IntToStr(FstrFunctions.Count));
    strResult.Add('Lines='     + IntToStr(FstrLines.Count));
    strResult.Add('=Modules=====================================');
    strResult.AddStrings(FstrModules);
    strResult.Add('=Units=====================================');
    strResult.AddStrings(FstrUnits);
    strResult.Add('=Functions=====================================');
    strResult.AddStrings(FstrFunctions);
    strResult.Add('=Lines=(Address=Line;Module;Unit;Function====================================');
    strResult.AddStrings(FstrLines);

    strResult.SaveToFile(aFile);
  finally
    strResult.Free;
  end;
end;

end.
