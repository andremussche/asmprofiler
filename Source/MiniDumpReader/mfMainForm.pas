unit mfMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  uDragDrop, ComCtrls;

type
  TfrmMain = class(TForm,
                   IDragDrop)
    dlgOpenDumpFile: TFileOpenDialog;
    lblDumpFile: TLabel;
    btnOpenDumpFile: TButton;
    lblExeFile: TLabel;
    btnOpenDebugFile: TButton;
    dlgOpenOrgExe: TFileOpenDialog;
    btnShowMinidump: TButton;
    chkModules: TCheckBox;
    chkThreads: TCheckBox;
    chkHandles: TCheckBox;
    chkDontCheckValidCallSite: TCheckBox;
    pgMain: TPageControl;
    tsMinidump: TTabSheet;
    tsCustom: TTabSheet;
    btnGetCustomLocationInfo: TButton;
    Memo1: TMemo;
    mmoCustomMemAddresses: TMemo;
    Label1: TLabel;
    mmoCustomLocationInfo: TMemo;
    procedure btnShowMinidumpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOpenDumpFileClick(Sender: TObject);
    procedure btnOpenDebugFileClick(Sender: TObject);
    procedure btnGetCustomLocationInfoClick(Sender: TObject);
  protected
    FDropTarget: TDropTarget;
    {IDragDrop}
    function  DropAllowed(const FileNames: array of string): Boolean;
    procedure Drop(const FileNames: array of string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  mcMiniDumpReader, DateUtils, mcOfflineDebugInfo;

{$R *.dfm}

procedure TfrmMain.btnOpenDebugFileClick(Sender: TObject);
begin
  if dlgOpenOrgExe.Execute then
  begin
    lblExeFile.Caption := dlgOpenOrgExe.FileName;
  end;
end;

procedure TfrmMain.btnOpenDumpFileClick(Sender: TObject);
begin
  if dlgOpenDumpFile.Execute then
  begin
    lblDumpFile.Caption     := dlgOpenDumpFile.FileName;
    btnShowMinidump.Enabled := FileExists(lblDumpFile.Caption);
  end;
end;

procedure TfrmMain.btnShowMinidumpClick(Sender: TObject);
var
  i: integer;
  reader: TMinidumpReader;
  module: TMiniDumpModule;
  thread: TMiniDumpThread;
  handle: TMiniDumpHandle;
begin
  if not FileExists(lblDumpFile.Caption) then
  begin
    MessageDlg('Minidump file not found!', mtError, [mbOK], 0);
    Exit;
  end;
//  if not FileExists(lblExeFile.Caption) then
//  begin
//    MessageDlg('Original exe file not found!', mtError, [mbOK], 0);
//    Exit;
//  end;

  Screen.Cursor := crHourGlass;
  reader := TMinidumpReader.Create(lblDumpFile.Caption);
  reader.DontCheckValidCallSite := chkDontCheckValidCallSite.Checked;
  try
    Memo1.Clear;
    Memo1.Lines.BeginUpdate;
    if reader.SystemInfo <> nil then
    begin
      Memo1.Lines.Add( '');
      Memo1.Lines.Add( 'Systeminfo:');
      Memo1.Lines.Add( format('  ProcessorArchitecture: %d', [reader.SystemInfo.ProcessorArchitecture]));
      Memo1.Lines.Add( format('  ProcessorLevel: %d', [reader.SystemInfo.ProcessorLevel]));
      Memo1.Lines.Add( format('  ProcessorRevision: %d', [reader.SystemInfo.ProcessorRevision]));
      Memo1.Lines.Add( format('  NumberOfProcessors: %d', [reader.SystemInfo.U.NumberOfProcessors]));
      Memo1.Lines.Add( 'OS:');
      Memo1.Lines.Add( format('  ProductType: %d', [reader.SystemInfo.U.ProductType]));
      Memo1.Lines.Add( format('  MajorVersion: %d', [reader.SystemInfo.MajorVersion]));
      Memo1.Lines.Add( format('  MinorVersion: %d', [reader.SystemInfo.MinorVersion]));
      Memo1.Lines.Add( format('  BuildNumber: %d', [reader.SystemInfo.BuildNumber]));
      Memo1.Lines.Add( format('  PlatformId: %d', [reader.SystemInfo.PlatformId]));
      Memo1.Lines.Add( format('  SuiteMask: %d', [reader.SystemInfo.U2.SuiteMask]));
      Memo1.Lines.Add( format('  Servicepack: %s', [reader.Servicepack]));
      Memo1.Lines.Add( 'CPU:');
      Memo1.Lines.Add( format('  VendorId0: %d', [reader.SystemInfo.Cpu.VendorId[0]]));
      Memo1.Lines.Add( format('  VendorId1: %d', [reader.SystemInfo.Cpu.VendorId[1]]));
      Memo1.Lines.Add( format('  VendorId2: %d', [reader.SystemInfo.Cpu.VendorId[2]]));
      Memo1.Lines.Add( format('  VersionInformation: %d', [reader.SystemInfo.Cpu.VersionInformation]));
      Memo1.Lines.Add( format('  FeatureInformation: %d', [reader.SystemInfo.Cpu.FeatureInformation]));
      Memo1.Lines.Add( format('  AMDExtendedCpuFeatures: %d', [reader.SystemInfo.Cpu.AMDExtendedCpuFeatures]));
      Memo1.Lines.Add( format('  ProcessorFeatures0: %d', [reader.SystemInfo.Cpu.ProcessorFeatures[0]]));
      Memo1.Lines.Add( format('  ProcessorFeatures1: %d', [reader.SystemInfo.Cpu.ProcessorFeatures[1]]));
    end;

    if reader.ProcessInfo <> nil then
    begin
      Memo1.Lines.Add( '');
      Memo1.Lines.Add( 'Process info:');
      Memo1.Lines.Add( format('  PID: %d', [reader.ProcessInfo.ProcessId]));
      Memo1.Lines.Add( format('  ProcessCreateTime: %d', [reader.ProcessInfo.ProcessCreateTime]));
      Memo1.Lines.Add( format('  ProcessUserTime: %d', [reader.ProcessInfo.ProcessUserTime]));
      Memo1.Lines.Add( format('  ProcessKernelTime: %d', [reader.ProcessInfo.ProcessKernelTime]));
    end;

    if chkModules.Checked then
    begin
      Memo1.Lines.Add( '');
      Memo1.Lines.Add( format('Modules (%d):', [reader.Modules.Count]));
      //show original exe if not set
      if (reader.Modules.Count > 0) and
         (lblExeFile.Caption = '')
      then
        lblExeFile.Caption := reader.Modules[0].FileName;
      for module in reader.Modules do
      begin
        Memo1.Lines.Add( '================================');
        Memo1.Lines.Add( format('Module: %s', [module.FileName]));
        Memo1.Lines.Add( format('BaseOfImage: %x', [module.BaseOfImage]));
        Memo1.Lines.Add( format('SizeOfImage: %d', [module.SizeOfImage]));
        Memo1.Lines.Add( format('CheckSum: %x', [module.CheckSum]));
        Memo1.Lines.Add( format('TimeDateStamp: %s', [DateTimeToStr(module.Time)]));
  //      Memo1.Lines.Add( format('VersionInfo.dwSignature: %d', [module.VersionInfo.dwSignature]));
  //      Memo1.Lines.Add( format('VersionInfo.dwStrucVersion: %d', [module.VersionInfo.dwStrucVersion]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileVersionMS: %d', [module.VersionInfo.dwFileVersionMS]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileVersionLS: %d', [module.VersionInfo.dwFileVersionLS]));
  //      Memo1.Lines.Add( format('VersionInfo.dwProductVersionMS: %d', [module.VersionInfo.dwProductVersionMS]));
  //      Memo1.Lines.Add( format('VersionInfo.dwProductVersionLS: %d', [module.VersionInfo.dwProductVersionLS]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileFlagsMask: %d', [module.VersionInfo.dwFileFlagsMask]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileFlags: %d', [module.VersionInfo.dwFileFlags]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileOS: %d', [module.VersionInfo.dwFileOS]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileType: %d', [module.VersionInfo.dwFileType]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileSubtype: %d', [module.VersionInfo.dwFileSubtype]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileDateMS: %d', [module.VersionInfo.dwFileDateMS]));
  //      Memo1.Lines.Add( format('VersionInfo.dwFileDateLS: %d', [module.VersionInfo.dwFileDateLS]));
      end;
    end;

    if chkThreads.Checked then
    begin
      Memo1.Lines.Add( '');
      Memo1.Lines.Add( format('Threads (%d):', [reader.Threads.Count]));
      for i := 0 to reader.Threads.Count - 1 do
      begin
        thread := reader.Threads[i];

        Memo1.Lines.Add( '================================');
        Memo1.Lines.Add( format('ThreadID: %d', [thread.ThreadId]));
        Memo1.Lines.Add( format('SuspendCount: %d', [thread.SuspendCount]));
        Memo1.Lines.Add( format('PriorityClass: %d', [thread.PriorityClass]));
        Memo1.Lines.Add( format('Priority: %d', [thread.Priority]));

        if thread.ThreadInfo <> nil then
        begin
          Memo1.Lines.Add( 'Extra info:');
          Memo1.Lines.Add( format('  DumpFlags: %d', [thread.ThreadInfo.DumpFlags]));
          Memo1.Lines.Add( format('  DumpError: %d', [thread.ThreadInfo.DumpError]));
          Memo1.Lines.Add( format('  ExitStatus: %d', [thread.ThreadInfo.ExitStatus]));
          Memo1.Lines.Add( format('  CreateTime: %d', [thread.ThreadInfo.CreateTime]));
          Memo1.Lines.Add( format('  ExitTime: %d', [thread.ThreadInfo.ExitTime]));
          Memo1.Lines.Add( format('  KernelTime: %d', [thread.ThreadInfo.KernelTime]));
          Memo1.Lines.Add( format('  UserTime: %d', [thread.ThreadInfo.UserTime]));
          Memo1.Lines.Add( format('  StartAddress: %d', [thread.ThreadInfo.StartAddress]));
          Memo1.Lines.Add( format('  Affinity: %d', [thread.ThreadInfo.Affinity]));
        end
        else
          Memo1.Lines.Add( 'Extra info: <none>');

        Memo1.Lines.Add('Stack: ');
        Memo1.Lines.Add( '--------------------------------');
        reader.AddThreadStackToStrings( lblExeFile.Caption,
                                        thread, memo1.Lines, True, True, True, True);
        Memo1.Lines.Add( '--------------------------------');
      end;
    end;

    if chkHandles.Checked then
    begin
      Memo1.Lines.Add( '');
      Memo1.Lines.Add( format('Handles (%d):', [reader.Handles.Count]));
      for i := 0 to reader.Handles.Count - 1 do
      begin
        handle := reader.Handles[i];

        Memo1.Lines.Add( '================================');
        Memo1.Lines.Add( format('Handle: %x', [handle.Handle]));
        Memo1.Lines.Add( format('TypeName: %s', [handle.TypeName]));
        Memo1.Lines.Add( format('ObjectName: %s', [handle.ObjectName]));
        Memo1.Lines.Add( format('Attributes: %d', [handle.Attributes]));
        Memo1.Lines.Add( format('GrantedAccess: %d', [handle.GrantedAccess]));
        Memo1.Lines.Add( format('HandleCount: %d', [handle.HandleCount]));
        Memo1.Lines.Add( format('PointerCount: %d', [handle.PointerCount]));
      end;
    end;

  finally
    Screen.Cursor := crDefault;

    Memo1.Lines.EndUpdate;
    Memo1.SelStart := 0;  //move to begin
    reader.Free;
  end;
end;


procedure TfrmMain.btnGetCustomLocationInfoClick(Sender: TObject);
var
  s, sLine, sInfo: string;
  strline: TStrings;
  FOfflineProcessDebugInfoList: TOfflineProcessDebugInfoList;
  reader: TMinidumpReader;
  addr: Pointer;
  iaddr: Integer;
begin
  mmoCustomLocationInfo.Clear;
  if not FileExists(lblDumpFile.Caption) then
  begin
    MessageDlg('Dump file needed for module info', mtError, [mbOK], 0);
    Exit;
  end;

  reader := TMinidumpReader.Create(lblDumpFile.Caption);
  reader.DontCheckValidCallSite := chkDontCheckValidCallSite.Checked;
  FOfflineProcessDebugInfoList := TOfflineProcessDebugInfoList.Create(reader, lblExeFile.Caption);
  strline := TStringList.Create;
  try
    for sLine in mmoCustomMemAddresses.Lines do
    begin
      //process all words of each line, so also "normal" text or complete error message can be pasted
      strline.DelimitedText := sLine;
      sInfo := '';
      for s in strline do
      begin
        //try each word, if valid location info found then we have something usefull
        if TryStrToInt(Trim(s), iaddr) or
           TryStrToInt('$' + UpperCase(Trim(s)), iaddr) then
        begin
          addr  := Pointer(iaddr);
          sInfo := FOfflineProcessDebugInfoList
                     .GetOfflineLocationInfoStr(addr, True, True, True, True, True);
          if sInfo = '' then
           if TryStrToInt('$' + UpperCase(Trim(s)), iaddr) then
           begin
             addr  := Pointer(iaddr);
             sInfo := FOfflineProcessDebugInfoList
                        .GetOfflineLocationInfoStr(addr, True, True, True, True, True);
           end;
          if sInfo <> '' then
          begin
            mmoCustomLocationInfo.Lines.Add(sInfo);
            Break;
          end;
        end
      end;

      if sInfo = '' then
        mmoCustomLocationInfo.Lines.Add(sLine);
    end;
  finally
    strline.Free;
    FOfflineProcessDebugInfoList.Free;
    reader.Free;
  end;
end;

procedure TfrmMain.Drop(const FileNames: array of string);
var s: string;
begin
  for s in FileNames do
  begin
    if LowerCase(ExtractFileExt(s)) = '.dmp' then
    begin
      lblDumpFile.Caption     := s;
      btnShowMinidump.Enabled := FileExists(s);
    end
    else if LowerCase(ExtractFileExt(s)) = '.exe' then
    begin
      lblExeFile.Caption := s;
    end;
  end;
end;

function TfrmMain.DropAllowed(const FileNames: array of string): Boolean;
var s: string;
begin
  Result := False;
  for s in FileNames do
  begin
    if (LowerCase(ExtractFileExt(s)) = '.dmp') or
       (LowerCase(ExtractFileExt(s)) = '.exe')
    then
      Exit(True)
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  lblDumpFile.Caption  := '';
  lblExeFile.Caption   := '';
  btnShowMinidump.Enabled := False;
  Memo1.Clear;

  mmoCustomMemAddresses.Clear;
  mmoCustomLocationInfo.Clear;
  pgMain.ActivePage := tsMinidump;

  //ready to accept files
  //DragAcceptFiles(Handle, True) ;
  FDropTarget := TDropTarget.Create(Self.Handle, Self);
end;

end.
