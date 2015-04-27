unit _frmSelectItems;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, VirtualTrees, _uProfileClasses,
  _frmProfileMain, _uProfileTypes, ComCtrls, CheckLst, _framUnitTreeview, Menus;

type
  { TODO 1 -oAM -cTodo : Add extra tabs for special parts (memory, dlls, app imports/exports, etc) }
  //for example:
  //             - Application Imports / Exports
  //             - Modules (bpl, dll) in application dir, load MAP file?
  { TODO 1 -oAM -cTodo : check/uncheck Delphi/Other/All units' }
  { TODO 1 -oAM -cTodo : Show other "plugins"? Memory, Thread CPU, Core CPU, Context switches, Disk, etc' }

  TfrmSelectItems = class(TForm)
    BitBtn1: TBitBtn;
    btnOK: TBitBtn;
    pgMain: TPageControl;
    tsMapFile: TTabSheet;
    TabSheet1: TTabSheet;
    framInternalItems: TframUnitTreeview;
    TabSheet2: TTabSheet;
    tsLoadedDllsOld: TTabSheet;
    framMapfile: TframUnitTreeview;
    framCustomItems: TframUnitTreeview;
    TabSheet4: TTabSheet;
    Label1: TLabel;
    tsProgramImports: TTabSheet;
    mmoImports: TMemo;
    mmoExports: TMemo;
    btnLoadProgramImportExport: TButton;
    btnLoadedModules: TButton;
    mmoModuleImportExports: TMemo;
    lbModules: TListBox;
    tsLoadedDlls: TTabSheet;
    framLoadedDlls: TframUnitTreeview;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnLoadProgramImportExportClick(Sender: TObject);
    procedure btnLoadedModulesClick(Sender: TObject);
    procedure lbModulesClick(Sender: TObject);
  private
    //FSelectedItemsCount: integer;
    //FCustomItemsCount: integer;
    FMapFileLoader: TMapFileLoader;
    FInternalDebugInfo: TDebugInfoStorage;
    FCustomDebugInfo: TDebugInfoStorage;
    FLoadedDllsInfo: TProgramLoadedDllsStorage;
    procedure SetMapFileLoader(const Value: TMapFileLoader);
    procedure SetCustomDebugInfo(const Value: TDebugInfoStorage);
    procedure SetInternalDebugInfo(const Value: TDebugInfoStorage);
    procedure SetLoadedDllsInfo(const Value: TProgramLoadedDllsStorage);
    { Private declarations }
  protected
  public
    { Public declarations }
    procedure SaveSelectedItemLists;

    property MapFileLoader: TMapFileLoader read FMapFileLoader write SetMapFileLoader;
    property InternalDebugInfo: TDebugInfoStorage read FInternalDebugInfo write SetInternalDebugInfo;
    property CustomDebugInfo: TDebugInfoStorage read FCustomDebugInfo write SetCustomDebugInfo;
    property LoadedDllsInfo: TProgramLoadedDllsStorage read FLoadedDllsInfo write SetLoadedDllsInfo;
  end;

var
  frmSelectItems: TfrmSelectItems;

implementation

uses
  Math, JclPeImage, jclSysInfo;

{$R *.dfm}

procedure TfrmSelectItems.SetMapFileLoader(const Value: TMapFileLoader);
begin
  FMapFileLoader := Value;
  framMapfile.DebugInfoStorage := Value;
end;

procedure TfrmSelectItems.FormCreate(Sender: TObject);
begin
  pgMain.ActivePageIndex := 0;

  tsLoadedDllsOld.TabVisible := False;
  tsProgramImports.TabVisible := False;
end;

procedure TfrmSelectItems.lbModulesClick(Sender: TObject);
var
  str:Tstrings;
  sModule:string;
begin
  mmoModuleImportExports.Clear;
  if lbModules.ItemIndex < 0 then exit;
  sModule := lbModules.Items[lbModules.ItemIndex];

  str := TStringList.create;
  JclPeImage.PeImportedFunctions(sModule, str, '', True);
  mmoModuleImportExports.Clear;
  mmoModuleImportExports.Lines.AddStrings(str);

  str.Clear;
  JclPeImage.PeExportedFunctions(sModule, str);
  mmoModuleImportExports.lines.Add('');
  mmoModuleImportExports.Lines.AddStrings(str);

  str.free;
end;

procedure TfrmSelectItems.btnLoadedModulesClick(Sender: TObject);
var
  str:Tstrings;
begin
  str := TStringList.create;
  jclSysInfo.LoadedModulesList(str,GetCurrentProcessId, False);
  lbModules.Clear;
  lbModules.Items.AddStrings(str);
  str.free; 
end;

procedure TfrmSelectItems.btnLoadProgramImportExportClick(Sender: TObject);
var
  str:Tstrings;
begin
  str := TStringList.create;

  JclPeImage.PeImportedFunctions(Application.ExeName, str, '', True);
  mmoImports.Clear;
  mmoImports.Lines.AddStrings(str);

  str.Clear;
  JclPeImage.PeExportedFunctions(Application.ExeName, str);
  mmoExports.Clear;
  mmoExports.Lines.AddStrings(str);

  str.free;
end;

procedure TfrmSelectItems.btnOKClick(Sender: TObject);
begin
  SaveSelectedItemLists;
end;

procedure TfrmSelectItems.SetCustomDebugInfo(
  const Value: TDebugInfoStorage);
begin
  FCustomDebugInfo := Value;
  framCustomItems.DebugInfoStorage := FCustomDebugInfo;
end;

procedure TfrmSelectItems.SetInternalDebugInfo(
  const Value: TDebugInfoStorage);
begin
  FInternalDebugInfo := Value;
  framInternalItems.DebugInfoStorage := FInternalDebugInfo;
end;

procedure TfrmSelectItems.SetLoadedDllsInfo(
  const Value: TProgramLoadedDllsStorage);
begin
  FLoadedDllsInfo := Value;

  //refresh
  if Value <> nil then
    FLoadedDllsInfo.LoadProgramLoadedDlls;

  framLoadedDlls.DebugInfoStorage := FLoadedDllsInfo;
end;

procedure TfrmSelectItems.SaveSelectedItemLists;
begin
  framMapfile.SaveSelectedItemsList;
  framInternalItems.SaveSelectedItemsList;
  framCustomItems.SaveSelectedItemsList;
  framLoadedDlls.SaveSelectedItemsList;
end;

end.


