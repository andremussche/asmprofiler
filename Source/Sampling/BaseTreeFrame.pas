unit BaseTreeFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, Menus;

type
  TframBaseTree = class(TFrame)
    vtreeItems: TVirtualStringTree;
    PopupMenu1: TPopupMenu;
    SavetoClipboard1: TMenuItem;
    SavetoHTML1: TMenuItem;
    SavetoRTF1: TMenuItem;
    SavetoText1: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure vtreeItemsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure SavetoClipboard1Click(Sender: TObject);
    procedure SavetoHTML1Click(Sender: TObject);
    procedure SavetoRTF1Click(Sender: TObject);
    procedure SavetoText1Click(Sender: TObject);
  private
    { Private declarations }
    procedure SaveTextToFile(const aFile: string; const aData: AnsiString);
  public
    { Public declarations }
  end;

implementation

uses
  Clipbrd;

{$R *.dfm}

procedure TframBaseTree.SaveTextToFile(const aFile: string;
  const aData: AnsiString);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(SaveDialog1.FileName, fmCreate);
  try
    fs.Write( aData[1], Length(aData));
  finally
    fs.Free;
  end;
end;

procedure TframBaseTree.SavetoClipboard1Click(Sender: TObject);
var
  h: HGLOBAL;
begin
  h := vtreeItems.ContentToClipboard(CF_TEXT, tstAll);
  Clipboard.SetAsHandle(CF_TEXT, h);
end;

procedure TframBaseTree.SavetoHTML1Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt := '*.html';
  if SaveDialog1.Execute then
    SaveTextToFile(SaveDialog1.FileName, vtreeItems.ContentToHTML(tstAll));
end;

procedure TframBaseTree.SavetoRTF1Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt := '*.rtf';
  if SaveDialog1.Execute then
    SaveTextToFile(SaveDialog1.FileName, vtreeItems.ContentToRTF(tstAll));
end;

procedure TframBaseTree.SavetoText1Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt := '*.txt';
  if SaveDialog1.Execute then
    SaveTextToFile(SaveDialog1.FileName, vtreeItems.ContentToText(tstAll, #9{tab}));
end;

procedure TframBaseTree.vtreeItemsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if (vtreeItems.Header.SortColumn <> HitInfo.Column) then
  begin
    vtreeItems.Header.SortColumn    := HitInfo.Column;
    vtreeItems.Header.SortDirection := sdAscending
  end
  else if (vtreeItems.Header.SortDirection = sdDescending) then
    vtreeItems.Header.SortDirection := sdAscending
  else
    vtreeItems.Header.SortDirection := sdDescending;

  vtreeItems.SortTree( HitInfo.Column, vtreeItems.Header.SortDirection );
end;

end.
