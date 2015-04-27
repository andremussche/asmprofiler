object frmMain: TfrmMain
  Left = 391
  Top = 330
  Width = 680
  Height = 427
  Caption = 'Profile Result Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    672
    400)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 69
    Height = 13
    Caption = 'Application dir:'
  end
  object edtDir: TJvDirectoryEdit
    Left = 88
    Top = 8
    Width = 579
    Height = 21
    DialogKind = dkWin32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edtDir'
    OnChange = edtDirChange
  end
  object vtTree: TVirtualStringTree
    Left = 8
    Top = 40
    Width = 659
    Height = 315
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    TabOrder = 1
    Columns = <>
  end
  object btnOpenViewer: TBitBtn
    Left = 8
    Top = 366
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open viewer'
    TabOrder = 2
    OnClick = btnOpenViewerClick
  end
  object lbItems: TListBox
    Left = 8
    Top = 40
    Width = 659
    Height = 316
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = 1
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    OnDblClick = lbItemsDblClick
  end
  object Button1: TButton
    Left = 589
    Top = 367
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
    Visible = False
    OnClick = Button1Click
  end
end
