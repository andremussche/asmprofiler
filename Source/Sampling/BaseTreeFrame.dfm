object framBaseTree: TframBaseTree
  Left = 0
  Top = 0
  Width = 508
  Height = 435
  TabOrder = 0
  object vtreeItems: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 508
    Height = 435
    Align = alClient
    CheckImageKind = ckDarkCheck
    DefaultNodeHeight = 14
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    Header.SortDirection = sdDescending
    IncrementalSearch = isAll
    IncrementalSearchTimeout = 2500
    PopupMenu = PopupMenu1
    TabOrder = 0
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnHeaderClick = vtreeItemsHeaderClick
    Columns = <>
  end
  object PopupMenu1: TPopupMenu
    Left = 104
    Top = 88
    object SavetoClipboard1: TMenuItem
      Caption = 'Save to Clipboard'
      OnClick = SavetoClipboard1Click
    end
    object SavetoHTML1: TMenuItem
      Caption = 'Save to HTML'
      OnClick = SavetoHTML1Click
    end
    object SavetoRTF1: TMenuItem
      Caption = 'Save to RTF'
      OnClick = SavetoRTF1Click
    end
    object SavetoText1: TMenuItem
      Caption = 'Save to Text'
      OnClick = SavetoText1Click
    end
  end
  object SaveDialog1: TSaveDialog
    Filter = 'HTML (*.html)|*.html|RTF (*.rtf)|*.rtf|Text (*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofPathMustExist, ofEnableSizing]
    Left = 208
    Top = 88
  end
end
