object framProfileResultProcsTree: TframProfileResultProcsTree
  Left = 0
  Top = 0
  Width = 843
  Height = 502
  TabOrder = 0
  object vtreeUnit: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 843
    Height = 502
    Align = alClient
    CheckImageKind = ckDarkCheck
    DefaultNodeHeight = 14
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    IncrementalSearch = isAll
    IncrementalSearchTimeout = 2500
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnCompareNodes = vtreeUnitCompareNodes
    OnFreeNode = vtreeUnitFreeNode
    OnGetText = vtreeUnitGetText
    OnHeaderClick = vtreeUnitHeaderClick
    OnInitNode = vtreeUnitInitNode
    Columns = <
      item
        Position = 0
        Width = 180
        WideText = 'Unit'
      end
      item
        Position = 1
        Width = 100
        WideText = 'Class'
      end
      item
        Position = 2
        Width = 60
        WideText = 'Functions'
      end
      item
        Position = 3
        Width = 60
        WideText = 'Calls'
      end
      item
        Position = 4
        WideText = 'Part %'
      end
      item
        Position = 5
        WideText = 'Total %'
      end
      item
        Position = 6
        Width = 75
        WideText = 'Total + CT %'
      end
      item
        Position = 7
        Width = 70
        WideText = 'Total time'
      end
      item
        Position = 8
        Width = 70
        WideText = 'Child time'
      end
      item
        Position = 9
        Width = 75
        WideText = 'Total + Child'
      end>
  end
  object PopupMenu1: TPopupMenu
    Left = 208
    Top = 184
    object SavetoCSV1: TMenuItem
      Caption = 'Save to HTML'
      OnClick = SavetoCSV1Click
    end
    object SavetoRTF1: TMenuItem
      Caption = 'Save to RTF'
      OnClick = SavetoRTF1Click
    end
    object SavetoText1: TMenuItem
      Caption = 'Save to CSV'
      OnClick = SavetoText1Click
    end
  end
end
