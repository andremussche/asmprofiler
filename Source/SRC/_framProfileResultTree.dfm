object framProfileResultTree: TframProfileResultTree
  Left = 0
  Top = 0
  Width = 921
  Height = 550
  TabOrder = 0
  object vtreeUnit: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 921
    Height = 550
    Align = alClient
    CheckImageKind = ckDarkCheck
    DefaultNodeHeight = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    Header.SortDirection = sdDescending
    IncrementalSearch = isAll
    IncrementalSearchTimeout = 2500
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 0
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnCompareNodes = vtreeUnitCompareNodes
    OnExpanding = vtreeUnitExpanding
    OnFreeNode = vtreeUnitFreeNode
    OnGetText = vtreeUnitGetText
    OnPaintText = vtreeUnitPaintText
    OnHeaderClick = vtreeUnitHeaderClick
    OnHeaderDblClick = vtreeUnitHeaderDblClick
    OnInitNode = vtreeUnitInitNode
    Columns = <
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 180
        WideText = 'Unit'
      end
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 100
        WideText = 'Class'
      end
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 2
        Width = 60
        WideText = 'Functions'
      end
      item
        Color = 13303754
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 3
        Width = 60
        WideText = 'Calls'
      end
      item
        Color = 13434879
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 4
        Width = 60
        WideText = 'Part %'
      end
      item
        Color = 12320767
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 5
        Width = 60
        WideText = 'Total %'
      end
      item
        Color = 12320767
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 6
        Width = 75
        WideText = 'Total + CT %'
      end
      item
        Color = 13557503
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 7
        Width = 70
        WideText = 'Total time'
      end
      item
        Color = 13557503
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 8
        Width = 70
        WideText = 'Child time'
      end
      item
        Color = 12572159
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 9
        Width = 75
        WideText = 'Total + Child'
      end
      item
        Color = 16769476
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 10
        Width = 75
        WideText = 'Average time'
      end
      item
        Color = 16769476
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 11
        Width = 75
        WideText = 'Avg + CT'
      end>
  end
  object PopupMenu1: TPopupMenu
    Left = 200
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
