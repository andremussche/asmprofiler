object framProfileTraceTree: TframProfileTraceTree
  Left = 0
  Top = 0
  Width = 781
  Height = 635
  TabOrder = 0
  object vtreeTrace: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 781
    Height = 635
    Align = alClient
    CheckImageKind = ckDarkCheck
    DefaultNodeHeight = 14
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    Header.AutoSizeIndex = -1
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
    OnCompareNodes = vtreeTraceCompareNodes
    OnFreeNode = vtreeTraceFreeNode
    OnGetText = vtreeTraceGetText
    OnPaintText = vtreeTracePaintText
    OnHeaderClick = vtreeTraceHeaderClick
    OnInitNode = vtreeTraceInitNode
    Columns = <
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 400
        WideText = 'Function'
      end
      item
        Color = 13303754
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 1
        WideText = '# Childs'
      end
      item
        Color = 13303754
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 2
        Width = 70
        WideText = '# All Childs'
      end
      item
        Color = 13557503
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 3
        Width = 100
        WideText = 'Total time'
      end
      item
        Color = 12320767
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 4
        Width = 60
        WideText = '%'
      end>
  end
  object PopupMenu1: TPopupMenu
    Left = 168
    Top = 112
    object Deletesmallitems1: TMenuItem
      Caption = 'Delete small items (< 1%)'
      OnClick = Deletesmallitems1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SavetoHTML1: TMenuItem
      Caption = 'Save to HTML'
      OnClick = SavetoHTML1Click
    end
    object SavetoRTF2: TMenuItem
      Caption = 'Save to RTF'
      OnClick = SavetoRTF2Click
    end
    object SavetoCSV2: TMenuItem
      Caption = 'Save to CSV'
      OnClick = SavetoCSV2Click
    end
  end
end
