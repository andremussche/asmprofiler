object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 799
  Height = 549
  TabOrder = 0
  object vtreeItems: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 799
    Height = 549
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
    Header.SortDirection = sdDescending
    IncrementalSearch = isAll
    IncrementalSearchTimeout = 2500
    TabOrder = 0
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnCompareNodes = vtreeItemsCompareNodes
    OnFreeNode = vtreeItemsFreeNode
    OnGetText = vtreeItemsGetText
    OnPaintText = vtreeItemsPaintText
    OnHeaderClick = vtreeItemsHeaderClick
    OnInitNode = vtreeItemsInitNode
    Columns = <
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 120
        WideText = 'Module'
      end
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 100
        WideText = 'Unit'
      end
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 2
        Width = 100
        WideText = 'Class'
      end
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 3
        Width = 150
        WideText = 'Function'
      end
      item
        Color = 13303754
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 4
        Width = 60
        WideText = 'Calls'
      end
      item
        Color = 12320767
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 5
        WideText = 'Own %'
      end
      item
        Color = 12320767
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 6
        Width = 75
        WideText = 'Own+Child %'
      end
      item
        Color = 13557503
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 7
        Width = 70
        WideText = 'Own time'
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
        Width = 70
        WideText = 'Own+Child'
      end>
  end
end
