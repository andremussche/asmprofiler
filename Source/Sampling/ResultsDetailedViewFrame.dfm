inherited framDetailedView: TframDetailedView
  Width = 664
  Height = 514
  object Splitter1: TSplitter [0]
    Left = 0
    Top = 285
    Width = 664
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  inherited vtreeItems: TVirtualStringTree
    Width = 664
    Height = 285
    Header.MainColumn = 0
    OnChange = vtreeItemsChange
    OnExpanded = vtreeItemsExpanded
    OnGetText = vtreeItemsGetText
    OnInitNode = vtreeItemsInitNode
    Columns = <
      item
        Color = 15921906
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 270
        WideText = 'Part'
      end
      item
        Color = 13303754
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 60
        WideText = 'Calls'
      end
      item
        Color = 12320767
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 2
        WideText = 'Own %'
      end
      item
        Color = 12320767
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 3
        Width = 75
        WideText = 'Own+Child %'
      end
      item
        Color = 13557503
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 4
        Width = 70
        WideText = 'Own time'
      end
      item
        Color = 13557503
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 5
        Width = 70
        WideText = 'Child time'
      end
      item
        Color = 12572159
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 6
        Width = 70
        WideText = 'Own+Child'
      end>
  end
  object Panel1: TPanel [2]
    Left = 0
    Top = 288
    Width = 664
    Height = 226
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 118
      Width = 664
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object GroupBox1: TGroupBox
      Left = 0
      Top = 121
      Width = 664
      Height = 105
      Align = alBottom
      Caption = 'Parent calls (called by ...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      inline framFunctionListParents: TframFunctionList
        Left = 2
        Top = 15
        Width = 660
        Height = 88
        Align = alClient
        TabOrder = 0
        inherited vtreeItems: TVirtualStringTree
          Width = 660
          Height = 88
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = 0
      Width = 664
      Height = 118
      Align = alClient
      Caption = ' Child calls (called ...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      inline framFunctionListChilds: TframFunctionList
        Left = 2
        Top = 15
        Width = 660
        Height = 101
        Align = alClient
        TabOrder = 0
        inherited vtreeItems: TVirtualStringTree
          Width = 660
          Height = 101
          Color = clBtnFace
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
  end
end
