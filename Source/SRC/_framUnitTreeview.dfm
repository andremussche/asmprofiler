object framUnitTreeview: TframUnitTreeview
  Left = 0
  Top = 0
  Width = 683
  Height = 438
  TabOrder = 0
  object vtreeItems: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 683
    Height = 407
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
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toCenterScrollIntoView, toSimpleDrawSelection]
    OnChecked = vtreeItemsChecked
    OnCompareNodes = vtreeItemsCompareNodes
    OnFreeNode = vtreeItemsFreeNode
    OnGetText = vtreeItemsGetText
    OnHeaderClick = vtreeItemsHeaderClick
    OnIncrementalSearch = vtreeItemsIncrementalSearch
    OnInitNode = vtreeItemsInitNode
    OnStateChange = vtreeItemsStateChange
    Columns = <
      item
        Position = 0
        Width = 300
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 80
        WideText = 'Address'
      end
      item
        Position = 2
        Width = 80
        WideText = 'End address'
      end
      item
        Position = 3
        Width = 200
        WideText = 'File'
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 407
    Width = 683
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 5
      Top = 8
      Width = 96
      Height = 13
      Caption = 'Incremental search:'
    end
    object lblIncrSearch: TLabel
      Left = 104
      Top = 8
      Width = 8
      Height = 13
      Caption = '()'
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 200
    Top = 160
    object SavetoHTML1: TMenuItem
      Caption = 'Save to HTML'
      OnClick = SavetoHTML1Click
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
