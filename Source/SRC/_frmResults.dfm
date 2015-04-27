object frmResults: TfrmResults
  Left = 216
  Top = 125
  Caption = 'Profile Result Viewer'
  ClientHeight = 732
  ClientWidth = 989
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    989
    732)
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 906
    Top = 699
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 989
    Height = 698
    ActivePage = TabSheet2
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = ImageList1
    Style = tsFlatButtons
    TabOrder = 1
    object TabSheet2: TTabSheet
      Caption = 'Profile times'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 981
        Height = 44
        Align = alTop
        Caption = ' Part '
        ParentBackground = False
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 19
          Width = 37
          Height = 13
          Caption = 'Thread:'
        end
        object cmbxThread: TComboBox
          Left = 88
          Top = 15
          Width = 217
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = cmbxThreadChange
        end
        object btnRefresh: TBitBtn
          Left = 308
          Top = 14
          Width = 75
          Height = 22
          Caption = 'Refresh'
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
            C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
            A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
            FCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBC2A6A4FF00FFFF00FFFF00FFFF00FF
            C2A6A4FEFCFBFEFCFBFEFCFBFEFCFBD8EBD6018A02018A02D8EBD6FEFCFBFEFC
            FBC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFBF7FEFBF7018A02D8EAD201
            8A02D8EAD2D8EAD2018A02FEFBF7FEFBF7C2A6A4FF00FFFF00FFFF00FFFF00FF
            C2A6A4FEF9F4FEF9F4018A02018A02D8E8D0FEF9F4FEF9F4D8E8D0FEF9F4FEF9
            F4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF7F0FEF7F0018A02018A0201
            8A02FEF7F0FEF7F0FEF7F0FEF7F0FEF7F0C2A6A4FF00FFFF00FFFF00FFFF00FF
            C2A6A4FEF5ECFEF5ECFEF5ECFEF5ECFEF5EC018A02018A02018A02FEF5ECFEF5
            ECC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9FEF3E9D8E3C7FEF3E9FE
            F3E9D8E3C7018A02018A02FEF3E9FEF3E9C2A6A4FF00FFFF00FFFF00FFFF00FF
            C2A6A4FFF1E5FFF1E5018A02D9E2C3D9E2C3018A02D9E2C3018A02FFF1E5FFF1
            E5C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFF0E2FFF0E2D9E1C1018A0201
            8A02D9E1C1DDCFC2DDCFC2DDCFC2DDCFC2C2A6A4FF00FFFF00FFFF00FFFF00FF
            C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC5B5A9C3B4A8C2B3A7C1B2
            A6C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFECDAFFECDAFFECDAFFECDAFF
            ECDAFFECDAB0A296B0A296B0A296B0A296C2A6A4FF00FFFF00FFFF00FFFF00FF
            C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFBF8F4FBF8F4E6DAD9C2A6
            A4FF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4FFE8D3FFE8D3FFE8D3FFE8D3FF
            E8D3C9B9ACFBF8F4DFCEC7C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDFCEC7C2A6A4FF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2
            A6A4C2A6A4C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          TabOrder = 1
          OnClick = btnRefreshClick
        end
        object chkOverheadCompensation: TCheckBox
          Left = 394
          Top = 18
          Width = 223
          Height = 17
          Caption = 'Overhead compensation'
          TabOrder = 2
        end
      end
      object pgProfileTimes: TPageControl
        AlignWithMargins = True
        Left = 0
        Top = 48
        Width = 981
        Height = 618
        Margins.Left = 0
        Margins.Top = 4
        Margins.Right = 0
        Margins.Bottom = 0
        ActivePage = tsTraceTree
        Align = alClient
        Images = ImageList1
        Style = tsFlatButtons
        TabOrder = 1
        OnChange = pgProfileTimesChange
        object tsFunctionOverview: TTabSheet
          Caption = 'Function overview'
          ImageIndex = 1
          object vtreeItems: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 973
            Height = 586
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
            PopupMenu = PopupMenu1
            TabOrder = 0
            TreeOptions.SelectionOptions = [toFullRowSelect]
            OnCompareNodes = vtreeItemsCompareNodes
            OnDblClick = vtreeItemsDblClick
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
                Width = 150
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
                Width = 150
                WideText = 'Function'
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
                WideText = 'Unit %'
              end
              item
                Color = 12320767
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
                Position = 5
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
                WideText = 'Total Child'
              end
              item
                Color = 12572159
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
                Position = 9
                Width = 70
                WideText = 'Total + CT'
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
        end
        object tsUnitOverview: TTabSheet
          Caption = 'Unit overview'
          ImageIndex = 1
          object Splitter2: TSplitter
            Left = 0
            Top = 261
            Width = 973
            Height = 3
            Cursor = crVSplit
            Align = alBottom
          end
          object Label12: TLabel
            Left = 0
            Top = 0
            Width = 973
            Height = 13
            Align = alTop
            Caption = 'Overview:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
          end
          inline framProfileResultTree1: TframProfileResultTree
            Left = 0
            Top = 13
            Width = 973
            Height = 248
            Align = alClient
            TabOrder = 0
            inherited vtreeUnit: TVirtualStringTree
              Width = 973
              Height = 248
              Font.Name = 'MS Sans Serif'
              OnChange = framProfileResultTree1vtreeUnitChange
              OnDblClick = framProfileResultTree1vtreeUnitDblClick
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
                  Color = 16704723
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
          end
          object Panel1: TPanel
            Left = 0
            Top = 264
            Width = 973
            Height = 322
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object Splitter1: TSplitter
              Left = 0
              Top = 105
              Width = 973
              Height = 3
              Cursor = crVSplit
              Align = alTop
            end
            object gbxParents: TGroupBox
              Left = 0
              Top = 0
              Width = 973
              Height = 105
              Align = alTop
              Caption = ' Parents '
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentBackground = False
              ParentFont = False
              TabOrder = 0
              inline framProfileResultParents: TframProfileResultTree
                Left = 2
                Top = 15
                Width = 969
                Height = 88
                Align = alClient
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                inherited vtreeUnit: TVirtualStringTree
                  Width = 969
                  Height = 88
                  Font.Name = 'MS Sans Serif'
                  OnDblClick = framProfileResultParentsvtreeUnitDblClick
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
                      Color = 16704723
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
              end
            end
            object Panel3: TPanel
              Left = 0
              Top = 108
              Width = 973
              Height = 214
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 1
              object gbxChilds: TGroupBox
                Left = 0
                Top = 100
                Width = 973
                Height = 114
                Align = alClient
                Caption = ' Childs  '
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentBackground = False
                ParentFont = False
                TabOrder = 0
                inline framProfileResultChilds: TframProfileResultTree
                  Left = 2
                  Top = 15
                  Width = 969
                  Height = 97
                  Align = alClient
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 0
                  inherited vtreeUnit: TVirtualStringTree
                    Width = 969
                    Height = 97
                    Font.Name = 'MS Sans Serif'
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
                        Color = 16704723
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
                end
              end
              object gbxDetails: TGroupBox
                Left = 0
                Top = 0
                Width = 973
                Height = 100
                Align = alTop
                Caption = ' Details '
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentBackground = False
                ParentFont = False
                TabOrder = 1
                object Label2: TLabel
                  Left = 149
                  Top = 30
                  Width = 47
                  Height = 13
                  Caption = 'Own time:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label3: TLabel
                  Left = 148
                  Top = 53
                  Width = 48
                  Height = 13
                  Caption = 'Child time:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label4: TLabel
                  Left = 147
                  Top = 76
                  Width = 49
                  Height = 13
                  Caption = 'Total time:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label5: TLabel
                  Left = 11
                  Top = 23
                  Width = 25
                  Height = 13
                  Caption = 'Calls:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label6: TLabel
                  Left = 11
                  Top = 50
                  Width = 58
                  Height = 13
                  Caption = 'Parent calls:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label7: TLabel
                  Left = 11
                  Top = 77
                  Width = 50
                  Height = 13
                  Caption = 'Child calls:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label8: TLabel
                  Left = 202
                  Top = 12
                  Width = 20
                  Height = 13
                  Caption = 'Min:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label9: TLabel
                  Left = 269
                  Top = 12
                  Width = 23
                  Height = 13
                  Caption = 'Max:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label10: TLabel
                  Left = 336
                  Top = 12
                  Width = 22
                  Height = 13
                  Caption = 'Avg:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                end
                object Label11: TLabel
                  Left = 411
                  Top = 12
                  Width = 27
                  Height = 13
                  Caption = 'Total:'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = [fsUnderline]
                  ParentFont = False
                end
                object edtMinOwnTime: TEdit
                  Left = 202
                  Top = 26
                  Width = 65
                  Height = 21
                  Color = 15923199
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 0
                  Text = 'edtMinOwnTime'
                end
                object edtMaxOwnTime: TEdit
                  Left = 269
                  Top = 26
                  Width = 65
                  Height = 21
                  Color = 15923199
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 1
                  Text = 'edtMaxOwnTime'
                end
                object edtAvgOwnTime: TEdit
                  Left = 336
                  Top = 26
                  Width = 65
                  Height = 21
                  Color = 16706784
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 2
                  Text = 'edtAvgOwnTime'
                end
                object edtCalls: TEdit
                  Left = 75
                  Top = 19
                  Width = 65
                  Height = 21
                  Color = 13303754
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 3
                  Text = 'edtCalls'
                end
                object edtParentCalls: TEdit
                  Left = 75
                  Top = 46
                  Width = 65
                  Height = 21
                  Color = 13303754
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 4
                  Text = 'edtParentCalls'
                end
                object edtChildCalls: TEdit
                  Left = 75
                  Top = 73
                  Width = 65
                  Height = 21
                  Color = 13303754
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 5
                  Text = 'edtChildCalls'
                end
                object edtMinChildTime: TEdit
                  Left = 202
                  Top = 49
                  Width = 65
                  Height = 21
                  Color = 15923199
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 6
                  Text = 'edtMinChildTime'
                end
                object edtMaxChildTime: TEdit
                  Left = 269
                  Top = 49
                  Width = 65
                  Height = 21
                  Color = 15923199
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 7
                  Text = 'edtMaxChildTime'
                end
                object edtAvgChildTime: TEdit
                  Left = 336
                  Top = 49
                  Width = 65
                  Height = 21
                  Color = 16706784
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 8
                  Text = 'edtAvgChildTime'
                end
                object edtMaxTotalTime: TEdit
                  Left = 269
                  Top = 73
                  Width = 65
                  Height = 21
                  Color = 14674943
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 9
                  Text = 'edtMaxTotalTime'
                end
                object edtAvgTotalTime: TEdit
                  Left = 336
                  Top = 73
                  Width = 65
                  Height = 21
                  Color = 16769476
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 10
                  Text = 'edtAvgTotalTime'
                end
                object edtMinTotalTime: TEdit
                  Left = 202
                  Top = 73
                  Width = 65
                  Height = 21
                  Color = 14674943
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 11
                  Text = 'edtMinTotalTime'
                end
                object edtTotalOwnTime: TEdit
                  Left = 411
                  Top = 26
                  Width = 65
                  Height = 21
                  Color = 4259584
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 12
                  Text = 'edtTotalOwnTime'
                end
                object edtTotalChildTime: TEdit
                  Left = 411
                  Top = 49
                  Width = 65
                  Height = 21
                  Color = 4227327
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 13
                  Text = 'edtTotalChildTime'
                  OnChange = edtTotalChildTimeChange
                end
                object edtTotalTotalTime: TEdit
                  Left = 411
                  Top = 73
                  Width = 65
                  Height = 21
                  Color = 12572159
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  TabOrder = 14
                  Text = 'edtTotalTotalTime'
                end
                object Chart1: TChart
                  Left = 482
                  Top = 15
                  Width = 489
                  Height = 83
                  Legend.Alignment = laTop
                  Legend.LegendStyle = lsSeries
                  Legend.Visible = False
                  MarginBottom = 1
                  MarginLeft = 0
                  MarginRight = 1
                  MarginTop = 1
                  Title.Text.Strings = (
                    'TChart')
                  Title.Visible = False
                  AxisVisible = False
                  BottomAxis.Title.Caption = 'Call #'
                  LeftAxis.Title.Caption = 'Duration (s)'
                  View3D = False
                  Zoom.Animated = True
                  Align = alRight
                  BevelOuter = bvNone
                  ParentColor = True
                  TabOrder = 15
                  Anchors = [akLeft, akTop, akRight, akBottom]
                  PrintMargins = (
                    15
                    41
                    15
                    41)
                  object OwnTimeSeries: TBarSeries
                    Marks.Arrow.Visible = True
                    Marks.Callout.Brush.Color = clBlack
                    Marks.Callout.Arrow.Visible = True
                    Marks.Visible = False
                    SeriesColor = 4259584
                    Title = 'Own time'
                    BarWidthPercent = 95
                    Gradient.Direction = gdTopBottom
                    MultiBar = mbStacked
                    XValues.Name = 'X'
                    XValues.Order = loAscending
                    YValues.Name = 'Bar'
                    YValues.Order = loNone
                  end
                  object ChildTimeSeries: TBarSeries
                    Marks.Arrow.Visible = True
                    Marks.Callout.Brush.Color = clBlack
                    Marks.Callout.Arrow.Visible = True
                    Marks.Visible = False
                    SeriesColor = 4227327
                    Title = 'Child time'
                    BarWidthPercent = 95
                    Gradient.Direction = gdTopBottom
                    MultiBar = mbStacked
                    XValues.Name = 'X'
                    XValues.Order = loAscending
                    YValues.Name = 'Bar'
                    YValues.Order = loNone
                  end
                end
              end
            end
          end
        end
        object tsTraceTree: TTabSheet
          Caption = 'Trace tree'
          ImageIndex = 2
          inline framProfileTraceTree1: TframProfileTraceTree
            Left = 0
            Top = 0
            Width = 973
            Height = 586
            Align = alClient
            TabOrder = 0
            inherited vtreeTrace: TVirtualStringTree
              Width = 973
              Height = 586
            end
          end
        end
        object tsText: TTabSheet
          Caption = 'Text'
          ImageIndex = -1
          object mmoProfileTimes: TMemo
            Left = 0
            Top = 0
            Width = 973
            Height = 586
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            Lines.Strings = (
              'Memo1')
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
        object tsCompleteTraceText: TTabSheet
          Caption = 'Complete trace'
          ImageIndex = 3
          object mmoResults: TMemo
            Left = 0
            Top = 31
            Width = 973
            Height = 555
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            Lines.Strings = (
              'mmoResults')
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 973
            Height = 31
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object btnCompleteTraceExecute: TButton
              Left = 3
              Top = 2
              Width = 75
              Height = 25
              Caption = 'Execute'
              TabOrder = 0
              OnClick = btnCompleteTraceExecuteClick
            end
          end
        end
        object tsInfo: TTabSheet
          Caption = 'Info'
          ImageIndex = 4
          object mmoInfoThread: TMemo
            Left = 0
            Top = 0
            Width = 973
            Height = 586
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            Lines.Strings = (
              'Memo1')
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
        object TabSheet1: TTabSheet
          Caption = 'Errors && Warnings'
          ImageIndex = 6
          object mmoErrorsAndWarnings: TMemo
            Left = 0
            Top = 0
            Width = 973
            Height = 586
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            Lines.Strings = (
              'Memo1')
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Info'
      ImageIndex = -1
      object mmoInfo: TMemo
        Left = 0
        Top = 0
        Width = 981
        Height = 666
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'mmoInfo')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 256
    Top = 214
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
  object PopupMenu2: TPopupMenu
    Left = 632
    Top = 512
    object Loadalldata1: TMenuItem
      Caption = 'Load all data'
      OnClick = Loadalldata1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Showlargechartinseperatepopup1: TMenuItem
      Caption = 'Show large chart in seperate popup'
      OnClick = Showlargechartinseperatepopup1Click
    end
  end
  object ImageList1: TImageList
    Left = 344
    Top = 216
    Bitmap = {
      494C010103000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B54D0800BD652100BD590800B5510800B5510800B55508000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A538
      0000C6824A00CE8E5200E7A26B002169F7009C8E8400D6965A00BD652100BD59
      1000A53800000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CE6900000000000000000000000000009C3808009C3808009C3808009C38
      08009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5380000D692
      5A00DE9A6300E7BE9400FFF3DE002169F700F7D7F700FFFBF700EFE3CE00C682
      4A00BD590800A53800000000000000000000A551210094492100944921009449
      2100944921009449210094492100944921009449210094492100944921009449
      210094492100944D2100A5512100000000000000000000000000000000000000
      0000CE690000CE690000CE690000CE6900009C380800FF9A9400EFA25A00DE86
      29009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BD754200E7A6
      6B00E7BE9400FFF3DE00EFCFAD00E7BE9400EFB68400E7BE9400F7EFD600FFF7
      E700BD693100AD4908000000000000000000AD4D2100FFF7EF00FFF3E700FFEF
      D600FFEBCE00FFE7C600FFE3C600FFDFBD00FFDFBD00FFDFBD00FFDFBD00FFDF
      BD00FFDFBD00FFDFBD0094492100000000000000000000000000000000000000
      0000CE6900000000000000000000000000009C3808009C3808009C3808009C38
      08009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD490800E7A67300D696
      5A00FFF7E700F7EFD600F7EFD600F7EFD600F7EBD600E7BA8C00EFB28400FFF3
      DE00E7BE9C00B5550800A538000000000000AD4D2100FFFBF7004271FF004271
      FF004271FF00FFEBD600A53C0800A53C0800A53C0800FFDFBD00009ACE00009A
      CE00009ACE00FFDFBD008C492100000000000000000000000000000000000000
      0000CE6900000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD713900E7A67300E7BE
      9400FFFBF700FFF7E700FFF3DE00EFE3CE00EFDFC600F7EFD600EFB68400EFCF
      AD00E7D3B500BD590800A538000000000000AD4D2100FFFFFF004271FF004271
      FF004271FF00FFEFDE00A53C0800A53C0800A53C0800FFE3BD00009ACE00009A
      CE00009ACE00FFDFBD008C492100000000000000000000000000000000000000
      0000CE6900000000000000000000000000009C3808009C3808009C3808009C38
      08009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C67D4A00E7A673003182
      E7003182E700FFFBF700FFFBEF0084867B0039516300EFE7CE00EFCFAD002169
      F7002169F700BD590800A538000000000000AD4D2100FFFFFF004271FF004271
      FF004271FF00FFF3E700A53C0800A53C0800A53C0800FFE3C600009ACE00009A
      CE00009ACE00FFDFBD008C492100000000000000000000000000000000000000
      0000CE690000CE690000CE690000CE6900009C380800FF9A9400EFA25A00DE86
      29009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD693100E7AE7B00E7C3
      9C00FFFBF700FFFBF700FFFBF700EFE3CE0084867B0073717300EFCFAD00EFCF
      AD00EFCFAD00BD590800A538000000000000AD4D2100FFFFFF00FFFFFF00FFFF
      FF00FFFBF700FFF7F700FFF7EF00FFF3E700FFEFD600FFEBD600FFE7CE00FFE3
      C600FFDFBD00FFDFBD008C492100000000000000000000000000000000000000
      0000CE6900000000000000000000000000009C3808009C3808009C3808009C38
      08009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD490800E7A67300E7AE
      7B00FFFBF700FFFBF700FFFBF700FFFBF700FFFBF700AD869C0073717300EFE3
      C600E7AA7300BD590800A538000000000000AD4D2100FFFFFF00CE9A9C00CE9A
      9C00CE9A9C00FFFFFF00E77D0000E77D0000E77D0000FFEFDE00009A0000009A
      0000009A0000FFDFBD008C492100000000000000000000000000000000000000
      0000CE6900000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BD754200E7A6
      7300E7C7A500FFFBF700FFFBF700FFFBF700FFFBF700FFFBF700E7D7BD00E7C3
      9C00BD611800B55108000000000000000000AD4D2100FFFFFF00CE9A9C00CE9A
      9C00CE9A9C00FFFFFF00E77D0000E77D0000E77D0000FFF3E700009A0000009A
      0000009A0000FFE3C6008C492100000000000000000000000000000000000000
      0000CE6900000000000000000000000000009C3808009C3808009C3808009C38
      08009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5380000CE8E
      5200E7A66B00E7BE9400FFF7E7003182E700F7EBF700FFFBF700E7C7A500BD65
      2100BD590800A53800000000000000000000AD4D2100FFFFFF00CE9A9C00CE9A
      9C00CE9A9C00FFFFFF00E77D0000E77D0000E77D0000FFF7EF00009A0000009A
      0000009A0000FFE7CE008C452100000000000000000000000000000000000000
      0000CE690000CE690000CE690000CE6900009C380800FF9A9400EFA25A00DE86
      29009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A538
      0000CE824A00DE9E6300DE9E63009C8E8400DE9E6B00D6925A00BD652100BD5D
      1000A5380000000000000000000000000000AD4D2100E7E7E700E7E7E700E7E7
      E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E3E700E7E3
      DE00E7DFD600E7DBCE00944D2900000000000000000000000000000000000000
      0000CE6900000000000000000000000000009C3808009C3808009C3808009C38
      08009C3808000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5380000A5380000BD693100BD652100BD652100BD5D1000A5380000A538
      000000000000000000000000000000000000AD5D2100AD612100AD5D2100AD5D
      2100AD5D2100AD5D2100AD5D2100AD5D2100AD5D2100AD612100AD612100AD61
      2100AD612100AD6129008C4921000000000000000000000000009C3808009C38
      08009C3808009C3808009C380800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AD450000AD450000AD450000A5380000000000000000
      000000000000000000000000000000000000AD5D2100EF963100EF963100EF96
      3100EF963100EF963100EF963100EF963100EF963100EF963100EF963100EF96
      3100EF963100EF963100BD5D18000000000000000000000000009C380800FF9A
      9400EFA25A00DE8629009C380800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A5380000BD713900C67D4A00BD590800B54D0800A53C00000000
      00000000000000000000000000000000000000000000CE711800CE711800CE71
      1800CE711800CE711800CE711800CE711800CE751800CE751800CE711800CE75
      1800CE711800CE711800000000000000000000000000000000009C3808009C38
      08009C3808009C3808009C380800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A5380000A5380000A5380000A5380000A5380000A53800000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F81FFFFFFFFF0000E007FFFFF7070000
      C0030001F0070000C0030001F707000080010001F7FF000080010001F7070000
      80010001F007000080010001F707000080010001F7FF0000C0030001F7070000
      C0030001F0070000E0070001F7070000F00F0001C1FF0000FC3F0001C1FF0000
      F81F8003C1FF0000F81FFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
end
