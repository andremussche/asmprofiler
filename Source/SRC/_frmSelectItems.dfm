object frmSelectItems: TfrmSelectItems
  Left = 344
  Top = 289
  Caption = 'Select profile items'
  ClientHeight = 619
  ClientWidth = 691
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    691
    619)
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 608
    Top = 586
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 527
    Top = 586
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    DoubleBuffered = True
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
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnOKClick
  end
  object pgMain: TPageControl
    Left = 0
    Top = 0
    Width = 691
    Height = 577
    ActivePage = tsMapFile
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object tsMapFile: TTabSheet
      Caption = 'Map file items'
      inline framMapfile: TframUnitTreeview
        Left = 0
        Top = 0
        Width = 683
        Height = 549
        Align = alClient
        TabOrder = 0
        inherited vtreeItems: TVirtualStringTree
          Height = 518
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toSimpleDrawSelection]
        end
        inherited Panel1: TPanel
          Top = 518
          inherited Label2: TLabel
            Width = 93
          end
          inherited lblIncrSearch: TLabel
            Width = 6
          end
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Internal items'
      ImageIndex = 2
      inline framInternalItems: TframUnitTreeview
        Left = 0
        Top = 0
        Width = 683
        Height = 549
        Align = alClient
        TabOrder = 0
        inherited vtreeItems: TVirtualStringTree
          Height = 518
          TreeOptions.SelectionOptions = [toMultiSelect, toSimpleDrawSelection]
        end
        inherited Panel1: TPanel
          Top = 518
          inherited Label2: TLabel
            Width = 93
          end
          inherited lblIncrSearch: TLabel
            Width = 6
          end
        end
      end
    end
    object tsLoadedDlls: TTabSheet
      Caption = 'Program loaded dlls'
      ImageIndex = 6
      inline framLoadedDlls: TframUnitTreeview
        Left = 0
        Top = 0
        Width = 683
        Height = 549
        Align = alClient
        TabOrder = 0
        inherited vtreeItems: TVirtualStringTree
          Height = 518
          TreeOptions.SelectionOptions = [toMultiSelect, toSimpleDrawSelection]
        end
        inherited Panel1: TPanel
          Top = 518
          inherited Label2: TLabel
            Width = 93
          end
          inherited lblIncrSearch: TLabel
            Width = 6
          end
        end
      end
    end
    object tsProgramImports: TTabSheet
      Caption = 'Program imports/exports'
      ImageIndex = 5
      object mmoImports: TMemo
        Left = 3
        Top = 40
        Width = 326
        Height = 497
        Lines.Strings = (
          'mmoImports')
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object mmoExports: TMemo
        Left = 335
        Top = 40
        Width = 326
        Height = 497
        Lines.Strings = (
          'Memo2')
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object btnLoadProgramImportExport: TButton
        Left = 3
        Top = 9
        Width = 86
        Height = 25
        Caption = 'Load'
        TabOrder = 2
        OnClick = btnLoadProgramImportExportClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Custom items'
      ImageIndex = 2
      inline framCustomItems: TframUnitTreeview
        Left = 0
        Top = 0
        Width = 683
        Height = 549
        Align = alClient
        TabOrder = 0
        inherited vtreeItems: TVirtualStringTree
          Height = 518
          TreeOptions.SelectionOptions = [toMultiSelect, toSimpleDrawSelection]
        end
        inherited Panel1: TPanel
          Top = 518
          inherited Label2: TLabel
            Width = 93
          end
          inherited lblIncrSearch: TLabel
            Width = 6
          end
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Plugins'
      ImageIndex = 4
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 421
        Height = 39
        Caption = 
          'Here will come some extra plugins, like monitoring CPU, HD, Netw' +
          'erk, Memory usage etc.'#13#10#13#10'These will be monitored by interval (2' +
          '50mec?) and parallel on the profiling process.'
      end
    end
    object tsLoadedDllsOld: TTabSheet
      Caption = 'Loaded dlls (old/raw)'
      ImageIndex = 3
      object btnLoadedModules: TButton
        Left = 3
        Top = 1
        Width = 75
        Height = 25
        Caption = 'Load'
        TabOrder = 0
        OnClick = btnLoadedModulesClick
      end
      object mmoModuleImportExports: TMemo
        Left = 319
        Top = 32
        Width = 310
        Height = 505
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object lbModules: TListBox
        Left = 3
        Top = 32
        Width = 310
        Height = 505
        ItemHeight = 13
        TabOrder = 2
        OnClick = lbModulesClick
      end
    end
  end
end
