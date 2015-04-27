object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Minidump reader'
  ClientHeight = 726
  ClientWidth = 693
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblDumpFile: TLabel
    Left = 143
    Top = 13
    Width = 53
    Height = 13
    Caption = 'lblDumpFile'
  end
  object lblExeFile: TLabel
    Left = 143
    Top = 44
    Width = 44
    Height = 13
    Caption = 'lblExeFile'
  end
  object btnOpenDumpFile: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Load dump file'
    TabOrder = 0
    OnClick = btnOpenDumpFileClick
  end
  object btnOpenDebugFile: TButton
    Left = 8
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Load original exe'
    TabOrder = 1
    OnClick = btnOpenDebugFileClick
  end
  object btnShowMinidump: TButton
    Left = 8
    Top = 70
    Width = 129
    Height = 25
    Caption = 'Show minidump content'
    TabOrder = 2
    OnClick = btnShowMinidumpClick
  end
  object chkModules: TCheckBox
    Left = 143
    Top = 75
    Width = 97
    Height = 17
    Caption = 'Modules'
    TabOrder = 3
  end
  object chkThreads: TCheckBox
    Left = 231
    Top = 75
    Width = 97
    Height = 17
    Caption = 'Threads'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chkHandles: TCheckBox
    Left = 319
    Top = 75
    Width = 97
    Height = 17
    Caption = 'Handles'
    TabOrder = 5
  end
  object chkDontCheckValidCallSite: TCheckBox
    Left = 432
    Top = 75
    Width = 229
    Height = 17
    Caption = 'Raw stack tracing (for compressed exes?)'
    TabOrder = 6
  end
  object pgMain: TPageControl
    Left = 0
    Top = 102
    Width = 693
    Height = 624
    ActivePage = tsCustom
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 7
    object tsMinidump: TTabSheet
      Caption = 'Minidump'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 685
        Height = 596
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsCustom: TTabSheet
      Caption = 'Custom'
      ImageIndex = 1
      DesignSize = (
        685
        596)
      object Label1: TLabel
        Left = 3
        Top = 11
        Width = 94
        Height = 13
        Caption = 'Memory addresses:'
      end
      object btnGetCustomLocationInfo: TButton
        Left = 4
        Top = 216
        Width = 109
        Height = 25
        Caption = 'Get location info'
        TabOrder = 0
        OnClick = btnGetCustomLocationInfoClick
      end
      object mmoCustomMemAddresses: TMemo
        Left = 3
        Top = 27
        Width = 679
        Height = 183
        Lines.Strings = (
          'mmoCustomMemAddresses')
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object mmoCustomLocationInfo: TMemo
        Left = 3
        Top = 247
        Width = 679
        Height = 346
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'mmoCustomMemAddresses')
        ScrollBars = ssBoth
        TabOrder = 2
      end
    end
  end
  object dlgOpenDumpFile: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Minidump (*.dmp)'
        FileMask = '*.dmp'
      end
      item
        DisplayName = 'Minidump (other extension)'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist]
    Left = 280
  end
  object dlgOpenOrgExe: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Executable with embeded debug info (*.exe)'
        FileMask = '*.exe'
      end
      item
        DisplayName = 'Map file (*.map)'
        FileMask = '*.map'
      end
      item
        DisplayName = 'JDBG file (*.jdbg)'
        FileMask = '*.jdbg'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist]
    Left = 384
  end
end
