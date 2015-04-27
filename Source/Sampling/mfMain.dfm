object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Delphi Stack Viewer'
  ClientHeight = 564
  ClientWidth = 946
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
  object Splitter1: TSplitter
    Left = 397
    Top = 0
    Height = 564
  end
  object Splitter2: TSplitter
    Left = 279
    Top = 0
    Height = 564
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 2
    Top = 2
    Width = 275
    Height = 560
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alLeft
    Caption = ' Processes '
    TabOrder = 0
    object lbProcesses: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 265
      Height = 507
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbProcessesClick
    end
    object pnlProcess: TPanel
      Left = 2
      Top = 528
      Width = 271
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnRefreshProcesses: TButton
        Left = 3
        Top = 1
        Width = 78
        Height = 25
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = btnRefreshProcessesClick
      end
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 284
    Top = 2
    Width = 111
    Height = 560
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alLeft
    Caption = ' Threads '
    TabOrder = 1
    object pnlThreads: TPanel
      Left = 2
      Top = 528
      Width = 107
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object btnRefreshThreads: TButton
        Left = 3
        Top = 1
        Width = 78
        Height = 25
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = btnRefreshThreadsClick
      end
    end
    object lbThreads: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 101
      Height = 507
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      OnClick = lbThreadsClick
    end
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 402
    Top = 2
    Width = 542
    Height = 560
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    Caption = ' Stack trace '
    TabOrder = 2
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 532
      Height = 451
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object pnlStack: TPanel
      Left = 2
      Top = 472
      Width = 538
      Height = 86
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 203
        Top = 6
        Width = 95
        Height = 13
        Caption = 'Snapshot duration: '
      end
      object lblDuration: TLabel
        Left = 300
        Top = 6
        Width = 22
        Height = 13
        Caption = '0 ms'
      end
      object btnRefreshStack: TButton
        Left = 4
        Top = 1
        Width = 78
        Height = 25
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = btnRefreshStackClick
      end
      object chkAutoRefresh: TCheckBox
        Left = 88
        Top = 5
        Width = 105
        Height = 17
        Caption = 'Auto refresh (1s)'
        TabOrder = 1
        OnClick = chkAutoRefreshClick
      end
      object chkRaw: TCheckBox
        Left = 116
        Top = 32
        Width = 109
        Height = 17
        Caption = 'Raw stack walking'
        Enabled = False
        TabOrder = 2
      end
      object Button1: TButton
        Left = 4
        Top = 28
        Width = 105
        Height = 25
        Caption = 'Try "StackWalk64"'
        TabOrder = 3
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 4
        Top = 57
        Width = 75
        Height = 25
        Caption = 'Start sampling'
        TabOrder = 4
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 85
        Top = 57
        Width = 75
        Height = 25
        Caption = 'Stop sampling'
        TabOrder = 5
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 166
        Top = 57
        Width = 75
        Height = 25
        Caption = 'Results'
        TabOrder = 6
        OnClick = Button4Click
      end
    end
  end
  object tmrRefresh: TTimer
    Enabled = False
    OnTimer = tmrRefreshTimer
    Left = 304
    Top = 32
  end
end
