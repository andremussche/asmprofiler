object Form1: TForm1
  Left = 69
  Top = 191
  Caption = 'Form1'
  ClientHeight = 545
  ClientWidth = 949
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    949
    545)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 104
    Top = 7
    Width = 75
    Height = 25
    Caption = 'Single test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 192
    Top = 7
    Width = 89
    Height = 25
    Caption = 'CPU cycle test'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 38
    Width = 489
    Height = 457
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Memo1')
    TabOrder = 5
    WordWrap = False
  end
  object btnProfileSelected: TBitBtn
    Left = 8
    Top = 504
    Width = 89
    Height = 25
    Caption = 'Profile selected'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 6
    Visible = False
  end
  object BitBtn2: TBitBtn
    Left = 192
    Top = 512
    Width = 75
    Height = 25
    Caption = 'BitBtn2'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 7
    Visible = False
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 328
    Top = 7
    Width = 89
    Height = 25
    Caption = 'AddCustomProcs'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
    Visible = False
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 424
    Top = 7
    Width = 75
    Height = 25
    Caption = 'Test custom'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 3
    Visible = False
    OnClick = BitBtn4Click
  end
  object btnLoad: TBitBtn
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 4
    Visible = False
    OnClick = btnLoadClick
  end
  object Button3: TButton
    Left = 536
    Top = 8
    Width = 75
    Height = 25
    Caption = 'list dbg'
    TabOrder = 8
    Visible = False
    OnClick = Button3Click
  end
  object btnAllBusyFunctionsOnce_1861msec: TButton
    Left = 536
    Top = 80
    Width = 153
    Height = 25
    Caption = 'All busy functions once'
    TabOrder = 9
    OnClick = btnAllBusyFunctionsOnce_1861msecClick
  end
  object btnAllSleepFunctionsOnce_1861msec: TButton
    Left = 536
    Top = 111
    Width = 153
    Height = 25
    Caption = 'All sleep functions once'
    TabOrder = 10
    OnClick = btnAllSleepFunctionsOnce_1861msecClick
  end
  object btn10xAllBusyFunctions: TButton
    Left = 536
    Top = 160
    Width = 153
    Height = 25
    Caption = '10x all busy functions'
    TabOrder = 11
    OnClick = btn10xAllBusyFunctionsClick
  end
  object btn10xAllSleepFunctions: TButton
    Left = 536
    Top = 191
    Width = 153
    Height = 25
    Caption = '10x all sleep functions'
    TabOrder = 12
    OnClick = btn10xAllSleepFunctionsClick
  end
  object BitBtn1: TBitBtn
    Left = 536
    Top = 240
    Width = 75
    Height = 25
    Caption = 'BitBtn1'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 13
    Visible = False
    OnClick = BitBtn1Click
  end
  object BitBtn5: TBitBtn
    Left = 536
    Top = 280
    Width = 75
    Height = 25
    Caption = 'BitBtn5'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 14
    Visible = False
    OnClick = BitBtn5Click
  end
  object Button4: TButton
    Left = 536
    Top = 336
    Width = 75
    Height = 25
    Caption = 'loaded dlls'
    TabOrder = 15
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 536
    Top = 392
    Width = 57
    Height = 21
    TabOrder = 16
    Text = '100'
  end
  object BitBtn6: TBitBtn
    Left = 599
    Top = 390
    Width = 113
    Height = 25
    Caption = 'Execute test x times'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 17
    OnClick = BitBtn6Click
  end
  object Button5: TButton
    Left = 536
    Top = 424
    Width = 75
    Height = 25
    Caption = 'level test'
    TabOrder = 18
    OnClick = Button5Click
  end
  object BitBtn7: TBitBtn
    Left = 536
    Top = 464
    Width = 75
    Height = 25
    Caption = 'exception test'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 19
    OnClick = BitBtn7Click
  end
end
