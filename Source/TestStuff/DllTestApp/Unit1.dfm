object Form1: TForm1
  Left = 331
  Top = 336
  Caption = 'Form1'
  ClientHeight = 310
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    641
    310)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 444
    Height = 234
    Caption = 
      'Short description:'#13#10'- this demo program will automatically load ' +
      '"AsmProfiler.dll" '#13#10'  (needs to be placed in same directory of e' +
      'xecutable or in search path)'#13#10'- the demo program is compiled wit' +
      'h following compiler options:'#13#10'  - stack frames ON'#13#10'  - optimiza' +
      'tion OFF'#13#10'  - Linker: Map file = detailed  '#13#10'  Map file is most ' +
      'important: it is used to determine which classes and functions c' +
      'an be profiled.'#13#10#13#10'How to use:'#13#10'- first, select which items you ' +
      'want to profile ("Select Items" button)'#13#10'  (hint: you can sort o' +
      'n unit name and use multi select)'#13#10'- then: Start! :-)'#13#10'- press o' +
      'ne of the test buttons on the right side'#13#10'- stop the profiler'#13#10'-' +
      ' "Show results" will give you an overview of the results'#13#10'  (hin' +
      't: double click on an item will jump to details, and you can als' +
      'o change the thread)'#13#10'  '
  end
  object btnLoadDll: TBitBtn
    Left = 480
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Load dll'
    Enabled = False
    TabOrder = 0
    Visible = False
    OnClick = btnLoadDllClick
  end
  object btnShowProfiler: TBitBtn
    Left = 464
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Show profiler'
    Enabled = False
    TabOrder = 1
    Visible = False
    OnClick = btnShowProfilerClick
  end
  object btnExecuteFunction: TBitBtn
    Left = 472
    Top = 224
    Width = 97
    Height = 25
    Caption = 'Execute function'
    Enabled = False
    TabOrder = 3
    Visible = False
    OnClick = btnExecuteFunctionClick
  end
  object btnAllBusyFunctionsOnce_1861msec: TButton
    Left = 473
    Top = 8
    Width = 153
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'All busy functions once'
    TabOrder = 4
    OnClick = btnAllBusyFunctionsOnce_1861msecClick
  end
  object btnAllSleepFunctionsOnce_1861msec: TButton
    Left = 473
    Top = 39
    Width = 153
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'All sleep functions once'
    TabOrder = 5
    OnClick = btnAllSleepFunctionsOnce_1861msecClick
  end
  object btn10xAllBusyFunctions: TButton
    Left = 473
    Top = 88
    Width = 153
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '10x all busy functions'
    TabOrder = 6
    OnClick = btn10xAllBusyFunctionsClick
  end
  object btn10xAllSleepFunctions: TButton
    Left = 473
    Top = 119
    Width = 153
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '10x all sleep functions'
    TabOrder = 7
    OnClick = btn10xAllSleepFunctionsClick
  end
  object BitBtn1: TBitBtn
    Left = 472
    Top = 200
    Width = 75
    Height = 25
    Caption = 'BitBtn1'
    Enabled = False
    TabOrder = 8
    Visible = False
    OnClick = BitBtn1Click
  end
  object btnAddFunction: TBitBtn
    Left = 464
    Top = 216
    Width = 121
    Height = 25
    Caption = 'Add custom function'
    Enabled = False
    TabOrder = 2
    Visible = False
    OnClick = btnAddFunctionClick
  end
  object Button1: TButton
    Left = 510
    Top = 161
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 9
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Check'
    TabOrder = 10
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 104
    Top = 256
    Width = 105
    Height = 25
    Caption = 'dummy form test'
    TabOrder = 11
    OnClick = Button4Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.profilerun'
    Filter = 'AsmProfiler run (*.profilerun)|*.profilerun'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 552
    Top = 216
  end
end
